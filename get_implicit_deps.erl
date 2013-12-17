#!/usr/bin/env escript
%%% vim: set ts=4 sts=4 sw=4 et:

-define('TMP_FILENAME', "/tmp/get_implicit_deps.tmp").

main([Help]) when Help =:= "?"; Help =:= "-h"; Help =:= "--help"; Help =:= "help" ->
    io:format("Usage:~n$0 <dirname>~n");

main([Dirname]) ->
    FullDirname = filename:join(cwd(), Dirname),
    case filelib:is_dir(FullDirname) of
        true ->
            parse(FullDirname);
        false ->
            case filelib:is_dir(Dirname) of
                true ->
                    parse(Dirname);
                false ->
                    io:format("Directory does not exists: ~s~n", [Dirname]),
                    exit(1)
            end
    end;
main([]) ->
    parse(cwd()).

parse(Dirname) ->
    Lines = lines(Dirname),
    Data = data(Lines),

    AppModules = app_modules(Data),
    DepsModules = deps_modules(Dirname),
    Functions = functions(Data),
    ImplicitDeps = implicit_deps(Functions, AppModules ++ DepsModules),

    display(ImplicitDeps).

lines(Dirname) ->
    ok = cmd("rm -f " ++ ?TMP_FILENAME),

    Grep = fun(Pattern) ->
        cmd("cd " ++ Dirname ++ " && grep -nIR \"" ++ Pattern ++ "\" * | grep -v \"\.eunit\" >> " ++ ?TMP_FILENAME)
    end,
    ok = Grep("[0-9a-z]*:[0-9a-z]*"),
    ok = Grep("apply(\\s*[0-9a-z]*\\s*,\\s*[0-9a-z]*\\s*,"),
    ok = Grep("module\\([0-9a-z]*\\)"),

    Lines = read_file(?TMP_FILENAME),
    ok = cmd("rm " ++ ?TMP_FILENAME),
    Lines.

display(ImplicitDeps) ->
    MaxLen = lists:foldl(fun({MF, _File}, Len) ->
        case length(mf2str(MF)) of
            L when L < Len ->
                Len;
            L ->
                L
        end
    end, 0, ImplicitDeps),
    lists:foldl(fun({{M, _F} = MF, {Filename, Fileline}}, Module) ->
        case Module of
            M ->
                nop;
            _ ->
                io:format("~n==== ~s ====~n", [string:to_upper(atom_to_list(M))])
        end,
        S = mf2str(MF),
        io:format("~s~s~s:~p~n", [S, string:copies(" ", MaxLen - length(S) + 1), Filename, Fileline]),
        M
    end, undefined, ImplicitDeps).

implicit_deps(Functions, Modules) ->
    lists:reverse(lists:foldl(fun({{M, F}, _File} = MF, R) ->
        case lists:member(M, Modules) of
            true ->
                R;
            false ->
                case can_apply(M, F) of
                    true ->
                        R;
                    false ->
                        [MF | R]
                end
        end
    end, [], Functions)).

data(Lines) ->
    [case re:run(Line, "^([^:]+):(\\d+):\s*(.+)$", [{capture, all, list}]) of
        {match, [_, Filename, Fileline, Code]} ->
            {{Filename, list_to_integer(Fileline)}, Code}
    end || Line <- Lines].

app_modules(Data) ->
    Modules = lists:foldl(fun({_File, Code}, R) ->
        case re:run(Code, "module\\((\\w+)\\)", [{capture, all, list}]) of
            {match, [_, Module]} ->
                [Module | R];
            nomatch ->
                R
        end
    end, [], Data),
    [list_to_atom(Module) || Module <- Modules].

deps_modules(Dirname) ->
    Filename = filename:join(Dirname, "rebar.config"),
    case filelib:is_file(Filename) of
        false ->
            [];
        true ->
            {ok, Data} = file:consult(Filename),
            Deps = proplists:get_value(deps, Data),
            [Module || {Module, _V, _O} <- Deps]
    end.

functions(Data) ->
    Functions = lists:foldl(fun({{Filename, _Fileline} = File, Code}, R) ->
        case re:run(Filename, "\\.erl$", [{capture, all, list}]) of
            {match, _} -> 
                R ++ case re:run(Code, "(\\w+):(\\w+)", [global, {capture, all, list}]) of
                    {match, Match1} ->
                        [{{M1, F1}, File} || [_, M1, F1] <- Match1];
                    nomatch ->
                        []
                end     
                ++ case re:run(Code, "apply\\(\\s*(\\w+)\\s*,\\s*(\\w+)\\s*,", [global, {capture, all, list}]) of
                    {match, Match2} ->
                        [{{M2, F2}, File} || [_, M2, F2] <- Match2];
                    nomatch ->
                        []
                end;
            nomatch ->
                R
        end
    end, [], Data),
    lists:usort([{{list_to_atom(M), list_to_atom(F)}, File} || {{M, F}, File} <- Functions]).

% private functions

read_file(Filename) ->
    case file:open(Filename, [raw, binary, read, exclusive, read_ahead]) of
        {ok, Handle} ->
            try
                read_file_line(Handle, [])
            after
                file:close(Handle)
            end;
        {error, Reason} ->
            erlang:raise(error, {Reason, Filename}, [])
    end.
    
read_file_line(Handle, Lines) ->
    case file:read_line(Handle) of
        {ok, Line} ->
            read_file_line(Handle, [trim(binary_to_list(Line)) | Lines]);
        eof ->
            lists:reverse(Lines)
    end.

trim(String) ->
    string:strip(String, right, $\n).

cmd(Cmd) ->
    Seed = "OK" ++ integer_to_list(random:uniform(899) + 100),
    case trim(os:cmd(Cmd ++ " && echo '" ++ Seed ++ "'")) of
        "" ->
            ok;
        Seed ->
            ok;
        Reason ->
            io:format("~s~nERROR:~n'~s'~n", [Cmd, Reason]),
            error
    end.

cwd() ->
    {ok, Dirname} = file:get_cwd(),
    Dirname.

mf2str({M, F}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F).

can_apply('MODULE', _) -> true;
can_apply(application, _) -> true;
can_apply(erlang, _) -> true;
can_apply(gen_server, _) -> true;
can_apply(io, _) -> true;
can_apply(lists, _) -> true;
can_apply(proplists, _) -> true;
can_apply(supervisor, _) -> true;
can_apply(timer, _) -> true;
can_apply(M, F) ->
    case can_apply(M, F, [], 10) of
        true ->
            io:format("WARNING: ~s:~s~n function exists.", [M, F]),
            true;
        false -> false
    end.

can_apply(_M, _F, _Args, 0) -> false;
can_apply(M, F, Args, N) ->
    try
        apply(M, F, Args),
        true
    catch
        error:undef ->
            can_apply(M, F, [undefined | Args], N - 1);
        _:_ ->
            true
    end.
