#!/usr/bin/env escript
%%% vim: set ts=4 sts=4 sw=4 et:

-define('TMP_FILENAME', "/tmp/get_implicit_deps.tmp").

main([Help]) when Help =:= "?"; Help =:= "-h"; Help =:= "--help"; Help =:= "help" ->
    io:format("Usage:~n$0[ <dirname>]~n");

main([Dirname]) ->
    FullDirname = filename:join(cwd(), Dirname),
    case {filelib:is_dir(FullDirname), filelib:is_dir(Dirname)} of
        {true, _} ->
            parse(FullDirname);
        {_, true} ->
            parse(Dirname);
        _ ->
            io:format("Directory does not exists: ~s~n", [Dirname]),
            exit(1)
    end;
main([]) ->
    parse(cwd()).

parse(Dirname) ->
    AppFile = Dirname ++ "/src/" ++ filename:basename(Dirname) ++ ".app.src",
    case filelib:is_file(AppFile) of
        true ->
            nop;
        false ->
            io:format("File does not exists: ~s~n", [AppFile]),
            exit(1)
    end,
    Data = data(lines(Dirname)),
    ImplicitDeps = implicit_deps(functions(Data), app_modules(Data) ++ deps_modules(Dirname)),
    display(ImplicitDeps).

lines(Dirname) ->
    ok = cmd("rm -f " ++ ?TMP_FILENAME),

    Grep = fun(Pattern, SearchInDeps) ->
        Params = case SearchInDeps of
            true -> "";
            false -> " --exclude-dir=deps"
        end,
        cmd("cd " ++ Dirname ++ " && grep -nIR \"" ++ Pattern ++ "\"" ++ Params ++ " * | grep -v \"\.eunit\" >> " ++ ?TMP_FILENAME)
    end,
    ok = Grep(regexp({function, v1}, bash, false), false),
    ok = Grep(regexp({function, v2}, bash, false), false),
    ok = Grep(regexp(module, bash, false), true),

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
        case code_search(Code, module, undefined) of
            [] ->
                R;
            [{{Module}, _}] ->
                [Module | R]
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
            case proplists:get_value(deps, Data) of
                undefined ->
                    [];
                Deps ->
                    lists:map(fun
                        ({Module, _V, _S}) -> Module;
                        ({Module, _V, _S, _O}) -> Module
                    end, Deps)
            end
    end.

functions(Data) ->
    Functions = lists:foldl(fun({{Filename, _Fileline} = File, Code}, R) ->
        case re:run(Filename, "\\.erl$", [{capture, all, list}]) of
            {match, _} -> 
                R
                    ++ code_search(Code, {function, v1}, File)
                    ++ code_search(Code, {function, v2}, File);
            nomatch ->
                R
        end
    end, [], Data),
    lists:usort([{{list_to_atom(M), list_to_atom(F)}, File} || {{M, F}, File} <- Functions]).

% private functions

regexp(lb, bash, _Save) ->
    "(";
regexp(lb, erlang, Save) ->
    "\\" ++ regexp(lb, bash, Save);
regexp(rb, bash, _Save) ->
    ")";
regexp(rb, erlang, Save) ->
    "\\" ++ regexp(rb, bash, Save);

regexp(atom, Type, true) ->
    "(" ++ regexp(atom, Type, false) ++ ")";
regexp(atom, _Type, false) ->
    "[a-zA-Z][a-zA-Z0-9_]*";

regexp(module, Type, Save) ->
    "module" ++ regexp(lb, Type, Save) ++ regexp(atom, Type, Save) ++ regexp(rb, Type, Save);
regexp({function, v1}, Type, Save) ->
    regexp(atom, Type, Save) ++ ":" ++ regexp(atom, Type, Save);
regexp({function, v2}, Type, Save) ->
    "apply\\s*" ++ regexp(lb, Type, Save) ++ "\\s*" ++ regexp(atom, Type, Save) ++ "\\s*,\\s*" ++ regexp(atom, Type, Save) ++ "\\s*,";
regexp(_, _, _) ->
    erlang:error(badarg).

code_search(Code, RegExpName, Info) ->
    case re:run(Code, regexp(RegExpName, erlang, true), [global, {capture, all, list}]) of
        {match, Match} ->
            [{list_to_tuple(T), Info} || [_ | T] <- Match];
        nomatch ->
            []
    end.

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

can_apply(error, _F) -> true;
can_apply(throw, _F) -> true;
can_apply(exit, _F) -> true;
can_apply('MODULE', _F) -> true;
can_apply(M, _F) ->
    case get(M) of
        undefined ->
            Result = module_exists(M),
            put(M, Result),
            Result;
        Result ->
            Result
    end.

module_exists(M) ->
    case code:which(M) of
        non_existing ->
            false;
        _ ->
            true
    end.
