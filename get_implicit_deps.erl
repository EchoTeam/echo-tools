#!/usr/bin/env escript
%%% vim: set ts=4 sts=4 sw=4 et:

-define('TMP_FILENAME', "/tmp/get_implicit_deps.tmp").
-define('ATOM_PATTERN', "[a-z]+\\w+").

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
        case re:run(Code, "module\\((" ++ ?ATOM_PATTERN ++ ")\\)", [{capture, all, list}]) of
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
            case proplists:get_value(deps, Data) of
                undefined ->
                    [];
                Deps ->
                    lists:map(fun({Module, _V, _O}) ->
                        Module
                    end, Deps)
            end
    end.

functions(Data) ->
    Functions = lists:foldl(fun({{Filename, _Fileline} = File, Code}, R) ->
        case re:run(Filename, "\\.erl$", [{capture, all, list}]) of
            {match, _} -> 
                R ++ case re:run(Code, "(" ++ ?ATOM_PATTERN ++ "):(" ++ ?ATOM_PATTERN ++ ")",
                                            [global, {capture, all, list}]) of
                    {match, Match1} ->
                        [{{M1, F1}, File} || [_, M1, F1] <- Match1];
                    nomatch ->
                        []
                end     
                ++ case re:run(Code, "apply\\s*\\(\\s*(" ++ ?ATOM_PATTERN ++ ")\\s*,\\s*(" ++ ?ATOM_PATTERN ++ ")\\s*,",
                                        [global, {capture, all, list}]) of
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

can_apply(M, F) ->
    case lists:member(M, ['MODULE',
        application,
        application_controller,
        application_master,
        asn1ct,
        base64,
        binary,
        calendar,
        code,
        crypto,
        dict,
        digraph,
        digraph_utils,
        disk_log,
        erl_epmd,
        erl_parse,
        erl_scan,
        erl_syntax,
        erl_syntax_lib,
        erlang,
        error_logger,
        ets,
        file,
        filename,
        file_sorter,
        ftp,
        gb_trees,
        gen,
        gen_event,
        gen_server,
        gen_tcp,
        gen_udp,
        http_uri,
        httpc,
        httpd_util,
        inet,
        inet_db,
        inet_parse,
        inets,
        io,
        io_lib,
        lists,
        math,
        net_adm,
        ordsets,
        os,
        proplists,
        prim_inet,
        public_key,
        queue,
        random,
        re,
        rpc,
        sets,
        ssl,
        string,
        supervisor,
        sys,
        timer,
        unicode,
        xmerl_scan,
        zlib
    ]) of
        true ->
            true;
        false ->
            case can_apply(M, F, 0) of
                {true, N} ->
                    io:format("EXISTS: ~s:~s/~p~n", [M, F, N]),
                    true;
                false -> false
            end
    end.

can_apply(_M, _F, 10) -> false;
can_apply(M, F, N) ->
    try
        apply(M, F, lists:seq(1, N)),
        {true, N}
    catch
        error:undef ->
            can_apply(M, F, N + 1);
        _:_ ->
            {true, N}
    end.
