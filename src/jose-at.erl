-module('jose-at').

-export([
    main/1
]).

-define(Usage,
    "Usage: jose-at <Command>~n"
    "~n"
    "Commands~n"
    "       run         Run~n"
    "       help        Print this message~n"
    "       version     Print version~n"
    "~n"
).

-define(Info,
    "~n"
    "See details at: \033[4;36mhttp://localhost:~b/index.html\033[0m~n"
    "~n"
    "Note: the link will not be available after exit.~n"
    "~n"
).
-define(Prompt,
    "Press 'Enter' to repeat or 'q' to exit: "
).
-define(CmdRepeat, "").
-define(CmdExit, "q").

-define(Version, "Version ~s (git-~s)~n").

main(Args) ->
    application:start(?MODULE),
    case Args of
        ["run"] -> run();
        ["help"] -> show_usage();
        ["version"] -> show_version();
        Args -> show_usage()
    end.

run() ->
    application:ensure_all_started(?MODULE),
    cpf_node:start(?MODULE),

    TestConfig = test_config(),
    ensure_log_dir(proplists:get_value(logdir, TestConfig)),
    extract_files(TestConfig),
    test_loop(TestConfig),

    cpf_node:stop().

test_loop(Config) ->
    ct:run_test(Config),
    io:format(?Info, [proplists:get_value(port, Config)]),
    console_loop(Config).

console_loop(Config) ->
    case read_input(?Prompt) of
        eof -> eof;
        ?CmdExit -> ok;
        ?CmdRepeat -> test_loop(Config);
        _Command -> console_loop(Config)
    end.
    
read_input(Prompt) ->
    case io:get_line(Prompt) of
        eof -> eof;
        Data -> string:trim(Data, both, "\n")
    end.

ensure_log_dir(LogDir) ->
    is_list(LogDir) andalso filelib:ensure_dir(LogDir ++ "/").

test_config() ->
    {ok, Env} = application:get_key(?MODULE, env),
    lists:filtermap(fun
        ({port, Value}) ->   {true, {port, Value}};
        ({config, Value}) -> {true, {config, Value}};
        ({logdir, Value}) -> {true, {logdir, Value}};
        ({suites, Value}) -> {true, {dir, Value}};
        (_Other) -> false
    end, Env).

extract_files(Config) ->
    {ok, Binary} = file:read_file(escript:script_name()),
    [_Header, Zip] = binary:split(Binary, <<"PK">>),
    {ok, Files} = zip:extract(<<"PK", Zip/binary>>, [memory]),
    lists:foreach(extract_file_fun(Config), Files).

extract_file_fun(Config) -> fun({Name, Content}) ->
    SuitesDir = proplists:get_value(dir, Config),
    ConfigFile = proplists:get_value(config, Config),

    IsSuite = lists:suffix("SUITE.erl", Name) orelse
              lists:suffix("SUITE.beam", Name),
    IsConfig = lists:suffix(".conf", Name),

    IsSuite andalso begin
        is_list(SuitesDir) andalso filelib:ensure_dir(SuitesDir ++ "/"),
        SuiteFile = filename:join(SuitesDir, filename:basename(Name)),
        file:write_file(SuiteFile, Content)
    end,
    IsConfig andalso file:write_file(ConfigFile, Content)
end.

show_usage() ->
    io:format(?Usage).

show_version() ->
    {ok, Vsn} = application:get_key(?MODULE, vsn),
    {ok, GitSha} = application:get_env(?MODULE, git_sha),
    io:format(?Version, [Vsn, GitSha]).
