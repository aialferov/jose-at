-module(jose_at_SUITE).

-export([
    all/0, suite/0,

    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,

    flatten_empty/1,
    flatten_no_deps/1,
    flatten_invalid_format/1,

    scriptize_empty/1,
    scriptize_with_deps/1,

    default_task/1,
    bad_request/1
]).

-include_lib("common_test/include/ct.hrl").

-define(Endpoint, "http://~s:~b/api/v1/jobs").
-define(ContentType, "application/json").
-define(Options, [{body_format, binary}]).

all() -> [
    flatten_empty,
    flatten_no_deps,
    flatten_invalid_format,

    scriptize_empty,
    scriptize_with_deps,

    default_task,
    bad_request
].

suite() -> [
    {require, host},
    {require, port}
].

init_per_suite(Config) ->
    [{host, config(host)},
     {port, config(port)}|Config].

end_per_suite(Config) -> Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config) -> Config.

flatten_empty(Config) ->
    {Result, Request} = {
        {200, []},
        #{<<"tasks">> => []}
    },
    {ok, Result} = request("flatten", jsx:encode(Request), Config).

flatten_no_deps(Config) ->
    {Result, Request} = {
        {200, [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]},
        #{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]}
    },
    {ok, Result} = request("flatten", jsx:encode(Request), Config).

flatten_invalid_format(Config) ->
    {Result, Request} = {
        {400, #{<<"reason">> => <<"invalid_format">>}},
        #{<<"tsks">> => []}
    },
    {ok, Result} = request("flatten", jsx:encode(Request), Config).

scriptize_empty(Config) ->
    {Result, Request} = {
        {200, <<"#!/bin/bash\n\n">>},
        #{<<"tasks">> => []}
    },
    {ok, Result} = request("scriptize", jsx:encode(Request), Config).

scriptize_with_deps(Config) ->
    {Result, Request} = {
        {200, <<
            "#!/bin/bash\n\n"
            "touch /tmp/file1\n"
            "echo 'Hello World!' > /tmp/file1\n"
            "cat /tmp/file1\n"
            "rm /tmp/file1\n"
        >>},
        #{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>,
              <<"requires">> => [
                <<"task-3">>
              ]},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
              <<"requires">> => [
                <<"task-1">>
              ]},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>,
              <<"requires">> => [
                <<"task-2">>,
                <<"task-3">>
              ]}
        ]}
    },
    {ok, Result} = request("scriptize", jsx:encode(Request), Config).

default_task(Config) ->
    {Result, Request} = {
        {200, <<
            "#!/bin/bash\n\n"
            "touch /tmp/file1\n"
            "cat /tmp/file1\n"
        >>},
        #{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]}
    },
    {ok, Result} = request("", jsx:encode(Request), Config).

bad_request(Config) ->
    Endpoint = "http://~s:~b/api/v42/jobs",
    {ok, {400, _Usage}} = request(Endpoint, "", <<"">>, Config).

request(Action, Body, Config) ->
    request(?Endpoint, Action, Body, Config).

request(EndpointFormat, Action, Body, Config) ->
    {Host, Port} = {host(Config), port(Config)},
    Query = if Action == "" -> "";
               Action /= "" -> "?tasks=" ++ Action end,
    Endpoint = format(EndpointFormat ++ Query, [Host, Port]),

    Request = {Endpoint, ?ContentType, [], Body},
    code_body(httpc:request(post, Request, [], ?Options)).

code_body(Response) -> case Response of
    {ok, {{_Version, Code, _Reason}, _Headers, <<"">>}} -> {ok, {Code, <<"">>}};
    {ok, {{_Version, Code, _Reason}, Headers, Body}} ->
        {ok, case is_content_type_json(Headers) of
            true -> {Code, jsx:decode(Body, [return_maps])};
            false -> {Code, Body}
        end};
    {error, Reason} -> {error, Reason}
end.

is_content_type_json(Headers) ->
    proplists:get_value("content-type", Headers, "") == "application/json".

config(Name) ->
    config(Name, case os:getenv(string:uppercase(atom_to_list(Name))) of
        Value when is_list(Value) -> Value;
        false -> ct:get_config(Name)
    end).

config(port, Value) when is_list(Value) -> list_to_integer(Value);
config(_Name, Value) -> Value.

host(Config) -> proplists:get_value(host, Config).
port(Config) -> proplists:get_value(port, Config).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
