-module('jose-at_test').

-include_lib("eunit/include/eunit.hrl").

-define(M, 'jose-at').

main_test() ->
    ?assertEqual(ok, ?M:main([])).
