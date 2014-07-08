-module(ets_cache_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    application:load(ets_cache),
    ok = application:set_env(ets_cache, clean_interval, 1),
    application:start(ets_cache).

%% 由于get_from_mnesia是内部使用，要单元测试就要启动mnesia，这里就不做这个函数的单元测试
all_test_() ->
    start(),
    [get_and_set(), ttl_die(), ttl_live(), get_with_default()].

get_and_set() ->
    [?_assertEqual([], ets_cache:get(test_key)),
     ?_assertEqual(ok, ets_cache:set(test_key, 1)),
     ?_assertEqual(1, ets_cache:get(test_key)),
     ?_assertEqual(ok, ets_cache:del(test_key)),
     ?_assertEqual([], ets_cache:get(test_key))
    ].

ttl_die() ->
    ets_cache:set(ttl_die_key, 1, 1),
    timer:sleep(2100),
    [?_assertEqual([], ets_cache:get(ttl_die_key))].

ttl_live() ->
    ets_cache:set(ttl_live_key, 1, 1),
    [?_assertEqual(1, ets_cache:get(ttl_live_key))].

get_with_default() ->
    F = fun() ->
                {expiration, 1, 600}
        end,
   [?_assertEqual(1, ets_cache:get_with_default(get_with_default, F)),
    ?_assertEqual(1, ets_cache:get(get_with_default))].
