-module(ets_cache_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    application:load(ets_cache),
    ok = application:set_env(ets_cache, clean_interval, 1),
    application:start(ets_cache),
    ets_cache:new_cache_table(test_cache_table, 2).

all_test_() ->
    start(),
    [get_and_set(), ttl_die(), ttl_live(), get_with_default(),
     get_and_set2(), ttl_die2(), ttl_live2(), get_with_default2(), all_tables()].

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
    [?_assertEqual(ok, ets_cache:set(ttl_live_key, 1, 1)),
     ?_assertEqual(1, ets_cache:get(ttl_live_key))].

get_with_default() ->
    F = fun() ->
                {expiration, 1, 600}
        end,
   [?_assertEqual(1, ets_cache:get_with_default(get_with_default, F)),
    ?_assertEqual(1, ets_cache:get(get_with_default))].


get_and_set2() ->
    [?_assertEqual([], ets_cache:get(test_cache_table, test_key)),
     ?_assertEqual(ok, ets_cache:set_t(test_cache_table, test_key, 1)),
     ?_assertEqual(1, ets_cache:get(test_cache_table, test_key)),
     ?_assertEqual(ok, ets_cache:del(test_cache_table, test_key)),
     ?_assertEqual([], ets_cache:get(test_cache_table, test_key))
    ].

ttl_die2() ->
    ets_cache:set_t(test_cache_table, ttl_die_key, 1, 1),
    timer:sleep(2100),
    [?_assertEqual([], ets_cache:get(test_cache_table, ttl_die_key))].

ttl_live2() ->    
    [?_assertEqual(ok, ets_cache:set_t(test_cache_table, ttl_live_key, 1, 1)),
     ?_assertEqual(1, ets_cache:get(test_cache_table, ttl_live_key))].

get_with_default2() ->
    F = fun() ->
                {expiration, 1, 600}
        end,
   [?_assertEqual(1, ets_cache:get_with_default(test_cache_table, get_with_default, F)),
    ?_assertEqual(1, ets_cache:get(test_cache_table, get_with_default))].

all_tables() ->
    [?_assertEqual(lists:sort([test_cache_table, cache_data_table]),  lists:sort(ets_cache:all_tables()))].
