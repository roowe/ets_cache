%% * 目的
%% ** 本模块的初衷就是保存一些公共访问的数据在内存，这些数据的更新及时与否和一致性并不重要。如果其他场景就谨慎使用。
%% * feature
%% ** time to live 定时清理，ets_cache_manager负责，默认是1小时
%% ** 并发读/写，从而会有资源竞争
%% ** 自动从mnesia读(废弃，在项目用get_with_default去封装）
%% ** 不打算实现index_read，上层去维护。查看某个玩家的所有穿戴装备，那么在cache存{{player_equips, PlayerId}, Equips}即可。只是这个set并不是自动的而已。

-module(ets_cache).

-export([new_cache_table/0, new_cache_table/1, new_cache_table/2]).

-export([
         set/2, set/3, set_inf/2, 
         set_t/3, set_t/4, set_t_inf/3,
         get/1, get/2,
         get_with_default/2, get_with_default/3,
         del/1, del/2
        ]).

%% 内部函数
-export([unixtime/0]).

-include("ets_cache.hrl").

-define(DEFAULT_EXPIRATION, 300).
%% new cache table
new_cache_table() ->
    case application:get_env(clean_interval) of
        undefined ->
            new_cache_table(?CACHE_DATA_TABLE);
        {ok, CleanInterval} ->
            new_cache_table(?CACHE_DATA_TABLE, CleanInterval)
    end.

new_cache_table(Table) ->
    new_cache_table(Table, 300).

new_cache_table(Table, CleanInterval) ->
    ets_cache_manager_sup:start_child(Table, CleanInterval).

%% set
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_EXPIRATION).

set_inf(Key, Value) ->
    set(Key, Value, infinity).

set(Key, Value, Expiration) ->
    set_t(?CACHE_DATA_TABLE, Key, Value, Expiration).

set_t(ETSTable, Key, Value) ->
    set_t(ETSTable, Key, Value, ?DEFAULT_EXPIRATION).

set_t_inf(ETSTable, Key, Value) ->
    set_t(ETSTable, Key, Value, infinity).

set_t(ETSTable, Key, Value, infinity) ->
    true = ets:insert(ETSTable, {Key, Value, infinity}),
    ok;
set_t(ETSTable, Key, Value, Expiration) ->
    ExpireAfter = unixtime() + Expiration,
    true = ets:insert(ETSTable, {Key, Value, ExpireAfter}),
    ok.


%% get
get(Key) ->
   ?MODULE:get(?CACHE_DATA_TABLE, Key).

get_with_default(Key, CalValFun) ->
    get_with_default(?CACHE_DATA_TABLE, Key, CalValFun).

get_with_default(ETSTable, Key, CalValFun) ->
    case ?MODULE:get(ETSTable, Key) of
        [] ->
            case CalValFun() of
                {expiration, Value, Expiration} ->
                    set_t(ETSTable, Key, Value, Expiration),
                    Value;
                {val, Value} ->
                    set_t(ETSTable, Key, Value),
                    Value;
                Other ->
                    Other
            end;
        Val ->
            Val
    end.

get(ETSTable, Key) ->
    case ets:lookup(ETSTable, Key) of
        [{_Key, Value, _Expiration}] ->
            Value;
        [] -> 
            []
    end.

%% del
del(Key) ->
    del(?CACHE_DATA_TABLE, Key).

del(ETSTable, Key) ->
    true = ets:delete(ETSTable, Key),
    ok.

%% 取得当前的unix时间戳，秒级
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
