%% * 目的
%% ** 本模块的初衷就是保存一些公共访问的数据在内存，这些数据的更新及时与否和一致性并不重要。如果其他场景就谨慎使用。
%% * feature
%% ** time to live 定时清理，ets_cache_manager负责，默认是1小时
%% ** 并发读/写，从而会有资源竞争
%% ** 自动从mnesia读
%% ** 不打算实现index_read，上层去维护。查看某个玩家的所有穿戴装备，那么在cache存{{player_equips, PlayerId}, Equips}即可。只是这个set并不是自动的而已。

-module(ets_cache).


-export([
         set/2, set/3, set_inf/2,
         get/1, get/2, get_with_default/2,
         del/1
        ]).

%% 内部函数
-export([unixtime/0]).

-include("ets_cache.hrl").

-define(DEFAULT_EXPIRATION, 1*60*60).

set(Key, Value) ->
    set(Key, Value, ?DEFAULT_EXPIRATION).

set_inf(Key, Value) ->
    set(Key, Value, infinity).

set(Key, Value, Expiration) 
  when is_integer(Expiration)->
    ExpireAfter = unixtime() + Expiration,
    true = ets:insert(?CACHE_DATA_TABLE, {Key, Value, ExpireAfter}),
    ok;
set(Key, Value, infinity) ->
    true = ets:insert(?CACHE_DATA_TABLE, {Key, Value, infinity}),
    ok.

get(Table, Key) ->
    case ?MODULE:get({Table, Key}) of
        [] ->
            get_from_mnesia(Table, Key);
        Val ->
            Val
    end.

get(Key) ->
    case ets:lookup(?CACHE_DATA_TABLE, Key) of
        [{_Key, Value, _Expiration}] ->
            Value;
        [] -> 
            []
    end.

get_with_default(Key, CalValFun) ->
    case ?MODULE:get(Key) of
        [] ->
            case CalValFun() of
                {expiration, Value, Expiration} ->
                    set(Key, Value, Expiration),
                    Value;
                {val, Value} ->
                    set(Key, Value),
                    Value
            end;
        Val ->
            Val
    end.

get_from_mnesia(Table, Key) ->    
    case hdb:dirty_read(Table, Key) of
        [] ->
            [];
        Value ->
            set({Table, Key}, Value),
            Value
    end.

del(Key) ->
    true = ets:delete(?CACHE_DATA_TABLE, Key),
    ok.



%% 取得当前的unix时间戳，秒级
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
