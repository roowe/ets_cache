-module(ets_cache_app).

-behaviour(application).
-include("ets_cache.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->    
    {ok, Pid} = ets_cache_sup:start_link(),
    ets_cache_sup:start_supervisor_child(ets_cache_manager_sup,
                                         ets_cache_manager_sup,
                                         []),
    ets_cache:new_cache_table(), %% start default table, ?CACHE_DATA_TABLE
    {ok, Pid}.

stop(_State) ->
    ok.
