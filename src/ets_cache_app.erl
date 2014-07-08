-module(ets_cache_app).

-behaviour(application).
-include("ets_cache.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ets:new(?CACHE_DATA_TABLE, [
                                named_table, 
                                public,
                                {read_concurrency, true},
                                {write_concurrency, true}
                               ]),
    ets_cache_sup:start_link().

stop(_State) ->
    ok.
