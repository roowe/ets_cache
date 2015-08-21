-module(ets_cache_manager_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0      
        ]).

-export([start_child/2]).

-export([which_children/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("ets_cache.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Table, CleanInterval) ->
    supervisor:start_child(?MODULE, [Table, CleanInterval*1000]).

which_children() ->    
    supervisor:which_children(ets_cache_manager_sup).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [
       {undefined,
        {ets_cache_manager, start_link, []},
        transient,
        16#ffffffff,
        worker,
        [ets_cache_manager]
       }
      ]
     }
    }.

