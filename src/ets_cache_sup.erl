-module(ets_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_supervisor_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_supervisor_child(ChildId, Mod, Args) ->
    child_reply(supervisor:start_child(
                  ?MODULE,
                  {ChildId, {Mod, start_link, Args},
                   transient, infinity, supervisor, [Mod]})).

child_reply({ok, _}) -> 
    ok;
child_reply(X) ->
    X.
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

