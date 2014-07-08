-module(ets_cache_manager).
-behaviour(gen_server).

-include("ets_cache.hrl").

-export([
         start_link/0
        ]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
          clean_interval
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    CleanInterval = case application:get_env(clean_interval) of
                        undefined ->
                            600*1000;
                        {ok, Val} ->
                            Val*1000
                    end,
    erlang:send_after(CleanInterval, self(), cleanup),    
    %% error_logger:info_msg("CleanInterval ~p~n", [CleanInterval]),
    {ok, #state{
            clean_interval = CleanInterval
           }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{
                        clean_interval = CleanInterval
                       } = State) ->
    Now = ets_cache:unixtime(),
    %% error_logger:info_msg("cleanup now ~p~n", [Now]),
    MS = ets:fun2ms(
           fun({_, _, Timeout}) when is_integer(Timeout)->
                   Timeout =< Now
           end),
    ets:select_delete(?CACHE_DATA_TABLE, MS),
    erlang:send_after(CleanInterval, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
