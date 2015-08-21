-module(ets_cache_manager).
-behaviour(gen_server).

-include("ets_cache.hrl").

-export([
         start_link/2,
         get_table/1
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
          table,
          clean_interval
         }).

get_table(Pid) ->
    gen_server:call(Pid, get_table).

start_link(Table, CleanInterval) ->
    gen_server:start_link(?MODULE, [Table, CleanInterval], []).

init([Table, CleanInterval]) ->
    ets:new(Table, [
                    named_table, 
                    public,
                    {read_concurrency, true},
                    {write_concurrency, true}
                   ]),
    erlang:send_after(CleanInterval, self(), cleanup),    
    %% error_logger:info_msg("CleanInterval ~p~n", [CleanInterval]),
    {ok, #state{
            table = Table,
            clean_interval = CleanInterval
           }}.
handle_call(get_table, _From, State) ->
    {reply, State#state.table, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{
                        table = Table,
                        clean_interval = CleanInterval
                       } = State) ->
    Now = ets_cache:unixtime(),
    %% error_logger:info_msg("cleanup now ~p~n", [Now]),
    MS = ets:fun2ms(
           fun({_, _, Timeout}) when is_integer(Timeout)->
                   Timeout =< Now
           end),
    ets:select_delete(Table, MS),
    erlang:send_after(CleanInterval, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
