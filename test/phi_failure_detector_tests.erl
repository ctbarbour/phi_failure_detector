-module(phi_failure_detector_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-record(state, {t :: pos_integer()}).

heartbeat_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_alive(), [{to_file, user}]))}.

prop_alive() ->
    ?SETUP(
       fun() ->
               {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
               fun() ->
                       ok = gen_server:stop(Pid)
               end
       end,
       ?FORALL(Cmds, commands(?MODULE),
               begin
                   {H, S, R} = run_commands(?MODULE, Cmds),
                   ?WHENFAIL(
                      io:format("History: ~p~nState: ~p~nResult: ~p~n", [H, S, R]),
                      aggregate(command_names(Cmds), R =:= ok))
               end)).

g_time(#state{t = Last}) ->
    ?LET(T, pos_integer(), Last + T).

initial_state() ->
    #state{t = erlang:system_time(milli_seconds)}.

command(S) ->
    oneof([
           {call, ?MODULE, add, [g_time(S)]},
           {call, ?MODULE, phi, [g_time(S)]}
          ]).

precondition(_Cmd, _S) ->
    true.

postcondition(_S, _Cmd, _R) ->
    true.

next_state(S, _V, {call, ?MODULE, add, [T]}) ->
    S#state{t = T};
next_state(S, _V, {call, ?MODULE, phi, _}) ->
    S.

add(T) ->
    gen_server:cast(?MODULE, {add, T}).

phi(T) ->
    gen_server:call(?MODULE, {phi, T}).

init([]) ->
    {ok, pfd_samples:new()}.

handle_call({phi, T}, _From, State) ->
    {reply, pfd_samples:phi(T, State), State}.

handle_cast({add, T}, State) ->
    {noreply, pfd_samples:add(T, State)}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
