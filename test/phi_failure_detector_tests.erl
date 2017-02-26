-module(phi_failure_detector_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(label, test).
-define(id, {127,0,0,1}).

-record(state, {
          t_last   :: pos_integer(),
          phi_last :: float(),
          rand     :: pid()
         }).

heartbeat_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_alive(), [{to_file, user}]))}.

prop_alive() ->
    ?SETUP(
       fun() ->
               {ok, _} = application:ensure_all_started(phi_failure_detector),
               {ok, _Pid} = phi_failure_detector:new(?label, ?id),
               fun() ->
                       application:stop(phi_failure_detector)
               end
       end,
       ?FORALL(Cmds, commands(?MODULE),
               begin
                   {H, S, R} = run_commands(?MODULE, Cmds),
                   ?WHENFAIL(
                      io:format("History: ~p~nState: ~p~nResult: ~p~n", [H, S, R]),
                      aggregate(command_names(Cmds), R =:= ok))
               end)).

g_time(#state{t_last = Last, rand = Rand}) ->
    ?LET(T, ?SUCHTHAT(X, ?LAZY(box_muller:rand(Rand)), X > 0), Last + T).

initial_state() ->
    {ok, Pid} = box_muller:start_link(50, 10),
    #state{rand = Pid, t_last = 0.0, phi_last = 0.0}.

command(S) ->
    frequency([
               {3, {call, ?MODULE, heartbeat, [g_time(S)]}},
               {1, {call, ?MODULE, phi, [g_time(S)]}}
              ]).

precondition(#state{t_last = L}, {call, _, heartbeat, [T]}) when T > L ->
    true;
precondition(_S, {call, _, heartbeat, _}) ->
    false;
precondition(#state{t_last = L}, {call, _, phi, [T]}) when T > L ->
    true;
precondition(_S, {call, _, phi, _}) ->
    false.

postcondition(_S, {call, _, phi, _}, _R) ->
    true;
postcondition(_S, {call, _, heartbeat, _}, _R) ->
    true.

next_state(S, _V, {call, _, heartbeat, [T]}) ->
    S#state{t_last = T};
next_state(S, V, {call, _, phi, _}) ->
    S#state{phi_last = V}.

heartbeat(T) ->
    phi_failure_detector:heartbeat(?label, ?id, T).

phi(T) ->
    phi_failure_detector:phi(?label, ?id, T).
