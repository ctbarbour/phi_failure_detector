-module(pfd_monitor).
-behavior(gen_server).

-export([start_link/2]).
-export([monitor/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          label         :: term(),
          id            :: term(),
          monitors = [] :: [phi_monitor()]
         }).

-record(phi_monitor, {
          pid                  :: pid(),
          mref                 :: reference(),
          threshold            :: float(),
          triggered    = false :: boolean(),
          last_trigger         :: undefined | pos_integer()
         }).

-type phi_monitor() :: #phi_monitor{}.

key(Label, ID) ->
    {?MODULE, {Label, ID}}.

start_link(Label, ID) ->
    gen_server:start_link(?MODULE, [Label, ID], []).

monitor(Label, ID, Threshold) ->
    Pid = gproc:lookup_local_name(key(Label, ID)),
    gen_server:cast(Pid, {monitor, self(), Threshold}).

init([Label, ID]) ->
    true = gproc:add_local_name(key(Label, ID)),
    {ok, tick(#state{label = Label, id = ID})}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({monitor, Pid, Threshold}, State) ->
    #state{monitors = Monitors} = State,
    MRef = erlang:monitor(process, Pid),
    PhiMonitor = #phi_monitor{pid = Pid, mref = MRef, threshold = Threshold},
    {noreply, State#state{monitors = [PhiMonitor | Monitors]}}.

handle_info({'EXIT', _MRef, process, Pid, _Reason}, State) ->
    Monitors = [M || M <- State#state.monitors,
                     M#phi_monitor.pid =/= Pid],
    {noreply, State#state{monitors = Monitors}};
handle_info(tick, State) ->
    {noreply, tick(State)};
handle_info({'DOWN', _MRef, process, Pid, _Reason}, State) ->
    Monitors = [M || M <- State#state.monitors,
                     M#phi_monitor.pid =/= Pid],
    {noreply, State#state{monitors = Monitors}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

tick(#state{monitors = []} = State) ->
    send_tick(State);
tick(State) ->
    Phi = pfd_service:phi(State#state.label, State#state.id),
    send_tick(notify(Phi, State)).

send_tick(State) ->
    _TRef = erlang:send_after(100, self(), tick),
    State.

notify(infinity, State) ->
    Monitors =
        lists:foldl(
          fun(M, Acc)
                when M#phi_monitor.triggered =:= false ->
                  _ = send_limit_reached(M, State#state.label, infinity),
                  [M#phi_monitor{triggered = true} | Acc];
             (M, Acc) ->
                  [M | Acc]
          end, [], State#state.monitors),
    State#state{monitors = Monitors};
notify(Phi, State) ->
    Monitors =
        lists:foldl(
          fun(M, Acc)
             when M#phi_monitor.triggered =:= false
                  andalso Phi >= M#phi_monitor.threshold ->
                  _ = send_limit_reached(M, State#state.label, Phi),
                  [M#phi_monitor{triggered = true, last_trigger = erlang:monotonic_time()} | Acc];
             (M, Acc)
                when M#phi_monitor.triggered =:= true
                     andalso Phi =< M#phi_monitor.threshold ->
                  _ = send_limit_cleared(M, State#state.label, Phi),
                  [M#phi_monitor{triggered = false} | Acc];
             (M, Acc) ->
                  [M | Acc]
          end, [], State#state.monitors),
    State#state{monitors = Monitors}.

send_limit_reached(#phi_monitor{pid = Pid, threshold = Threshold}, Label, Phi) ->
    Pid ! {threshold_limit_reached, {Label, {Phi, Threshold}}}.

send_limit_cleared(#phi_monitor{pid = Pid, threshold = Threshold}, Label, Phi) ->
    Pid ! {threshold_limit_cleared, {Label, {Phi, Threshold}}}.
