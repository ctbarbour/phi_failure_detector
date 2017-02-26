-module(box_muller).
-behavior(gen_server).

-export([new/2]).
-export([rand/1]).

-export([start_link/2]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(box_muller, {
          mean    :: float(),
          stddev  :: float(),
          next    :: float(),
          pair    :: boolean()
         }).

new(Mean, StdDev) ->
    #box_muller{mean = Mean, stddev = StdDev, next = 0.0, pair = false}.

rand(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, rand);
rand(#box_muller{mean = Mean, stddev = StdDev, pair = false} = BM) ->
    Theta = 2 * math:pi() * rand:uniform(),
    Rho = math:sqrt(-2 * math:log(1 - rand:uniform())),
    Scale = StdDev * Rho,
    X = Mean + Scale * math:cos(Theta),
    Y = Mean + Scale * math:sin(Theta),
    {X, BM#box_muller{next = Y, pair = true}};
rand(#box_muller{pair = true, next = Next} = BM) ->
    {Next, BM#box_muller{pair = false, next = undefined}}.

start_link(Mean, StdDev) ->
    gen_server:start_link(?MODULE, [Mean, StdDev], []).

init([Mean, StdDev]) ->
    {ok, new(Mean, StdDev)}.

handle_call(rand, _From, BM0) ->
    {X, BM} = rand(BM0),
    {reply, X, BM}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
