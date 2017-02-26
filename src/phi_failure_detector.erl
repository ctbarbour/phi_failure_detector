-module(phi_failure_detector).

-export([new/2]).
-export([new/3]).
-export([phi/1]).
-export([phi/2]).
-export([phi/3]).
-export([heartbeat/2]).
-export([heartbeat/3]).
-export([monitor/3]).

%% @doc Creates a new Phi Failure Detector
%%
%% {@link new/3}
%% @end

-spec new(Label, ID) -> {ok, Pid}
                            when Label :: atom(),
                                 ID    :: term(),
                                 Pid   :: pid().
new(Label, ID) ->
    new(Label, ID, []).

%% @doc Creates a new Phi Failure Detector
%%
%% Creating a new Phi Failure Detector creates a process to manage the sampling window of
%% inter-arrival times of heartbeats and allows a client to query the Phi value at a point in time.
%% The unique identifier for a Phi Failure Detector is the combination of `Label' and `ID'.  The
%% `Label' and `ID' combination are used with {@link heartbeat/2}.
%% @end

-spec new(Label, ID, Opts) -> {ok, Pid}
                                  when Label :: atom(),
                                       ID    :: term(),
                                       Opts  :: list(),
                                       Pid   :: pid().
new(Label, ID, Opts)->
    pfd_sup:start_service(Label, ID, Opts).

%% @doc Calculate Phi for all endpoints that share the same `Label'.
%%
%% See {@link phi/2} for more details on Phi.
%% @end

-spec phi(Label) -> Status
                        when Label :: atom(),
                             Status :: [{{atom(), term()}, float()}].

phi(Label) ->
    [{S, phi(L, I)} || {L, I} = S <- pfd_service:select(Label)].

%% @doc Calculate the Phi value for a service endpoint
%%
%% Phi represents a dynamically scalable suspicion level of a service endpoint based on the
%% inter-arrival times recorded with each {@link heartbeat/2}. A distribution of
%% inter-arrival times are used to compute the value of Phi at some point in time. The estimation of
%% inter-arrival times assumes a normal distribution. The follow function is used to determine phi:
%% -log10(1 - cdf(tnow - tlast))
%% @end

-spec phi(Label, ID) -> Phi
                            when Label :: atom(),
                                 ID    :: term(),
                                 Phi   :: float().


phi(Label, ID) ->
    phi(Label, ID, t_now()).

-spec phi(Label, ID, T) -> Phi
                               when Label :: atom(),
                                    ID    :: term(),
                                    T     :: pos_integer(),
                                    Phi   :: float().

phi(Label, ID, T) ->
    pfd_service:phi(Label, ID, T).

%% @doc Add a successful heartbeat to the sample set for calculating Phi
%%
%% When a heartbeat arrives we store the inter-arrival time from the previously received heartbeat
%% and store it in a fix sized sliding window. The data regarding the oldest heartbeat is dropped
%% from window.
%% @end

-spec heartbeat(Label, ID) -> ok
                                  when Label :: atom(),
                                       ID    :: term().

heartbeat(Label, ID) ->
    heartbeat(Label, ID, t_now()).

heartbeat(Label, ID, T) ->
    pfd_service:heartbeat(Label, ID, T).

-spec monitor(Label, ID, Threshold) -> ok
                                           when Label     :: atom(),
                                                ID        :: term(),
                                                Threshold :: float().

monitor(Label, ID, Threshold) ->
    pfd_monitor:monitor(Label, ID, Threshold).


t_now() ->
    erlang:system_time(micro_seconds).
