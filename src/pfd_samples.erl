-module(pfd_samples).

-export([new/0]).
-export([new/3]).
-export([phi/1]).
-export([phi/2]).
-export([add/1]).
-export([add/2]).

-define(SQRT_TWO, math:sqrt(2)).
-define(DEFAULT_MIN_STD_DEV, 5000.0).
-define(DEFAULT_MAX_SAMPLE_SIZE, 1000).
-define(DEFAULT_HEARTBEAT_ESTIMATE, 5000.0).
-define(TIME_UNIT, micro_seconds).

-record(pfd, {
          samples         = []                       :: list(float()),
          max_sample_size = ?DEFAULT_MAX_SAMPLE_SIZE :: pos_integer(),
          min_std_dev     = ?DEFAULT_MIN_STD_DEV     :: float(),
          last_sample                                :: undefined | pos_integer()
         }).

-type samples() :: #pfd{}.

-export_type([samples/0]).

-spec new() -> PhiAccrualFailureDetector
                   when PhiAccrualFailureDetector :: samples().

new() ->
    new(?DEFAULT_MAX_SAMPLE_SIZE,
        ?DEFAULT_MIN_STD_DEV,
        ?DEFAULT_HEARTBEAT_ESTIMATE).

-spec new(MaxSampleSize, MinStdDev, FirstHeartbeatEstimate)
         -> PhiAccrualFailureDetector
                when MaxSampleSize             :: pos_integer(),
                     MinStdDev                 :: float(),
                     FirstHeartbeatEstimate    :: float(),
                     PhiAccrualFailureDetector :: samples().

new(MaxSampleSize, MinStdDev, FirstHeartbeatEstimate)
  when
       MaxSampleSize > 0 andalso
       MinStdDev > 0.0 andalso
       FirstHeartbeatEstimate > 0.0 ->
    InitStdDev = FirstHeartbeatEstimate / 4,
    #pfd{
       samples = [FirstHeartbeatEstimate - InitStdDev,
                  FirstHeartbeatEstimate + InitStdDev],
       min_std_dev = MinStdDev,
       max_sample_size = MaxSampleSize
      }.

-spec add(PhiAccrualFailureDetector0) ->
                       PhiAccrualFailureDetector
                           when PhiAccrualFailureDetector0 :: samples(),
                                PhiAccrualFailureDetector  :: samples().
add(State) ->
    add(t_now(), State).

-spec add(Timestamp, PhiAccrualFailureDetector0) ->
                       PhiAccrualFailureDetector
                           when Timestamp                  :: pos_integer(),
                                PhiAccrualFailureDetector0 :: samples(),
                                PhiAccrualFailureDetector  :: samples().

add(Now, State) ->
    #pfd{samples = Samples0, max_sample_size = MaxSampleSize} = State,
    case State#pfd.last_sample of
        undefined ->
            State#pfd{last_sample = Now};
        LastSample ->
            Delta = Now - LastSample,
            Samples = lists:sublist([Delta | Samples0], MaxSampleSize),
            State#pfd{samples = Samples, last_sample = Now}
    end.

-spec t_now() -> Timestamp
                     when Timestamp :: non_neg_integer().

t_now() ->
    erlang:system_time(?TIME_UNIT).

-spec average(Samples)
             -> Average
                    when Samples :: list(float()),
                         Average :: float().

average(Samples) ->
    lists:sum(Samples) / length(Samples).

-spec std_dev(Samples) -> StandardDeviation
                              when Samples           :: list(float()),
                                   StandardDeviation :: float().

std_dev(Samples) ->
    math:sqrt(variance(Samples)).

-spec variance(Samples) -> Variance
                               when Samples  :: list(float()),
                                    Variance :: float().

variance(Samples) ->
    Avg = average(Samples),
    F = fun(X, Sum) -> Sum + (X - Avg) * (X - Avg) end,
    lists:foldl(F, 0.0, Samples).

-spec phi(PhiAccrualFailureDetector) ->
                 Phi
                     when PhiAccrualFailureDetector :: samples(),
                          Phi                       :: float().

phi(State) ->
    phi(t_now(), State).

-spec phi(Timestamp, PhiAccrualFailureDetector) ->
                 Phi
                     when Timestamp                 :: pos_integer(),
                          PhiAccrualFailureDetector :: samples(),
                          Phi                       :: float() | infinity.

phi(Now, #pfd{last_sample = LastSample, min_std_dev = MinStdDev, samples = Samples}) ->
    case LastSample of
        undefined ->
            0.0;
        _ ->
            StdDev = max(MinStdDev, std_dev(Samples)),
            Avg = average(Samples),
            Diff = Now - LastSample,
            phi(Diff, Avg, StdDev)
    end.

-spec phi(Diff, Avg, StdDev) -> Phi
                                    when Diff   :: float(),
                                         Avg    :: float(),
                                         StdDev :: float(),
                                         Phi    :: float() | infinity.

phi(Diff, Avg, StdDev) ->
    case p_later(Diff, Avg, StdDev) of
        0.0 ->
            infinity;
        P ->
            -math:log10(P)
    end.

-spec cdf_normal(X, Avg, StdDev) ->
                        Probability
                            when X           :: float(),
                                 Avg         :: float(),
                                 StdDev      :: float(),
                                 Probability :: float().

cdf_normal(X, Avg, StdDev) ->
    0.5 * (1.0 + math:erf((X - Avg) / (StdDev * ?SQRT_TWO))).

-spec p_later(X, Avg, StdDev) -> P
                     when X      :: float(),
                          Avg    :: float(),
                          StdDev :: float(),
                          P      :: float().

p_later(X, Avg, StdDev) ->
    1.0 - cdf_normal(X, Avg, StdDev).
