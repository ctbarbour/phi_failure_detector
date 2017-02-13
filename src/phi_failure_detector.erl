-module(phi_failure_detector).

-export([new/2]).
-export([new/3]).
-export([phi/1]).
-export([phi/2]).
-export([heartbeat/2]).
-export([monitor/3]).

new(Label, ID) ->
    new(Label, ID, []).

new(Label, ID, Opts)->
    pfd_sup:start_service(Label, ID, Opts).

phi(Label) ->
    [{S, phi(L, I)} || {L, I} = S <- pfd_service:select(Label)].

phi(Label, ID) ->
    pfd_service:phi(Label, ID).

heartbeat(Label, ID) ->
    pfd_service:heartbeat(Label, ID).

monitor(Label, ID, Threshold) ->
    pfd_monitor:monitor(Label, ID, Threshold).


