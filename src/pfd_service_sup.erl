-module(pfd_service_sup).
-behavior(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(Label, ID, Opts) ->
    supervisor:start_link(?MODULE, [Label, ID, Opts]).

init([Label, ID, Opts]) ->
    Service = #{
      id => {Label, ID, pfd_service},
      start => {pfd_service, start_link, [Label, ID, Opts]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [pfd_service]
     },
    Monitor = #{
      id => {Label, ID, pfd_monitor},
      start => {pfd_monitor, start_link, [Label, ID]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [pfd_monitor]
     },
    SupFlags = #{
      strategy => one_for_all,
      intensity => 10,
      period => 10
     },
    Children = [Service, Monitor],
    {ok, {SupFlags, Children}}.

