-module(pfd_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_service/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_service(Label, ID, Opts) ->
    supervisor:start_child(?MODULE, [Label, ID, Opts]).

init([]) ->
    SupFlags = #{
      strategy => simple_one_for_one,
      intensity => 10,
      period => 10
     },
    ChildSpec = #{
      id => undefined,
      start => {pfd_service_sup, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [pfd_service]
     },
    {ok, {SupFlags, [ChildSpec]}}.
