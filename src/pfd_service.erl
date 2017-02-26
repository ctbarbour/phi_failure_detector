-module(pfd_service).
-behavior(gen_server).

-export([start_link/2]).
-export([start_link/3]).
-export([heartbeat/3]).
-export([phi/3]).
-export([select/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          label :: term(),
          pfd   :: pfd_samples:samples()
         }).

select(Label) ->
    KeyPattern = key(Label, '_'),
    MatchHead = {KeyPattern, '_', '_'},
    Guard = [],
    Result = ['$$'],
    R = gproc:select([{MatchHead, Guard, Result}]),
    [L || [{n, l, {?MODULE, L}}, _, _] <- R].

key(Label, ID) ->
    {n, l, {?MODULE, {Label, ID}}}.

start_link(Label, ID) ->
    start_link(Label, ID, []).

start_link(Label, ID, Opts) ->
    gen_server:start_link(?MODULE, [Label, ID, Opts], []).

heartbeat(Label, ID, T) ->
    Pid = gproc:lookup_pid(key(Label, ID)),
    gen_server:cast(Pid, {heartbeat, T}).

phi(Label, ID, T) ->
    Pid = gproc:lookup_pid(key(Label, ID)),
    gen_server:call(Pid, {phi, T}).

init([Label, ID, _Opts]) ->
    true = gproc:reg(key(Label, ID)),
    Pfd = pfd_samples:new(),
    {ok, #state{label = Label, pfd = Pfd}}.

handle_call({phi, T}, _From, State) ->
    #state{pfd = Pfd} = State,
    {reply, pfd_samples:phi(T, Pfd), State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({heartbeat, T}, State) ->
    Pfd = pfd_samples:add(T, State#state.pfd),
    {noreply, State#state{pfd = Pfd}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
