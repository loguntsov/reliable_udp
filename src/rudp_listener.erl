-module(rudp_listener).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("listener.hrl").
-include("logger.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/3, start_link/4,
  stop/1,
  get_listener/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-type listener() :: #listener{}.
-export_type([
  listener/0
]).

-record(state, {
  socket :: port(),
  listener :: listener(),
  workers :: rudp_round_robin:round_robin()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Workers, Port, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [ Workers, Port, Opts ]).
start_link(Workers, Port, Opts) ->
  gen_server:start_link(?MODULE, [ Workers,  Port, Opts ], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

get_listener(Pid) ->
  rudp_listener_registrar:lookup(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ WorkersCount, Port, Opts ]) ->
  { ok, Socket } = gen_udp:open(Port, [{ active, once }, binary, {buffer, 5640 * 1024 } ] ++ Opts),
  { ok, AcceptorPid } = rudp_listener_acceptor:start_link(),
  { ok, {_, Port1} } = inet:sockname(Socket),
  Listener = #listener{
    pid = self(),
    socket = Socket,
    port = Port1,
    acceptor = AcceptorPid
  },

  Workers = lists:map(fun(_Index) ->
    { ok, Pid } = rudp_listener_worker:start_link(Listener),
    Pid
  end, lists:seq(1, WorkersCount)),

  rudp_listener_registrar:register(Listener),

  {ok, #state{
    listener = Listener,
    socket = Socket,
    workers = rudp_round_robin:new(Workers)
  }}.

handle_call(stop, From, State) ->
  gen_server:reply(From, ok),
  { stop, normal, State };

handle_call(get_listener, _From, State) ->
  { reply, State#state.listener, State };
handle_call(_Request, _From, State) ->
  {reply, error, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({udp, Socket, IP, _InPortNo, Packet}, State) ->
  %% ?INFO("Received raw packet ~p ~p ~p", [ Socket, IP, Packet ]),
  { Worker, NewState } = get_worker(State),
  rudp_listener_worker:process(Worker, IP, Packet),
  %% erlang:garbage_collect(),
  inet:setopts(Socket, [{active, once}]),
  { noreply, NewState };

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  gen_udp:close(State#state.socket),
  rudp_listener_registrar:unregister(self()),

  Listener = State#state.listener,

  exit(Listener#listener.acceptor, normal),

  lists:foreach(fun(Pid) ->
    exit(Pid, normal)
  end, rudp_round_robin:all(State#state.workers)),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_worker(State) ->
  { Pid, NewWorkers } = rudp_round_robin:next(State#state.workers),
  { Pid, State#state{ workers = NewWorkers }}.