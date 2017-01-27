-module(rudp_listener_acceptor).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  accept/2,
  fire/3
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(item, {
  pid :: pid(),
  from :: term(),
  monitor :: reference()
}).

-record(incommings, {
  ip :: binary(),
  packet :: term(),
  expired :: pos_integer()
}).

-define(LEVELS_OF_QOS, 1).

-record(state, {
  handlers = [] :: [#item{}],
  incommings = [] :: [#incommings{}],
  last_filtered = 0 :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

accept(Pid, Timeout) ->
 try
  gen_server:call(Pid, { accept, self() }, Timeout)
 catch
   error:Reason ->
    gen_server:cast(Pid, { cancel, self() }),
    { error, Reason }
 end.

fire(Pid, IP, ParsedPacket) ->
  gen_server:cast(Pid, { fire, IP, ParsedPacket }).

init([]) ->
  {ok, #state{}}.

handle_call({accept, ProtocolPid }, From, State) ->
  Item = #item{
    pid = ProtocolPid,
    monitor = monitor(process, ProtocolPid),
    from = From
  },
  NewState0 = State#state{
    handlers = [ Item | State#state.handlers ]
  },
  Now = rudp_time:now_ms(),
  NewState1 = filter_incommings(Now, NewState0),
  NewState2 = assign(NewState1),
  { noreply, NewState2 };

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({ fire, IP, ParsedPacket }, State) ->
  Now = rudp_time:now_ms(),
  NewState0 = filter_incommings(Now, State),
  NewState1 = NewState0#state{
    incommings = [#incommings{ ip = IP, packet = ParsedPacket, expired = Now + rudp_app:env(connection_timeout) } | State#state.incommings ]
  },
  NewState3 = assign(NewState1),
  { noreply, NewState3 };

handle_cast({ cancel, Pid }, State) ->
  case lists:keyfind(Pid, #item.pid, State#state.handlers) of
    false -> { noreply, State };
    Item ->
      demonitor(Item#item.monitor),
      { noreply, State#state{
        handlers = lists:keydelete(Pid, #item.pid, State#state.handlers)
      }}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', MonitorRef, _Type, _Pid, _Info}, State) ->
  NewHandlers = lists:keydelete(MonitorRef, #item.monitor, State#state.handlers),
  { noreply, State#state{
    handlers = NewHandlers
  }};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assign(State = #state{
  handlers = [Item|Tail],
  incommings = [#incommings{ ip = IP, packet = ParsedPacket }|IncTail]
}) ->
  gen_server:reply(Item#item.from, { ok, IP, ParsedPacket }),
  demonitor(Item#item.monitor),
  assign(State#state{
    handlers = Tail,
    incommings = IncTail
  });

assign(State) -> State.

filter_incommings(Now, State = #state{ last_filtered = Filtered }) when Now =< Filtered -> State;
filter_incommings(Now, State) ->
  NewIncommings = lists:filter(fun(#incommings{ expired = T }) ->
    T > Now
  end, State#state.incommings),
  State#state{
    incommings = NewIncommings,
    last_filtered = Now
  }.
