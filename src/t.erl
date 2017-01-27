-module(t).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("logger.hrl").

%% API
-compile(export_all).

%% Create server
s() ->
  make_server(1234).

%% Create connection
c(Listener) ->
  random:seed(erlang:now()),
  connect(Listener, "54.154.235.68", 1234). %% s2

%% Test case 1: Create one server and one connection
t() ->
  Socket = c(s()),
  messages(),
  Data = iolist_to_binary([ <<"HELLO-", (integer_to_binary(I))/binary, " ">> || I <- lists:seq(1,100000)]),
  ?INFO("Data size ~p.", [ size(Data) ]),
  gen_rudp:async_send_binary(Socket, Data),
  messages(),
  ?INFO("Socket ~p", [ Socket ]).

%% Test case 2: Create poll of N (100) clients
p() ->
  spawn_link(fun() ->
  { ok, Listener }= gen_rudp:start_listener(1234),
  Data = iolist_to_binary([ <<"HELLO-", (integer_to_binary(I))/binary, " ">> || I <- lists:seq(1,10000)]),
  lists:foreach(fun(I) ->
    timer:sleep(1),
    spawn_link(fun() ->
      Socket = c(Listener),
      catch (fun ClientLoop() ->
      ?INFO("Data size ~p.", [ size(Data) ]),
      gen_rudp:async_send_binary(Socket, Data),
      ?INFO("Socket ~p", [ Socket ]),
      receive
        { rudp_received, _, _Binary } ->
          io:format("Received binary back ~n", [])
        after 30000 ->
          error(cant_receive_data)
      end,
      ClientLoop()
      end)()
    end)
  end, lists:seq(1, 300)) end).


make_listener(Port) ->
  { ok, Listener } = gen_rudp:start_listener(Port),
  Listener.

% Create server on port Port
% It will print RECEIVED RIGHT Message # when some message is received.
make_server(Port) ->
  Self = self(),
  spawn(fun() ->
    L = make_listener(Port),
    Self ! { listener, L }
  end),
  Listener = receive
    { listener, L } -> L
    after 10000 ->
      error(timeout)
  end,
  spawn(fun() ->
    ?INFO("Acceptor process ~p",[ self() ]),
    C = spawn_link(fun() ->
      (fun Loop(C0) ->
        receive
          s -> io:format("RECEIVED RIGHT Message: ~p ~n", [ C0 ])
        end,
        Loop(C0+1)
      end)(1)
    end),
    server_loop(C, Listener)
  end),
  Listener.


%% Server loop for accept connections
server_loop(C, Listener) ->
  { ok, Socket } = gen_rudp:accept(Listener),
  Pid = spawn_link(fun() ->
    ?INFO("Start receiving messages for process ~p. Socket: ~p ", [ self(), Socket ]),
    (fun Loop() ->
      receive
        {rudp_received,_, Binary} ->
          gen_rudp:async_send_binary(Socket, Binary),
          C ! s;
        Any ->
          ?INFO("Message: ~p ~n", [ Any ])
      end,
      Loop()
    end)()
  end),
  ok = gen_rudp:controlling_process(Socket, Pid),
  server_loop(C, Listener).

%% Connect with reconnection
connect(Listener, IP, Port) ->
  case gen_rudp:connect(Listener, IP, Port, []) of
    { ok, Socket } -> Socket;
    Error ->
      ?ERROR("Error of connection ~p", [ Error ]),
      timer:sleep(random:uniform(1000) + 500),
      connect(Listener, IP, Port)
  end.

messages() ->
  receive
    Any ->
      ?INFO("Message: ~p ~n", [ Any ]),
      messages()
    after 1000 -> ok
  end.

