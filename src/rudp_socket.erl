-module(rudp_socket).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  receiver/2,
  sender/2,
  statistic/1
]).

-include("socket.hrl").

-type socket_port() :: pos_integer().
-type socket() :: #rudp_socket{}.

-export_type([
  socket_port/0, socket/0
]).

receiver(Socket, Fun) ->
  ReceiverPid = Socket#rudp_socket.receiver,
  case erlang:is_process_alive(ReceiverPid) of
    false ->
      { error, socket_closed };
    true ->
      Fun(ReceiverPid)
  end.

sender(Socket, Fun) ->
  SenderPid = Socket#rudp_socket.sender,
  case erlang:is_process_alive(SenderPid) of
    false ->
      { error, socket_closed };
    true ->
      Fun(SenderPid)
  end.

statistic(Socket) ->
  { ok, StatSender } = rudp_sender:get_statistic(Socket#rudp_socket.sender),
  #{
    sender => StatSender
  }.
