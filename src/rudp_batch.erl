-module(rudp_batch).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("priorities.hrl").
-include("logger.hrl").

%% API
-export([
  new/5,
  transform/2,
  priority/1, from/1, batch_number/1,
  is_expired/1,
  get_packet/1,
  is_empty/1
]).

-record(batch, {
  id :: reference(),
  batch_number :: pos_integer(),
  priority = ?PRIORITY_LOW :: rudp_qos:priority(),
  binary :: binary(),
  packets :: [binary()],
  from :: term(),
  expired :: rudp_time:time()
}).

-type batch() :: #batch{}.
-export_type([
  batch/0
]).

new(Id, From, Priority, Data, Timeout) ->
  #batch{
    id = Id,
    priority = Priority,
    binary = Data,
    from = From,
    expired = rudp_time:now_micro() + Timeout * 1000
  }.

transform(BatchNumber, Batch) when is_binary(Batch#batch.binary) ->
  NewBatch = Batch#batch{
    batch_number = BatchNumber,
    packets = split(Batch#batch.binary, rudp_udp:max_body_size()),
    binary = undefined
  },
  %% ?INFO("Transform batch ~p", [ NewBatch ]),
  NewBatch.

priority(Batch) ->
  Batch#batch.priority.

batch_number(Batch) ->
  Batch#batch.batch_number.

from(Batch) ->
  Batch#batch.from.

is_expired(Batch) ->
  Now = rudp_time:now_micro(),
  Batch#batch.expired =< Now.

is_empty(Batch) when is_list(Batch#batch.packets) ->
  Batch#batch.packets =:= [].

get_packet(#batch{ packets = [] }) -> undefined;
get_packet(Batch = #batch{ packets = [Packet|Tail]}) ->
  { ok, Packet, Batch#batch{ packets = Tail } }.

%% INTERNAL

split(Binary, Size) when is_binary(Binary) ->
  lists:reverse(split(Binary, [], Size)).

split(Binary, Acc, Size) when size(Binary) > Size ->
  Part1 = binary:part(Binary, 0, Size),
  Part2 = binary:part(Binary, Size, size(Binary) - Size),
  split(Part2, [ Part1 | Acc ], Size);
split(Binary, Acc, _) when is_binary(Binary) ->
  [ Binary | Acc ].

