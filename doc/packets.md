# Packets specification.

## Packet structure specification.

There is binary description for all types of packets specified in RUDP protocol.
All these packets described in rudp_packet.erl 

All packets should starts from 1 byte - type of packet. The next bytes depends from type of this packet. 
But usually the next value is 4 bytes of connection id to identify packet.

All bytes streams presented in big-endian endianness if it is not presented in specification. 

### Connect packet.

All interaction should be started from this packet. It sends initial data for opponent side (receiver).

<<1, Port:2-bytes, ConnId:4-bytes, ProtocolVersion:8-bytes>>

Where:
  Port -- the port when opponent should send packets.
  ConnId -- 4 bytes to identify connection for sender side. Receiver should include this ConnId for each packet to identify connection for sender.
  ProtocolVersion -- protocol version. In current implementation it should be always 1.

```
-define(CONNECT, 1).
connect(ConnId, Port, ProtocolVersion) ->
   <<?CONNECT:8/integer, Port:16/unsigned-integer, ConnId:4/binary, ProtocolVersion:64/unsigned-integer>>.
```   

### Connect acknowledge

This is second packet in interaction from receiver to started of connection. It says: i got you start packet and allow interaction with my ReceiverConnId.

<<2, ConnId:4-bytes, ReceiverConnId:4-bytes>>

Where:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ConnId -- identifier of connection for starter side.
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ReceiverConnId -- identifier of connection on receiver side. It uses to address connection on receiver side by starter.

```
-define(CONNECT_ACK, 2).
connect_ack(ConnId, ReceivedConnectionId) ->
  <<(header(?CONNECT_ACK, ConnId))/binary, ReceivedConnectionId:4/binary>>.
```  

### Close connection packet

If some side wants close connection, it should send this packet to opponent. 

<<3, ConnId:4-bytes>>

Where:
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ConnId -- connection id for opponent.

```
-define(CLOSE, 3).
close(ConnId) ->
  <<(header(?CLOSE, ConnId))/binary>>.
```  

### Ping packet

Ping packet uses when connection is idle and system should be sure that connection is still alive.
This request sends every _ping_interval_ (see options) miliseconds.

<< 9, ConnId:4-bytes>>

```
-define(PING, 9).
ping(ConnId, Count) ->
  <<(header(?PING, ConnId))/binary, Count:8/unsigned-integer>>.
```  

### Ping acknowledge (pong packet)

Acknowledge for ping.

<< 10, ConnId:4-bytes>>

```
-define(PING_ACK, 10).
ping_ack(ConnId, Count) ->
  <<(header(?PING_ACK, ConnId))/binary, Count:8/unsigned-integer>>.
```  

### Data packet

Data packet with payload. 

<<5, ConnId:4-bytes, PacketNumber:4-bytes, BatchNumber:1-bytes, Flags:1-bytes, Payload:N-bytes>>

Where:
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;PacketNumber -- packet number is connection sequence.
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;BatchNumber -- number of batch.
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Flags -- some bit flags.
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;000000AB
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;A - is 1 when packet is first in the batch
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;B - is 1 when packet is last in the batch. 

```
-define(DATA, 5).
data(ConnId, PacketNumber, BatchNumber, Flags, Data) ->
  Start = to_bit(maps:get(start, Flags, false)),
  Finish = to_bit(maps:get(finish, Flags, false)),
  <<(header(?DATA, ConnId))/binary, PacketNumber:32/unsigned-integer, BatchNumber:8/unsigned-integer, 0:6, Start:1, Finish:1, Data/binary>>.
```  

### Data acknowledge packet.

Acknowledge for data payload.

When receiver received data it always must send data ack packet to sender.
Sender deletes packet with PacketNumber from memory, and never repeats it again.

```
-define(DATA_ACK,6).
data_ack(ConnId, PacketNumber) ->
  <<(header(?DATA_ACK, ConnId))/binary, PacketNumber:32/unsigned-integer>>.
```  

### Request to repeat the data

When packet was lost. Receiver can ask sender repeat data packet by packet numbers. 

<<7, ConnId:4-bytes, ListOfPacketNumber:N-bytes>>

Where:
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ListOfPacketNumber -- binary string with this format:
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<<PacketNumber:4-bytes, ...., PacketNumberN:4-bytes>>

```
-define(DATA_REPEAT, 7).
data_repeat(ConnId, PacketsNumbers) ->
  Bodies = data_repeat_loop(PacketsNumbers, []),
  lists:map(fun(Body) ->
    <<(header(?DATA_REPEAT, ConnId))/binary, Body/binary>>
  end, Bodies).
```  

### Direct UDP packet

Direct UDP is very fast and non reliable way to send little payload (less than MTU, 1500 bytes).

Each time when you sends data, it added to output queue to use packet's sequence.
But when you need fast way to send UDP packet, sender can send it directly uses this type of packet.
Receiver will receive this packet, and never confirm receprion.


<<8, ConnID:4-bytes, Payload:N-bytes>>.

````
-define(DATA_UDP, 8).
data_udp(ConnId, Data) ->
  case size(Data) < rudp_udp:max_body_size() of
    true ->
      <<(header(?DATA_UDP, ConnId))/binary, Data/binary>>;
    false ->
      error(big_packet, [ Data ])
  end.
````  





-define(CLOSE_ACK, 4).





