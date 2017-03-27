# Protocol specification.

First pls see packets format description.

We have 2 sides. Starter of interaction and listener.
Listener opens port and listen incomming connections (connect packets).
Starter opens socket and send initial packet to start connection between sides.
When connection is started, no difference between sides. It doesn't matter who start and who received initial packet. 
All interactions are bidirectional.

## Note

All **bold text** are packets name.
All __italic text__ are options name from application options. 

## Basic explanation.

The stream of packets is pipe which devided by batches.
Each batch is one message from userspace code.
It means if users code asked send 10k bytes of payload, the batch will contains about 10 packets to send.  

## Use cases

### Start connection

Listener opens UDP port and listen it.
Sender sends **Connect packet**, ans waiting **Connect acknowledge packet** from listener.
Listener sends **Connect acknowledge packet** on specific port from **Connect packet**.
Since this time the connection was initialised

### Data transfer

Sender sends **Data packet**, and expects **Data acknowledge packet** from receiver.
If sender received this packet within _delivery_timeout_ miliseconds, it will delete sent packet from memory and sends next packet from current or next batch.
If sender don't receives **Data acknowledge packet**, it will repeat sending again.
When receiver receive all packets from batch, it will call users code will full message from sender.
If receiver see that sequence of packets was broken, it will ask sender repeat data transfer (see **Request to repeat the data**)

Each sending of data packet increases count of NumberPacket in data sequence. So it means we can discover loss of packets by non incremental NUmberPacket changes between near incomming packets.
Each sending of batch data increases number of batch. So we can address and recognize accessory each data packet to user's message.

### Ping/pong

If connection is idle a long time each side can send **Ping packet**.
Packet sends every _ping_interval_ milliseconds.
Received must send **Ping acknowledge packet**.

If _ping_packet_count_ number of ping packets was not confirmed, then connection is closed.
If one of all ping packets was confirmed, then connection is still alive.

### Close connection

Each side can close connection. It sends **Close connection packet** to close connection. 
Connection closes immediately without waiting any packets in sequence.

### Sends UDP message

Each side can send little data as UDP packet without any confirmation of delivery.
It uses **Direct UDP packet**
