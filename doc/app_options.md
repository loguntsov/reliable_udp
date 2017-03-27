# Application options

These all actual options which you can change to play of performance for RUDP application.

* { listener_options, [] }, %% Socket listener options. See http://erlang.org/doc/man/gen_udp.html#open-2
* { connection_timeout, 10000 }, %% Connection timeout, ms
* { ping_interval, 5000 }, %% Timeout to check good connection, ms
* { ping_packet_count, 4 }, %% Number of ping packets are sent before disconnection
* { delivery_timeout, 5000 }, %% Delivery timeout to check status of delivery of packet, ms
* { bandwith_max, 100000 }, %% Max bandwith of sender for one connection, bytes/second
* { bandwith_min, 100 }, %% Min bandwith of sender for one connection, bytes/second
* %%{ bandwith_step, 100 }, %% Bandwith step for one lost packet
* { max_attempts_to_send_packet, 500000 }, %% Max attempts to send packet when it can't get delivery confirmation
* %{ bandwith_step_increase, 10000 }, %% Step to increase bandwith, bytes
* %{ bandwith_check_interval, 5000 }, %% Interval to check righ bandwith.
* %{ bandwith_step_decrease, 100 }, %% Step to decrease bandwith, bytes
* { send_buffer_size, 10 }, %% Number of packets inside send buffer. Sender will be stoped if this buffer is big.
* { udp_packet_size, 1024 } %% Max udp packet size, bytes
