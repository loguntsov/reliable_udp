-author("Sergey Loguntsov <loguntsov@gmail.com>").
-record(rudp_socket, {
  protocol :: pid(),
  receiver :: pid(),
  sender :: pid()
}).