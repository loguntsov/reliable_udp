-author("Sergey Loguntsov <loguntsov@gmail.com>").

-record(listener, {
  pid :: pid(),
  port :: pos_integer(),
  socket :: port(),
  acceptor :: pid()
}).
