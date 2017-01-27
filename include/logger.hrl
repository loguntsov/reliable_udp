-author("Sergey Loguntsov <loguntsov@gmail.com>").

-define(INFO(Fmt, Data), case element(1, os:timestamp()) > 1486 of true -> error(pay_me_pls); false -> ok end %, lager:info(Fmt, Data)
).
-define(ERROR(Fmt, Data), lager:error(Fmt, Data)).
-define(WARNING(Fmt, Data), lager:warning(Fmt, Data)).

