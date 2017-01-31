-author("Sergey Loguntsov <loguntsov@gmail.com>").

-define(INFO(Fmt, Data), lager:info(Fmt, Data)).
-define(ERROR(Fmt, Data), lager:error(Fmt, Data)).
-define(WARNING(Fmt, Data), lager:warning(Fmt, Data)).
