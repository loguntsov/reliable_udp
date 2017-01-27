-module(rudp_time).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
	now_sec/0, now_ms/0, now_micro/0,
	unix_time/1, unix_time_ms/1, unix_time_micro/1
]).

unix_time({MegaSec, Sec, _}) ->
	MegaSec * 1000 * 1000 + Sec.

unix_time_micro({MegaSec, Sec, MicroSec}) ->
	(MegaSec * 1000000 + Sec ) * 1000000 + MicroSec.

unix_time_ms(Time) ->
	unix_time_micro(Time) div 1000.

now_sec() ->
	unix_time(os:timestamp()).

now_ms() ->
	unix_time_ms(os:timestamp()).

now_micro() ->
	unix_time_micro(os:timestamp()).