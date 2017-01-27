APP:=rudp
SRC_PATH:= -pa _build/default/lib/*/ebin
ERL_RUN_ARGS:= $(SRC_PATH) +pc unicode -name $(APP)@`hostname` -s rudp_app main


.PHONY: all
all: compile

.PHONY: compile
compile:
	./rebar3 compile

.PHONY: run
run: compile clear-log
	erl $(ERL_RUN_ARGS)

.PHONY: upload
upload: compile upload1 upload2
	rsync -rltxSRzv \
	    --exclude .git \
	    --exclude *.log* \
	    --exclude *.pid \
	    --exclude .idea \
	    . server:~/rudp

.PHONY: s1
s1:
	ssh sergey@54.229.209.50

.PHONY: upload1
upload1:
	rsync -rltxSRzv \
	    --exclude .git \
	    --exclude *.log* \
	    --exclude *.pid \
	    --exclude .idea \
	    --exclude *.erl \
	    . sergey@54.229.209.50:~/rudp

.PHONY: s2
s2:
	ssh sergey@54.154.235.68

.PHONY: upload2
upload2:
	rsync -rltxSRzv \
	    --exclude .git \
	    --exclude *.log* \
	    --exclude *.pid \
	    --exclude .idea \
	    --exclude *.erl \
	    . sergey@54.154.235.68:~/rudp

clear-log:
	rm -f log/*

kill:
	kill `cat server.pid` || true

daemon: kill
	run_erl -daemon /tmp/ log/ "erl $(ERL_RUN_ARGS)"

