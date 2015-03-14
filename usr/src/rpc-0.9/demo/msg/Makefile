#
# @(#)Makefile	2.7 91/03/11 4.0 RPCSRC
#
CC = gcc
BIN = printmsg msg_svc rprintmsg
GEN = msg_clnt.c msg_svc.c msg.h
LIB =
LDFLAGS=
RPCCOM = ../../rpcgen/rpcgen -I
CFLAGS = -g

all: $(BIN)

#
# This is the non-networked version of the program
#
printmsg: printmsg.o
	$(CC) $(LDFLAGS) -o $@ printmsg.o

#
# note: no xdr routines are generated here, due this service's
#       use of basic data types.
#
$(GEN): msg.x
	$(RPCCOM) msg.x

msg_svc: msg_proc.o msg_svc.o
	$(CC) $(LDFLAGS) -o $@ msg_proc.o msg_svc.o $(LIB)

rprintmsg: rprintmsg.o msg_clnt.o
	$(CC) $(LDFLAGS) -o $@ rprintmsg.o msg_clnt.o $(LIB)

rprintmsg.o: rprintmsg.c msg.h

msg_proc.o: msg_proc.c msg.h

clean:
	rm -f $(GEN) *.o

clobber: clean
	rm -f $(BIN)

