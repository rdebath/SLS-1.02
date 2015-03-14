# makefile for rpcgen
SRCS = rpc_main.c rpc_hout.c rpc_cout.c rpc_parse.c rpc_scan.c rpc_util.c \
	rpc_svcout.c rpc_clntout.c
HDRS = rpc_util.h rpc_parse.h rpc_scan.h
OBJS = $(SRCS:.c=.o)

PROG = rpcgen-old
CC = gcc
CFLAGS = -O2
LDFLAGS = -N
LDLIBS =
DESTDIR=

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

$(OBJS): $(HDRS)

install: $(PROG)
	install -s $(PROG) $(DESTDIR)/usr/bin

clean:
	rm -f $(OBJS)

clobber: clean
	rm -f $(PROG) 

