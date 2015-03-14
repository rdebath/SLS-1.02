CC = gcc
CFLAGS = -O2 -Wall -I..
LDFLAGS = -N
LDLIBS = ../rpcsvc/librpcsvc.a # -lrpclib
GENFILES = rstat_clnt.c
RPCGEN = ../rpcgen/rpcgen
RPCSVCDIR = ../rpcsvc
OBJS = rstat.o rstat_clnt.o

PROG = rstat

all: $(PROG)

$(PROG): $(OBJS)

rstat_clnt.c:
	$(RPCGEN) -l $(RPCSVCDIR)/rstat.x -o rstat_clnt.c

clean:
	rm -f core *.o *~ $(GENFILES)

clobber: clean
	rm -f $(PROG)	

