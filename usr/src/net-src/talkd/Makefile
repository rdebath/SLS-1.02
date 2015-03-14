CC = gcc
PROG = ntalkd
OBJS = talkd.o announce.o process.o table.o print.o
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -Dosockaddr=sockaddr
LDFLAGS = -N
LDLIBS = ../libbsd/libbsd.a

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) -o $(PROG) $(OBJS) $(LDLIBS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
