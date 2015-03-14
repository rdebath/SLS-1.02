CC = gcc
LDFLAGS = -N
CFLAGS = -O2 -include ../../../bsd/bsd.h -I../../../bsd -I.
OBJS = main.o getinfo.o debug.o send.o skip.o list.o subr.o commands.o
LEX = flex
LFLAGS = -I
LDLIBS = ../../../libbsd/libbsd.a
PROG = nslookup

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) -o $(PROG) $(OBJS) $(LDLIBS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

