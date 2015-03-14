CC = gcc
PROG = rcp
OBJS = rcp.o # krcmd.o kcmd.o
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd # -DKERBEROS -DCRYPT 
LDLIBS = ../libbsd/libbsd.a # -lkrb -ldes
LDFLAGS = -N

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
