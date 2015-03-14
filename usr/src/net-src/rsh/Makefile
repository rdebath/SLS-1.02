CC = gcc
PROG = rsh
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd # -DKERBEROS -DCRYPT
OBJS = rsh.o # krcmd.o kcmd.o des_rw.o
LDLIBS = ../libbsd/libbsd.a # -lkrb -ldes
LDFLAGS = -N

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
