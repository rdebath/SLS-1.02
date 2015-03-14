CC = gcc
PROG = rlogind
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -Dnotyet # -DKERBEROS -DCRYPT
OBJS = rlogind.o # des_rw.o
LDLIBS = ../libbsd/libbsd.a # -lutil -lkrb -ldes
LDFLAGS =

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
