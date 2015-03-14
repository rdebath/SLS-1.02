CC = gcc
PROG = rlogin
OBJS = rlogin.o # krcmd.o kcmd.o des_rw.o
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -DBSD=43 # -DKERBEROS -DCRYPT
LDLIBS = ../libbsd/libbsd.a # -lkrb -ldes
LDFLAGS =

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
