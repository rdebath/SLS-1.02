CC = gcc
PROG = rshd
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd \
	-Denviron=rshd_environ -DSHADOW_PWD # -DKERBEROS -DCRYPT
OBJS = rshd.o # des_rw.o
LDLIBS = ../libbsd/libbsd.a -lshadow # -lutil -lkrb -ldes
LDFLAGS = -lshadow

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
