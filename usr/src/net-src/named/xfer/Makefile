PROG = named-xfer
OBJS = named-xfer.o ../db_glue.o
CFLAGS = -O2 -include ../../bsd/bsd.h -I../../bsd -I..
LDFLAGS = -N
LDLIBS = ../../libbsd/libbsd.a

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

