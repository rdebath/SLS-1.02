CC = gcc
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd
LDFLAGS = -N
OBJS = cmds.o cmdtab.o ftp.o glob.o main.o domacro.o
PROG = ftp

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
