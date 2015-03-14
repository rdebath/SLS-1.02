CC = gcc
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -include ftphack.h -DSETPROCTITLE -DSHADOW_PWD
OBJS = ftpd.o ftpcmd.o glob.o logwtmp.o popen.o vers.o
YACC = bison -y
LDFLAGS =
LDLIBS = ../libbsd/libbsd.a -lshadow
PROG = ftpd

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
