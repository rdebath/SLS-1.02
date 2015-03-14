CC = gcc
PROG = talk
OBJS = finger.o lprint.o net.o sprint.o util.o
OBJS = ctl.o ctl_transact.o display.o get_addrs.o get_names.o \
	init_disp.o invite.o io.o look_up.o msgs.o talk.o
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -DMSG_EOR -Dosockaddr=sockaddr
LDFLAGS = -N
LDLIBS = ../libbsd/libbsd.a -lcurses

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
