CC	= gcc
CFLAGS	= -O6 -m486 -funroll-loops -Wall
LD	= gcc
LDFLAGS	= -s -N

BINDIR	= /usr/local/bin

OBJS	= str.o strplay.o

.c.o:
	$(CC) -c $(CFLAGS) -o $*.o $<

all: str srec

install: all
	cp str srec $(BINDIR)
	chown bin.bin $(BINDIR)/str $(BINDIR)/srec
	chmod 755 $(BINDIR)/str $(BINDIR)/srec
	ln -f $(BINDIR)/str $(BINDIR)/str15
	ln -f $(BINDIR)/str $(BINDIR)/str32
	ln -f $(BINDIR)/srec $(BINDIR)/splay

str: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o str

srec: recplay.o
	$(LD) $(LDFLAGS) recplay.o -o srec

clean:
	rm -f $(OBJS) recplay.o str srec a.out core

str.o: str.c str.h
strplay.o: strplay.c str.h
recplay.o: recplay.c
