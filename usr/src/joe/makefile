# Makefile for Joe's Own Editor

CC = cc
CFLAGS = -O
OBJS = main.o termcap.o vfile.o pathfunc.o queue.o blocks.o vs.o va.o scrn.o \
       b.o bw.o tw.o pw.o help.o heap.o toomany.o queue.o zstr.o edfuncs.o \
       kbd.o w.o reg.o tab.o pattern.o random.o regex.o undo.o menu.o macro.o \
       poshist.o

foo:
	@echo Type make followed by one of the following
	@echo
	@echo bsd hpux xenix sv posix termidx install clean

xenix: $(OBJS) ttyxenix.o olddir.o
	$(CC) $(CFLAGS) -o joe $(OBJS) ttyxenix.o olddir.o -lx

posix: $(OBJS) ttyposix.o
	$(CC) $(CFLAGS) -o joe $(OBJS) ttyposix.o

bsd: $(OBJS) ttybsd.o
	$(CC) $(CFLAGS) -o joe $(OBJS) ttybsd.o

sv: $(OBJS) ttysv.o
	$(CC) $(CFLAGS) -o joe $(OBJS) ttysv.o

hpux: $(OBJS) ttyhpux.o
	$(CC) $(CFLAGS) -o joe $(OBJS) ttyhpux.o

termidx: termidx.o
	$(CC) $(CFLAGS) -o termidx termidx.o

install: joe termidx
	strip joe
	strip termidx
	mv joe /usr/local/bin
	cp joerc /usr/local/lib/joerc
	mv termidx /usr/local/bin
	chmod a+x /usr/local/bin/joe
	chmod a+r /usr/local/lib/joerc
	chmod a+x /usr/local/bin/termidx

clean:
	rm -f $(OBJS) ttyxenix.o ttyposix.o ttybsd.o ttyhpux.o ttysv.o
