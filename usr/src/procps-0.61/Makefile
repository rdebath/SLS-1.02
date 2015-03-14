libobjs= snap.o devname.o pwcache.o alloc.o whattime.o sysinfo.o
liba= libproc.a
lib= proc
mandir= /usr/man/man1
instdir= /bin
etcdir= /etc
xinstdir= /usr/X386/bin
xdefinstdir= /usr/X386/lib/X11/app-defaults
install= mv

CC= gcc
CFLAGS= -O6 -Wall
#CFLAGS= -O6 -Wall -ggdb
LFLAGS= -s -L.
#LFLAGS= -L. -ggdb

all:	$(liba) ps uptime tload free w top psupdate

$(liba):	$(libobjs)
	ar rcs $(liba) $(libobjs)

ps:	$(liba) ps.o
	gcc $(LFLAGS) -N -o ps ps.o -l$(lib)

w:	$(liba) w.o
	gcc $(LFLAGS) -N -o w w.o -l$(lib)

psupdate:	$(liba) psupdate.o
	gcc $(LFLAGS) -N -o psupdate psupdate.o -l$(lib)

top:	$(liba) top.o
	gcc $(LFLAGS) -N -o top top.o -ltermcap -l$(lib)

uptime:	$(liba) uptime.o
	gcc $(LFLAGS) -N uptime.o -o uptime -l$(lib)

tload:	$(liba) tload.o
	gcc $(LFLAGS) -N tload.o -o tload -l$(lib)

free:	free.o
	gcc $(LFLAGS) -N free.o -o free

utmp:	utmp.o
	gcc $(LFLAGS) -N utmp.o -o utmp

xproc/Makefile: xproc/Imakefile
	(cd xproc; xmkmf -a)

xload:	xproc/xload.c xproc/get_load.c xproc/Makefile
	(cd xproc; make xload; strip xload; mv xload ..)

xmem:	xproc/xmem.c xproc/get_mem.c xproc/Makefile
	(cd xproc; make xmem; strip xmem; mv xmem ..)

XConsole:	xproc/XConsole.o
	(cd xproc; \
	gcc $(CFLAGS) -L /usr/X386/lib -s -o $@ XConsole.o \
	 -lXaw -lXmu -lXt -lX11 -lm; \
	mv XConsole ..)

#	(cd xproc; make XConsole; strip XConsole; mv XConsole ..)



xprogs:	xload xmem XConsole

install:
	for i in ps uptime tload free w top; do \
	if [ -x $$i ] ; then \
	chown root $$i; \
	$(install) $$i $(instdir); fi; done
	if [ -x psupdate ] ; then \
	chown root psupdate; \
	$(install) psupdate $(etcdir); fi;
	cp *.1 $(mandir)
	if [ -x xload ] ; then \
	chown root xload; $(install) xload $(xinstdir); \
	cp xproc/XLoad.ad $(xdefinstdir)/XLoad; fi
	if [ -x xmem ] ; then \
	chown root xmem; $(install) xmem $(xinstdir); \
	cp xproc/XMem.ad $(xdefinstdir)/Xmem; fi
	if [ -x XConsole ] ; then \
	chown root XConsole; chmod u+s XConsole; \
	$(install) XConsole $(xinstdir); fi

clean:	
	rm -f xproc/Makefile* xproc/*.o
	rm -f xproc/xmem.c xproc/get_mem.c
	rm -f $(liba)
	rm -f ps psupdate uptime tload free w top utmp xload xmem XConsole
	rm -f *.o
