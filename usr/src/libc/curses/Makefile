#
# This is Makefile of curses for Linux
#

include ../Makeconfig

include ../Makerules

LIB=../libcurses.a
LIBALIAS:=../$(REALLIBALIAS)

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libcurses

CURSESFLAGS=-DPOSIX -D__USE_BSD_SIGNAL

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS) $(CURSESFLAGS)
INC= -I.

OBJS=	addbytes.o addch.o addstr.o box.o clear.o clrtobot.o \
	clrtoeol.o cr_put.o cr_tty.o delch.o deleteln.o \
	delwin.o endwin.o erase.o fullname.o getch.o getstr.o \
	idlok.o id_subwins.o initscr.o insch.o insertln.o \
	longname.o move.o mvprintw.o mvscanw.o mvwin.o \
	newwin.o overlay.o overwrite.o printw.o putchar.o \
	refresh.o scanw.o scroll.o toucholap.o standout.o \
	touchwin.o tstp.o

#	curses.o unctrl.o
ALIASES =

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

#	$(RANLIB) $(LIB)
#	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>1&

obj: $(OBJS)

clean:
	$(RM) -f core *.o *.a tmp_make foo
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
