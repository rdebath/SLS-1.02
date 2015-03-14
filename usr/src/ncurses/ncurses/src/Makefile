
# This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for 
#  details. If they are missing then this copy is in violation of  
#  the copyright conditions.                                        

# SRCDIR is the directory where terminfo entries, the compile and
# dump programs, live. Change as you see fit.

SRCDIR= /usr/lib/terminfo
CFLAGS= -O6 -DSRCDIR='"$(SRCDIR)"' 
DFLAGS= -DTRACE -DSRCDIR='"$(SRCDIR)"'
PFLAGS= -pg -DTRACE -DSRCDIR='"$(SRCDIR)"'
CC= gcc 

INCLUDE= /usr/include
LIB= /usr/lib

SRCS=	comp_main.c comp_hash.c comp_captab.c comp_scan.c comp_error.c \
	comp_parse.c read_entry.c \
	dump.c names.c  \
	lib_setup.c lib_kernel.c lib_tparm.c lib_tputs.c lib_slk.c lib_pad.c\
	lib_unctrl.c lib_raw.c lib_vidattr.c lib_trace.c lib_ti.c lib_keyname.c\
	lib_beep.c lib_doupdate.c lib_refresh.c lib_initscr.c lib_newwin.c \
	lib_addch.c lib_addstr.c lib_scroll.c lib_clreol.c lib_touch.c \
	lib_mvcur.c lib_delwin.c lib_endwin.c lib_clrbot.c lib_move.c \
	lib_printw.c lib_scanw.c lib_erase.c lib_getch.c lib_options.c \
	lib_box.c lib_clear.c lib_delch.c lib_deleteln.c lib_insch.c \
	lib_insertln.c lib_getstr.c lib_mvwin.c lib_longname.c lib_tstp.c \
	lib_newterm.c lib_set_term.c lib_overlay.c lib_scrreg.c lib_color.c

#
# Normal, optimised object files
#
LIBOBJS= lib_setup.o lib_kernel.o lib_tparm.o read_entry.o lib_tputs.o lib_pad.o\
	lib_unctrl.o lib_raw.o lib_vidattr.o lib_trace.o lib_beep.o names.o\
	lib_doupdate.o lib_refresh.o lib_initscr.o lib_newwin.o lib_addch.o \
	lib_addstr.o lib_scroll.o lib_clreol.o lib_touch.o lib_mvcur.o lib_keyname.o\
	lib_delwin.o lib_endwin.o lib_clrbot.o lib_move.o lib_printw.o \
	lib_scanw.o lib_erase.o lib_getch.o lib_options.o lib_acs.o lib_slk.o\
	lib_box.o lib_clear.o lib_delch.o lib_deleteln.o lib_insch.o lib_ti.o\
	lib_insertln.o lib_getstr.o lib_mvwin.o lib_longname.o lib_tstp.o \
	lib_newterm.o lib_set_term.o lib_overlay.o lib_scrreg.o lib_color.o

#
# Debugging object files
#
DLIBOBJS= lib_setup.d lib_kernel.d lib_tparm.d read_entry.d lib_tputs.d lib_pad.d\
	lib_unctrl.d lib_raw.d lib_vidattr.d lib_trace.d lib_beep.d names.d\
	lib_doupdate.d lib_refresh.d lib_initscr.d lib_newwin.d lib_addch.d \
	lib_addstr.d lib_scroll.d lib_clreol.d lib_touch.d lib_mvcur.d lib_keyname.d\
	lib_delwin.d lib_endwin.d lib_clrbot.d lib_move.d lib_printw.d \
	lib_scanw.d lib_erase.d lib_getch.d lib_options.d lib_acs.d lib_slk.d\
	lib_box.d lib_clear.d lib_delch.d lib_deleteln.d lib_insch.d lib_ti.p\
	lib_insertln.d lib_getstr.d lib_mvwin.d lib_longname.d lib_tstp.d \
	lib_newterm.d lib_set_term.d lib_overlay.d lib_scrreg.d lib_color.d

#
# Profiled Optimised object files
 
PLIBOBJS= lib_setup.p lib_kernel.p lib_tparm.p read_entry.p lib_tputs.p lib_pad.p\
	lib_unctrl.p lib_raw.p lib_vidattr.p lib_trace.p lib_beep.p names.p\
	lib_doupdate.p lib_refresh.p lib_initscr.p lib_newwin.p lib_addch.p \
	lib_addstr.p lib_scroll.p lib_clreol.p lib_touch.p lib_mvcur.p lib_keyname.p\
	lib_delwin.p lib_endwin.p lib_clrbot.p lib_move.p lib_printw.p \
	lib_scanw.p lib_erase.p lib_getch.p lib_options.p lib_acs.p lib_slk.p\
	lib_box.p lib_clear.p lib_delch.p lib_deleteln.p lib_insch.p lib_ti.p\
	lib_insertln.p lib_getstr.p lib_mvwin.p lib_longname.p lib_tstp.p \
	lib_newterm.p lib_set_term.p lib_overlay.p lib_scrreg.p lib_color.p

COMPOBJS= comp_main.o comp_hash.o comp_captab.o comp_scan.o comp_error.o \
	comp_parse.o read_entry.o

.SUFFIXES: .d .p

.c.d:
	$(CC) -c $(DFLAGS) -o $*.d $*.c

.c.p:
	$(CC) -pg -c $(CFLAGS) -o $*.p $*.c

LIBS= libncurses.a libdcurses.a libpcurses.a

all:	tic untic $(LIBS) 

install: all
	sed -e 's/clude *"curses.h" *$$/clude <ncurses.h>/' terminfo.h > $(INCLUDE)/term.h
	cp curses.h $(INCLUDE)/ncurses.h
	cp unctrl.h $(INCLUDE)/unctrl.h
	cp $(LIBS) $(LIB)              
	if [ ! -d $(SRCDIR) ]; then mkdir $(SRCDIR); fi
	install -m 500 -o root -g system tic $(SRCDIR)/tic
	install -m 500 -o root -g system untic $(SRCDIR)/untic
	cp ../data/* $(SRCDIR)
	(cd $(SRCDIR); ./tic console)

lib:	terminfo.h libncurses.a	
libncurses.a:	${LIBOBJS}
	ar rv libncurses.a ${LIBOBJS}
	ranlib libncurses.a
	cp *.a $(LIB)

dlib:	libdcurses.a	
libdcurses.a:	${DLIBOBJS}
	ar rv libdcurses.a ${DLIBOBJS}
	ranlib libdcurses.a
	cp *.a $(LIB)

plib:	libpcurses.a	
libpcurses.a:	${PLIBOBJS}
	ar rv libpcurses.a ${PLIBOBJS}
	ranlib libpcurses.a

tic: ${COMPOBJS}
	$(CC) -o tic ${COMPOBJS}

untic: dump.o names.o read_entry.o
	$(CC) -o untic dump.o names.o read_entry.o

keys.tries: keys.list
	awk -f MKkeys.awk keys.list > keys.tries

lib_options.c: keys.tries

terminfo.h: Caps MKterm.h.awk
	awk -f MKterm.h.awk Caps > terminfo.h

comp_captab.c: Caps MKcaptab.awk
	awk -f MKcaptab.awk Caps > comp_captab.c

comp_main.o: compiler.h terminfo.h

dump.o: dump.c terminfo.h compiler.h

lib_keyname.c: keys.list MKkeyname.awk
	awk -f MKkeyname.awk keys.list > lib_keyname.c

names.c: Caps MKnames.awk
	awk -f MKnames.awk Caps
	cat boolnames boolfnames boolcodes numnames numfnames numcodes strnames strfnames strcodes> names.c
	-rm boolnames boolfnames boolcodes numnames numfnames numcodes strnames strfnames strcodes

clean:
	-rm -f *.[aodp] comp_captab.c tags lib_keyname.c keys.tries names.c

clobber: clean
	-rm tic untic terminfo.h

