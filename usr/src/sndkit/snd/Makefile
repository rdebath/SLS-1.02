# Makefile for any machine
#
# $Author: espie $
# $Id: makefile,v 1.9 1992/10/27 22:52:59 espie Exp espie $
# $Revision: 1.9 $
# $Log: makefile,v $
# Revision 1.9  1992/10/27  22:52:59  espie
# *** empty log message ***
#
# Revision 1.8  1992/07/23  13:52:28  espie
# Added tools.c.
#
# Revision 1.7  1992/07/22  14:50:25  espie
# *** empty log message ***
#
# Revision 1.6  1992/07/17  13:43:00  espie
# *** empty log message ***
#
# Revision 1.5  1992/07/16  17:02:00  espie
# Added randomize/lookup/open.
#
# Revision 1.4  1992/07/15  12:45:41  espie
# *** empty log message ***
#
# Revision 1.3  1992/07/14  14:23:41  espie
# Small changes.
#
# Revision 1.2  1992/07/14  13:20:53  espie
# *** empty log message ***
#
# Revision 1.1  1992/07/13  17:21:11  espie
# Initial revision

INSTALL_DIR = $(HOME)/bin/sparc

sparc:
	$(MAKE) all MACHINE=ss10 O=.o CC=cc MAIN_OPTS="-O" \
	COPTS="-O -c -Bstatic" LIBS=-lm
sgi:
	$(MAKE) all MACHINE=sgi O=.u MAIN_OPTS="-O" \
	COPTS="$(MAIN_OPTS) -j -DSIGNED" LIBS="-laudio -lm"
pc:
	$(MAKE) all MACHINE=soundblaster O=.o MAIN_OPTS="-O" \
	COPTS="$(MAIN_OPTS) -c" LIBS="-lm"
linux:
	$(MAKE) all MACHINE=linux O=.o MAIN_OPTS="" \
	COPTS="$(MAIN_OPTS) -O6 -funroll-loops -c" LIBS=-lm
	
OBJECTS = main$O $(MACHINE)_audio$O read$O commands$O \
audio$O automaton$O player$O getopt$O open$O tools$O \
termio$O dump_song$O setup_audio$O notes$O

O2 = analyzer$O read$O open$O dump_song$O tools$O notes$O


all: tracker randomize analyzer

install: 
	-rm -f $(INSTALL_DIR)/tracker $(INSTALL_DIR)/randomize \
	$(INSTALL_DIR)/analyzer
	cp tracker $(INSTALL_DIR)
	chmod a-w $(INSTALL_DIR)/tracker
	cp randomize $(INSTALL_DIR)
	chmod a-w $(INSTALL_DIR)/randomize
	cp analyzer $(INSTALL_DIR)
	chmod a-w $(INSTALL_DIR)/analyzer

devel: 
	-rm -f $(INSTALL_DIR)/dtracker 
	cp tracker $(INSTALL_DIR)/dtracker

tracker: ${OBJECTS}
	$(CC) -o tracker ${MAIN_OPTS} ${OBJECTS} ${LIBS}
#	strip tracker

analyzer: ${O2}
	$(CC) -o analyzer ${MAIN_OPTS} ${O2} ${LIBS}

randomize: randomize.c
	$(CC) -o randomize ${MAIN_OPTS} randomize.c 

main$O: main.c song.h
	$(CC) ${COPTS} main.c

tools$O: main.c 
	$(CC) ${COPTS} tools.c
$(MACHINE)_audio$O: $(MACHINE)_audio.c 
	$(CC) ${COPTS} $(MACHINE)_audio.c

open$O: open.c
	$(CC) ${COPTS} open.c

audio$O: audio.c
	$(CC) ${COPTS} audio.c

automaton$O: automaton.c song.h channel.h
	$(CC) ${COPTS} automaton.c

getopt$O: getopt.c getopt.h
	$(CC) ${COPTS} getopt.c
getopt1$O: getopt1.c getopt.h
	$(CC) ${COPTS} getopt1.c

player$O: player.c song.h channel.h
	$(CC) ${COPTS} player.c

read$O: read.c song.h
	$(CC) ${COPTS} read.c

commands$O: commands.c channel.h song.h
	$(CC) ${COPTS} commands.c

termio$O: termio.c
	$(CC) ${COPTS} termio.c
dump_song$O: dump_song.c
	$(CC) $(COPTS) dump_song.c
analyzer$O: analyzer.c
	$(CC) $(COPTS) analyzer.c
setup_audio$O: setup_audio.c
	$(CC) $(COPTS) setup_audio.c
notes$O: notes.c
	$(CC) $(COPTS) notes.c


clean:
	-rm *.u *.o tracker randomize core str.tar str.tar.Z
export:
	-rm tracker.tar.Z
	tar cvf tracker.tar *.c *.h Makefile docs/* 
	compress tracker.tar
