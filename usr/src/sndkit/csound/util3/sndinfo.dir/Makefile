# Makefile.m4	1.1	(IRCAM)	8/3/85	00:02:43 
# flavor of UNIX in use
# paths to libraries
# final resting place for installed software
# directory for carl include files
# where help files will live
# directory for tutorials 
# location of sources at CARL
# path to manual pages
# command to install man
# path to sys include directory
# path to vaxuba include directory

PROG =		sndinfo

INCLUDE =	/usr/include/local
DESTDIR =	/usr/local/ircam
CFLAGS =	-O -I${INCLUDE}
INSTALLMAN =	cp ${PROG}.1 /usr/local/man/man1
LIBS =		/usr/local/lib/sound/libcarl.a /usr/local/lib/sound/libieee.a -lnm -lm 
OBJS =		
DBUG =

${PROG}:	${PROG}.o ${OBJS} ${DBUG}
		${CC} ${CFLAGS} ${PROG}.o ${DBUG} ${OBJS} ${LIBS} -o ${PROG}

install:	${PROG}
		cp ${PROG} ${DESTDIR}
		${INSTALLMAN}
		date > install
clean:
		rm -f core a.out *.o ${PROG}

ircam:		${PROG}
		cp ${PROG} ..
		date > ircam
