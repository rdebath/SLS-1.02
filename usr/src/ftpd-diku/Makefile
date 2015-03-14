CFLAGS= -O6 -s  -I. -Inetinet -Isupport -DSETPROCTITLE -DNBBY=8 -DLOG_DAEMON -DSHADOW_PWD
CC=gcc -w
LIBC=-lshadow #-lnet
YACC=bison -y
MISSINGOBJS= fnmatch.o strcasestr.o ftw.o

SRCS=	ftpd.c ftpcmd.c glob.c logwtmp.c popen.c access.c extensions.c \
		realpath.c acl.c private.c authenticate.c free.c
OBJS=	ftpd.o ftpcmd.o glob.o logwtmp.o popen.o access.o extensions.o \
		realpath.o acl.o private.o authenticate.o free.o ${MISSINGOBJS}
MAN=	ftpd.8

BINDIR=		/usr/bin
ETCDIR=		/usr/etc/inet
MANDIR=		/usr/man
MANEXT=		8
LINTFLAGS=	

all: ftpd ftpcount mkgroup

check-headers:
	@if [ `uname -s` = "Linux" ]; then ./check-h; fi

${MISSINGOBJS}:
	( cd support; make ${MISSINGOBJS} )

ftpcount:	ftpcount.c pathnames.h ${MISSINGOBJS}
	${CC} ${CFLAGS} ftpcount.c ${MISSINGOBJS} -o ftpcount ${LIBC}

ftpd: check-headers ${OBJS} ${LIBC} 
	${CC} ${CFLAGS} -o ftpd ${OBJS} ${LIBC}

mkgroup: mkgroup.o
	${CC} ${CFLAGS} -o $@ $@.c ${LIBC}

clean: 
	-rm -f ${OBJS} $@ core ftpcmd.c *.o ftpd ftpcount mkgroup ${MISSINGOBJS}
	-(cd support; make clean)

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN} ftpd ftpcount
	-mv -f ${ETCDIR}/ftpd ${ETCDIR}/ftpd-old
	install -c -s -o bin -g bin -m 755 ftpd ${ETCDIR}/ftpd
	install -c -s -o bin -g bin -m 755 ftpcount ${BINDIR}/ftpcount
	install -c -o bin -g bin -m 755 ftpd.8 ${MANDIR}/man${MANEXT}/ftpd.${MANEXT}

lint: ${SRCS}
	lint ${CFLAGS} ${LINTFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

FRC:	

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

