#
#	MIME header translator
#		toMime, fromMime and XMIME/nntpd
#
#			92.05.25 Yutaka Sato <ysato@etl.go.jp>
#


PATCHVER =	1.5.11
PATCHDONE =	patched-$(PATCHVER)
PATCHFILE =	mime-patch$(PATCHVER)

LOCALFILES =	README Makefile \
		mmhencode.c xmime.c xhist.c $(PATCHFILE)

FILES =		$(LOCALFILES) \
		mmsencode.c str_codes.c str_stdio.h str_stdio.c \
		strcasecmp.c strstr.c \
		codes.c config.h

OBJS	=	mmsencode.o str_codes.o str_stdio.o codes.o \
		strcasecmp.o strstr.o
LINKS	= str_stdio.h config.h codes.c mmsencode.c str_codes.c str_stdio.c 

ALL	=	toMime fromMime nntpd.patch

.c.o:
		rm -f $@
		cc -c -I. -O $*.c

all:		$(LINKS) libmms.a $(ALL)

libmms.a:	$(OBJS)
		ar r libmms.a $(OBJS)
		ranlib libmms.a

fromMime:	mmhencode.c $(OBJS)
		cc -o fromMime -DMIME2LOCAL mmhencode.c  $(OBJS)

toMime:		mmhencode.c $(OBJS)
		cc -o toMime -DLOCAL2MIME mmhencode.c  $(OBJS)

nntpd.patch:;	sh $(PATCHFILE)

ar:		mimehead.tar
mimehead.tar:	$(FILES)
		tar cfh mimehead.tar $(FILES)

dist:		nntp+mime.Z.uu
nntp+mime.Z.uu:	mimehead.tar
		compress<mimehead.tar | uuencode nntp+mime.Z > nntp+mime.Z.uu

clean:;		rm $(OBJS) libmms.a toMime fromMime

codes.c:;	ln -s ../../../metamail/codes.c .
config.h:;	ln -s ../../../config.h .
mmsencode.c:;	ln -s ../mmsencode.c .
str_codes.c:;	ln -s ../str_codes.c .
str_stdio.c:;	ln -s ../lib/str_stdio.c .
str_stdio.h:;	ln -s ../lib/str_stdio.h .
strcasecmp.c:;	ln -s ../lib/strcasecmp.c .
strstr.c:;	ln -s ../lib/strstr.c .

