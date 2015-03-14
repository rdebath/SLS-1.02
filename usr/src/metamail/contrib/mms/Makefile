# Copyright (c) 1992 Electrotechnical Laboratry (ETL)
#
# Permission to use, copy, modify, and distribute this material
# for any purpose and without fee is hereby granted, provided
# that the above copyright notice and this permission notice
# appear in all copies, and that the name of ETL not be
# used in advertising or publicity pertaining to this
# material without the specific, prior written permission
# of an authorized representative of ETL.
# ETL MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY
# OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS",
# WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.

#
#	MMS: Metamail Server
#		May 1992, Yutaka Sato <ysato@etl.go.jp>
#

FOOBAR-sun4 =	-Bstatic
STATICFLAG =	FOOBAR${HOST_ARCH}
CFLAGS =	-g -I.  ${$(STATICFLAG)} 
LDLIBS =
CONFIGDIR =	../..

METAMAILDIR =	../../metamail
METAMAILLIB =	_mailto.o _codes.o _uue.o
RICHMAILDIR =	../../richmail
RICHMAILLIB =	_richtext.o _richlex.o
LOCALCFLAGS = -I${CONFIGDIR} -I${METAMAILDIR} -I${RICHMAILDIR} -Ilib ${CFLAGS}

MMS_SRCS =	mms.c \
		mmsserver.c \
		mmsauth.c \
		mmsclient.c \
		mmsparser.c \
		mmsviewer.c \
		mmssynthe.c \
		mmstransl.c \
		mmsencode.c \
		str_codes.c \
		lib/str_stdio.h \
		lib/str_stdio.c \
		lib/strcasecmp.c \
		lib/strstr.c

MMS_OBJS =	mms.o \
		mmsserver.o \
		mmsauth.o \
		mmsclient.o \
		mmsparser.o \
		mmsviewer.o \
		mmssynthe.o \
		mmstransl.o \
		mmsencode.o \
		str_codes.o \
		lib/str_stdio.o \
		lib/strcasecmp.o \
		lib/strstr.o

MMSFILES =	README.MMS \
		Makefile \
		mmstest.sh \
		test.mms \
		mms.el \
		$(MMS_SRCS) \
		news/README \
		news/README.jp \
		news/Makefile \
		news/mmhencode.c \
		news/xhist.c \
		news/xmime.c \
		news/mime-patch1.5.11

.c.o:
		rm -f $@
		cc -c  $(LOCALCFLAGS) $*.c

all:		mms libmms.a
clean:
		-rm -f mms _*.c *.o *.BAK

mms:		Makefile libmms.a $(MMS_OBJS) $(METAMAILLIB) $(RICHMAILLIB)
		cc $(LOCALCFLAGS) -o mms mms.o libmms.a -ltermcap $(LDLIBS)

libmms.a:	$(MMS_OBJS) $(METAMAILLIB) $(RICHMAILLIB)
		ar r libmms.a $(MMS_OBJS) $(METAMAILLIB) $(RICHMAILLIB)
		ranlib libmms.a

str_codes.o:	$(METAMAILDIR)/codes.c str_codes.c lib/str_stdio.h
		cc -c $(LOCALCFLAGS) str_codes.c

_mailto.o:	$(METAMAILDIR)/mailto.c
		sed -e 's/^static char \*CharacterSet /char *CharacterSet  /'\
			< $(METAMAILDIR)/mailto.c > _mailto.c; \
		cc -c $(LOCALCFLAGS) _mailto.c -o _mailto.o \
			-Dmain=mailto_main;

_codes.o:	$(METAMAILDIR)/codes.c lib/str_stdio.h
		cc -c $(LOCALCFLAGS) $(METAMAILDIR)/codes.c -o _codes.o

_uue.o:		$(METAMAILDIR)/uue.c
		cc -c $(LOCALCFLAGS) $(METAMAILDIR)/uue.c -o _uue.o

_richtext.o:	Makefile $(RICHMAILDIR)/richtext.c
		cc -c $(LOCALCFLAGS) $(RICHMAILDIR)/richtext.c -o _richtext.o \
			-Dmain=richtext_main \
			-DInitSignals=richtext_InitSignals \
			-DFinalizeTerminal=richtext_FinalizeTerminal \
			-Dcleanup=richtext_cleanup \
			-Dcontrolputc=richtext_controlputc \
			-Dnomemabort=richtext_nomemabort \
			-Drichtextreset=richtext_richtextreset

_richlex.o:	$(RICHMAILDIR)/richlex.c
		cc -c $(LOCALFLAGS) $(RICHMAILDIR)/richlex.c -o _richlex.o \
			-Drichtextreset=richtext_richtextreset

pr:;		pr -f $(MMSFILES) | lpr
ar:		../mms.tar ../mms.shar
../mms.tar:	tar $(MMSFILES)
		rcp    "../mms-"`mms -version`".tar" \
			etlfs:/usr/share/src/mime

tar:		$(MMSFILES)
		tar cf ../mms.tar $(MMSFILES)
		tar cf "../mms-"`mms -version`".tar" $(MMSFILES)
		compress < ../mms.tar | uuencode mms.tar.Z > ../mms.tar.Z.uu

../mms.shar:	$(MMSFILES)
		shar $(MMSFILES) > ../mms.shar


lib/strcasecmp.o:	lib/strcasecmp.c
			cd lib; cc -c -O strcasecmp.c
lib/strstr.o:		lib/strstr.c
			cd lib; cc -c -O strstr.c
lib/str_stdio.o:	lib/str_stdio.c lib/str_stdio.h
			cd lib; cc -c -O str_stdio.c
