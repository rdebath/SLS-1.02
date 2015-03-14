#								Csound Makefile
# edit the following definitions according to need:

INCLUDE = /usr/local/include
LIBLIB = /usr/local/lib/lib
DESTDIR = /usr/local/bin
MANDIR = /usr/man
MANL = $(MANDIR)/manl
CATL = $(MANDIR)/catl

DEFINES = -DWINDOWS -DSFIRCAM
# choose any of -
#	-DSYS5:	    invoke System 5 headers (e.g. 'string.h' for 'strings.h')
#			(must also enable this in util/scot/Makef)
#	-DWINDOWS:  include window system calls (to X11 or Starbase graphics)
#	-DLOFI	    include real-time sound i/o via DEC lofi board
# and at most one of -
#	-DSFIRCAM:  read/write IRCAM-format soundfiles, with 1024-byte header;
#	-DSFSUN41:  read/write SUN4.1-format soundfiles, with their own header;
#			else read/write sound with no headers

CFLAGS = -O -f $(DEFINES) -I$(INCLUDE)/csound
#	-O2		to be more specific about the optimization level
#	-f		compile single-precision floats (4.3,VAX,DEC-RISC)
# 	-float  	as above, for Silicon Graphics Unix
# 	-fsingle	as above, for SUN Unix
#	-f68881		employ the 68881 co-processor
#	-ffpa		employ the SUN fl-pt accelerator
#	+x		generate HP inline 68020 and 68881 code
#	+Nd1200		expand SYS5 pass3 tablesize
#	-I/usr/demo/SOUND/multimedia	to find SUN4.1 audio includes

LIBS =	-lcsound -lX11 -lm
#	-lcsound				shared csound modules
#	-lX11					for X window calls
#	-lbsdipc -ldd98710 -lsb1 -lsb2		for HP Starbase Graphics
#	-l881					tell -lm to use 68881
#	-laudio					on SUN, for SFSUN41

WINOBJ = winX11.o
# if WINDOWS enabled in DEFINES, choose one of -
#	winX11.o	for X11 window graphics
#    	win98.o		for Starbase Graphics on HP98700 terminals

LOFIOBJ =
# if LOFI enabled in DEFINES, include
#	LoFi.o	play.o

# do not edit below this line
# --------------------------------------------------------------------------

# macros passed to lower-level Makef files
#	CFLAGS	DESTDIR	INCLUDE	LIBLIB
M = "CFLAGS=$(CFLAGS)" "DESTDIR=$(DESTDIR)" "INCLUDE=$(INCLUDE)" "LIBLIB=$(LIBLIB)"

SHELL = /bin/sh

OBJS =	main.o musmon.o oload.o rdorch.o express.o rdscor.o rtevents.o insert.o \
	entry.o fgens.o aops.o ugens1.o ugens2.o ugens3.o ugens4.o ugens5.o \
	ugens6.o ugens7.o disprep.o window.o winascii.o $(WINOBJ) \
	sfheaders.o soundin.o soundio.o ulaw.o \
	memalloc.o memfiles.o auxfd.o rtctl.o $(LOFIOBJ)

SOBJS = scsort.o scxtract.o sread.o sort.o extract.o twarp.o swrite.o

I	 = $(INCLUDE)/csound
CSLIB	 = $(LIBLIB)csound.a
PROGS    = csound
SUBDIRS  = anals utils1 utils2  #utils3			#utils3 not curr operative

all:		includes $(CSLIB) $(PROGS) $(SUBDIRS)	#must be run in this order

includes:
		-mkdir $I
		cd include; cp *.h $I
		cd util1/cscore; cp cscore.h $I
#		-rmdir $I
#		ln -s include $I
#		cp util1/cscore/cscore.h include

csound:		$(OBJS) $(SOBJS) $(CSLIB)
		cc $(CFLAGS) -n -o csound $(OBJS) $(SOBJS) $(LIBS)

$(OBJS):	$I/sysdep.h cs.h prototypes.h

$(SOBJS):	sort.h

oload.o:	oload.h

insert.o:	insert.h aops.h

entry.o:	insert.h aops.h ugens1.h ugens2.h ugens3.h $I/lpc.h ugens4.h \
		ugens5.h ugens6.h ugens7.h $I/dsputil.h window.h rtctl.h \
		disprep.h soundio.h

aops.o:		aops.h

ugens1.o:	ugens1.h

ugens2.o:	ugens2.h

ugens3.o:	ugens3.h

ugens4.o:	ugens4.h $I/lpc.h

ugens5.o:	ugens5.h

ugens6.o:	ugens6.h

ugens7.o:	ugens7.h soundio.h $I/fft.h $I/dsputil.h $I/pvoc.h

disprep.o:	disprep.h window.h $I/fft.h $I/dsputil.h

window.o winascii.o winX11.o:	window.h

rtctl.o:	window.h rtctl.h

fgens.o:	window.h soundio.h

main.o musmon.o soundin.o soundio.o:  soundio.h

sfheaders.o:	soundio.h $I/sfheader.h

LoFi.o play.o:	LoFi.h


MAKE = make -f Makef $M

LIBSRC = lib/dsputil.c lib/fft.c lib/pvoc.c lib/sndf.c
LIBHDR = $I/dsputil.h $I/fft.h $I/pvoc.h $I/sndf.h
cslib $(CSLIB):	$(LIBSRC) $(LIBHDR)
		cd lib;  $(MAKE) $@

ADSYNS = hetro 						#targets in anal/adsyn
#LPCS = ptrack anallpc lpinfo lpconcat lpdump lpinfed	#targets in anal/lpc
LPCS = lpcanal						#targets in anal/lpc
PVOCS = pvanal 						#targets in anal/pvoc
ANALS = adsyns $(ADSYNS) lpcs $(LPCS) pvocs $(PVOCS)	#targets in anal
anals $(ANALS):
		cd anal; $(MAKE) $@

SCOTS = scots						#targets in util1/scot
CSCORES = cscores					#targets in util1/cscore
SORTEXS = scsort extract				#targets in util1/sortex
UTILS1 = $(SCOTS) $(CSCORES) sortexs $(SORTEXS)		#targets in util1
utils1 $(UTILS1):
		cd util1; $(MAKE) $@

UTILS2 = sndinfo scale					#targets in util2
utils2 $(UTILS2):
		cd util2; $(MAKE) $@

SNDINS = sndin sndout   				#targets in util3/
UNDECS = undec vaxify   				#targets in util3/undecv
UTILS3 = sndins $(SNDINS) rescale undecs $(UNDECS)      #targets in util3
utils3 $(UTILS3):
		cd util3; $(MAKE) $@

install:	$(PROGS)
		-strip $(PROGS)
		-rm $(DESTDIR)/csound
		cp $(PROGS) $(DESTDIR)
		cd anal; $(MAKE) install
		cd util1; $(MAKE) install
		cd util2; $(MAKE) install
	#	cd util3; $(MAKE) install		# currently inoperative

MANPP = csound hetro lpcanal pvanal
manpp:
		-for i in $(MANPP); do rm -f $(CATL)/$$i.l; done
		-for i in $(MANPP); do cp man/$$i.man $(MANL)/$$i.l; done

printmanual:
		cd manual; make print

clean:
		-rm $(OBJS) $(SOBJS) $(PROGS)
		cd lib; $(MAKE) clean
		cd anal; $(MAKE) clean
		cd util1; $(MAKE) clean
		cd util2; $(MAKE) clean
		cd util3; $(MAKE) clean
		cd manual; make clean
		cd testfiles; rm -f core score.srt score.xtr
		cd scorefiles; rm -f core score.srt score.xtr
		cd morefiles; rm -f core score.srt score.xtr

rmbak:
		-rm *.bak *~
		cd include; rm -f *.bak *~
		cd lib; $(MAKE) rmbak
		cd anal; $(MAKE) rmbak
		cd util1; $(MAKE) rmbak
		cd util2; $(MAKE) rmbak
		cd util3; $(MAKE) rmbak
		cd man; rm -f *.bak *~
		cd manual; rm -f *.bak *~
		cd testfiles; rm -f *.bak *~
		cd scorefiles; rm -f *.bak *~ 
		cd morefiles; rm -f *.bak *~ 

SOURCE = /nfs/users/bv
EXPORT = /nfs/users/bv/export
PUBLIC = /fa/ftp/pub
export:
		cd $(EXPORT); rm -rf csound;
		cd $(SOURCE); tar cf - csound | (cd $(EXPORT); tar xf -);
		cd $(EXPORT)/csound/manual; make pstar; \
			mv manual.ps.tar $(PUBLIC)/csound.manual.ps.tar;
		cd $(EXPORT)/csound; make rmbak; make clean;
		cd $(EXPORT); tar cf $(PUBLIC)/csound.tar csound; rm -rf csound;
		cd $(PUBLIC); cp csound.tar tmp; compress tmp; \
			mv tmp.Z csound.tar.Z;
		cd $(PUBLIC); cp csound.manual.ps.tar tmp; compress tmp; \
			mv tmp.Z csound.manual.ps.tar.Z;




