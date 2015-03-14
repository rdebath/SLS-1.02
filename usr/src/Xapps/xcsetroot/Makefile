###########################################################################
# X Window System Makefile generated from template file Imake.tmpl
# $XConsortium: Imake.tmpl,v 1.91 88/10/23 22:37:10 jim Exp $
#
# Do not change the body of the imake template file.  Server-specific
# parameters may be set in the appropriate .macros file; site-specific
# parameters (but shared by all servers) may be set in site.def.  If you
# make any changes, you'll need to rebuild the makefiles using
# "make World" (at best) or "make Makefile; make Makefiles" (at least) in
# the top level directory.
#
# If your C preprocessor doesn't define any unique symbols, you'll need
# to set BOOTSTRAPCFLAGS when rebuilding imake (usually when doing
# "make Makefile", "make Makefiles", or "make World").
#
# If you absolutely can't get imake to work, you'll need to set the
# variables at the top of each Makefile as well as the dependencies at the
# bottom (makedepend will do this automatically).
#

###########################################################################
# platform-specific configuration parameters - edit Vax.macros to change

# platform:  $XConsortium: Vax.macros,v 1.49 88/10/23 11:01:02 jim Exp $

BOOTSTRAPCFLAGS =
             AS = as
             CC = cc
            CPP = /lib/cpp
             LD = ld
           LINT = lint
        INSTALL = install
           TAGS = ctags
             RM = rm -f
             MV = mv
             LN = ln -s
         RANLIB = ranlib
RANLIBINSTFLAGS = -t
             AR = ar clq
             LS = ls
       LINTOPTS = -axz
    LINTLIBFLAG = -C
           MAKE = make
STD_CPP_DEFINES =
    STD_DEFINES =

###########################################################################
# site-specific configuration parameters - edit site.def to change

# site:  $XConsortium: site.def,v 1.16 88/10/12 10:30:24 jim Exp $

###########################################################################
# definitions common to all Makefiles - do not edit

          SHELL = 	/bin/sh

        DESTDIR =
      USRLIBDIR = $(DESTDIR)/usr/lib
         BINDIR = $(DESTDIR)/usr/bin/X11
         INCDIR = $(DESTDIR)/usr/include/X11
         ADMDIR = $(DESTDIR)/usr/adm
         LIBDIR = $(USRLIBDIR)/X11
     LINTLIBDIR = $(USRLIBDIR)/lint
        FONTDIR = $(LIBDIR)/fonts
       XINITDIR = $(LIBDIR)/xinit
         XDMDIR = $(LIBDIR)/xdm
         UWMDIR = $(LIBDIR)/uwm
         AWMDIR = $(LIBDIR)/awm
         TWMDIR = $(LIBDIR)/twm
        MANPATH = $(DESTDIR)/usr/man
  MANSOURCEPATH = $(MANPATH)/man
         MANDIR = $(MANSOURCEPATH)n
      LIBMANDIR = $(MANSOURCEPATH)3
    XAPPLOADDIR = $(LIBDIR)/app-defaults

   INSTBINFLAGS = -s -m 0755
   INSTUIDFLAGS = -s -m 4755
   INSTLIBFLAGS = -m 0664
   INSTINCFLAGS = -m 0444
   INSTMANFLAGS = -m 0444
   INSTAPPFLAGS = -m 0444
  INSTKMEMFLAGS = -s -g kmem -m 2755
        FCFLAGS = -t
    CDEBUGFLAGS = -O

        PATHSEP = /
         DEPEND = $(DEPENDSRC)/makedepend
          IMAKE = $(IMAKESRC)/imake
            RGB = $(RGBSRC)/rgb
             FC = $(BDFTOSNFSRC)/bdftosnf
      MKFONTDIR = $(MKFONTDIRSRC)/mkfontdir
      MKDIRHIER = $(SCRIPTSSRC)/mkdirhier.sh

         CFLAGS = $(CDEBUGFLAGS) $(INCLUDES) $(STD_DEFINES) $(DEFINES)
      LINTFLAGS = $(LINTOPTS) $(INCLUDES) $(STD_DEFINES) $(DEFINES) -DLINT
        LDFLAGS = $(CDEBUGFLAGS) $(SYS_LIBRARIES) $(SYSAUX_LIBRARIES)
            TOP = /mit/x11
      CLIENTSRC = $(TOP)/clients
        DEMOSRC = $(TOP)/demos
         LIBSRC = /usr/lib
        FONTSRC = $(TOP)/fonts
     INCLUDESRC = $(TOP)/X11
      SERVERSRC = $(TOP)/server
        UTILSRC = /usr/athena
     SCRIPTSSRC = $(UTILSRC)/scripts
     EXAMPLESRC = $(TOP)/examples
     CONTRIBSRC = $(TOP)/contrib
         DOCSRC = $(TOP)/doc
         RGBSRC = $(TOP)/rgb
      DEPENDSRC = $(UTILSRC)/makedepend
       IMAKESRC = $(UTILSRC)/imake
       IRULESRC = $(UTILSRC)/imake.includes
        XLIBSRC = $(LIBSRC)
         XMUSRC = $(LIBSRC)/Xmu
     TOOLKITSRC = $(LIBSRC)
     AWIDGETSRC = $(LIBSRC)/Xaw
     OLDXLIBSRC = $(LIBSRC)/oldX
    BDFTOSNFSRC = $(FONTSRC)/bdftosnf
   MKFONTDIRSRC = $(FONTSRC)/mkfontdir
   EXTENSIONSRC = $(TOP)/extensions
   EXTENSIONLIB = $(EXTENSIONSRC)/lib/libXext.a
           XLIB = $(XLIBSRC)/libX11.a
         XMULIB = $(XMUSRC)/libXmu.a
        OLDXLIB = $(OLDXLIBSRC)/liboldX.a
       XTOOLLIB = $(TOOLKITSRC)/libXt.a
         XAWLIB = $(AWIDGETSRC)/libXaw.a
       LINTXLIB = $(XLIBSRC)/llib-lX11.ln
        LINTXMU = $(XMUSRC)/llib-lXmu.ln
      LINTXTOOL = $(TOOLKITSRC)/llib-lXt.ln
        LINTXAW = $(AWIDGETSRC)/llib-lXaw.ln
       INCLUDES = 
      MACROFILE = Vax.macros
   ICONFIGFILES = $(IRULESRC)/Imake.tmpl \
			$(IRULESRC)/$(MACROFILE) $(IRULESRC)/site.def
  IMAKE_DEFINES =
      IMAKE_CMD = $(NEWTOP)$(IMAKE) -TImake.tmpl -I$(NEWTOP)$(IRULESRC) \
			-s Makefile $(IMAKE_DEFINES)
         RM_CMD = $(RM) *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ *.a \
			.emacs_* tags TAGS make.log MakeOut

###########################################################################
# rules:  $XConsortium: Imake.rules,v 1.71 88/10/23 22:46:34 jim Exp $

###########################################################################
# start of Imakefile

LOCAL_LIBRARIES = $(XTOOLLIB) $(XLIB)
           SRCS = ip.c readgif.c readxwd.c xcsetroot.c
           OBJS = ip.o readgif.o readxwd.o xcsetroot.o

        PROGRAM = xcsetroot

all:: xcsetroot

xcsetroot: $(OBJS) $(LOCAL_LIBRARIES)
	$(RM) $@
	$(CC) -o $@ $(OBJS) $(LOCAL_LIBRARIES) $(LDFLAGS) $(SYSLAST_LIBRARIES)

relink::
	$(RM) $(PROGRAM)
	$(MAKE) $(MFLAGS) $(PROGRAM)

install:: xcsetroot
	$(INSTALL) -c $(INSTALLFLAGS) xcsetroot $(BINDIR)

install.man:: xcsetroot.man
	$(INSTALL) -c $(INSTMANFLAGS) xcsetroot.man $(MANDIR)/xcsetroot.n

depend:: $(DEPEND)

depend::
	$(DEPEND) -s "# DO NOT DELETE" -- $(CFLAGS) -- $(SRCS)

$(DEPEND):
	@echo "making $@"; \
	cd $(DEPENDSRC); $(MAKE)

clean::
	$(RM) $(PROGRAM)

###########################################################################
# Imake.tmpl common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) \#*

Makefile:: $(IMAKE)

Makefile:: Imakefile \
	$(IRULESRC)/Imake.tmpl \
	$(IRULESRC)/Imake.rules \
	$(IRULESRC)/site.def \
	$(IRULESRC)/$(MACROFILE)
	-@if [ -f Makefile ]; then \
		echo "$(RM) Makefile.bak; $(MV) Makefile Makefile.bak"; \
		$(RM) Makefile.bak; $(MV) Makefile Makefile.bak; \
	else exit 0; fi
	$(IMAKE_CMD) -DTOPDIR=$(TOP)

$(IMAKE):
	@echo "making $@"; \
	cd $(IMAKESRC); $(MAKE) BOOTSTRAPCFLAGS=$(BOOTSTRAPCFLAGS)

tags::
	$(TAGS) -w *.[ch]
	$(TAGS) -xw *.[ch] > TAGS

###########################################################################
# empty rules for directories that do not have SUBDIRS - do not edit

install::
	@echo "install done"

install.man::
	@echo "install.man done"

Makefiles::

###########################################################################
# dependencies generated by makedepend

# DO NOT DELETE THIS LINE -- make depend depends on it.

ip.o: ip.c common.h /usr/include/X11/Xlib.h /usr/include/sys/types.h
ip.o: /usr/include/X11/X.h /usr/include/X11/Xutil.h /usr/include/stdio.h
ip.o: extern.h
readgif.o: readgif.c common.h /usr/include/X11/Xlib.h
readgif.o: /usr/include/sys/types.h /usr/include/X11/X.h
readgif.o: /usr/include/X11/Xutil.h /usr/include/stdio.h extern.h gif.h
readxwd.o: readxwd.c common.h /usr/include/X11/Xlib.h
readxwd.o: /usr/include/sys/types.h /usr/include/X11/X.h
readxwd.o: /usr/include/X11/Xutil.h /usr/include/stdio.h extern.h
readxwd.o: /usr/include/X11/XWDFile.h /usr/include/X11/copyright.h
xcsetroot.o: xcsetroot.c /usr/include/X11/Xatom.h
xcsetroot.o: /usr/include/X11/bitmaps/gray common.h /usr/include/X11/Xlib.h
xcsetroot.o: /usr/include/sys/types.h /usr/include/X11/X.h
xcsetroot.o: /usr/include/X11/Xutil.h /usr/include/stdio.h
