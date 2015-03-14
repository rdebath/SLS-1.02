#
# Makefile for iostream
#

include ../Makeconfig

LIB:=../$(REALLIBC)
LIBALIAS:=../$(REALLIBALIAS)

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=
NOSTDINC=-nostdinc++

DIRS=stdio

.SUFFIXES:
.SUFFIXES: .S .C .o .c .s .i

.s.o :
	$(CC) $(CFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.S.o :
	$(CC) $(CFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.c.o :
	$(CC) $(CFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.c.s :
	$(CC) $(CFLAGS) -S $<

.c.i :
	$(CC) $(CFLAGS) -E $< -o $*.i

.C.o:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../iostream $(WRAP_C_INCLUDES) -c $(XTRAFLAGS) $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.C.s:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../iostream $(WRAP_C_INCLUDES) -S $(XTRAFLAGS) $<


.C.i:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../iostream $(WRAP_C_INCLUDES) -E $(XTRAFLAGS) $< -o $*.i

ifdef SHARED

OBJS= PlotFile.o SFile.o __flow.o editbuf.o filebuf.o \
	floatconv.o fstream.o igetline.o igetsb.o indstream.o \
	iomanip.o iostream.o makebuf.o outfloat.o parsestream.o \
	procbuf.o sbufvform.o sbufvscan.o sgetline.o stdiostream.o \
	stdstreams.o stream.o streambuf.o strstream.o

else

OBJS= PlotFile.o SFile.o __flow.o editbuf.o filebuf.o \
	floatconv.o fstream.o igetline.o igetsb.o indstream.o \
	iomanip.o iostream.o makebuf.o outfloat.o parsestream.o \
	procbuf.o sbufvform.o sbufvscan.o sgetline.o stdiostream.o \
	stdstreams.o stream.o streambuf.o strstream.o _sbufs.o

endif

ALIASES= _sbufs.o

# The following include files are merely to provide partial
# compatibilty with the old stream library from libg++.

COMPAT_INCLUDES = istream.h ostream.h stream.h

# The following include files are publicly visible, and
# should be installed where user programs can find them.

USER_INCLUDES = editbuf.h fstream.h iomanip.h iostream.h \
	indstream.h parsestream.h streambuf.h strstream.h \
	PlotFile.h SFile.h procbuf.h stdiostream.h \
	$(COMPAT_INCLUDES)

library: lib
	for i in $(DIRS); do \
	  echo making $$i; \
	  (cd $$i; $(MAKE) library); \
	done


lib: $(OBJS) $(OBJ1S) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi


asm: $(ASMS)
	for i in $(DIRS); \
	do \
		echo making asm in $$i; \
		(cd $$i; $(MAKE) asm); \
	done

install:
	[ -d $(gxx_includedir) ] || mkdir $(gxx_includedir)
	for FILE in $(USER_INCLUDES) ; do \
		rm -f $(gxx_includedir)/$$FILE ; \
		$(INSTALL_DATA) $$FILE $(gxx_includedir)/$$FILE ; \
	done

clean:
	$(RM) -f core *.o *.a tmp_make foo
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done
	for i in $(DIRS); \
	do \
		echo cleaning $$i; \
		(cd $$i; $(MAKE) clean); \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
