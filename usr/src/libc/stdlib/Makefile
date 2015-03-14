#
# This is Makefile of stdlib functions for Linux.
#

include ../Makeconfig

include ../Makerules

LIB:=../$(REALLIBC)
LIBALIAS:=../$(REALLIBALIAS)

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)
INC=-I.

ifdef SHARED

OBJS = __random.o _quicksort.o abort.o atexit.o atof.o \
	atoi.o atol.o bsearch.o exit.o labs.o ldiv.o \
	mblen.o mbstowcs.o mbtowc.o on_exit.o qsort.o rand.o \
	strtol.o strtoul.o wcstombs.o wctomb.o

else

OBJS = __random.o _quicksort.o abort.o abs.o atexit.o atof.o \
	atoi.o atol.o bsearch.o exit.o labs.o ldiv.o \
	mblen.o mbstowcs.o mbtowc.o on_exit.o qsort.o rand.o \
	random.o srand.o strtol.o strtoul.o wcstombs.o wctomb.o

endif

ASMS = 
ALIASES = abs.o random.o srand.o


library: $(OBJS) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

#	$(RANLIB) $(LIB)
#	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>1&

obj: $(OBJS)

asm: $(AMS)

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
