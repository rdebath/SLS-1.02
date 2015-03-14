#
# Makefile for malloc library functions
#

include ../Makeconfig

include ../Makerules

LIB:=../$(REALLIBC)
LIBALIAS:=../$(REALLIBALIAS)
LIBMCHECK=../libmcheck.a

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)
INC= -I. -I. -I.

ifdef SHARED

OBJS  = malloc.o free.o realloc.o calloc.o \
	valloc.o memalign.o mcheck.o mtrace.o mstats.o

TARGET=library

endif

ifdef DEBUG

OBJS  = malloc.o free.o realloc.o calloc.o \
	valloc.o memalign.o mcheck.o mtrace.o mstats.o cfree.o

TARGET=library libmcheck

endif

ifdef PROFILE

OBJS  = malloc.o free.o realloc.o calloc.o \
	valloc.o memalign.o mcheck.o mtrace.o mstats.o cfree.o

TARGET=library

endif

ifdef LITE

OBJS  = malloc.o free.o realloc.o calloc.o \
	valloc.o memalign.o mcheck.o mtrace.o mstats.o

TARGET=library

endif

ifdef STATIC

OBJS  = malloc.o free.o realloc.o calloc.o \
	valloc.o memalign.o mcheck.o mtrace.o mstats.o cfree.o

TARGET=library

endif

ALIASES = cfree.o

all: $(TARGET)

library: $(OBJS) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

#	$(RANLIB) $(LIB)
#	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>1&

libmcheck: mcheck-init.o
	$(AR) uvc $(LIBMCHECK) mcheck-init.o

obj: $(OBJS)

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
