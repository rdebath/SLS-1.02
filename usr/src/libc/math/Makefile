#
# Makefile for som generic math functions.
#

include ../Makeconfig

include ../Makerules

LIB:=../libm.a
LIBALIAS:=../libmalias.a

JUMP_DIR:=../jump/libm
JUMP_LIB=libm

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=-I. 

ifdef SHARED

OBJS = __finite.o

ALIASES = infnan.o isinf.o isnan.o rint.o copysign.o \
	finite.o drem.o

else

OBJS = __finite.o infnan.o isinf.o isnan.o rint.o \
	 copysign.o finite.o drem.o

ALIASES=

endif

ASMS =

library: $(OBJS) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

#	$(RANLIB) $(LIB)
#	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>1&

obj: $(OBJS)

asm: $(ASMS)

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
