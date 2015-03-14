#
# This is Makefile of the math lib for Linux
#

include ../../../../Makeconfig

include ../../../../Makerules

LIB=../../../../libm.a

JUMP_DIR:=../../../../jump/libm
JUMP_LIB=libm

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

MATHFLAGS=-ffast-math -mieee-fp

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS) $(MATHFLAGS)
INC = -I.

OBJS = acos.o acosh.o asin.o asinh.o atan.o atan2.o atanh.o \
	ceil.o cos.o cosh.o __drem.o erf.o exp.o fabs.o floor.o \
	fmod.o frexp.o hypot.o j0.o j1.o jn.o lgamma.o log.o \
	log10.o pow.o sin.o sinh.o sqrt.o tan.o tanh.o \
	__copysign.o __infnan.o __isinf.o __isnan.o __rint.o

ASMS = acos.s acosh.s asin.s asinh.s atan.s atan2.s atanh.s \
	ceil.s cos.s cosh.s __drem.s erf.s exp.s fabs.s floor.s \
	fmod.s frexp.s hypot.s j0.s j1.s jn.s lgamma.s log.s \
	log10.s pow.s sin.s sinh.s sqrt.s tan.s tanh.s \
	__copysign.s __infnan.s __isinf.s __isnan.s __rint.s

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)


asm: $(ASMS)

obj: $(OBJS)

test-math: test-math.c $(OBJS)
	$(CC) -o $@ test-math.c $(OBJS)

clean:
	$(RM) -f core *.o *.a tmp_make foo test-math a.out
	-for i in *.c *.S;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

dep:
	$(CPP) -M *.c *.S > .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
