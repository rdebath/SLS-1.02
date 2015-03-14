#
# Makefile for termcap functions
#
#

include ../../Makeconfig

include ../../Makerules

LIB	=../../libtermcap.a

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libtermcap

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

OBJS = term_data.o
ASMS = term_data.s

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)


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
