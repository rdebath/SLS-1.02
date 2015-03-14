#
# Makefile for locale functions
#

include ../../Makeconfig

include ../../Makerules

LIB:=../../$(REALLIBC)

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

OBJS = C-collate.o C-ctype.o C-ctype_ct.o C-ctype_mb.o \
	C-monetary.o C-numeric.o C-response.o C-time.o # setlocale.o

ASMS = C-collate.s C-ctype.s C-ctype_ct.s C-ctype_mb.s \
	C-monetary.s C-numeric.s C-response.s C-time.s # setlocale.o

library:
	@echo Nothing to be done.

__library: $(OBJS)
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
