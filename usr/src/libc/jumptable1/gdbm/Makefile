# This is Makefile of gdbm 1.5 for Linux
#

include ../../Makeconfig

include ../../Makerules

LIB =../../libdbm.a

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libdbm

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=-I../../gdbm

OBJS = gdbm_errno.o
ASMS = gdbm_errno.s

DUMMY = gdbm_d.o

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)


asm: $(ASMS)

dummy: $(DUMMY)

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
