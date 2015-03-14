# This is Makefile of inet lib for Linux
#

include ../../Makeconfig

include ../../Makerules

LIB:=../../$(REALLIBC)
#LIB=../../libnet.a

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=-I../../inet

OBJS = _res.o herrlist.o rexecoptions.o h_errno.o
ASMS = _res.s herrlist.s rexecoptions.S h_errno.s
DUMMY= gdbm_d.o

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)


dummy: $(DUMMY)

asm: $(ASMS)

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
