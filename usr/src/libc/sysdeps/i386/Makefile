#
# Makefile for i386 specific functions.
#

include ../../Makeconfig

include ../../Makerules

LIB:=../../$(REALLIBC)
LIBALIAS:=../../$(REALLIBALIAS)

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=-I. 

ifdef SHARED

OBJS = __longjmp.o __setjmp.o hton.o sigsetjmp.o

else

OBJS = __longjmp.o __setjmp.o hton.o ntoh.o sigsetjmp.o div.o

endif

ASMS = __longjmp.s __setjmp.s hton.s ntoh.s sigsetjmp.s div.s
ALIASES = ntoh.o div.o

library: $(OBJS) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

#	$(RANLIB) $(LIB)
#	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>1&

obj: $(OBJS)

asm: $(ASMS)

__longjmp.o : __longjmp.c
	$(CC) $(CFLAGS) -fno-omit-frame-pointer -c $?
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

__setjmp.o : __setjmp.c
	$(CC) $(CFLAGS) -fno-omit-frame-pointer -c $?
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

sigsetjmp.o : sigsetjmp.c
	$(CC) $(CFLAGS) -fno-omit-frame-pointer -c $?
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

__longjmp.s : __longjmp.c
	$(CC) $(CFLAGS) -fno-omit-frame-pointer -S $?

__setjmp.s : __setjmp.c
	$(CC) $(CFLAGS) -fno-omit-frame-pointer -S $?

sigsetjmp.s : sigsetjmp.c
	$(CC) $(CFLAGS) -fno-omit-frame-pointer -S $?

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
