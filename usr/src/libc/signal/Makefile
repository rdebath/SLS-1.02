#
# Makefile for signal functions
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

ifdef SHARED

OBJS =  sigaddset.o sigdelset.o sigempty.o sigfillset.o \
	sigismem.o

else

OBJS = gsignal.o kill.o sigaction.o sigaddset.o sigblock.o \
	sigdelset.o sigempty.o sigfillset.o siggetmask.o \
	sigismem.o sigpause.o sigproc.o sigsetmask.o ssignal.o

endif

ALIASES = gsignal.o kill.o sigaction.o sigblock.o siggetmask.o \
	sigpause.o sigproc.o sigsetmask.o ssignal.o

library: $(OBJS) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

#	$(RANLIB) $(LIB)
#	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>1&

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
