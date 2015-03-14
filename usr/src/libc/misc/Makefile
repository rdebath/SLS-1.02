#
# This is Makefile for some misc functions.
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

OBJS = drand48.o getusersh.o insremque.o ldexp.o modf.o \
	syslog.o usleep.o 

else

OBJS = brk.o drand48.o getdtsz.o gethstnm.o getpgsz.o \
	getrusage.o getusersh.o insremque.o ioctl.o ldexp.o mknod.o \
	modf.o sbrk.o select.o setregid.o setreuid.o syslog.o \
	usleep.o utimes.o

endif

ALIASES = brk.o getdtsz.o gethstnm.o getpgsz.o getrusage.o ioctl.o \
	mknod.o sbrk.o select.o setregid.o setreuid.o \
	utimes.o


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
