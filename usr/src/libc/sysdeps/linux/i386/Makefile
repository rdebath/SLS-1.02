#
# Makefile of Linux specific functions for i386
#

include ../../../Makeconfig

include ../../../Makerules

LIB:=../../../$(REALLIBC)
LIBALIAS:=../../../$(REALLIBALIAS)

JUMP_DIR:=../../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=-I.

DIRS=math

ifdef SHARED

DIRS=

endif	# shared

ifdef DEBUG

DIRS=

endif	# debug

ifdef PROFILE

DIRS=

endif	# profile

ifdef SHARED

ifeq "$(HOST_MACHINE)" "i386-att-sysv"

OBJ1S = ____sig.o __brk.o  __load.o __sbrk.o __select.o __sigact.o \
	__wait.o __wait3.o __waitpid.o accept.o bind.o
OBJ2S=	connect.o getpeernam.o getprio.o getsocknam.o getsockopt.o \
	listen.o mmap.o ptrace.o readdir.o recv.o revcfrom.o
OBJ3S=	send.o sendto.o setpgrp.o setsockopt.o shutdown.o \
	sigsuspend.o socket.o socketpair.o tell.o
OBJ4S= ulimit.o syscall.o __siggtmsk.o __sigstmsk.o libc_exit.o
OBJ5S=
OBJ6S=
OBJ7S=
OBJ8S=
OBJ9S=
OBJ10S=
OBJ11S=

else

OBJS = ____sig.o __brk.o  __load.o __sbrk.o __select.o __sigact.o \
	__wait.o __wait3.o __waitpid.o accept.o bind.o \
	connect.o getpeernam.o getprio.o getsocknam.o getsockopt.o \
	listen.o mmap.o ptrace.o readdir.o recv.o revcfrom.o \
	send.o sendto.o setpgrp.o setsockopt.o shutdown.o \
	sigsuspend.o socket.o socketpair.o tell.o \
	ulimit.o syscall.o __siggtmsk.o __sigstmsk.o libc_exit.o

endif

else

ifeq "$(HOST_MACHINE)" "i386-att-sysv"

OBJ1S = ____sig.o __brk.o  __load.o __sbrk.o __select.o __sigact.o \
	__vfork.o __wait.o __wait3.o __waitpid.o accept.o bind.o
OBJ2S=	connect.o getpeernam.o getprio.o getsocknam.o getsockopt.o \
	listen.o mmap.o ptrace.o readdir.o recv.o revcfrom.o
OBJ3S=	send.o sendto.o setpgrp.o setsockopt.o shutdown.o \
	sigsuspend.o socket.o socketpair.o tell.o
OBJ4S=	ulimit.o vfork.o syscall.o __siggtmsk.o __sigstmsk.o libc_exit.o
OBJ5S=
OBJ6S=
OBJ7S=
OBJ8S=
OBJ9S=
OBJ10S=
OBJ11S=

else

OBJS = ____sig.o __brk.o  __load.o __sbrk.o __select.o __sigact.o \
	__vfork.o __wait.o __wait3.o __waitpid.o accept.o bind.o \
	connect.o getpeernam.o getprio.o getsocknam.o getsockopt.o \
	listen.o mmap.o ptrace.o readdir.o recv.o revcfrom.o \
	send.o sendto.o setpgrp.o setsockopt.o shutdown.o \
	sigsuspend.o socket.o socketpair.o tell.o \
	ulimit.o vfork.o syscall.o __siggtmsk.o __sigstmsk.o \
	libc_exit.o

endif

endif

# __fcntl.o __ioctl.o __open.o syscall.o

ALIASES= __vfork.o vfork.o

ifdef MATH

DIRS=math

OBJS=

OBJ1S=
OBJ2S=
OBJ3S=
OBJ4S=
OBJ5S=
OBJ6S=
OBJ7S=

ALIASES=

endif


library: lib
	if [ x"$(DIRS)"x != xx ]; then \
	  for i in .. $(DIRS); do \
	    if [ x$${i}x != x..x ]; then \
	      echo making $$i; \
	      (cd $$i; $(MAKE) library); \
	    else true; fi; \
	  done; \
	else true; fi

ifeq "$(HOST_MACHINE)" "i386-att-sysv"

lib: $(OBJ1S) $(OBJ2S) $(OBJ3S) $(OBJ4S)  $(ALIASES)
	if [ x"$(OBJ1S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ1S); \
	else true; fi
	if [ x"$(OBJ2S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ2S); \
	else true; fi
	if [ x"$(OBJ3S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ3S); \
	else true; fi
	if [ x"$(OBJ4S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ4S); \
	else true; fi
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../../../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi
	
else

lib: $(OBJS) $(ALIASES)
	echo $(LIB) $(REALLIBC)
	if [ x"$(OBJS)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJS); \
	else true; fi
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../../../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi

endif

asm: $(ASMS)
	for i in $(DIRS); \
	do \
		echo making asm in $$i; \
		(cd $$i; $(MAKE) asm); \
	done

clean:
	$(RM) -f core *.o *.a tmp_make foo
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done
	for i in $(DIRS); \
	do \
		echo cleaning $$i; \
		(cd $$i; $(MAKE) clean); \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
