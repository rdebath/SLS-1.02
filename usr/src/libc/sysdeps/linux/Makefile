#
# Makefile for Linux specific functions
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

INC= -I./i386 -I../..

ifdef MATH

DIRS=i386

OBJS=
OBJ1S=
OBJ2S=
OBJ4S=
OBJ5S=
OBJ6S=
OBJ7S=
OBJ8S=
OBJ9S=
OBJ10S=
OBJ11S=

else	# the math library

DIRS=i386

ifeq "$(HOST_MACHINE)" "i386-att-sysv"

# stupid SVR3

OBJS=
OBJ1S = __access.o __chdir.o __chmod.o __chown.o __close.o __dup.o \
	__dup2.o __execve.o __fchmod.o __fchown.o
OBJ2S=	__flock.o __fork.o __fpathconf.o __fstat.o __getdtsz.o \
	__getegid.o __geteuid.o __getgid.o __getgrps.o __gethstnm.o
OBJ3S=	__getitmr.o __getpgsz.o __getpid.o __getppid.o __getrusag.o \
	__gettod.o __getuid.o __isatty.o __kill.o __link.o
OBJ4S=	__lseek.o __lstat.o __mkdir.o __mknod.o \
	__pathconf.o __pipe.o __read.o __readlink.o __rmdir.o
OBJ5S=	__setgid.o __setitmr.o __setregid.o __setreuid.o __setsid.o \
	__settod.o __setuid.o __sigblock.o __sigpause.o
OBJ6S=	__sigproc.o __stat.o __symlink.o __sysconf.o \
	__tcgetatr.o __times.o __umask.o __unlink.o __utimes.o
OBJ7S=	__wait4.o __write.o acct.o alarm.o chroot.o creat.o \
	fstatfs.o ftruncate.o getpgrp.o getrlimit.o idle.o
OBJ8S=	ioperm.o iopl.o mkfifo.o morecore.o mount.o \
	munmap.o nice.o nlist.o pause.o reboot.o rename.o setegid.o
OBJ9S=	seteuid.o setgroups.o sethstnm.o setpgid.o setprio.o \
	setrlimit.o signal.o sigpending.o statfs.o stime.o swapoff.o
OBJ10S=	swapon.o sync.o time.o truncate.o umount.o uname.o \
	uselib.o ustat.o utime.o vhangup.o vm86.o
OBJ11S=	__fcntl.o __ioctl.o __open.o fsync.o # syscall.o

else	# AT&T SVR3

OBJS = __access.o __chdir.o __chmod.o __chown.o __close.o __dup.o \
	__dup2.o __execve.o __fchmod.o __fchown.o \
	__flock.o __fork.o __fpathconf.o __fstat.o __getdtsz.o \
	__getegid.o __geteuid.o __getgid.o __getgrps.o __gethstnm.o \
	__getitmr.o __getpgsz.o __getpid.o __getppid.o __getrusag.o \
	__gettod.o __getuid.o __isatty.o __kill.o __link.o \
	__lseek.o __lstat.o __mkdir.o __mknod.o \
	__pathconf.o __pipe.o __read.o __readlink.o __rmdir.o \
	__setgid.o __setitmr.o __setregid.o __setreuid.o __setsid.o \
	__settod.o __setuid.o __sigblock.o __sigpause.o \
	__sigproc.o __stat.o __symlink.o __sysconf.o \
	__tcgetatr.o __times.o __umask.o __unlink.o __utimes.o \
	__wait4.o __write.o acct.o alarm.o chroot.o creat.o \
	fstatfs.o ftruncate.o getpgrp.o getrlimit.o idle.o \
	ioperm.o iopl.o mkfifo.o morecore.o mount.o \
	munmap.o nice.o nlist.o pause.o reboot.o rename.o setegid.o \
	seteuid.o setgroups.o sethstnm.o setpgid.o setprio.o \
	setrlimit.o signal.o sigpending.o statfs.o stime.o swapoff.o \
	swapon.o sync.o time.o truncate.o umount.o uname.o \
	uselib.o ustat.o utime.o vhangup.o vm86.o \
	__fcntl.o __ioctl.o __open.o fsync.o # syscall.o

endif	# AT&T SVR3

endif	# the math library

library: lib
	for i in $(DIRS); do \
	  echo making $$i; \
	  (cd $$i; $(MAKE) library); \
	done

ifeq "$(HOST_MACHINE)" "i386-att-sysv"

lib: $(OBJ1S) $(OBJ2S) $(OBJ3S) $(OBJ4S) $(OBJ5S) $(OBJ6S) $(OBJ7S) \
	$(OBJ8S) $(OBJ9S) $(OBJ10S) $(OBJ11S)
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
	if [ x"$(OBJ5S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ5S); \
	else true; fi
	if [ x"$(OBJ6S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ6S); \
	else true; fi
	if [ x"$(OBJ7S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ7S); \
	else true; fi
	if [ x"$(OBJ8S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ8S); \
	else true; fi
	if [ x"$(OBJ9S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ9S); \
	else true; fi
	if [ x"$(OBJ10S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ10S); \
	else true; fi
	if [ x"$(OBJ11S)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJ11S); \
	else true; fi

else

lib: $(OBJS) $(OBJ1S)
	if [ x"$(OBJS)"x != xx ]; then \
	  $(AR) uvc $(LIB) $(OBJS); \
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
