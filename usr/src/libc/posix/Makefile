#
# Makefile for POSIX functions
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

OBJS = cfmakeraw.o cfsetget.o clock.o confstr.o ctermid.o cuserid.o \
	execl.o execle.o execlp.o execv.o execvp.o \
	fnmatch.o getcwd.o getenv.o \
	getopt.o getopt1.o glob.o killpg.o mktemp.o \
	putenv.o raise.o readv.o sleep.o system.o tcdrain.o \
	tcflow.o tcflush.o tcgetpgrp.o tcsendbrk.o \
	tcsetattr.o tcsetpgrp.o ttyname.o writev.o

else

OBJS = cfmakeraw.o cfsetget.o clock.o confstr.o ctermid.o cuserid.o \
	environ.o execl.o execle.o execlp.o execv.o execve.o execvp.o \
	fnmatch.o fork.o fpathconf.o getcwd.o getegid.o getenv.o \
	geteuid.o getgid.o getgrps.o getopt.o getopt1.o getpid.o \
	getppid.o getuid.o glob.o killpg.o mktemp.o pathconf.o \
	putenv.o raise.o readv.o setgid.o \
	setsid.o setuid.o sleep.o sysconf.o system.o tcdrain.o \
	tcflow.o tcflush.o tcgetattr.o tcgetpgrp.o tcsendbrk.o \
	tcsetattr.o tcsetpgrp.o times.o ttyname.o wait.o wait3.o \
	wait4.o waitpid.o writev.o

endif

ALIASES = environ.o execve.o fork.o fpathconf.o getegid.o geteuid.o \
	getgid.o getgrps.o getpid.o getppid.o getuid.o pathconf.o \
	setgid.o setsid.o setuid.o sysconf.o \
	tcgetattr.o times.o wait.o wait3.o wait4.o waitpid.o

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
