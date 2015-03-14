#
# This is Makefile of io for Linux
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
INC= -I. -I..

ifdef SHARED

OBJS= ftw.o getdirname.o getwd.o

else

OBJS = access.o chdir.o chmod.o chown.o close.o creat.o dup.o \
	dup2.o fchmod.o fchown.o fcntl.o flock.o fstat.o ftw.o \
	getdirname.o getwd.o isatty.o link.o lseek.o lstat.o mkdir.o \
	open.o pipe.o read.o readlink.o rmdir.o stat.o \
	symlink.o umask.o unlink.o write.o

endif

ALIASES = access.o chdir.o chmod.o chown.o close.o dup.o dup2.o \
	fchmod.o fchown.o fcntl.o flock.o fstat.o isatty.o link.o \
	lseek.o lstat.o mkdir.o open.o pipe.o read.o readlink.o \
	rmdir.o stat.o symlink.o umask.o unlink.o write.o

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
