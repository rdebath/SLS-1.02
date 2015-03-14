#
# Makefile for the global data
#

include ../Makeconfig

include ../Makerules

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

DOSUBDIRS=lib

DIRS	=ctype curses gdbm inet iostream locale misc posix \
	regex string sysdeps termcap time math

ifdef DEBUG

DIRS	=ctype inet iostream locale misc posix \
	regex string sysdeps time

endif

ifdef PROFILE

DIRS	=ctype inet iostream locale misc posix \
	regex string sysdeps time

endif

ifdef SHARED

DIRS	=ctype curses gdbm inet iostream locale misc posix \
	regex string sysdeps termcap time

endif

ifdef LITE

DIRS	=ctype inet iostream locale misc posix \
	regex string sysdeps termcap time

DIRS	=ctype inet iostream locale misc posix \
	regex string sysdeps termcap time

endif

ifdef MATH
DIRS=math
endif

LIBC_DATA1S= ctype/ctype-info.o curses/curses.o curses/unctrl.o \
	gdbm/gdbm_errno.o inet/h_errno.o inet/_res.o \
	inet/rexecoptions.o iostream/stdstrbufs.o \
	iostream/stdio/stdio_data.o \
	misc/__environ.o misc/errno.o posix/opt_data.o \
	regex/re_data.o sysdeps/__brk_addr.o \
	termcap/term_data.o time/time_data.o

#LIBC_DATA1S= ctype/ctype-info.o curses/curses.o curses/unctrl.o \
#	gdbm/gdbm_errno.o inet/inet_d.o iostream/stdstrbufs.o \
#	iostream/stdio/stdio_data.o \
#	misc/__environ.o misc/errno.o posix/opt_data.o \
#	regex/re_data.o sysdeps/__brk_addr.o \
#	termcap/term_data.o time/time_data.o

LIBGDBM_DATA1S= gdbm/gdbm_errno.o

LIBINET_DATA1S= inet/h_errno.o inet/_res.o inet/rexecoptions.o

LIBM_DATA1S=math/signgam.o

OBJS = 

LIBS= libc.data1.o libc.text1.o

lib:
	$(MAKE) dosubdirs DOSUBDIRS=library

dep:
	$(MAKE) dosubdirs DOSUBDIRS=dep

asm:
	$(MAKE) dosubdirs DOSUBDIRS=asm

jump: lib data text

data: $(OBJS) libc.data1.o libm.data1.o # libnet.data1.o

text: libc.text1.o # libnet.text1.o

libm.data1.o: $(LIBM_DATA1S)
	$(LD) -x -r -o $@ $?

libnet.data1.o: $(LIBINET_DATA1S)
	$(LD) -x -r -o $@ $?

libgdbm.data1.o: $(LIBGDBM_DATA1S)
	$(LD) -x -r -o $@ $?

libc.data1.o: $(LIBC_DATA1S)
	$(LD) -x -r -o $@ $?

dosubdirs:
	for i in $(DIRS); do \
	  echo making $(DOSUBDIRS) in $$i; \
	  (cd $$i; $(MAKE) $(DOSUBDIRS)); \
	done

clean:
	$(RM) -f $(LIBS)
	for i in $(DIRS); \
	do \
		echo cleaning $$i; \
		(cd $$i; $(MAKE) clean); \
	done
