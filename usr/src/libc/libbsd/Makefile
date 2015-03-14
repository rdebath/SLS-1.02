#
# libbsd	A collection of library modules similar or equal to their
#		BSD UNIX equivalents, to make the job of porting BSD pro-
#		grams to LINUX easier.
#
# Author:	Rick Sladkey, <jrs@world.std.com>
#

include ../Makeconfig

include ../Makerules

LIB:=../libbsd.a

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) # $(XCFLAGS)
INC= -I$(CROSSINCDIR)/bsd -I$(CROSSINCDIR)  \
	-include $(CROSSINCDIR)/bsd/bsd.h

BSDOBJS	= daemon.o logwtmp.o logout.o pty.o login_tty.o 
GNUOBJS	=
OBJS	= $(BSDOBJS) $(GNUOBJS) \
	  sgtty.o cfsetspeed.o revoke.o getusersh.o setpassent.o \
	  snprintf.o roundup.o checkrhosts.o strsep.o

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)
	-$(AR) -d $(LIB) __.SYMDEF > /dev/null 2>&1
	$(REALRANLIB) $(LIB)

obj: $(OBJS)

asm: $(AMS)

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
