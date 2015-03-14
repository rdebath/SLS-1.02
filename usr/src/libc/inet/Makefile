#
# This is Makefile of inet lib for Linux
#

include ../Makeconfig

include ../Makerules

LIB=../libnet.a
LIB:=../$(REALLIBC)
LIBALIAS:=../$(REALLIBALIAS)

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)
INC= -I. -I..

OBJS1 = gethstnmad.o getnetbyad.o getnetbynm.o getnetent.o \
	getproto.o getprtent.o getprtname.o getservent.o \
	getsrvbynm.o getsrvbypt.o herror.o inet_addr.o \
	inet_lnaof.o inet_mkadr.o inet_net.o inet_netof.o \
	inet_ntoa.o rcmd.o res_comp.o res_debug.o res_init.o \
	res_mkqry.o res_query.o res_send.o rexec.o ruserpass.o \
	sethostent.o bindresvport.o

OBJS = $(OBJS1)
ALIASES =

library: $(OBJS)
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
