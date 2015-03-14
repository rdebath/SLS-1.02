#
# This is Makefile of gdbm 1.5 for Linux
#

include ../Makeconfig

include ../Makerules

LIB=../libdbm.a
LIBALIAS:=../$(REALLIBALIAS)

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libdbm

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC= -I. -I..

OBJS = dbminit.o delete.o fetch.o store.o seq.o dbmopen.o \
	dbmdelete.o dbmfetch.o dbmstore.o dbmseq.o dbmclose.o \
	dbmdirfno.o dbmpagfno.o gdbmopen.o gdbmdelete.o gdbmfetch.o \
	gdbmstore.o gdbmclose.o gdbmreorg.o gdbmseq.o \
	bucket.o falloc.o findkey.o hash.o update.o global.o version.o
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
