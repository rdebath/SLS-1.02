#
# Makefile for iostream
#

include ../../Makeconfig

LIB:=../../$(REALLIBC)

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

NOSTDINC=-nostdinc++

DIRS=stdio

.SUFFIXES:
.SUFFIXES: .S .C .o .c .s .i

.s.o :
	$(CC) $(CFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.S.o :
	$(CC) $(CFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.c.o :
	$(CC) $(CFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.c.s :
	$(CC) $(CFLAGS) -S $<

.c.i :
	$(CC) $(CFLAGS) -E $< -o $*.i

.C.o:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../../iostream $(WRAP_C_INCLUDES) -c $(XTRAFLAGS) $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.C.s:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../../iostream $(WRAP_C_INCLUDES) -S $(XTRAFLAGS) $<


.C.i:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../../iostream $(WRAP_C_INCLUDES) -E $(XTRAFLAGS) $< -o $*.i

OBJS = stdstrbufs.o
ASMS = stdstrbufs.s

library: lib
	for i in $(DIRS); \
	do \
		echo making $$i; \
		(cd $$i; $(MAKE) library); \
	done

lib: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)


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
		(cd $$i; $(MAKE) clean $(MFLAGS)); \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
