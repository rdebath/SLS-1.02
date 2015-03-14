#
# Makefile for ctype functions
#
#
# Host root directory, maybe /usr for Linux

include ../../Makeconfig

include ../../Makerules

LIB:=../../$(REALLIBC)

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

OBJS  = ctype-extn.o ctype.o # ctype-info.o
OBJS  = ctype-info.o
ASMS  = ctype-info.s


library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)

asm: $(ASMS)

test: test_ctype.o
	$(CC) -o test_ctype test_ctype.o

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
