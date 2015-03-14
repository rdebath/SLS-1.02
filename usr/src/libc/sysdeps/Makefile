#
# Makefile for Linux specific functions
#

include ../Makeconfig

include ../Makerules

LIB:=../$(REALLIBC)

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

ifdef MATH

DIRS=linux

else

DIRS=i386 linux

endif

OBJS=

library:
	for i in $(DIRS); do \
	  echo making $$i; \
	  (cd $$i; $(MAKE) library); \
	done


lib: $(OBJS) $(OBJ1S)
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
		(cd $$i; $(MAKE) clean); \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
