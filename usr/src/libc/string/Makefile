#
# Makefile for string functions
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
INC=-I. -I..

ifdef SHARED

OBJS = __memccpy.o bcmp.o bcopy.o bzero.o ffs.o \
	memchr.o memcmp.o memcpy.o memfrob.o memmem.o memmove.o \
	memset.o strcasecmp.o strcat.o strchr.o strcmp.o \
	strcoll.o strcpy.o strcspn.o strdup.o strerror.o strfry.o \
	strlen.o strncscmp.o strncat.o strncmp.o strncpy.o \
	strpbrk.o strrchr.o strsep.o strsignal.o strspn.o \
	strstr.o strtok.o strxfrm.o swab.o

else

OBJS = __memccpy.o bcmp.o bcopy.o bzero.o ffs.o index.o memccpy.o \
	memchr.o memcmp.o memcpy.o memfrob.o memmem.o memmove.o \
	memset.o rindex.o strcasecmp.o strcat.o strchr.o strcmp.o \
	strcoll.o strcpy.o strcspn.o strdup.o strerror.o strfry.o \
	strlen.o strncscmp.o strncat.o strncmp.o strncpy.o \
	strpbrk.o strrchr.o strsep.o strsignal.o strspn.o \
	strstr.o strtok.o strxfrm.o swab.o errlist.o siglist.o

endif

ALIASES = index.o memccpy.o rindex.o errlist.o siglist.o

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
