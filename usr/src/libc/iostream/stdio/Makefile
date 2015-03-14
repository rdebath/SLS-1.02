#
# Makefile for stdio
#

include ../../Makeconfig

LIB:=../../$(REALLIBC)
LIBALIAS:=../../$(REALLIBALIAS)

JUMP_DIR:=../../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INC=-I.

NOSTDINC=-nostdinc++

.SUFFIXES:
.SUFFIXES: .S .o .c .s .i .C

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
	$(CC) $(CFLAGS) $(NOSTDINC) -I../../iostream -I../../iostream/stdio -c $(XTRAFLAGS) $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.C.s:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../../iostream -I../../iostream/stdio -S $(XTRAFLAGS) $<

.C.i:
	$(CC) $(CFLAGS) $(NOSTDINC) -I../../iostream -I../../iostream/stdio -E $(XTRAFLAGS) $< -o $*.i


ifdef SHARED

OBJS= clearerr.o emulate.o fclose.o fdopen.o feof.o ferror.o \
	fflush.o fgetc.o fgetpos.o fgets.o fileno.o fopen.o \
	fprintf.o fputc.o fputs.o fread.o freopen.o fscanf.o \
	fseek.o fsetpos.o ftell.o fwrite.o gets.o getw.o \
	macros.o perror.o popen.o printf.o puts.o putw.o \
	rewind.o scanf.o setbuf.o setbuffer.o \
	setlinebuf.o setvbuf.o sprintf.o sscanf.o stdio.o \
	sysd-stdio.o tempnam.o tmpfile.o tmpnam.o ungetc.o \
	vfprintf.o vfscanf.o vprintf.o vscanf.o vsprintf.o \
	vsscanf.o setfileno.o
else

OBJS= clearerr.o emulate.o fclose.o fdopen.o feof.o ferror.o \
	fflush.o fgetc.o fgetpos.o fgets.o fileno.o fopen.o \
	fprintf.o fputc.o fputs.o fread.o freopen.o fscanf.o \
	fseek.o fsetpos.o ftell.o fwrite.o gets.o getw.o \
	macros.o perror.o popen.o printf.o puts.o putw.o \
	remove.o rewind.o scanf.o setbuf.o setbuffer.o \
	setlinebuf.o setvbuf.o sprintf.o sscanf.o stdio.o \
	sysd-stdio.o tempnam.o tmpfile.o tmpnam.o ungetc.o \
	vfprintf.o vfscanf.o vprintf.o vscanf.o vsprintf.o \
	vsscanf.o setfileno.o

endif

ALIASES= remove.o

library: $(OBJS) $(ALIASES)
	$(AR) uvc $(LIB) $(OBJS)
	if [ x"$(ALIASES)"x != xx -a x"$(LIBALIAS)"x != x../../x ]; then \
	  $(AR) uvc $(LIBALIAS) $(ALIASES); \
	else true; fi


asm: $(ASMS)

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
