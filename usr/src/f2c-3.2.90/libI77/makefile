.SUFFIXES: .c .o

CFLAGS = -DSkip_f2c_Undefs -DNON_UNIX_STDIO -O -I../src

# compile, then strip unnecessary symbols
.c.o:
	cc $(CFLAGS) -c $*.c
	ld -r -x $*.o
	mv a.out $*.o

OBJ =	Version.o backspace.o dfe.o due.o iio.o inquire.o rewind.o rsfe.o \
	rdfmt.o sue.o uio.o wsfe.o sfe.o fmt.o lio.o lread.o open.o \
	close.o util.o endfile.o wrtfmt.o wref.o err.o fmtlib.o rsne.o wsne.o

libI77.a:	$(OBJ)
		ar r libI77.a $?
		ranlib libI77.a
install:	libI77.a
	cp libI77.a /usr/lib/libI77.a
	ranlib /usr/lib/libI77.a

lio.o:	lio.h
SRC=	lio.h fio.h fmt.h backspace.c dfe.c due.c iio.c inquire.c rewind.c \
	rsfe.c rdfmt.c sue.c uio.c wsfe.c sfe.c fmt.c lio.c lread.c open.c \
	close.c util.c endfile.c wrtfmt.c wref.c err.c fmtlib.c

Version.o: Version.c
	cc -c Version.c


clean:
	rm -f $(OBJ) libI77.a

clobber:	clean
	rm -f libI77.a

backspace.o:  fio.h
close.o:  fio.h
dfe.o:  fio.h
dfe.o:  fmt.h
due.o:  fio.h
endfile.o:  fio.h
err.o:  fio.h
fmt.o:  fio.h
fmt.o:  fmt.h
ftest.o:  fio.h
iio.o:  fio.h
iio.o:  fmt.h
inquire.o:  fio.h
lib.o:  fio.h
lio.o:  fio.h
lio.o:  fmt.h
lio.o:  lio.h
lread.o:  fio.h
lread.o:  fmt.h
lread.o:  lio.h
lread.o:  fp.h
nio.o:  fio.h
nio.o:  fmt.h
nio.o:  lio.h
open.o:  fio.h
rdfmt.o:  fio.h
rdfmt.o:  fmt.h
rdfmt.o:  fp.h
rewind.o:  fio.h
rsfe.o:  fio.h
rsfe.o:  fmt.h
rsne.o: fio.h lio.h
sfe.o:  fio.h
stest.o:  fio.h
sue.o:  fio.h
uio.o:  fio.h
util.o:  fio.h
wref.o:  fio.h fmt.h fp.h
wrtfmt.o:  fio.h
wrtfmt.o:  fmt.h
wsfe.o:  fio.h
wsfe.o:  fmt.h
wsne.o: fio.h lio.h

check:
	xsum README Version.c backspace.c close.c dfe.c due.c \
	endfile.c err.c fio.h fmt.c fmt.h fmtlib.c fp.h iio.c \
	inquire.c lio.c lio.h lread.c makefile open.c rdfmt.c \
	rewind.c rsfe.c rsne.c sfe.c sue.c uio.c util.c wref.c \
	wrtfmt.c wsfe.c wsne.c >zap
	cmp zap libI77.xsum && rm zap || diff libI77.xsum zap
