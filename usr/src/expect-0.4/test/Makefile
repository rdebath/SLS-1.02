CC = gcc
CPLUSPLUS = g++
CPLUSPLUSLIBDIR = -L/depot/gnu/arch/lib
CPLUSPLUSLIB = -lg++

CFLAGS = -g -I..
LIBEXPECT = ../libexpect.a
#LIBEXPECT = /depot/sundry/plat/lib/libexpect.a

SCRIPTS = su.exp noidle script.exp bonfield.exp

all: chesslib chesslib2 chesslib++

# this can be compiled with either cc or gcc
chesslib: chesslib.o $(LIBEXPECT)
	$(CC) -g -o chesslib chesslib.o $(LIBEXPECT)

# this can be compiled with either cc or gcc
chesslib2: chesslib2.o $(LIBEXPECT)
	$(CC) -g -o chesslib2 chesslib2.o $(LIBEXPECT)

# this is compiled with c++
chesslib++: chesslib++.o $(LIBEXPECT)
	$(CPLUSPLUS) -g -o chesslib++ chesslib++.o $(LIBEXPECT) \
		$(CPLUSPLUSLIBDIR) $(CPLUSPLUSLIB)

chesslib++.o: chesslib++.c
	$(CPLUSPLUS) -c $(CFLAGS) chesslib++.c

cleanup:
	rm -f expect devtty exho dumb test.raw test.results test.tmp

# copy some contributed scripts over to public-accessible directory
SCRIPTDIR = ~ftp/pub/expect/scripts
ftp:
	rcp README.scripts durer:$(SCRIPTDIR)/README
	rcp $(SCRIPTS) durer:$(SCRIPTDIR)
	rsh durer ls -l $(SCRIPTDIR)
