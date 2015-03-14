# Makefile for getty 2.0.7 distribution
#

SHELL=	/bin/sh
CC=	gcc

#  Take out SIMPLEINIT if you aren't using poe's simple init program
#
CFLAGS=	-O6 -Wall -fpcc-struct-return -DSIMPLEINIT
LFLAGS=	-s

LIBS=	-ltermcap
LINT=	lint -abchuvx
LLIBS=	

# where things go
BIN=	/etc
UUBIN=	/etc

# what are we making
SRC=	main.c funcs.c defaults.c table.c 
OBJ=	main.o funcs.o defaults.o table.o 
UUSRC=	uufuncs.c main.c funcs.c defaults.c table.c 
UUOBJ=	uufuncs.o uumain.o funcs.o defaults.o table.o 

# rules

all:	getty uugetty

getty:	$(OBJ)
	$(CC) $(LFLAGS) -o $@ $(OBJ) $(LIBS) 

uugetty: $(UUOBJ)
	$(CC) $(LFLAGS) -o $@ $(UUOBJ) $(LIBS) 

test: getty
	-mv $(BIN)/testgetty $(BIN)/testgetty-
	mv getty $(BIN)/testgetty

install:
	-mv $(BIN)/getty $(BIN)/getty-
	-mv $(UUBIN)/uugetty $(UUBIN)/uugetty-
	-cp getty $(BIN)
	-cp uugetty $(UUBIN)
	chmod 700 $(BIN)/getty $(UUBIN)/uugetty

lint:	$(SRC) umain.c
	@echo "linting getty sources..."
	echo "GETTY" >lint.out
	$(LINT) $(SRC) $(LLIBS) >>lint.out
	@echo "linting uugetty sources..."
	@echo '' >>lint.out
	echo 'UUGETTY' >>lint.out
	$(LINT) -DUUGETTY $(UUSRC) $(LLIBS) >>lint.out
	@echo "lint output is in lint.out"

clean:
	rm -f *.o core *.out Make.Log Makefile.old

clobber: clean
	rm -f getty uugetty

realclean: clobber

# make depend is broke, but the dependencies below are valid
#
#	depend:	makedep umain.c tune.h
#		chmod +x makedep
#		./makedep

# special dependencies follow

uumain.o:
	$(CC) $(CFLAGS) -DUUGETTY -c main.c -o uumain.o

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
# If this runs make out of memory, delete /usr/include lines.

defaults.o : defaults.c getty.h config.h tune.h extern.h funcs.h mytermio.h \
  defaults.h 
funcs.o : funcs.c getty.h config.h tune.h extern.h funcs.h mytermio.h table.h 
getutent.o : getutent.c getty.h config.h tune.h extern.h funcs.h mytermio.h 
main.o : main.c getty.h config.h tune.h extern.h funcs.h mytermio.h defaults.h \
  table.h release.h 
putenv.o : putenv.c getty.h config.h tune.h extern.h funcs.h mytermio.h 
strdup.o : strdup.c getty.h config.h tune.h extern.h funcs.h mytermio.h 
table.o : table.c getty.h config.h tune.h extern.h funcs.h mytermio.h table.h 
uumain.o : main.c getty.h config.h tune.h extern.h funcs.h mytermio.h defaults.h \
  table.h release.h uufuncs.h
uufuncs.o : uufuncs.c uufuncs.h

# WARNING: Put nothing here or make depend will gobble it up!
