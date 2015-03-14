#
# This Makefile is for use when distributing Tcl to the outside world.
# It is normally set up by running the "config" script.  Before modifying
# this file by hand, you should read through the "config" script to see
# what it does.
#
# Some changes you may wish to make here:
#
# 1. To compile for non-UNIX systems (so that only the non-UNIX-specific
# commands are available), change the OBJS line below so it doesn't
# include ${UNIX_OBJS}.  Also, add the switch "-DTCL_GENERIC_ONLY" to
# CFLAGS.  Lastly, you'll have to provide your own replacement for the
# "panic" procedure (see panic.c for what the current one does).
#
# 2. ANSI-C procedure prototypes are turned on by default if supported
# by the compiler.  To turn them off, add "-DNO_PROTOTYPE" to CFLAGS
# below.
#
# 3. If you want to put Tcl-related information in non-standard places,
#    change the following definitions below to reflect where you want
#    thing:
#
#    TCL_LIBRARY	Directory containing the library of Tcl scripts.
#			Note: if the TCL_LIBRARY environment variable is
#			specified at run-time then it overrides the value
#			specified in this file.
#    LIB_DIR		Directory containing the archive libtcl.a
#    MAN_DIR		Directory containing manual entries.  Tcl will
#			use subdirectories "man3" and "mann".

TCL_LIBRARY =	/usr/local/lib/tcl
LIB_DIR =	/usr/local/lib
MAN3_DIR =	/usr/local/man/man3
MANN_DIR =	/usr/local/man/mann

CC =		cc
CFLAGS =	-O -I. -DTCL_LIBRARY=\"${TCL_LIBRARY}\"

GENERIC_OBJS =	regexp.o tclAssem.o tclBasic.o tclCkalloc.o \
	tclCmdAH.o tclCmdIL.o tclCmdMZ.o tclExpr.o tclGet.o \
	tclHash.o tclHistory.o tclParse.o tclProc.o tclUtil.o \
	tclVar.o

UNIX_OBJS = panic.o tclEnv.o tclGlob.o tclUnixAZ.o tclUnixStr.o \
	tclUnixUtil.o 

COMPAT_OBJS =

OBJS = ${GENERIC_OBJS} ${UNIX_OBJS} ${COMPAT_OBJS}

all: libtcl.a

libtcl.a: ${OBJS}
	rm -f libtcl.a
	ar cr libtcl.a ${OBJS}
	ranlib libtcl.a

install: libtcl.a
	-if [ ! -d $(TCL_LIBRARY) ] ; then mkdir -p $(TCL_LIBRARY); fi
	-if [ ! -d $(LIB_DIR) ] ; then mkdir -p $(LIB_DIR); fi
	-if [ ! -d $(MAN3_DIR) ] ; then mkdir -p $(MAN3_DIR); fi
	-if [ ! -d $(MANN_DIR) ] ; then mkdir -p $(MANN_DIR); fi
	cp -f library/*.tcl library/tclIndex $(TCL_LIBRARY)
	cp -f libtcl.a $(LIB_DIR)
	cp -f doc/*.3 $(MAN3_DIR)
	cp -f doc/*.n $(MANN_DIR)

tclTest: tclTest.o libtcl.a
	${CC} ${CFLAGS} tclTest.o libtcl.a -o tclTest

test: tclTest
	( echo cd tests ; echo source all ) | ./tclTest

configured:
	@echo "The configuration script \"./config\" hasn't been run"
	@echo "successfully yet.  Please run it as described in the "
	@echo "README file, then run make again."
	exit 1

clean:
	rm -f ${OBJS} libtcl.a tclTest.o tclTest

# The following target is used during configuration to compile
# a test program to see if certain facilities are available on
# the system.

configtest:
	${CC} ${CFLAGS} test.c

${OBJS}: tcl.h tclHash.h tclInt.h configured
${UNIX_OBJS}: tclUnix.h
