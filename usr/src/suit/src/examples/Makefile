######################################################################
# Makefile for SUIT applications
######################################################################

# this denotes where the SUIT distribution lives
SUIT	  =/usr/src/suit
XLIBS	  =
X_INC_DIR =/usr/X386/include/X11

CC	=	gcc
LDLIBS	=	-lsuit -lsrgp -lX11 -lm -lc
LDFLAGS	=	

#DEBUG	=	-g
OPTIMIZE =	-O

########################### NOTE ####################################
#
# comment out the -static flag to 
# make dynamically linked executables.
# This will make your programs take up less room on disk.
#
#####################################################################
#STATIC= -static



CFLAGS	=	$(STATIC) -fpcc-struct-return  -Wall -I$(X_INC_DIR) -I$(SUIT)/include $(DEBUG) $(OPTIMIZE)

######################################################################

COMPILE.c=$(CC) $(CFLAGS) $(CPPFLAGS) -c

empty: empty.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o empty empty.o $(LDLIBS) 
	strip empty
######################################################################

clean:
	rm -f *.o core empty
