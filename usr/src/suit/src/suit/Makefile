# suit makefile
# last modified by Chris Long on 11 July 1991
#
# syntax is
#
#       make <platform> [ PROG=<program> ]
#
# The platform must be specified.  The default program is all, if none
# is specified.

VERSION	=	2.3
SUIT	=/usr/src/suit

XLIBS   =
LDFLAGS =       -L$(SUIT)/lib $(SGILDFLAGS)

#DEBUG  =       -g
OPTIMIZE =      -DPRODUCTION_VERSION
#PROFILE =	-pg

CFLAGS  =       -I/usr/include/X11 -I/usr/local/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE)

COMPILE.o.c=$(CC) -c $(CFLAGS)

OBJECTS	= \
arrow.o		ball.o		bounded.o	bulletin.o	\
button.o	chips.o		clock.o		deftypes.o	\
dialog.o	dyncreat.o	dyndebug.o	dyndel.o	\
dynfind.o	dynput.o	dynsize.o	dynsort.o	\
editprop.o	elevator.o	filebox.o	font.o		\
hash.o		help.o		gp.o		hints.o		\
initial.o	input.o		interest.o	menu.o		\
motif.o		output.o	placemat.o	polygon.o	\
property.o	radio.o		scrolbox.o	spring.o	\
stacker.o	suithash.o	suitmem.o	switch.o	\
textbox.o	textedit.o	tools.o		trashcan.o	\
tree.o		utility.o	uvalogo.o

##############################################
### A function extractor I use for the docs -- mjc4y
CEXTRACT = cextract

HEADERS = -I/usr/include/X11 -I/usr/local/include/X11 -I../../include 

######################################################################


all: $(OBJECTS) $(DEST)

linux:
	make $(PROG) ARCH=linux CC=gcc LDLIBS='-lsrgp -lX11 -lm -lc' RANLIB=ranlib CFLAGS='-I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Wall -fpcc-struct-return' DEST=../../lib/libsuit.a

linux-o:
	make $(PROG) ARCH=linux CC='gcc -B/usr/dll/jump/' LDLIBS='-lsrgp -lX11 -lm -lc' RANLIB=ranlib CFLAGS='-I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Wall -fpcc-struct-return'

linux-share:
	make $(PROG) ARCH=linux CC='gcc -B/usr/dll/jump/' LDLIBS='-lsrgp -lX11 -lm -lc' RANLIB=ranlib CFLAGS='-I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Wall -fpcc-struct-return' DEST=../../lib/libsuit.so

sgi:
	make $(PROG) ARCH=sgi CC=cc LDLIBS='-lsuit -lsrgp -lX11 -lm' RANLIB=\# CFLAGS='-float -ansi -fullwarn -woff 269,309,22,24,302,303,270,182 -acpp -prototypes -I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Dsgi' DEST=../../sgi/lib/libsuit.a

sun3:
	make $(PROG) ARCH=sun3 CC=/gnu/bin/gcc LDLIBS='-lsuit -lsrgp -lX11 -lm' RANLIB=ranlib CFLAGS='-fpcc-struct-return -I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Wall' DEST=../../sun3/lib/libsuit.a

sparc:
	make $(PROG) ARCH=sparc CC=/gnu/bin/gcc LDLIBS='-lsuit -lsrgp -lX11 -lm' RANLIB=ranlib CFLAGS='-I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Wall -fpcc-struct-return' DEST=../../sparc/lib/libsuit.a

sparcShare:
	make $(PROG) ARCH=sparc CC=/gnu/bin/gcc LDLIBS='-lsuit -lsrgp -lX11 -lm' RANLIB=ranlib CFLAGS='-I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE) -Wall -fpcc-struct-return -fPIC' DEST=../../sparc/lib/libsuit.so.$(VERSION)

rs6000:
	make $(PROG) ARCH=rs6000 CC=xlc LDLIBS='-bnodelcsect -lsuit -lsrgp -lX11 -lm' RANLIB=ranlib DEST=../../rs6000/lib/libsuit.a

dec:
	make $(PROG) ARCH=dec CC=cc LDLIBS='-lsuit -lsrgp -lX11 -lm' RANLIB=ranlib CFLAGS='-I/usr/include/X11 -I../../include $(DEBUG) $(OPTIMIZE) $(PROFILE)' DEST=../../dec/lib/libsuit.a


all:	$(OBJECTS)

../../lib/libsuit.a: $(OBJECTS)
	ar rv $(DEST) $(OBJECTS) >/dev/null
	$(RANLIB) $(DEST)

../../lib/libsuit.so.$(VERSION): $(OBJECTS)
	ld -o ../../lib/libsuit.so.$(VERSION) $(OBJECTS)

../../lib/libsuit.so: $(OBJECTS)
	mkimage -l /lib/libsuit -v 2.3 -a 0x62215000 -j 0x4000 -g 0x2000 \
		-- $(OBJECTS) $(LDFLAGS) $(LDLIBS)
	mkstubs -l /lib/libsuit -v 2.3 -a 0x62215000 -j 0x4000 -g 0x2000 \
		-- libsuit
	verify-shlib -l libsuit.so.2.3 libsuit.sa

share: $(OBJECTS)
	ld -o ../../lib/libsuit.so.$(VERSION) $(OBJECTS) -L../../lib -lsrgp -lX11 -lXext -lm -lc

clean:
	/bin/rm -f *.o *~ example mytest randy dennis


chuck: chuck.o $(DEST)
	/bin/rm -f chuck
	$(CC) $(PROFILE) $(LDFLAGS) -o chuck chuck.o $(LDLIBS)

example: example.o $(DEST)
	/bin/rm -f example
	$(CC) $(PROFILE) $(LDFLAGS) -o example example.o $(LDLIBS)

bug: bug.o $(DEST)
	/bin/rm -f bug
	$(CC) $(PROFILE) $(LDFLAGS) -o bug bug.o $(LDLIBS)

rob: rob.o $(DEST)
	/bin/rm -f rob
	$(CC) $(PROFILE) $(LDFLAGS) -o rob rob.o $(LDLIBS)

dennis: dennis.o $(DEST)
	/bin/rm -f rob
	$(CC) $(PROFILE) $(LDFLAGS) -o dennis dennis.o $(LDLIBS)

kim: kim.o $(DEST)
	/bin/rm -f kim
	$(CC) $(PROFILE) $(LDFLAGS) -o kim kim.o $(LDLIBS)

mytest: mytest.o $(DEST)
	/bin/rm -f mytest
	$(CC) $(LDFLAGS) -o mytest mytest.o $(LDLIBS)

	
randy: randy.o $(DEST)
	/bin/rm -f randy
	$(CC) $(LDFLAGS) -o randy randy.o $(LDLIBS) 
	randy

# only produce ansi prototypes
FunctionList.new: $(SRCS) 
	$(CEXTRACT) +ansi-code  $(HEADERS) +sort-all -dual-output -tab-width=8\
	-externs $(SRCS) > /users/suit/doc/FunctionList.new

FLAGS  = -I/usr/include/X11 -I/usr/local/include/X11 -I../../include


SRGPSRCS = 					\
/users/suit/srgp/src/srgp/srgp_attrib.c		\
/users/suit/srgp/src/srgp/srgp_canvas.c		\
/users/suit/srgp/src/srgp/srgp_color_X.c	\
/users/suit/srgp/src/srgp/srgp_cursor.c		\
/users/suit/srgp/src/srgp/srgp_echo_X.c		\
/users/suit/srgp/src/srgp/srgp_font.c		\
/users/suit/srgp/src/srgp/srgp_input.c		\
/users/suit/srgp/src/srgp/srgp_input_X.c	\
/users/suit/srgp/src/srgp/srgp_inquire.c	\
/users/suit/srgp/src/srgp/srgp_marker.c		\
/users/suit/srgp/src/srgp/srgp_output.c		\
/users/suit/srgp/src/srgp/srgp_pattern.c	\
/users/suit/srgp/src/srgp/srgp_raster.c		\
/users/suit/srgp/src/srgp/srgp_state.c		\
/users/suit/srgp/src/srgp/srgp_state_X.c	\
/users/suit/srgp/src/srgp/geom.c

saber:
	#load -C -I/usr/local/include/X11 -D__unix__ -DX11 $(SRGPSRCS)
	#load -C $(FLAGS) -D__unix__ $(SRCS)
	#load -C /usr/lib/libm.a
	#load -C ../../sparc/lib/libsrgp.a
	#load -C /usr/local/lib/X11/libX11.a
