#An ANSI C compiler: gnu's "gcc"
#CC = cc   # for the dec
#CC = cc   # for the SGI
#CC = xlc # for the rs6000
#CC = /gnu/bin/gcc # elsewhere
CC = gcc -B/usr/dll/jump/
#CC = gcc

# to pass options in to the compilation, say things like 'make CFLAGS=-g'

#The following flags must be defined for UNIX/X11 implementations

CPPFLAGS = -DX11 -DUNIX

#Very important!  The first flag (struct-return) makes it safe to link
#  gcc objects with cc objects.

GCCFLAGS = -fpcc-struct-return -O -I/usr/src/suit/include
#GCCFLAGS = -bnodelcsect -DRS6000   # for the rs6000
#CFLAGS = -float -ansi -acpp -prototypes   # for the SGI

.c.o:
	$(CC) $(GCCFLAGS) $(CFLAGS) $(CPPFLAGS) -c $*.c


OBJECTS = 		\
srgp_attrib.o		\
srgp_canvas.o		\
srgp_color_X.o		\
srgp_cursor.o		\
srgp_echo_X.o		\
srgp_error.o		\
srgp_font.o		\
srgp_input.o		\
srgp_input_X.o		\
srgp_inquire.o		\
srgp_marker.o		\
srgp_output.o		\
srgp_raster.o		\
srgp_pattern.o		\
srgp_state.o		\
srgp_state_X.o		\
geom.o

all: $(OBJECTS)

share:
	mkimage -l /lib/libsrgp -v 2.3 -a 0x62200000 -j 0x4000 -g 0x2000 \
		-- $(OBJECTS) -lX11 -lc
	mkstubs -l /lib/libsrgp -v 2.3 -a 0x62200000 -j 0x4000 -g 0x2000 \
		-- libsrgp
	verify-shlib -l libsrgp.so.2.3 libsrgp.sa

libsrgp.a: $(OBJECTS) 
#	ld -o libsrgp.so.2.3 $(OBJECTS) -lX11 -lc
	ar rv libsrgp.a $(OBJECTS)
	ranlib libsrgp.a
	echo For the sgi, type \'ar ts libsrgp.a\'

$(OBJECTS): srgplocal.h srgp_sphigs.h srgppublic.h srgp_errtypes.h macros.h
geom.o: geom.h
srgp_pattern.o: default_patterns.h


clean:
	/bin/rm -f *.o libsrgp.a libsrgp.so.2.3
