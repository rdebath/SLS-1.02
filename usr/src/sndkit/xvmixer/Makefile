CC	= gcc
LD	= gcc
CFLAGS	= -Wall
LDFLAGS =
LIBRARIES=	-lxview -lolgx -lrpclib -lX11

.c.o:		$(CC) -c $(CFLAGS) $@ 

all:	xvmixer

xvmixer:	xvmixer.o
		$(LD) $(LDFLAGS) -o $@ xvmixer.o $(LIBRARIES)

xvmixer.o:	locked.bit unlocked.bit rec.bit

clean:
		rm -f *.o xvmixer core
