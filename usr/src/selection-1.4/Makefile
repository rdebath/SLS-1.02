# Makefile for selection utility
# Andrew Haylett, 14th December 1992

CC=	gcc
LD=	ld

# define REVERSE_BUTTONS if you are a lefty
# MOUSEFLAGS = -DREVERSE_BUTTONS

CFLAGS=	-O3 -Wall $(MOUSEFLAGS)
LDFLAGS= -s

selection:	stuff.o mouse.o
	$(CC) $(LDFLAGS) -o selection stuff.o mouse.o

test-mouse:	test-mouse.o mouse.o
	$(CC) $(LDFLAGS) -o test-mouse test-mouse.o mouse.o

mouse.o:	mouse.c mouse.h

stuff.o:	stuff.c mouse.h

test-mouse.o:	test-mouse.c mouse.h

clean:
	rm -f stuff.o mouse.o test-mouse.o selection test-mouse
