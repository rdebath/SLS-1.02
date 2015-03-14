# Indicate the correct paths for each of the following libraries
#
# XLIB          Your X11 (R4 or R5) library.
# TCLLIB        Tcl Version 6.5 library.
# TKLIB         Tk Version 3.0 library.
# TCLSRC        Tcl Version 6.5 source directory.
# TKSRC         Tk Version 3.0 source directory.
#
XLIB=-L/usr/X11/lib -lX11
TCLLIB=-L/usr/X11/lib -ltcl
TKLIB=-L/usr/X11/lib -ltk
TCLSRC=-I/usr/src/usr.bin/tk3.0/tcl
TKSRC=-I/usr/src/usr.bin/tk3.0
#
INCLUDES=-I. -I/usr/X11/include $(TCLSRC) $(TKSRC)
CDEBUGFLAGS=-O
LIBS=$(TKLIB) $(TCLLIB) $(XLIB) -lm
CFLAGS= $(CDEBUGFLAGS) $(INCLUDES) $(DEFINES)

SRCS=	main.c tkDragDrop.c
OBJS=	main.o tkDragDrop.o

CC =   cc 

dish:  $(OBJS)
	rm -f dish
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) -o dish

$(OBJS):$(HDRS)

clean:
	rm -f $(OBJS) dish core

lint:
	lint $(INCLUDES) $(SRCS)
