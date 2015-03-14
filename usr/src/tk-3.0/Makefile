#
# This is a simplified Makefile for use in Tk distributions.  Before using
# it to compile Tk, you may wish to reset some of the following variables:
#
# TCL_DIR -		Name of directory holding tcl.h and tcl.a.
# XLIB -		Name of archive containing Xlib binaries.
# TK_LIBRARY -		Full path name of directory to contain scripts
#			and other library files used by Tk.  This value
#			is available to applications as the variable
#			$tk_library.  If the environment variable
#			TK_LIBRARY is defined by a user, it will override
#			the value specified in this Makefile.
# LIB_DIR		Directory to contain the archive libtcl.a
# BIN_DIR		Directory to contain executables such as wish.
# MAN_DIR		Directory to contain manual entries for Tcl.
#

# You may also wish to add some of the following switches to the CFLAGS
# variable:
#
# -DNO_PROTOTYPE	Turns off ANSI-style procedure prototypes and the
#			corresponding compile-time checks.  Without this
#			defininition, prototypes will be turned on if the
#			compiler supports ANSI C by defining __STDC__.
#

TCL_DIR		= tcl
XLIB		= -lX11
TK_LIBRARY	= /usr/local/lib/tk
LIB_DIR		= /usr/local/lib
BIN_DIR		= /usr/local
MAN1_DIR	= /usr/local/man/man1
MAN3_DIR	= /usr/local/man/man3
MANN_DIR	= /usr/local/man/mann
CC		= cc
CFLAGS		= -I. -I$(TCL_DIR) -O -DTK_LIBRARY=\"${TK_LIBRARY}\"

LIBS = libtk.a $(TCL_DIR)/libtcl.a

WIDGOBJS = tkButton.o tkEntry.o tkFrame.o tkListbox.o \
	tkMenu.o tkMenubutton.o tkMessage.o tkScale.o \
	tkScrollbar.o

CANVOBJS = tkCanvas.o tkCanvArc.o tkCanvBmap.o tkCanvLine.o \
	tkCanvPoly.o tkCanvPs.o tkCanvText.o tkCanvWind.o \
	tkRectOval.o tkTrig.o

TEXTOBJS = tkText.o tkTextBTree.o tkTextDisp.o tkTextIndex.o tkTextTag.o

OBJS = tk3d.o tkArgv.o tkAtom.o tkBind.o tkBitmap.o tkCmds.o \
	tkColor.o tkConfig.o tkCursor.o tkError.o tkEvent.o \
	tkFocus.o tkFont.o tkGet.o tkGC.o tkGeometry.o tkGrab.o \
        tkOption.o tkPack.o tkPlace.o tkPreserve.o tkSelect.o \
        tkSend.o tkWindow.o tkWm.o $(WIDGOBJS) \
	$(CANVOBJS) $(TEXTOBJS)

WIDGSRCS = tkButton.c tkEntry.c tkFrame.c tkListbox.c \
	tkMenu.c tkMenubutton.c tkMessage.c tkScale.c \
	tkScrollbar.c tkText.c tkTextBTree.c tkTextDisp.c \
	tkTextIndex.c

CANVSRCS = tkCanvas.c tkCanvArc.c tkCanvBmap.c tkCanvLine.c \
	tkCanvPoly.c tkCanvPs.c tkCanvText.c tkCanvWind.c \
	tkRectOval.c tkTrig.c

TEXTSRCS = tkText.c tkTextBTree.c tkTextDisp.c tkTextIndex.c tkTextTag.c

SRCS = tk3d.c tkArgv.c tkAtom.c tkBind.c tkBitmap.c tkCmds.c \
	tkColor.c tkConfig.c tkCursor.c tkError.c tkEvent.c \
	tkFocus.c tkFont.c tkGet.c tkGC.c tkGeometry.c tkGrab.c \
        tkOption.c tkPack.c tkPlace.c tkPreserve.c tkSelect.c \
        tkSend.c tkWindow.c tkWm.c $(WIDGSRCS) \
	$(CANVSRCS) $(TEXTSRCS)

all: libtk.a wish

wish: main.o $(LIBS)
	$(CC) $(CFLAGS) main.o $(LIBS) $(XLIB) -lm -o wish

libtk.a: $(OBJS)
	rm -f libtk.a
	ar cr libtk.a $(OBJS)
	ranlib libtk.a

$(TCL_DIR)/libtcl.a:
	cd $(TCL_DIR); make libtcl.a

install:
	-if [ ! -d $(TK_LIBRARY) ] ; then mkdir -p $(TK_LIBRARY); fi
	-if [ ! -d $(LIB_DIR) ] ; then mkdir -p $(LIB_DIR); fi
	-if [ ! -d $(BIN_DIR) ] ; then mkdir -p $(BIN_DIR); fi
	-if [ ! -d $(MAN1_DIR) ] ; then mkdir -p $(MAN1_DIR); fi
	-if [ ! -d $(MAN3_DIR) ] ; then mkdir -p $(MAN3_DIR); fi
	-if [ ! -d $(MANN_DIR) ] ; then mkdir -p $(MANN_DIR); fi
	rm -rf $(TK_LIBRARY)/*
	cp -r library/*.tcl library/tclIndex library/demos $(TK_LIBRARY)
	cp -f libtk.a $(LIB_DIR)
	cp -f wish $(BIN_DIR)
	cp -f doc/*.1 $(MAN1_DIR)
	cp -f doc/*.3 $(MAN3_DIR)
	cp -f doc/*.n $(MANN_DIR)
	cd tcl; make install

clean:
	rm -f $(OBJS) main.o libtk.a wish	

$(OBJS): tk.h tkInt.h tkConfig.h
$(WIDGOBJS): default.h
$(CANVOBJS): default.h tkCanvas.h
$(TEXTOBJS): default.h tkText.h
main.o: tk.h tkInt.h
