include ../../../Configfile

CFLAGS=		$(FLAGS)

AOBJS=		do_blit.o do_mask.o do_2blit.o do_2mask.o misc.o
COBJS=		bit_blit.o bit_destroy.o bit_line.o \
		bit_on.o bit_open.o bit_point.o bit_size.o fast_scroll.o $(SCREEN).o
OBJS=		$(AOBJS) $(COBJS)

all:		$(OBJS)

install:	all

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f screen.h

screen.h:
		ln -s $(SCREEN).h screen.h

$(COBJS):	screen.h
$(AOBJS):	asm.h
