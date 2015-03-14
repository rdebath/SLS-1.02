include ../../../Configfile

CFLAGS=		$(FLAGS) $(BLITLIBFLAGS)

OBJS=		bitmap.o blit.o line.o pixel.o fast_scroll.o dummy.o bit_size.o bit_on.o sun.o

all:		$(OBJS)

install:	all

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f blit.c

rops:		rops.o bitmap.o blit.o
		$(CC) $(CFLAGS) -o rops rops.o blit.o bitmap.o
                
test_rop:	test_rop.o bitmap.o blit.o
		$(CC) $(CFLAGS) -o test_rop test_rop.o blit.o bitmap.o
                
blit.c:		blit.C
		sed "s/^[$(BLITLIBOPT)]/#/" < blit.C | $(CPP) | sed 's/^[1-9%]/#/' > blit.c
