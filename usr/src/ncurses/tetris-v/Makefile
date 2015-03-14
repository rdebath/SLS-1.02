
CFLAGS= -I/usr/local/include -DTRACE -O 
LIBES=-lncurses 
BINDIR=/usr/games
PROG=tetris
CFILES=main.c shape.c support.c score.c 
OFILES=main.o shape.o support.o score.o 

$(PROG):	$(OFILES)
	cc $(CFLAGS) -o $(PROG) $(OFILES) $(LIBES)

$(OFILES): defs.h

install: $(PROG)
	strip $(PROG)
	install -s $(PROG) $(BINDIR)

clean:
	rm -f *.o tetris core 

