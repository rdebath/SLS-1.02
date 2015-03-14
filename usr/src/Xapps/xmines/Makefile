#
# make install	installs the executables in the directory specified by BIN
#
# make clean	removes all temporary files.

CFLAGS = -O
CC = gcc

all: xbombs

#####################
# CROSS ASSEMBLER   #
#####################

xbombs: xmines.o X11.o score.o
	$(CC) -o xbombs xmines.o score.o X11.o -lX11

X11.o: covered.pat flag.pat gray.pat highlight.pat marked.pat mine.pat ohno.pat shades.pat smiley.pat uncovered.pat uncoveredcolor.pat

clean:
	rm -f *.o
