CC	= gcc
CFLAGS	= -O6
MIDIFILE = ./midifile

all:		midplay fmplay sbiset fmtest

midplay:	midplay.o $(MIDIFILE)/midifile.o $(MIDIFILE)/crack.o
		$(CC) -o midplay midplay.o $(MIDIFILE)/midifile.o $(MIDIFILE)/crack.o

fmplay:		fmplay.o $(MIDIFILE)/midifile.o $(MIDIFILE)/crack.o
		$(CC) -o fmplay fmplay.o $(MIDIFILE)/midifile.o $(MIDIFILE)/crack.o

fmp:		fmp.o ${MIDIFILE}/midifile.o
		$(CC) -s -N -o fmp fmp.o ${MIDIFILE}/midifile.o

fmtest:		fmtest.o
		$(CC) -s -N -o fmtest fmtest.o

sbiset:		sbiset.o
		$(CC) -s -N -o sbiset sbiset.o

$(MIDIFILE)/midifile.o:	$(MIDIFILE)/midifile.c
		cd $(MIDIFILE);make midifile.o

$(MIDIFILE)/crack.o:	$(MIDIFILE)/crack.c
		cd $(MIDIFILE);make crack.o

clean:
	rm -f midplay fmplay fmp fmtest sbiset core *.o
	cd $(MIDIFILE);make clean
