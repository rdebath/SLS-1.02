CFLAGS= -g -I/usr/demo/SOUND -DLIBSPEAK
LIBS= -L. -L/usr/demo/SOUND -lspeak -laudio -lm

CCOBJECTS=english.o parse.o phoneme.o saynum.o spellword.o speak.o

scat: libspeak.a scat.o
	cc -o scat scat.o $(LIBS)

test: libspeak.a test.o
	cc -o test test.o $(LIBS)

libspeak.a: $(CCOBJECTS)
	ar ruv libspeak.a $(CCOBJECTS)
	ranlib libspeak.a

phoneme: $(CCOBJECTS)
	$(CC) -o phoneme $(CCOBJECTS)

clean:
	rm $(CCOBJECTS)
