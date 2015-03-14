include ../../Configfile

# hash table size (for keywords)
BUCKETS=	59
CFLAGS=		$(FLAGS) -DHSIZE=$(BUCKETS)

all:		croff

install:	all

croff:		keys.o croff.o hash.o
		$(CC) -o $(LEXLIB) croff croff.o hash.o keys.o

keys.c:		keywords mktable
		./mktable -n keywords $(BUCKETS) < keywords > keys.c

mktable:	mktable.o hash.o
		$(CC) $(CFLAGS) -o mktable mktable.o hash.o

clean:
		rm -f *.o core croff.c keys.c mktable

clobber:	clean
		rm -f croff

hash.o:		hash.h

mktable.o:	hash.h
