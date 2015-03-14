OFILES = crt0.o hello.o salib.o sparc-stub.o
CC = gcc
AS = gcc
CFLAGS = -g -O
COMPILE.c = $(CC) $(CFLAGS) -c
COMPILE.s = $(COMPILE.c)

all:	aload hello

hello:	$(OFILES)
	ld -N -Ttext 0x40000000 -o hello $(OFILES) -lc

aload:	aload.o
	$(CC) $(CFLAGS) -o aload aload.o -lbfd -liberty

tmp:	crt0.o tmp.o salib.o goFast.a sparc-stub.o
	ld -N -Ttext 0x40000000 -o tmp crt0.o tmp.o salib.o sparc-stub.o goFast.a -lc

hd:	$(OFILES)
	ld -o hd $(OFILES) -lc

ho:	$(OFILES) alt-salib.o
	ld -o ho /lib/crt0.o hello.o sparc-stub.o alt-salib.o -lc

sparc-stub.o: ../sparc-stub.c
	$(CC) $(CFLAGS) -c ../sparc-stub.c

clean:
	rm -f *.o hello hd ho hellos tmp tmps aload
