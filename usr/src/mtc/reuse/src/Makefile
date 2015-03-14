TARGET	= SUN
MC	= MC
OPT	= -O
UNIX	= UNIX
CFLAGS	= $(OPT) -D$(UNIX)
CC	= cc

# targets

all:	$(MC)

MC:	System.o
	rm -f Main
	echo p Main | mc -noindex -norange

M2C:	System.o
	rm -f Main
	for file in `ls *.md`; do ln $$file `basename $$file .md`.def; done
	for file in `ls *.mi`; do ln $$file `basename $$file .mi`.mod; done
	make Main -f MakefileM2C

m2c:
	co RCS/*
	ln *.m[id] ../m2c
	cd ../m2c; rm -f Checks.[hc] DynArrDrv.c DynArray.[hc] General.[hc] \
	   Heap.[hc] IO.[hc] IODrv.c Idents.[hc] Layout.[hc] Lists.[hc] Main.c \
	   Memory.[hc] MemoryDrv.c RelDrv.c Relations.[hc] Sets.[hc] SetsC.[hc] \
	   SetsDrv.c StdIO.[hc] StringMem.[hc] Strings.[hc] SysIODrv.c System.[hc] \
	   Texts.[hc] Times.[hc] Positions.[hc] Errors.[hc] Source.[hc] Sort.[hc]
	cp System.c ../m2c
	cd ../m2c; make sources
	cd ../m2c; rm -f *.m[id]

test:	System.o
	echo p DynArrDrv| mc
	echo p IODrv	| mc
	echo p MemoryDrv| mc
	echo p RelDrv	| mc
	echo p SetsDrv	| mc
	echo p SysIODrv	| mc

lint:
	lint -u *.c

CLEAN:	clean
	rm -f *.[dimor] *.sym *.def *.mod

clean:
	rm -f *Drv.[dimor] *Drv.sym *Drv Main.[dimor] Main t *.bak

.c.o:
	$(CC) $(CFLAGS) -c $*.c;
