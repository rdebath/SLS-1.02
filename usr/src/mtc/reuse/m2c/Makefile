OPT	= -O
UNIX	= UNIX
CFLAGS	= $(OPT) -D$(UNIX)
CC	= cc
M2FLAGS	= -c -d/usr/local/lib/mocka/lib
SYSTEM	= System.o

OBJS	= Checks.o DynArray.o General.o Heap.o IO.o Idents.o Layout.o \
	  Lists.o Memory.o Relations.o Sets.o SetsC.o StdIO.o StringMem.o \
	  Strings.o System.o Texts.o Times.o Arguments.o SYSTEM_.o \
	  Positions.o Errors.o Source.o Sort.o
CLIBS	= Arguments.o SYSTEM_.o


libreuse.a	: $(OBJS)
	ar uc libreuse.a $(OBJS)
	ranlib libreuse.a


all	: $(CLIBS)

# HEAD

sources	: Checks.h

sources	: Checks.c
Checks.o	: Checks.c Checks.h
Checks.o	: IO.h
Checks.o	: System.h

sources	: DynArrDrv.c
all	: DynArrDrv
DynArrDrv.o	: DynArrDrv.c
DynArrDrv.o	: DynArray.h
DynArrDrv.o	: IO.h

sources	: DynArray.h

sources	: DynArray.c
DynArray.o	: DynArray.c DynArray.h
DynArray.o	: General.h
DynArray.o	: Memory.h
DynArray.o	: IO.h

sources	: Errors.h
Errors.h	: Positions.h
Errors.h	: IO.h

sources	: Errors.c
Errors.o	: Errors.c Errors.h
Errors.o	: Memory.h
Errors.o	: IO.h
Errors.o	: Positions.h
Errors.o	: StringMem.h
Errors.o	: Strings.h
Errors.o	: Idents.h
Errors.o	: Sets.h
Errors.o	: Sort.h
Errors.o	: System.h
Errors.o	: Strings.h

sources	: General.h

sources	: General.c
General.o	: General.c General.h
General.o	: System.h

sources	: Heap.h

sources	: Heap.c
Heap.o	: Heap.c Heap.h
Heap.o	: General.h
Heap.o	: Memory.h

sources	: IO.h
IO.h	: System.h

sources	: IO.c
IO.o	: IO.c IO.h
IO.o	: General.h
IO.o	: Memory.h
IO.o	: System.h

sources	: IODrv.c
all	: IODrv
IODrv.o	: IODrv.c
IODrv.o	: IO.h

sources	: Idents.h
Idents.h	: IO.h
Idents.h	: Strings.h
Idents.h	: StringMem.h

sources	: Idents.c
Idents.o	: Idents.c Idents.h
Idents.o	: DynArray.h
Idents.o	: Strings.h
Idents.o	: StringMem.h
Idents.o	: IO.h
Idents.o	: Strings.h
Idents.o	: StringMem.h

sources	: Layout.h
Layout.h	: IO.h

sources	: Layout.c
Layout.o	: Layout.c Layout.h
Layout.o	: IO.h

sources	: Lists.h
Lists.h	: IO.h

sources	: Lists.c
Lists.o	: Lists.c Lists.h
Lists.o	: Memory.h
Lists.o	: IO.h

sources	: Main.c
all	: Main
Main.o	: Main.c
Main.o	: General.h
Main.o	: Memory.h
Main.o	: Heap.h
Main.o	: DynArray.h
Main.o	: Lists.h
Main.o	: Strings.h
Main.o	: Texts.h
Main.o	: StringMem.h
Main.o	: Idents.h
Main.o	: Sets.h
Main.o	: SetsC.h
Main.o	: Relations.h
Main.o	: IO.h
Main.o	: StdIO.h
Main.o	: Layout.h
Main.o	: Positions.h
Main.o	: Errors.h
Main.o	: Sort.h
Main.o	: Source.h
Main.o	: System.h
Main.o	: Checks.h
Main.o	: Times.h

sources	: Memory.h

sources	: Memory.c
Memory.o	: Memory.c Memory.h
Memory.o	: General.h
Memory.o	: System.h

sources	: MemoryDrv.c
all	: MemoryDrv
MemoryDrv.o	: MemoryDrv.c
MemoryDrv.o	: Memory.h
MemoryDrv.o	: IO.h

sources	: Positions.h
Positions.h	: IO.h

sources	: Positions.c
Positions.o	: Positions.c Positions.h
Positions.o	: IO.h

sources	: RelDrv.c
all	: RelDrv
RelDrv.o	: RelDrv.c
RelDrv.o	: IO.h
RelDrv.o	: Sets.h
RelDrv.o	: Relations.h

sources	: Relations.h
Relations.h	: IO.h
Relations.h	: Sets.h

sources	: Relations.c
Relations.o	: Relations.c Relations.h
Relations.o	: IO.h
Relations.o	: DynArray.h
Relations.o	: Sets.h
Relations.o	: Sets.h

sources	: Sets.h
Sets.h	: IO.h

sources	: Sets.c
Sets.o	: Sets.c Sets.h
Sets.o	: General.h
Sets.o	: DynArray.h
Sets.o	: IO.h

sources	: SetsC.h
SetsC.h	: IO.h
SetsC.h	: Sets.h

sources	: SetsC.c
SetsC.o	: SetsC.c SetsC.h
SetsC.o	: Sets.h
SetsC.o	: IO.h

sources	: SetsDrv.c
all	: SetsDrv
SetsDrv.o	: SetsDrv.c
SetsDrv.o	: Sets.h
SetsDrv.o	: IO.h

sources	: Sort.h

sources	: Sort.c
Sort.o	: Sort.c Sort.h

sources	: Source.h
Source.h	: System.h

sources	: Source.c
Source.o	: Source.c Source.h
Source.o	: System.h

sources	: StdIO.h

sources	: StdIO.c
StdIO.o	: StdIO.c StdIO.h
StdIO.o	: IO.h
StdIO.o	: IO.h

sources	: StringMem.h
StringMem.h	: IO.h
StringMem.h	: Strings.h

sources	: StringMem.c
StringMem.o	: StringMem.c StringMem.h
StringMem.o	: DynArray.h
StringMem.o	: Strings.h
StringMem.o	: IO.h

sources	: Strings.h
Strings.h	: IO.h

sources	: Strings.c
Strings.o	: Strings.c Strings.h
Strings.o	: General.h
Strings.o	: IO.h
Strings.o	: IO.h

sources	: SysIODrv.c
all	: SysIODrv
SysIODrv.o	: SysIODrv.c
SysIODrv.o	: System.h
SysIODrv.o	: Checks.h

sources	: System.h

sources	: Texts.h
Texts.h	: IO.h
Texts.h	: Lists.h
Texts.h	: Strings.h

sources	: Texts.c
Texts.o	: Texts.c Texts.h
Texts.o	: IO.h
Texts.o	: Lists.h
Texts.o	: Strings.h
Texts.o	: StringMem.h
Texts.o	: Lists.h

sources	: Times.h

sources	: Times.c
Times.o	: Times.c Times.h
Times.o	: System.h
Times.o	: IO.h

SYSTEM_.o	: SYSTEM_.c SYSTEM_.h

DynArrDrv	: $(SYSTEM)	\
	DynArrDrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	DynArray.o	\
;	$(CC) $(CFLAGS) -o DynArrDrv $(SYSTEM)	\
	DynArrDrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	DynArray.o	\
	$(CLIBS)

IODrv	: $(SYSTEM)	\
	IODrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
;	$(CC) $(CFLAGS) -o IODrv $(SYSTEM)	\
	IODrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	$(CLIBS)

Main	: $(SYSTEM)	\
	Main.o	\
	Times.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	Checks.o	\
	Source.o	\
	Sort.o	\
	Errors.o	\
	Strings.o	\
	Sets.o	\
	DynArray.o	\
	Idents.o	\
	StringMem.o	\
	Positions.o	\
	Layout.o	\
	StdIO.o	\
	Relations.o	\
	SetsC.o	\
	Texts.o	\
	Lists.o	\
	Heap.o	\
;	$(CC) $(CFLAGS) -o Main $(SYSTEM)	\
	Main.o	\
	Times.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	Checks.o	\
	Source.o	\
	Sort.o	\
	Errors.o	\
	Strings.o	\
	Sets.o	\
	DynArray.o	\
	Idents.o	\
	StringMem.o	\
	Positions.o	\
	Layout.o	\
	StdIO.o	\
	Relations.o	\
	SetsC.o	\
	Texts.o	\
	Lists.o	\
	Heap.o	\
	$(CLIBS)

MemoryDrv	: $(SYSTEM)	\
	MemoryDrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
;	$(CC) $(CFLAGS) -o MemoryDrv $(SYSTEM)	\
	MemoryDrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	$(CLIBS)

RelDrv	: $(SYSTEM)	\
	RelDrv.o	\
	Relations.o	\
	Sets.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	DynArray.o	\
;	$(CC) $(CFLAGS) -o RelDrv $(SYSTEM)	\
	RelDrv.o	\
	Relations.o	\
	Sets.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	DynArray.o	\
	$(CLIBS)

SetsDrv	: $(SYSTEM)	\
	SetsDrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	Sets.o	\
	DynArray.o	\
;	$(CC) $(CFLAGS) -o SetsDrv $(SYSTEM)	\
	SetsDrv.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	Sets.o	\
	DynArray.o	\
	$(CLIBS)

SysIODrv	: $(SYSTEM)	\
	SysIODrv.o	\
	Checks.o	\
	IO.o	\
	Memory.o	\
	General.o	\
;	$(CC) $(CFLAGS) -o SysIODrv $(SYSTEM)	\
	SysIODrv.o	\
	Checks.o	\
	IO.o	\
	Memory.o	\
	General.o	\
	$(CLIBS)

# TAIL

clean	:
	rm -f core *.o

CLEAN	: clean
	rm -f libreuse.a

cleanc	:
	rm -f Main.o DynArrDrv IODrv Main MemoryDrv RelDrv SetsDrv SysIODrv

.SUFFIXES: .md .mi .h .c .o

.mi.c:
	mtc $(M2FLAGS) $*.mi;

.md.h:
	mtc $(M2FLAGS) $*.md;

.c.o:
	$(CC) $(CFLAGS) -c $*.c
