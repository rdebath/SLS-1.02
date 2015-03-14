LIB	= $(HOME)/lib
INCDIR	= $(LIB)/include
TARGET	= SUN
REUSE	= libreuse.a
OPT	= -O
UNIX	= UNIX
CFLAGS	= $(OPT) -D$(UNIX) -D$(TARGET)
CC	= cc
OBJS	= System.o General.o Memory.o DynArray.o StringMem.o Idents.o Sets.o Time.o \
	  Errors.o Positions.o Source.o

# targets

all:	$(TARGET)

PCS10 PCS20:	$(REUSE)

SUN:	$(REUSE)
	ranlib $(REUSE)

$(REUSE):	$(OBJS)
	ar uc $(REUSE) $(OBJS)
	-ranlib $(REUSE)

install:	$(TARGET)
	install -c -m 664 $(REUSE) $(LIB)
	-ranlib $(LIB)/$(REUSE)
	cp *.h $(INCDIR)
	chmod u+w,a+r $(INCDIR)/*.h

clean:
	rm -f *.o *Drv core

# test programs

test:	MemoryDrv DynArrDrv SetsDrv IdentsDrv StrMemDrv ErrorsDrv

MemoryDrv:	MemoryDrv.o $(REUSE)
	$(CC) -o MemoryDrv $(CFLAGS) MemoryDrv.o $(REUSE)

DynArrDrv:	DynArrDrv.o $(REUSE)
	$(CC) -o DynArrDrv $(CFLAGS) DynArrDrv.o $(REUSE)

SetsDrv:	SetsDrv.o $(REUSE)
	$(CC) -o SetsDrv $(CFLAGS) SetsDrv.o $(REUSE)

IdentsDrv:	IdentsDrv.o $(REUSE)
	$(CC) -o IdentsDrv $(CFLAGS) IdentsDrv.o $(REUSE)

StrMemDrv:	StrMemDrv.o $(REUSE)
	$(CC) -o StrMemDrv $(CFLAGS) StrMemDrv.o $(REUSE)

ErrorsDrv:	ErrorsDrv.o $(REUSE)
	$(CC) -o ErrorsDrv $(CFLAGS) ErrorsDrv.o $(REUSE)

lint:
	lint -u *.c

# dependencies

DynArray.o:	DynArray.h Memory.h
DynArrDrv.o:	DynArray.h
General.o:	General.h
Idents.o:	Idents.h StringMem.h DynArray.h
Memory.o:	Memory.h System.h General.h
MemoryDrv.o:	Memory.h
Sets.o:		Sets.h DynArray.h General.h
SetsDrv.o:	Sets.h
StringMem.o:	StringMem.h DynArray.h
Time.o:		Time.h
Positions.o:	Positions.h
Errors.o:	Errors.h Positions.h
ErrorsDrv.o:	Errors.h Positions.h Sets.h Idents.h
Source.o:	Source.h System.h

.SUFFIXES:	.c .o

.c.o:
	$(CC) $(CFLAGS) -c $*.c
