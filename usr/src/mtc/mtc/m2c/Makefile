BIN	= $(HOME)/bin
LIB	= $(HOME)/lib
OPT	= -g
CC	= cc

                # C library (reuse)
REUSE	= ../../reuse/m2c

                # C library
CLIBS	= $(REUSE)/libreuse.a

                # options for cc
CFLAGS	= -w -I$(REUSE) $(OPT)

                # options for mtc
M2FLAGS	= -c

                # "runtime system"
RTS	= SYSTEM_.h SYSTEM_.c Arguments.h Arguments.c

all	:

install:	mtc mtc.sh $(BIN) $(LIB)/mtc
	if test $(LIB) = $(BIN); then echo error: BIN = LIB; false; else true; fi
	sed 's;LIB;$(LIB);g' < mtc.sh > $(BIN)/mtc
	chmod a+rx $(BIN)/mtc
	install -c -s -m 755 mtc $(LIB)/mtc
	install -c -m 644 Scanner.Tab $(LIB)/mtc
	install -c -m 644 Parser.Tab $(LIB)/mtc
	cp $(RTS) $(LIB)/mtc
	chmod a+r $(LIB)/mtc/[AES]*.[hc]

# installation directories

$(BIN):
	sh -c "if test ! -d $(BIN); then mkdir $(BIN); else true; fi"

$(LIB):
	sh -c "if test ! -d $(LIB); then mkdir $(LIB); else true; fi"

$(LIB)/mtc:	$(LIB)
	sh -c "if test ! -d $(LIB)/mtc; then mkdir $(LIB)/mtc; else true; fi"

# swap all byte pairs of the file Scanner.Tab
# reverse all BITSETs (long words) of the file Parser.Tab

bin.conv:	convert
	dd conv=swab < Scanner.Tab > .Scanner.Tab; mv .Scanner.Tab Scanner.Tab
	dd conv=swab < Parser.Tab | convert > .Parser.Tab; mv .Parser.Tab Parser.Tab

convert:	convert.c
	$(CC) -o convert convert.c


# HEAD

sources	: AssocTab.h

sources	: AssocTab.c
AssocTab.o	: AssocTab.c AssocTab.h

sources	: Base.h

sources	: Base.c
Base.o	: Base.c Base.h

sources	: Code.h
Code.h	: Tree.h

sources	: Code.c
Code.o	: Code.c Code.h
Code.o	: Tree.h
Code.o	: Base.h
Code.o	: Tree.h
Code.o	: Defs.h
Code.o	: Values.h
Code.o	: Types.h
Code.o	: AssocTab.h
Code.o	: GenIdents.h
Code.o	: Errors.h
Code.o	: GenIdents.h
Code.o	: Defs.h
Code.o	: Values.h

sources	: DefMods.h
DefMods.h	: Tree.h

sources	: DefMods.c
DefMods.o	: DefMods.c DefMods.h
DefMods.o	: Base.h
DefMods.o	: Scanner.h
DefMods.o	: Parser.h
DefMods.o	: Errors.h
DefMods.o	: Positions.h
DefMods.o	: Tree.h
DefMods.o	: Defs.h
DefMods.o	: AssocTab.h

sources	: Defs.h
Defs.h	: Values.h

sources	: Defs.c
Defs.o	: Defs.c Defs.h
Defs.o	: Positions.h
Defs.o	: Values.h
Defs.o	: Values.h

sources	: Errors.h
Errors.h	: Positions.h

sources	: Errors.c
Errors.o	: Errors.c Errors.h
Errors.o	: Scanner.h
Errors.o	: Positions.h

sources	: GenIdents.h

sources	: GenIdents.c
GenIdents.o	: GenIdents.c GenIdents.h

sources	: Parser.h
Parser.h	: Tree.h

sources	: Parser.c
Parser.o	: Parser.c Parser.h
Parser.o	: Positions.h
Parser.o	: Errors.h
Parser.o	: Scanner.h
Parser.o	: Scanner.h
Parser.o	: Positions.h
Parser.o	: Tree.h
Parser.o	: Defs.h

sources	: Positions.h

sources	: Positions.c
Positions.o	: Positions.c Positions.h

sources	: Scanner.h
Scanner.h	: Tokens.h
Scanner.h	: Positions.h

sources	: Scanner.c
Scanner.o	: Scanner.c Scanner.h
Scanner.o	: Source.h
Scanner.o	: Errors.h
Scanner.o	: Tokens.h

sources	: Semantics.h
Semantics.h	: Tree.h

sources	: Semantics.c
Semantics.o	: Semantics.c Semantics.h
Semantics.o	: Tree.h
Semantics.o	: Tree.h
Semantics.o	: UniqueIds.h
Semantics.o	: Values.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: GenIdents.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Types.h
Semantics.o	: Defs.h
Semantics.o	: Values.h
Semantics.o	: Defs.h
Semantics.o	: UniqueIds.h
Semantics.o	: GenIdents.h
Semantics.o	: Errors.h
Semantics.o	: Errors.h
Semantics.o	: Defs.h
Semantics.o	: GenIdents.h
Semantics.o	: Defs.h
Semantics.o	: Tree.h
Semantics.o	: Code.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: Defs.h
Semantics.o	: GenIdents.h
Semantics.o	: Defs.h

sources	: Source.h

sources	: Source.c
Source.o	: Source.c Source.h
Source.o	: Scanner.h

sources	: Tokens.h

sources	: Tokens.c
Tokens.o	: Tokens.c Tokens.h

sources	: Tree.h
Tree.h	: Positions.h
Tree.h	: Defs.h
Tree.h	: Values.h
Tree.h	: UniqueIds.h

sources	: Tree.c
Tree.o	: Tree.c Tree.h
Tree.o	: Positions.h
Tree.o	: Positions.h

sources	: Types.h
Types.h	: Values.h
Types.h	: Defs.h

sources	: Types.c
Types.o	: Types.c Types.h
Types.o	: Base.h
Types.o	: Tree.h
Types.o	: Defs.h
Types.o	: Values.h
Types.o	: Code.h

sources	: UniqueIds.h

sources	: UniqueIds.c
UniqueIds.o	: UniqueIds.c UniqueIds.h
UniqueIds.o	: Errors.h

sources	: Values.h

sources	: Values.c
Values.o	: Values.c Values.h
Values.o	: Tree.h
Values.o	: Defs.h
Values.o	: Types.h
Values.o	: Defs.h
Values.o	: Tree.h

sources	: mtc.c
all	: mtc
mtc.o	: mtc.c
mtc.o	: Base.h
mtc.o	: Scanner.h
mtc.o	: Parser.h
mtc.o	: Tree.h
mtc.o	: DefMods.h
mtc.o	: Defs.h
mtc.o	: Semantics.h
mtc.o	: Code.h
mtc.o	: Errors.h
mtc.o	: Tree.h
mtc.o	: Defs.h
mtc.o	: Code.h

SYSTEM_.o	: SYSTEM_.c SYSTEM_.h

mtc	: $(SYSTEM)	\
	mtc.o	\
	Code.o	\
	Values.o	\
	Tree.o	\
	Positions.o	\
	UniqueIds.o	\
	Errors.o	\
	Scanner.o	\
	Tokens.o	\
	Source.o	\
	Defs.o	\
	Types.o	\
	Base.o	\
	GenIdents.o	\
	AssocTab.o	\
	Semantics.o	\
	DefMods.o	\
	Parser.o	\
;	$(CC) $(CFLAGS) -o mtc $(SYSTEM)	\
	mtc.o	\
	Code.o	\
	Values.o	\
	Tree.o	\
	Positions.o	\
	UniqueIds.o	\
	Errors.o	\
	Scanner.o	\
	Tokens.o	\
	Source.o	\
	Defs.o	\
	Types.o	\
	Base.o	\
	GenIdents.o	\
	AssocTab.o	\
	Semantics.o	\
	DefMods.o	\
	Parser.o	\
	$(CLIBS)

# TAIL

clean	:
	rm -f core *.o convert

.SUFFIXES: .md .mi .h .c .o

.mi.c:
	mtc $(M2FLAGS) $*.mi;

.md.h:
	mtc $(M2FLAGS) $*.md;

.c.o:
	$(CC) $(CFLAGS) -c $*.c
