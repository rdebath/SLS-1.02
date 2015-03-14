BIN	= $(HOME)/bin
LIB	= $(HOME)/lib

SRCM	= Errors.md Errors.mi GetImports.mi
GEN	= Parser.md Parser.mi Scanner.md Scanner.mi

GetImports:	$(SRCM) $(GEN)
	echo p GetImports | mc -d ../../reuse/src -noindex -norange

install:	GetImports makemake $(BIN) $(LIB)/makemake
	if test $(LIB) = $(BIN); then echo error: BIN = LIB; false; else true; fi
	sed 's;^LIB=.*;LIB=$(LIB)/makemake;' < makemake > $(BIN)/makemake
	chmod a+rx $(BIN)/makemake
	install -c -s -m 755 GetImports $(LIB)/makemake
	install -c -m 644 Scanner.Tab $(LIB)/makemake
	install -c -m 644 Parser.Tab $(LIB)/makemake
	install -c -m 644 makemake.awk $(LIB)/makemake

# installation directories

$(BIN):
	sh -c "if test ! -d $(BIN); then mkdir $(BIN); else true; fi"

$(LIB):
	sh -c "if test ! -d $(LIB); then mkdir $(LIB); else true; fi"

$(LIB)/makemake:	$(LIB)
	sh -c "if test ! -d $(LIB)/makemake; then mkdir $(LIB)/makemake; else true; fi"

# swap all byte pairs of the file Scanner.Tab
# reverse all BITSETs (long words) of the file Parser.Tab

bin.conv:	convert
	dd conv=swab < Scanner.Tab > .Scanner.Tab; mv .Scanner.Tab Scanner.Tab
	dd conv=swab < Parser.Tab | convert > .Parser.Tab; mv .Parser.Tab Parser.Tab

convert:	convert.o
	cc -o convert convert.o

Scanner.md Scanner.mi:	modula.rex
	rex -d modula.rex;

Parser.md Parser.mi:	modula.ell
	ell -di modula.ell;

m2c:	$(SRCM) $(GEN)
	make -f MakefileC sources MFLAGS=-d../../reuse/src

# support for mkid

modId:	modula.rex
	echo SCANNER modId | cat - modula.rex | \
	sed 's/\(RETURN.*Ident\)/yyEcho; IO.WriteNl (IO.StdOutput); \1/' | \
	$(BIN).t/rex -sd;
	sed 's/@/modId/' < ../../front/src/Id.mi > modIdDrv.mi
	echo p modIdDrv | mc -d ../../reuse/src
	mv modIdDrv modId

installId:	modId $(LIB)/Id
	echo exec $(LIB)/Id/modId -l$(LIB)/Id "2> /dev/null" > $(BIN)/modId
	chmod a+rx $(BIN)/modId
	install -c -m 644 modId.Tab $(LIB)/Id
	install -c -s -m 755 modId $(LIB)/Id

$(LIB)/Id:	$(LIB)
	sh -c "if test ! -d $(LIB)/Id; then mkdir $(LIB)/Id; else true; fi"

cleanId:
	rm -f modId*

clean:	cleanId
	rm -f *.[dimor] core ERRORS LISTING convert *.bak

CLEAN:	clean
	rm -f [A-Z]*.[hc] Parser.m? Scanner.m? GetImports Scanner.Tab Parser.Tab
