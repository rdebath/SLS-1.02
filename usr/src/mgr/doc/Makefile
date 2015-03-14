include ../Configfile

DOCUMENT=	doc.1 doc.2 doc.3 doc.4 doc.5 doc.6
CRFLAGS=	-k '' '' -c '\fI' '\fP'

all:		manual.out

install:	all

manual.out:	abstract.out body.out
		cat abstract.out body.out > manual.out

body.out:	macros $(DOCUMENT) tocindex
		$(TBL) macros $(DOCUMENT) tocindex | croff/croff $(CRFLAGS) | $(ROFF) -ms > body.out

abstract.out:	macros abstract.ms
		$(ROFF) -ms $(ROFFLAGS) macros abstract.ms > abstract.out

tocindex:	macros $(DOCUMENT) croff/croff
		$(TBL) macros $(DOCUMENT) | croff/croff $(CRFLAGS) | $(ROFF) -ms -rI1 $(ROFFLAGS) >/dev/null 2> tmpindex
		grep '^\.Fc' tmpindex | sort +2 -n > tocindex
		grep '^[a-z]' tmpindex | sort +0 -1 -d +1 -2 -n +2 -3 -r | $(AWK) -f index.awk >> tocindex
		rm -f tmpindex

croff/croff:
		cd croff; make install

clean:
		rm -f tocindex tmpindex abstract.out body.out
		cd croff; make clean

clobber:	clean
		rm -f manual.out
		cd croff; make clobber
