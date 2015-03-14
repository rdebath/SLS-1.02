TROFF = groff -Tps
NROFF = groff -Tascii
OPTS = -p -t -e -ms

ROFF_FILES = rpc.prog.ms rpcgen.ms xdr.nts.ms

all:
	@echo "Use make all.nroff or all.troff explicitly if you have groff"

all.nroff: $(MSFILES)
	$(NROFF) $(OPTS) $(ROFF_FILES) >all.nroff

all.troff: $(MSFILES)
	$(TROFF) $(OPTS) $(ROFF_FILES) >all.troff

.SUFFIXES: .ms .nroff .troff

.ms.nroff:
	$(NROFF) $(OPTS) $< >$@

.ms.troff:
	$(TROFF) $(OPTS) $< >$@

clean:
	rm -f *.nroff *.troff

clobber: clean

