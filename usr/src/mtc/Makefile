SRC	=.
INST	=/usr/local/lib/mtc

all :

	sh -c " if test ! -f swapped ; then \
	 	PATH=${PATH}:$(SRC)/mtc/m2c ;\
		make -C $(SRC)/mtc/m2c   bin.conv;\
		make -C $(SRC)/mtc/make  bin.conv;\
		touch swapped; else true; fi"
	make -C $(SRC)/reuse/m2c
	make -C $(SRC)/mtc/m2c    TARGET=SUN BIN=/usr/bin LIB=$(INST)
	make -C $(SRC)/mtc/make  -f MakefileC GetImports BIN=/usr/bin LIB=$(INST)


install:

	sh -c "if test ! -d $(INST) ; then mkdir $(INST) ; else true; fi"
	sh -c "if test ! -d $(INST)/lib; then mkdir $(INST)/lib; else true; fi"
	sh -c "if test ! -d $(INST)/include; then mkdir $(INST)/include; else true; fi"
	sh -c "if test ! -d $(INST)/src; then mkdir $(INST)/src; else true; fi"

	make -C $(SRC)/mtc/m2c   install TARGET=SUN BIN=/usr/bin LIB=$(INST)
	make -C $(SRC)/mtc/make  -f MakefileC GetImports install BIN=/usr/bin LIB=$(INST)

	install -m 644 $(SRC)/mm_linux.awk 	$(INST)/makemake/makemake.awk
	install -m 644 $(SRC)/reuse/m2c/libreuse.a 	$(INST)/lib
	install -m 644 $(SRC)/reuse/m2c/*.h 	$(INST)/include
	install -m 644 $(SRC)/reuse/src/*.m[di]     $(INST)/src
	install -m 644 $(SRC)/COPYRIGHT	$(INST)
	install -m 644 $(SRC)/README    $(INST)
