include ../../Configfile

TARGETS=	mgrload hpmgr icon ico ify misc plot sh text2font texmgr

all:
		for i in $(TARGETS) $(GROPBM) $(MOVIECLIENTS) ; do (cd $$i; make all) done

install:
		for i in $(TARGETS) $(GROPBM) $(MOVIECLIENTS) ; do (cd $$i; make install) done

depend:
		for i in mgrload icon $(GROPBM) tests; do (cd $$i; make depend) done

clean:
		for i in $(TARGETS) gropbm play_data tests; do (cd $$i; make clean) done

clobber:
		for i in $(TARGETS) gropbm play_data tests; do (cd $$i; make clobber) done
