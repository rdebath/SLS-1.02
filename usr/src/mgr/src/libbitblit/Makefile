include ../../Configfile

all:
		for i in $(BITBLIT) bitmap movie; do (cd $$i; make all); done
		rm -f libbitblit.a
		for i in $(BITBLIT) bitmap movie; do (cd $$i; ar qv ../libbitblit.a *.o); done

install:	all
		install libbitblit.a $(LIBDIR)
		ranlib $(LIBDIR)/libbitblit.a

depend:
		for i in bitmap test; do (cd $$i; make depend); done

clean:
		for i in $(BITBLIT) bitmap movie; do (cd $$i; make clean); done

clobber:
		for i in $(BITBLIT) bitmap movie test; do (cd $$i; make clobber); done
		rm -f libbitblit.a
