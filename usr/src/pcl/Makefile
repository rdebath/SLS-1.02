INSTALL = /bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
lisplibdir = $(libdir)/lisp


ALLLSP = pkg.lsp walk.lsp iterate.lsp macros.lsp low.lsp \
         clisp-low.lsp \
         fin.lsp defclass.lsp defs.lsp fngen.lsp lap.lsp plap.lsp cache.lsp \
         dlap.lsp boot.lsp defgenerics.lsp vector.lsp slots.lsp init.lsp \
         std-class.lsp cpl.lsp braid.lsp fsc.lsp methods.lsp combin.lsp \
         dfun.lsp precom1.lsp precom2.lsp fixup.lsp defcombin.lsp \
         ctypes.lsp construct.lsp env.lsp compat.lsp

ALLFAS = pkg.fas walk.fas iterate.fas macros.fas low.fas \
         clisp-low.fas \
         fin.fas defclass.fas defs.fas fngen.fas lap.fas plap.fas cache.fas \
         dlap.fas boot.fas defgenerics.fas vector.fas slots.fas init.fas \
         std-class.fas cpl.fas braid.fas fsc.fas methods.fas combin.fas \
         dfun.fas precom1.fas precom2.fas fixup.fas defcombin.fas \
         ctypes.fas construct.fas env.fas compat.fas


# Choose one of the packers:

# Standard Unix packer. Requires GNU tar.
PACK = tar
PACKOPT = cvfhz
PACKEXT = .tar.Z

# The one with the best performance.
#PACK = lha
#PACKOPT = a
#PACKEXT = .lzh

# Popular DOS packer.
#PACK = zip
#PACKOPT = -r
#PACKEXT = .zip

# Popular Atari packer.
#PACK = zoo
#PACKOPT = -add
#PACKEXT = .zoo

all: defsys.fas stamp.fas $(ALLFAS) pcl.mem

defsys.fas defsys.lib : defsys.lsp
	clisp -q -c defsys

$(ALLFAS) : stamp.fas

stamp.fas : $(ALLLSP) defsys.fas
	clisp -m 4MB -q -i defsys -x '(pcl::compile-pcl)'
	touch stamp.fas

pcl.mem : defsys.fas $(ALLFAS)
	clisp -m 4MB -q -i defsys -x '(pcl::load-pcl) (use-package "PCL") (saveinitmem)'
	mv lispinit.mem pcl.mem

install : pcl.mem force
	if [ ! -d $(libdir) ] ; then mkdir $(libdir) ; fi
	if [ ! -d $(lisplibdir) ] ; then mkdir $(lisplibdir) ; fi
	$(INSTALL_DATA) pcl.mem $(lisplibdir)/pcl.mem
	if [ ! -d $(bindir) ] ; then mkdir $(bindir) ; fi
	echo 'exec $(lisplibdir)/lisp.run -m 4MB -M $(lisplibdir)/pcl.mem "$$@"' > $(bindir)/clos
	chmod a+x $(bindir)/clos

distrib : src-distrib clisp-distrib

src-distrib : force
	$(PACK) $(PACKOPT) /tmp/pcl-src$(PACKEXT) README Makefile *.lsp *.txt

clisp-distrib : force pcl.mem
	$(PACK) $(PACKOPT) /tmp/pcl$(PACKEXT) README get-pcl.txt pcl.mem

clean: force
	-rm -f *.lib *.fas *.mem

force:
