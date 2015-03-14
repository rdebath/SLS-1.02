#
# This is Makefile for libgcc.a
#

include ../Makeconfig

srcdir=.

LIBGCC=../libgcc.a

LIBGCC1=libgcc1.a
LIBGCC2=libgcc2.a

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libgcc

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

INCLUDES = -I. -I$(srcdir) -I$(srcdir)/config
INC=$(INCLUDES)

CCLIBFLAGS= $(OPTFLAGS)
LIBGCC2_CFLAGS=$(OPTFLAGS) $(DEBUGFLAGS)


.SUFFIXES:
.SUFFIXES: .S .o .c .s .i

.s.o :
	$(CC) $(CFLAGS) -c $<

.S.o :
	$(CC) $(CFLAGS) -c $<

.c.o :
	$(CC) $(CFLAGS) -c $<
#	$(LD) $(LDFLAGS) $*.o
#	$(MV) a.out $*.o

.c.s :
	$(CC) $(CFLAGS) -S $<

.c.i :
	$(CC) $(CFLAGS) -E $< -o $*.i


ifeq "$(HOST_MACHINE)" "i386-att-sysv"

# Members of libgcc1.a.
LIB1FUNC1S = _mulsi3 _udivsi3 _divsi3 _umodsi3 _modsi3 \
	_lshrsi3 _lshlsi3 _ashrsi3 _ashlsi3 \
	_divdf3 _muldf3 _negdf2 _adddf3 _subdf3 \
	_fixdfsi _fixsfsi _floatsidf _floatsisf _truncdfsf2
LIB1FUNC2S=_extendsfdf2 _addsf3 _negsf2 _subsf3 _mulsf3 _divsf3 \
	_eqdf2 _nedf2 _gtdf2 _gedf2 _ltdf2 _ledf2 \
	_eqsf2 _nesf2 _gtsf2 _gesf2 _ltsf2 _lesf2

LIB1FUNC1S=
LIB1FUNC2S=

# Library members defined in libgcc2.c.

LIB2FUNC1S = _muldi3 _divdi3 _moddi3 _udivdi3 _umoddi3 _negdi2 \
	_lshrdi3 _lshldi3 _ashldi3 _ashrdi3  \
	_udiv_w_sdiv _udivmoddi4 _cmpdi2 _ucmpdi2 _floatdidf
LIB2FUNC2S=_floatdisf _fixunsdfsi _fixunssfsi _fixunsdfdi _fixdfdi \
	_fixunssfdi _fixsfdi __gcc_bcmp _varargs _eprintf \
	_builtin_new _caps_New _builtin_del \
	_bb _shtab _clear_cache _trampoline __main _exit _ctors

LIB2FUNC1S= 
LIB2FUNC2S= _muldi3 _udivdi3 _umoddi3 _udivmoddi4 \
	_caps_New _builtin_del _builtin_new _eprintf

else

# Members of libgcc1.a.
LIB1FUNCS = _mulsi3 _udivsi3 _divsi3 _umodsi3 _modsi3 \
	_lshrsi3 _lshlsi3 _ashrsi3 _ashlsi3 \
	_divdf3 _muldf3 _negdf2 _adddf3 _subdf3 \
	_fixdfsi _fixsfsi _floatsidf _floatsisf _truncdfsf2 \
	_extendsfdf2 _addsf3 _negsf2 _subsf3 _mulsf3 _divsf3 \
	_eqdf2 _nedf2 _gtdf2 _gedf2 _ltdf2 _ledf2 \
	_eqsf2 _nesf2 _gtsf2 _gesf2 _ltsf2 _lesf2

#LIB1FUNCS=

# Library members defined in libgcc2.c.

LIB2FUNCS = _muldi3 _divdi3 _moddi3 _udivdi3 _umoddi3 _negdi2 \
	_lshrdi3 _lshldi3 _ashldi3 _ashrdi3  \
	_udiv_w_sdiv _udivmoddi4 _cmpdi2 _ucmpdi2 _floatdidf \
	_floatdisf _fixunsdfsi _fixunssfsi _fixunsdfdi _fixdfdi \
	_fixunssfdi _fixsfdi __gcc_bcmp _varargs _eprintf \
	_builtin_new _caps_New _builtin_del \
	_bb _shtab _clear_cache _trampoline __main _exit _ctors

endif

SHAREDLIBGCC2OBJS= _muldi3.o _udivdi3.o _umoddi3.o _udivmoddi4.o \
	_caps_New.o _builtin_del.o _builtin_new.o _eprintf.o

all: sharedlibrary

ifeq "$(HOST_MACHINE)" "i386-att-sysv"

libgcc1.a: libgcc1.c
	-$(RM) -f tmplibgcc1.a
	-set -e; \
	mkdir tmpcopy; \
	for name in .. $(LIB1FUNC1S); \
	do \
	  if [ x$${name} != x.. ]; then \
	    echo $${name}; \
	    $(CC) $(CCLIBFLAGS) $(INCLUDES) -c -DL$${name} \
	    	$(srcdir)/libgcc1.c -o ./tmpcopy/$${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	  else true; fi; \
	done
	for name in .. $(LIB1FUNC2S); \
	do \
	  if [ x$${name} != x.. ]; then \
	    echo $${name}; \
	    $(CC) $(CCLIBFLAGS) $(INCLUDES) -c -DL$${name} \
	    	$(srcdir)/libgcc1.c -o ./tmpcopy/$${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	  else true; fi; \
	done
	(cd ./tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc1.a *.o)
	$(RM) -rf ./tmpcopy
	mv tmplibgcc1.a libgcc1.a

libgcc2.a: libgcc2.c 
	-rm -f tmplibgcc2.a
	set -e; \
	mkdir tmpcopy; \
	for name in .. $(LIB2FUNC1S); \
	do \
	  if [ x$${name} != x.. ]; then \
	    echo $${name}; \
	    $(CC) $(LIBGCC2_CFLAGS) $(INCLUDES) -c -DL$${name} \
		$(srcdir)/libgcc2.c -o ./tmpcopy/$${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	  else true; fi; \
	done
	for name in .. $(LIB2FUNC2S); \
	do \
	  if [ x$${name} != x.. ]; then \
	    echo $${name}; \
	    $(CC) $(LIBGCC2_CFLAGS) $(INCLUDES) -c -DL$${name} \
		$(srcdir)/libgcc2.c -o ./tmpcopy/$${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	  else true; fi; \
	done
	(cd ./tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc2.a *.o)
	$(RM) -rf ./tmpcopy
	mv tmplibgcc2.a libgcc2.a

else

libgcc1.a: libgcc1.c
	-$(RM) -f tmplibgcc1.a
# Actually build it in tmplibgcc1.a, then rename at end,
# so that libgcc1.a itself remains nonexistent if compilation is aborted.
# -e causes any failing command to make this rule fail.
# -e doesn't work in certain shells, so we test $$? as well.
	-set -e; \
	mkdir tmpcopy; \
	for name in .. $(LIB1FUNCS); \
	do \
	  if [ x$${name} != x.. ]; then \
	    echo $${name}; \
	    $(CC) $(CCLIBFLAGS) $(INCLUDES) -c -DL$${name} \
	    	$(srcdir)/libgcc1.c -o ./tmpcopy/$${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	  else true; fi; \
	done
	(cd ./tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc1.a *.o)
	$(RM) -rf ./tmpcopy
# Some shells crash when a loop has no items.
# So make sure there is always at least one--`..'.
# Then ignore it.
# We don't use -e here because there are if statements
# that should not make the command give up when the if condition is false.
# Instead, we test for failure after each command where it matters.
	-for file in .. $(LIB1FUNCS_EXTRA); \
	do \
	  if [ x$${file} != x.. ]; then \
	    name=`echo $${file} | sed -e 's/[.]c$$//' -e 's/[.]asm$$//'`; \
	    echo $${name}; \
	    if [ $${name}.asm = $${file} ]; then \
	      cp $${file} $${name}.s || exit 1; file=$${name}.s; \
	    else true; fi; \
	    $(CC) $(CCLIBFLAGS) $(INCLUDES) -c $${file}; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	    $(AR) $(AR_FLAGS) tmplibgcc1.a $${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	    rm -f $${name}.[so]; \
	  else true; \
	  fi; \
	done
	if [ -f tmplibgcc1.a ]; then \
	  mv tmplibgcc1.a libgcc1.a; \
	else true; fi

# Compiling libgcc2.a requires making sure that cc1, etc. have been compiled.
# But recompiling cc1 should not force recompilation of libgcc2.a.
# If you want to force recompilation, delete libgcc2.a.
# Depend on install-cross-tools to make sure we set up properly 
# to run the assembler before we try compiling anything.
# install-cross-tools does nothing if not cross compiling.
libgcc2.ready: $(GCC_PASSES) $(CROSS_TOOLS)
	-if [ -f libgcc2.ready ] ; then \
		true; \
	else \
		touch libgcc2.ready; \
	fi

libgcc2.a: libgcc2.c 
#libgcc2.ready $(CONFIG_H) $(LIB2FUNCS_EXTRA) \
#   machmode.h longlong.h gbl-ctors.h config.status
# Actually build it in tmplibgcc2.a, then rename at end,
# so that libgcc2.a itself remains nonexistent if compilation is aborted.
	-rm -f tmplibgcc2.a
# -e causes any failing command to make this rule fail.
# -e doesn't work in certain shells, so we test $$? as well.
	set -e; \
	mkdir tmpcopy; \
	for name in $(LIB2FUNCS); \
	do \
	  echo $${name}; \
	  $(CC) $(LIBGCC2_CFLAGS) $(INCLUDES) -c -DL$${name} \
	      $(srcdir)/libgcc2.c -o ./tmpcopy/$${name}.o; \
	  if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	done
	(cd ./tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc2.a *.o)
	$(RM) -rf ./tmpcopy
# Some shells crash when a loop has no items.
# So make sure there is always at least one--`..'.
# Then ignore it.
# We don't use -e here because there are if statements
# that should not make the command give up when the if condition is false.
# Instead, we test for failure after each command where it matters.
	-for file in .. $(LIB2FUNCS_EXTRA); \
	do \
	  if [ x$${file} != x.. ]; then \
	    name=`echo $${file} | sed -e 's/[.]c$$//' -e 's/[.]asm$$//'`; \
	    echo $${name}; \
	    if [ $${name}.asm = $${file} ]; then \
	      cp $${file} $${name}.s || exit 1; file=$${name}.s; \
	    else true; fi; \
	    $(CC) $(LIBGCC2_CFLAGS) $(INCLUDES) -c $${file}; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	    $(AR) $(AR_FLAGS) tmplibgcc2.a $${name}.o; \
	    rm -f $${name}.[so]; \
	  else true; \
	  fi; \
	done
	mv tmplibgcc2.a libgcc2.a
# These lines were deleted from above the mv command
# because ranlibing libgcc.a itself should suffice.
#	-if [ x${HPUX_GAS} = x ] ; then \
#	  if $(RANLIB_TEST) ; then $(RANLIB) tmplibgcc2.a; else true; fi \
#	else true; fi

endif

# Combine the various libraries into a single library, libgcc.a, for
# building shared library.
sharedlibrary: $(LIBGCC1) $(LIBGCC2)
	-$(RM) -rf tmplibgcc.a $(LIBGCC) tmpcopy
	mkdir tmpcopy
	(cd tmpcopy; $(AR) x ../$(LIBGCC2) $(SHAREDLIBGCC2OBJS))
	(cd tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc.a *.o)
	$(RM) -rf tmpcopy
	mv tmplibgcc.a $(LIBGCC)

# Combine the various libraries into a single library, libgcc.a.
library: $(LIBGCC1) $(LIBGCC2)
	-$(RM) -rf tmplibgcc.a $(LIBGCC) tmpcopy
	mkdir tmpcopy
	-if [ x$(LIBGCC1) != x ];			\
	then (cd tmpcopy; $(AR) x ../$(LIBGCC1));	\
	else true;					\
	fi
	(cd tmpcopy; $(AR) x ../$(LIBGCC2))
	(cd tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc.a *.o)
	$(RM) -rf tmpcopy
# Actually build it in tmplibgcc.a, then rename at end,
# so that libgcc.a itself remains nonexistent if compilation is aborted.
	mv tmplibgcc.a $(LIBGCC)

clean:
	$(RM) -fr core *.o *.a tmp_make foo libgcc2.ready tmpcopy
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

dep:
	$(CPP) -M *.c > .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
