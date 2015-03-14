#
# Makefile for the Linux C library
#

include ./Makeconfig

# What we want to do by default.
TARGET=lib

# for cleanup
CLEANLIBS= libc.a libm.a libtermcap.a libcurses.a libg.a libc_p.a \
	libmcheck.a libdbm.a libcompat.a libgcc.a libgmon.a libbsd.a \
	libalias.a libmalias.a

CLEANDIRS= crt bsd cvt ctype curses dirent gdbm gmon grp inet io \
	iostream locale login malloc math misc mntent posix pwd regex \
	rpc setjmp signal stdlib string sysdeps termcap time ufc \
	jumptable1 jumptable2 compat gcc libbsd malloc-930326

ifdef DEBUG

LIBS:= $(REALLIBC) libmcheck.a

# for debug library
DIRS = bsd cvt ctype dirent grp inet io iostream locale login \
	malloc misc mntent posix pwd regex rpc setjmp signal stdlib \
	string sysdeps time ufc jumptable1 jumptable2

endif

ifdef PROFILE

# for profile library

LIBS:= $(REALLIBC) libgmon.a

DIRS = bsd crt cvt ctype dirent gmon grp inet io iostream locale \
	login malloc-930326 misc mntent posix pwd regex rpc setjmp \
	signal stdlib string sysdeps time ufc jumptable1 jumptable2

endif

ifdef STATIC

LIBS	= libc.a libm.a libtermcap.a libcurses.a \
	libdbm.a libbsd.a # libnet.a librpc.a

# for static library
DIRS = crt bsd cvt ctype curses dirent gdbm grp inet io iostream \
	locale login malloc-930326 math misc mntent posix pwd regex \
	rpc setjmp signal stdlib string sysdeps termcap time ufc \
	jumptable1 jumptable2 libbsd

endif

ifdef MATH

ifdef SHARED

LIBS	= libm.a libmalias.a
LIBMALIAS=yes

else

LIBS	= libm.a

endif

# for math library 
DIRS = math sysdeps jumptable1

JUMP_LIB=libm
JUMP_DIR:=./jump/$(SHLIB)

STUBNAMES=libm

SYSLIBS=-lc -lgcc

endif

ifdef SHARED

ifndef MATH

LIBS	= libc.a libtermcap.a libcurses.a \
	libdbm.a libcompat.a libgcc.a libalias.a

# for shared library 
DIRS = bsd cvt ctype curses dirent gdbm grp inet io \
	iostream locale login malloc-930326 misc mntent posix pwd regex \
	rpc setjmp signal stdlib string sysdeps termcap time ufc \
	jumptable1 jumptable2 compat gcc

SHAREDLIBGCC1OBJ1S = _mulsi3.o _udivsi3.o _divsi3.o _umodsi3.o \
	_modsi3.o _lshrsi3.o _lshlsi3.o _ashrsi3.o _ashlsi3.o

SHAREDLIBGCC1OBJ2S = _divdf3.o _muldf3.o _negdf2.o _adddf3.o _subdf3.o \
	_fixdfsi.o _fixsfsi.o _floatsidf.o _floatsisf.o _truncdfsf2.o

SHAREDLIBGCC1OBJ3S = _extendsfdf2.o _addsf3.o _negsf2.o _subsf3.o \
	_mulsf3.o _divsf3.o _eqdf2.o _nedf2.o _gtdf2.o _gedf2.o

SHAREDLIBGCC1OBJ4S = _ltdf2.o _ledf2.o _eqsf2.o _nesf2.o _gtsf2.o \
	_gesf2.o _ltsf2.o _lesf2.o

SHAREDLIBGCC2OBJ1S= _divdi3.o _moddi3.o _negdi2.o _lshrdi3.o \
	 _lshldi3.o _ashldi3.o _ashrdi3.o _udiv_w_sdiv.o

SHAREDLIBGCC2OBJ2S= _cmpdi2.o _ucmpdi2.o _floatdidf.o _floatdisf.o

SHAREDLIBGCC2OBJ3S= _fixunsdfsi.o _fixunssfsi.o _fixunsdfdi.o \
	_fixdfdi.o _fixunssfdi.o _fixsfdi.o __gcc_bcmp.o _varargs.o

SHAREDLIBGCC2OBJ4S= _bb.o _shtab.o _clear_cache.o _trampoline.o \
	__main.o _exit.o _ctors.o

JUMP_LIB=libc
JUMP_DIR:=./jump/$(SHLIB)

STUBNAMES=libc libcurses libgcc libtermcap libdbm

endif		# not math library.

STUBLIBS:= $(shell for l in $(STUBNAMES); do echo $$l.sa;done)

ifndef FIRST_PASS

JUMP_PARAMS=$(JUMP_DIR)/jump.params.build
SHLIB_NAME:= $(shell awk -F= ' { if ($$1 == "Name")  print $$2 }' $(JUMP_PARAMS))
SHLIB_TEXT:= $(shell awk -F= ' { if ($$1 == "Text")  print $$2 }' $(JUMP_PARAMS))
SHLIB_DATA:= $(shell awk -F= ' { if ($$1 == "Data")  print $$2 }' $(JUMP_PARAMS))
SHLIB_JUMP:= $(shell awk -F= ' { if ($$1 == "Jump")  print $$2 }' $(JUMP_PARAMS))
SHLIB_GOT:= $(shell awk -F= ' { if ($$1 == "GOT")  print $$2 }' $(JUMP_PARAMS))
SHLIB_VERSION:= $(shell awk -F= ' { if ($$1 == "Version")  print $$2 }' $(JUMP_PARAMS))

SHLIB_VERSION_MAJOR:=$(shell awk -F= ' { \
  if ($$1 == "Version") { \
    for (i = 1; i <= length ($$2); i++) { \
      if (substr ($$2, i, 1) == ".") { \
	print substr ($$2, 1, i - 1); break; \
      } } } } ' $(JUMP_PARAMS))

SHLIB_FILE:=$(shell basename $(SHLIB_NAME)).so.$(SHLIB_VERSION)

SHLIB_FILE_MAJOR:=$(shell basename $(SHLIB_NAME)).so.$(SHLIB_VERSION_MAJOR)

TARGET=shared

endif

endif

ifdef LITE

LIBS	= libc.a libtermcap.a libcompat.a libgcc.a libalias.a

# for light C library without curses, gdbm and gmon
DIRS = bsd cvt ctype dirent grp inet io iostream locale login \
	malloc-930326 misc mntent posix pwd regex rpc setjmp signal \
	stdlib string sysdeps termcap time ufc jumptable1 \
	jumptable2 compat gcc

TARGET=lite

endif

all: $(TARGET)

lib:
	for i in $(DIRS); do \
		echo making $$i; \
		(cd $$i; $(MAKE)); \
	done
	for l in $(LIBS); \
	do \
	  $(AR) -dv $$l  __.SYMDEF; \
	  $(REALRANLIB) -v $$l; \
	done

dep:
	for i in $(DIRS); do \
		echo making depend $$i; \
		(cd $$i; $(MAKE) dep); \
	done

clean:
	$(RM) -f $(CLEANLIBS) libalias.a core *.o *.s *.sa *.so.* \
		verify.out
	for i in $(CLEANDIRS); do \
	   echo cleaning $$i; \
	   (cd $$i; $(MAKE) clean); \
	done

lite: lib build.lite

shared: lib build.shared

build.lite: 
	$(MKIMAGE) -l $(SHLIB_NAME) -v $(SHLIB_VERSION) -a $(SHLIB_TEXT) \
		-d $(SHLIB_DATA) -j $(SHLIB_JUMP)  -g $(SHLIB_GOT) \
		-- $(LIBS)
	mv $(SHLIB_FILE) lib.so # do this trick for stupid fs
	$(STRIP) lib.so
	mv lib.so $(SHLIB_FILE)

build.shared:
	$(RM) -f $(STUBLIBS)
	$(MKIMAGE) -l $(SHLIB_NAME) -v $(SHLIB_VERSION) -a $(SHLIB_TEXT) \
		-d $(SHLIB_DATA) -j $(SHLIB_JUMP)  -g $(SHLIB_GOT) \
		-- $(LIBS) $(SYSLIBS)
	$(MKSTUBS) -l $(SHLIB_NAME) -v $(SHLIB_VERSION) -a $(SHLIB_TEXT) \
		-d $(SHLIB_DATA) -j $(SHLIB_JUMP)  -g $(SHLIB_GOT) \
		-- $(STUBNAMES)
	$(VERIFY) -l $(SHLIB_FILE) $(STUBLIBS)
	mv $(SHLIB_FILE) lib.so # do this trick for stupid fs
	$(STRIP) lib.so
	mv lib.so $(SHLIB_FILE)
	if [ x"$(SHAREDLIBGCC2OBJ2S)"x != xx ]; then \
	  if [ -d tmpcopy ]; then $(RM) -f ./tmpcopy/*; \
	  else mkdir tmpcopy; fi; \
	else true; fi
	if [ x"$(SHAREDLIBGCC1OBJ1S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC1OBJ1S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC1OBJ1S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC1OBJ2S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC1OBJ2S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC1OBJ2S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC1OBJ3S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC1OBJ3S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC1OBJ3S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC1OBJ4S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC1OBJ4S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC1OBJ4S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC2OBJ1S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC2OBJ1S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC2OBJ1S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC2OBJ2S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC2OBJ2S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC2OBJ2S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC2OBJ3S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC2OBJ3S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC2OBJ3S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC2OBJ4S)"x != xx ]; then \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libgcc.a $(SHAREDLIBGCC2OBJ4S)); \
	  (cd ./tmpcopy; \
	   $(AR) ucv ../libgcc.sa $(SHAREDLIBGCC2OBJ4S)); \
	else true; fi
	if [ x"$(SHAREDLIBGCC2OBJ2S)"x != xx ]; then \
	  $(RM) -fr tmpcopy/*.o; \
	  (cd ./tmpcopy; $(AR) -x ../libalias.a); \
	  (cd ./compat; $(MAKE) __old_libc.o; \
	   mv __old_libc.o ../tmpcopy); \
	  (cd ./tmpcopy; \
	   $(AR) -x $(TARGET_LIB_DIR)/libc.a __load.o); \
	  (cd ./tmpcopy; $(AR) ucv ../libc.sa *.o); \
	  $(RM) -fr tmpcopy; \
	else true; fi
	if [ x"$(LIBMALIAS)"x != xx ]; then \
	  if [ -d tmpcopy ]; then $(RM) -f ./tmpcopy/*; \
	  else mkdir tmpcopy; fi; \
	  (cd ./compat; $(MAKE) __old_libm.o; \
	   mv __old_libm.o ../tmpcopy); \
	  (cd ./tmpcopy; $(AR) -x ../libmalias.a); \
	  (cd ./tmpcopy; $(AR) ucv ../libm.sa *.o); \
	  $(RM) -fr tmpcopy; \
	else true; fi
	for l in $(STUBLIBS); do \
	  $(AR) -dv $$l __.SYMDEF; \
	  $(REALRANLIB) -v $$l; \
	done

install:
	@echo which version? debug, static, or profiling.

# install static library
install.static:
	(cd crt ; $(MAKE) install)
	if [ ! -d $(TARGET_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIB_DIR); \
	else true; fi
	cp $(LIBS) $(TARGET_LIB_DIR)

# install shared library
install.shared:
	$(MAKE) install.so SHARED=true

# install debug library
install.debug: libg.a
	if [ ! -d $(TARGET_LIBEXTRA_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIBEXTRA_DIR); \
	else true; fi
	cp libg.a $(TARGET_LIBEXTRA_DIR)

# install profiling library
install.profile: libc_p.a libgmon.a
	(cd crt ; $(MAKE) PROFILE=true install)
	if [ ! -d $(TARGET_LIBEXTRA_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIBEXTRA_DIR); \
	else true; fi
	cp libc_p.a libgmon.a $(TARGET_LIBEXTRA_DIR)

ifeq "$(HOST_MACHINE)" "linux"

install.so:
	if [ ! -d $(TARGET_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIB_DIR); \
	else true; fi
	cp $(STUBLIBS) $(TARGET_LIB_DIR)
	if [ ! -d $(TARGET_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_SO_DIR); \
	else true; fi
	if [ -f $(TARGET_SO_DIR)/$(SHLIB_FILE) ]; then \
	  (cd $(TARGET_SO_DIR); \
	   cp $(SHLIB_FILE) /tmp; \
	   if [ $$? -eq 0 ]; then \
	     ln -sf /tmp/$(SHLIB_FILE) $(SHLIB_FILE_MAJOR); \
	   else exit 1; fi; \
	   mv $(SHLIB_FILE) $(SHLIB_FILE_MAJOR).old); \
	else true; fi
	cp $(SHLIB_FILE) $(TARGET_SO_DIR)
	(cd $(TARGET_SO_DIR); \
	   ln -sf $(SHLIB_FILE) $(SHLIB_FILE_MAJOR))

else

install.so:
	if [ ! -d $(TARGET_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIB_DIR); \
	else true; fi
	cp $(STUBLIBS) $(TARGET_LIB_DIR)
	if [ ! -d $(TARGET_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_SO_DIR); \
	else true; fi
	cp $(SHLIB_FILE) $(TARGET_SO_DIR)

endif
