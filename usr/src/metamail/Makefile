# Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)
# 
# Permission to use, copy, modify, and distribute this material 
# for any purpose and without fee is hereby granted, provided 
# that the above copyright notice and this permission notice 
# appear in all copies, and that the name of Bellcore not be 
# used in advertising or publicity pertaining to this 
# material without the specific, prior written permission 
# of an authorized representative of Bellcore.  BELLCORE 
# MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
# OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
# WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
#

# For the ATOMICMAIL distribution, you want the second of the two following lines
# For the more freely-publishable without-ATOMICMAIL version, you want the first.
# ATOMICMAIL=
ATOMICMAIL=atomicmail

# System V users may change the following to remove the "-s" and all will still work.
LN = ln -s

# To shut rm up on SGI machines (where symlinks have mode 0000)
RM = rm -f

# You may also want to change the CFLAGS variable, which will be passed on 
# to all subdirectories.    If you do that, the changes will only work right if you
# start your make in the top-level directory, not if you start in a subdirectory.

# Note that the FOOBAR hack is to get -Bstatic on sun4's but nowhere else.
#It is very ugly, but what can you do?
FOOBAR-sun4=-Bstatic

STATICFLAG=FOOBAR${HOST_ARCH}

CFLAGS = -g -I.  ${$(STATICFLAG)} 
# The following is better if you want to make sure you run with SYSV defined
# CFLAGS = -g -I.  ${$(STATICFLAG)}  -DSYSV

# You may have to include other libraries to the link to get things like
# gethostname() resolved or use shared libraries.  This is done with the
# LDLIBS variable.
#
# For Sun and BSD systems, the following should work...
LDLIBS =
# On SGI machines, we need -lsun for getpw...(), and -lc_s saves some space.
# LDLIBS = -lsun -lc_s
# ISC SysVr3.2.2 has a shared C library and requires libinet.a to resolve
# gethostname() references.
# LDLIBS = -linet -lc_s

#
# This is where config.h lives
CONFIGDIR = .

install: basics ${ATOMICMAIL} 

atomicmail::
	(cd atomicmail ; make CONFIGDIR=.. CFLAGS="${CFLAGS}" LDLIBS="${LDLIBS}")
	-${RM} bin/atomicmail
	${LN} `pwd`/atomicmail/src/atomicmail bin/atomicmail
	-${RM} bin/docdist.am
	${LN} `pwd`/atomicmail/apps/docdist.am bin/docdist.am
	-${RM} bin/mrwizard-seed.am
	${LN} `pwd`/atomicmail/apps/mrwizard-seed.am bin/mrwizard-seed.am
	-${RM} bin/mrwizard.am
	${LN} `pwd`/atomicmail/apps/mrwizard.am bin/mrwizard.am
	-${RM} bin/nametune.am
	${LN} `pwd`/atomicmail/apps/nametune.am bin/nametune.am
	-${RM} bin/scheduler.am
	${LN} `pwd`/atomicmail/apps/scheduler.am bin/scheduler.am
	-${RM} bin/surveyor.am
	${LN} `pwd`/atomicmail/apps/surveyor.am bin/surveyor.am


basics::
	(cd metamail ; make  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" LDLIBS="${LDLIBS}")
	-${RM} bin/metamail
	${LN} `pwd`/metamail/metamail bin/metamail
	-${RM} bin/mmencode
	${LN} `pwd`/metamail/mmencode bin/mmencode
	-${RM} bin/mailto
	${LN} `pwd`/metamail/mailto bin/mailto
	-${RM} bin/splitmail
	${LN} `pwd`/metamail/splitmail bin/splitmail
	(cd richmail ; make  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" LDLIBS="${LDLIBS}")
	-${RM} bin/richtext
	${LN} `pwd`/richmail/richtext bin/richtext
	-${RM} bin/richtoatk
	${LN} `pwd`/richmail/richtoatk bin/richtoatk
	(cd fonts ; make  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" LDLIBS="${LDLIBS}")
	-${RM} bin/shownonascii
	${LN} `pwd`/fonts/shownonascii bin/shownonascii
	-${RM} bin/mailto-hebrew
	${LN} `pwd`/fonts/mailto-hebrew bin/mailto-hebrew
	(cd ezview ; make  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" LDLIBS="${LDLIBS}")
	-${RM} bin/ezview
	${LN} `pwd`/ezview/ezview bin/ezview

clean:
	-${RM} -rf *.BAK
	(cd metamail ; make clean)
	(cd ezview ; make clean)
	(cd richmail ; make clean)
	(cd fonts ; make clean)
	-(cd atomicmail ; make clean)

