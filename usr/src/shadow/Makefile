#
# Copyright 1988,1989,1990,1991,1992 John F. Haugh II
# All rights reserved.
#
# Permission is granted to copy and create derivative works for any
# non-commercial purpose, provided this copyright notice is preserved
# in all copies of source code, or included in human readable form
# and conspicuously displayed on all copies of object code or
# distribution media.
#
# This software is provided on an AS-IS basis and the author makes
# no warrantee of any kind.
#
#	@(#)Makefile	3.25.1.13	08:23:37  - Shadow password system
#
#	@(#)Makefile	3.25.1.13	08:23:37	10/1/92
#
SHELL = /bin/sh

#
# Set this flag to decide what level of code "get" returns.
# The base USENET release was release 1.  It is no longer supported.
# The version with the utilities added was release 2.
# The version with database-like file access is release 3.
RELEASE = 3
GFLAGS = -t -r$(RELEASE)

# Define the directory login is copied to.  BE VERY CAREFUL!!!  BSD old SunOS
# seems to use /bin, USG seems to use /etc, SunOS 4.1.1 seems to use /usr/bin.
# If you define SCOLOGIN, you MUST use /etc as LOGINDIR.
LOGINDIR = /bin
# LOGINDIR = /etc
# LOGINDIR = /usr/bin

# Define any special libraries required to access the directory routines.
# Some systems require -lndir for the directory routines.  SCO Xenix uses
# -lx for that.  Your system might need nothing.
# NDIR = -lndir
# NDIR = -lx
NDIR =

LIBSHADDEP=libshadow.a
# Pick your favorite C compiler and tags command
CC = gcc
TAGS = ctags

# OS.  Pick one of USG (AT&T, SYSV, SYS3), BSD, SUN (SunOS 2 and 3),
# SUN4 (SunOS 4.1.1.), or UNIXPC (AT&T PC/7300, 3B1)
# OS = -DUSG -DSYS3
OS = -DUSG
# OS = -DBSD
# OS = -DSUN
# OS = -DSUN4
# OS = -DUSG -DUNIXPC

# Do you have to do ranlib (probably SUN, BSD and XENIX)?
RANLIB = ranlib
# RANLIB = echo

# Enable the following if you are running SCO TCP/IP.  It is a /bin/login
# which understands the *ahem* novel way they do rlogin/telnet.
# SCOLOGIN = scologin

# Configuration Flags
#
#	DEST_INCLUDE_DIR - local include files
#	LIBS - system libraries
#		-lsocket - needed for TCP/IP and possibly SYSLOG
#		-ldbm or -lndbm - needed for DBM support
#		-lcrypt - needed for SCO crypt() functions
#	CFLAGS - C compiler flags
#		-DLAI_TCP - needed for SCO Xenix Lachman TCP/IP

DEST_INCLUDE_DIR = /usr/include

# Flags for SCO Xenix/386
# CFLAGS = -O -M3 -g $(OS) -I$(DEST_INCLUDE_DIR)
# LIBS = -lcrypt -lndbm
# LIBS = -lcrypt -ldbm
LIBS =
LDFLAGS = -s -N
LTFLAGS = 

# Flags for normal machines
CFLAGS = -O6 $(OS) -I$(DEST_INCLUDE_DIR)
# LIBS =
# LDFLAGS = -g

# Flags for SunOS 4.1.1
# CFLAGS = -O2 $(OS) -I$(DEST_INCLUDE_DIR)
# LIBS =
# LDFLAGS = 

# This should be Slibsec.a for small model, or Llibsec.a for
# large model or whatever.  MUST AGREE WITH CFLAGS!!!  For non-Intel
# machines, just use libsec.a
# LIBSEC = Slibsec.a
LIBSEC = libsec.a

# Names for root user and group, and bin user and group.  See your
# /etc/passwd and /etc/group files.  BSD and SUN use "wheel", most
# others use "root" for RGID.
RUID = root
RGID = root
# RGID = wheel
BUID = bin
BGID = bin

# Where the login.defs file will be copied.  Must agree with config.h
DEST_LOGIN_DEFS = /etc/login.defs

# Rules for .L (lint) files.
.SUFFIXES: .L
LINT = lint
LINTFLAGS = $(OS) -Dlint

.c.L:
	$(LINT) -pxu $(LINTFLAGS) $*.c > $*.L

LOBJS = lmain.o login.o env.o valid.o setup.o shell.o age.o \
	utmp.o sub.o mail.o motd.o log.o ttytype.o failure.o \
	tz.o console.o hushed.o 

LSRCS = lmain.c login.c env.c valid.c setup.c shell.c age.c \
	utmp.c sub.c mail.c motd.c log.c ttytype.c failure.c \
	tz.c console.c hushed.c 

SOBJS = smain.o env.o entry.o susetup.o shell.o \
	sub.o mail.o motd.o sulog.o age.o tz.o hushed.o 

SSRCS = smain.c env.c entry.c setup.c shell.c \
	pwent.c sub.c mail.c motd.c sulog.c shadow.c age.c pwpack.c rad64.c \
	tz.c hushed.c 

POBJS = passwd.o obscure.o
PSRCS = passwd.c obscure.c

GPSRCS = gpmain.c

GPOBJS = gpmain.o

PWOBJS = pwconv.o

PWSRCS = pwconv.c pwent.c shadow.c pwpack.c rad64.c

PWUNOBJS = pwunconv.o

PWUNSRCS = pwunconv.c pwent.c shadow.c pwpack.c rad64.c

SULOGOBJS = sulogin.o entry.o env.o age.o setup.o \
	valid.o shell.o tz.o

SULOGSRCS = sulogin.c entry.c env.c age.c pwent.c setup.c \
	shadow.c shell.c valid.c pwpack.c tz.c

MKPWDOBJS = mkpasswd.o

MKPWDSRCS = mkpasswd.c

NGSRCS = newgrp.c env.c shell.c

NGOBJS = newgrp.o env.o shell.o

CHFNSRCS = chfn.c fields.c
CHFNOBJS = chfn.o fields.o
CHSHSRCS = chsh.c fields.c
CHSHOBJS = chsh.o fields.o
CHAGEOBJS = chage.o fields.o
CHAGESRCS = chage.c fields.c
CHPASSOBJS = chpasswd.o
CHPASSSRCS = chpasswd.c
DPSRCS = dpmain.c
DPOBJS = dpmain.o

ALLSRCS = age.c dialchk.c dialup.c entry.c env.c lmain.c log.c login.c mail.c \
	motd.c obscure.c passwd.c pwconv.c pwent.c pwunconv.c getpass.c \
	setup.c shadow.c shell.c smain.c sub.c sulog.c sulogin.c ttytype.c \
	utmp.c valid.c port.c newgrp.c gpmain.c grent.c mkpasswd.c pwpack.c \
	chfn.c chsh.c chage.c rad64.c encrypt.c chpasswd.c shadowio.c pwio.c \
	newusers.c groupio.c fields.c pwdbm.c grpack.c grdbm.c sppack.c \
	spdbm.c dpmain.c gshadow.c gsdbm.c gspack.c sgroupio.c useradd.c \
	userdel.c patchlevel.h usermod.c copydir.c mkrmdir.c groupadd.c \
	groupdel.c groupmod.c tz.c console.c hushed.c getdef.c scologin.c \
	logoutd.c groups.c pwauth.c 

#FILES1 = README patchlevel.h newgrp.c Makefile config.h pwunconv.c obscure.c \
FILES1 = README patchlevel.h newgrp.c config.h pwunconv.c obscure.c \
	age.c id.c

FILES2 = passwd.c port.c lmain.c sulogin.c pwpack.c dialup.c

FILES3 = chfn.c chsh.c smain.c faillog.c pwconv.c shadow.c

FILES4 = gpmain.c chage.c pwent.c valid.c setup.c entry.c ttytype.c port.h

FILES5 = pwio.c encrypt.c chpasswd.c newusers.c rad64.c dialchk.c faillog.h \
	pwdbm.c grdbm.c gshadow.c sppack.c

FILES6 = gspack.c spdbm.c lastlog.h shell.c login.c sub.c dpmain.c mail.c \
	env.c pwd.h.m4 grpack.c shadow.h log.c grent.c motd.c dialup.h \
	fields.c gsdbm.c utmp.c failure.c

FILES7 = groupio.c shadowio.c sgroupio.c groups.c copydir.c mkrmdir.c \
	mkpasswd.c pwauth.c pwauth.h

FILES8 = useradd.c usermod.c login.defs

FILES9 = groupadd.c groupdel.c groupmod.c tz.c console.c hushed.c getdef.c \
	scologin.c logoutd.c sulog.c getpass.c userdel.c

FILES_SUN4 = Makefile.sun4 README.sun4 config.h.sun4
FILES_SVR4 = Makefile.svr4 config.h.svr4

MAN_1 = chage.1 chfn.1 chsh.1 id.1 login.1 newgrp.1 passwd.1 su.1 \
	useradd.1 userdel.1 usermod.1 groupadd.1 groupdel.1 groupmod.1 \
	groups.1
MAN_3 = shadow.3 pwauth.3
MAN_4 = faillog.4 passwd.4 porttime.4 shadow.4
MAN_5 = login.5
MAN_8 = chpasswd.8 dpasswd.8 faillog.8 newusers.8 pwconv.8 pwunconv.8 \
	sulogin.8 mkpasswd.8 logoutd.8 pwauth.8

DOCS1 = $(MAN_1) $(MAN_3) $(MAN_4)
DOCS2 = $(MAN_5) $(MAN_8)
DOCS = $(DOCS1) $(DOCS2)

BINS = su login pwconv pwunconv passwd sulogin faillog newgrp sg gpasswd \
	mkpasswd chfn chsh chage chpasswd newusers dpasswd id useradd \
	userdel usermod groupadd groupdel groupmod $(SCOLOGIN) logoutd \
	groups

all:	$(BINS) $(DOCS)

#.PRECIOUS: libshadow.a

libshadow.a: \
	libshadow.a(dialchk.o) \
	libshadow.a(dialup.o) \
	libshadow.a(encrypt.o) \
	libshadow.a(getdef.o) \
	libshadow.a(getpass.o) \
	libshadow.a(grdbm.o) \
	libshadow.a(grent.o) \
	libshadow.a(groupio.o) \
	libshadow.a(grpack.o) \
	libshadow.a(gshadow.o) \
	libshadow.a(gsdbm.o) \
	libshadow.a(gspack.o) \
	libshadow.a(sgroupio.o) \
	libshadow.a(port.o) \
	libshadow.a(pwdbm.o) \
	libshadow.a(pwent.o) \
	libshadow.a(pwio.o) \
	libshadow.a(pwpack.o) \
	libshadow.a(pwauth.o) \
	libshadow.a(rad64.o) \
	libshadow.a(spdbm.o) \
	libshadow.a(shadow.o) \
	libshadow.a(shadowio.o) \
	libshadow.a(sppack.o)
	$(RANLIB) libshadow.a

libsec: $(LIBSEC)(shadow.o)
	$(RANLIB) $(LIBSEC)

save:
	[ ! -d save ] && mkdir save
	-cp $(LOGINDIR)/login save
	-cp /etc/mkpasswd /etc/pwconv /etc/pwunconv /etc/sulogin /etc/chpasswd \
		/etc/newusers /etc/useradd /etc/userdel /etc/usermod \
		/etc/groupadd /etc/groupdel /etc/groupmod /etc/logoutd \
		/etc/login.defs save
	-mv /usr/bin/su /bin/passwd /bin/gpasswd /bin/dpasswd /bin/faillog \
		/usr/bin/newgrp /bin/chfn /bin/chsh /bin/chage /usr/bin/id \
		/bin/scologin save
	-cp /bin/su /bin/passwd /bin/gpasswd /bin/dpasswd /bin/faillog \
		/bin/newgrp /bin/chfn /bin/chsh /bin/chage /bin/id \
		/bin/scologin save
	-cp $(DEST_INCLUDE_DIR)/dialup.h $(DEST_INCLUDE_DIR)/shadow.h \
		$(DEST_INCLUDE_DIR)/pwd.h save

restore:
	[ -d save ]
	-(cd save ; cp login $(LOGINDIR) )
	-(cd save ; -cp mkpasswd pwconv pwunconv sulogin chpasswd \
		newusers useradd userdel usermod groupadd groupdel groupmod \
		logoutd login.defs /etc)
	-(cd save ; cp su passwd gpasswd dpasswd faillog newgrp chfn chsh \
		chage id scologin /bin)
	-(cd save ; cp dialup.h shadow.h pwd.h $(DEST_INCLUDE_DIR) )

install:
	strip $(BINS)
	cp login $(LOGINDIR)/login
	cp mkpasswd pwconv pwunconv sulogin chpasswd newusers \
		useradd userdel usermod groupadd groupdel groupmod logoutd /etc
	cp su passwd gpasswd dpasswd faillog newgrp chfn chsh chage id /bin
	rm -f /bin/sg
	-ln /bin/newgrp /bin/sg
	cp dialup.h shadow.h pwd.h $(DEST_INCLUDE_DIR)
	chown $(RUID) $(LOGINDIR)/login /etc/pwconv /etc/pwunconv /etc/sulogin \
		/bin/su /bin/passwd /bin/gpasswd /bin/newgrp /etc/mkpasswd \
		/bin/dpasswd /bin/chsh /bin/chfn /bin/chage /etc/useradd \
		/etc/userdel /etc/usermod /etc/groupadd /etc/groupdel \
		/etc/groupmod /etc/logoutd
	chgrp $(RGID) $(LOGINDIR)/login /etc/pwconv /etc/pwunconv /etc/sulogin \
		/bin/su /bin/passwd /bin/gpasswd /bin/newgrp /etc/mkpasswd \
		/bin/dpasswd /bin/chsh /bin/chfn /bin/chage /etc/useradd \
		/etc/userdel /etc/usermod /etc/groupadd /etc/groupdel \
		/etc/groupmod /etc/logoutd
	chown $(BUID) /bin/faillog /bin/id $(DEST_INCLUDE_DIR)/shadow.h \
		$(DEST_INCLUDE_DIR)/dialup.h $(DEST_INCLUDE_DIR)/pwd.h
	chgrp $(BGID) /bin/faillog /bin/id $(DEST_INCLUDE_DIR)/shadow.h \
		$(DEST_INCLUDE_DIR)/dialup.h $(DEST_INCLUDE_DIR)/pwd.h
	chmod 700 /etc/pwconv /etc/pwunconv /etc/sulogin /etc/mkpasswd \
		/etc/chpasswd /etc/newusers /bin/dpasswd /etc/logoutd \
		/etc/useradd /etc/userdel /etc/usermod /etc/groupadd \
		/etc/groupdel /etc/groupmod
	chmod 4711 $(LOGINDIR)/login /bin/su /bin/passwd /bin/gpasswd \
		/bin/newgrp /bin/chfn /bin/chsh /bin/chage
	chmod 711 /bin/faillog /bin/id
	chmod 444 $(DEST_INCLUDE_DIR)/shadow.h $(DEST_INCLUDE_DIR)/dialup.h \
		$(DEST_INCLUDE_DIR)/pwd.h
	[ -f $(DEST_LOGIN_DEFS) ] || (cp login.defs $(DEST_LOGIN_DEFS) ; \
		chown $(RUID) $(DEST_LOGIN_DEFS) ; \
		chgrp $(RGID) $(DEST_LOGIN_DEFS) ; \
		chmod 600 $(DEST_LOGIN_DEFS) )
	[ -z "$(SCOLOGIN)" ] || (cp scologin /bin/login ; \
		chown $(RUID) /bin/login ; \
		chgrp $(RGID) /bin/login ; \
		chmod 755 /bin/login )

lint:	su.lint login.lint pwconv.lint pwunconv.lint passwd.lint sulogin.lint \
	faillog.lint newgrp.lint gpasswd.lint mkpasswd.lint chfn.lint \
	chsh.lint chage.lint dpasswd.lint id.lint useradd.lint userdel.lint \
	usermod.lint groupadd.lint groupdel.lint groupmod.lint logoutd.lint \
	$(ALLSRCS:.c=.L)

tags:	$(ALLSRCS)
	$(TAGS) $(ALLSRCS)

README:
	[ -f s.README ] && get -t -r$(RELEASE) s.README
	
$(DOCS):
	[ -f s.$@ ] && get -t -r$(RELEASE) s.$@

login.defs:
	[ -f s.login.defs ] && get -t -r$(RELEASE) s.login.defs

Makefile.sun4:
	[ -f s.Makefile.sun4 ] && get -t -r$(RELEASE) s.Makefile.sun4

Makefile.svr4:
	[ -f s.Makefile.svr4 ] && get -t -r$(RELEASE) s.Makefile.svr4

README.sun4:
	[ -f s.README.sun4 ] && get -t -r$(RELEASE) s.README.sun4

config.h.sun4:
	[ -f s.config.h.sun4 ] && get -t -r$(RELEASE) s.config.h.sun4

config.h.svr4:
	[ -f s.config.h.svr4 ] && get -t -r$(RELEASE) s.config.h.svr4

login:	$(LOBJS) libshadow.a
	$(CC) -o login $(LDFLAGS) $(LOBJS) -L. -lshadow $(LIBS)

login.lint: $(LSRCS)
	$(LINT) $(LINTFLAGS) $(LSRCS) > login.lint

su:	$(SOBJS) $(LIBSHADDEP)
	$(CC) -o su $(LDFLAGS) $(SOBJS) -L. -lshadow $(LIBS)

su.lint:	$(SSRCS)
	$(LINT) $(LINTFLAGS) -DSU $(SSRCS) > su.lint

passwd:	$(POBJS) $(LIBSHADDEP)
	$(CC) -o passwd $(LDFLAGS) $(POBJS) -L. -lshadow $(LIBS)

passwd.lint: $(PSRCS)
	$(LINT) $(LINTFLAGS) -DPASSWD $(PSRCS) > passwd.lint

gpasswd: $(GPOBJS) $(LIBSHADDEP)
	$(CC) -o gpasswd $(LDFLAGS) $(GPOBJS) -L. -lshadow $(LIBS)

gpasswd.lint: $(GPSRCS)
	$(LINT) $(LINTFLAGS) $(GPSRCS) > gpasswd.lint

dpasswd: $(DPOBJS) $(LIBSHADDEP)
	$(CC) -o dpasswd $(LDFLAGS) $(DPOBJS) -L. -lshadow $(LIBS)

dpasswd.lint: $(DPSRCS)
	$(LINT) $(LINTFLAGS) $(DPSRCS) > dpasswd.lint

pwconv:	$(PWOBJS) $(LIBSHADDEP) config.h
	$(CC) -o pwconv $(LDFLAGS) $(PWOBJS) -L. -lshadow $(LIBS)

pwconv.lint: $(PWSRCS) config.h
	$(LINT) $(LINTFLAGS) -DPASSWD $(PWSRCS) > pwconv.lint

pwunconv: $(PWUNOBJS) $(LIBSHADDEP) config.h
	$(CC) -o pwunconv $(LDFLAGS) $(PWUNOBJS) -L. -lshadow $(LIBS)

pwunconv.lint: $(PWUNSRCS)
	$(LINT) $(LINTFLAGS) -DPASSWD $(PWUNSRCS) > pwunconv.lint

sulogin: $(SULOGOBJS) $(LIBSHADDEP)
	$(CC) -o sulogin $(LDFLAGS) $(SULOGOBJS) -L. -lshadow $(LIBS)

sulogin.lint: $(SULOGSRCS)
	$(LINT) $(LINTFLAGS) $(SULOGSRCS) > sulogin.lint

faillog: faillog.o
	$(CC) -o faillog $(LDFLAGS) faillog.o $(LIBS)

faillog.lint: faillog.c faillog.h config.h
	$(LINT) $(LINTFLAGS) faillog.c > faillog.lint

mkpasswd: $(MKPWDOBJS) $(LIBSHADDEP)
	$(CC) -o mkpasswd $(LDFLAGS) $(MKPWDOBJS) -L. -lshadow $(LIBS)

mkpasswd.lint: $(MKPWDSRCS)
	$(LINT) $(LINTFLAGS) $(MKPWDSRCS) > mkpasswd.lint

newgrp: $(NGOBJS) $(LIBSHADDEP)
	$(CC) -o newgrp $(LDFLAGS) $(NGOBJS) -L. -lshadow $(LIBS)

newgrp.lint: $(NGSRCS)
	$(LINT) $(LINTFLAGS) $(NGSRCS) > newgrp.lint

sg:	newgrp
	/bin/rm -f sg
	ln newgrp sg

sg.lint: newgrp.lint
	ln newgrp.lint sg.lint

chfn:	$(CHFNOBJS) $(LIBSHADDEP)
	$(CC) -o chfn $(LDFLAGS) $(CHFNOBJS) -L. -lshadow $(LIBS)

chfn.lint:	$(CHFNSRCS)
	$(LINT) $(LINTFLAGS) $(CHFNSRCS) > chfn.lint

chsh:	$(CHSHOBJS) $(LIBSHADDEP)
	$(CC) -o chsh $(LDFLAGS) $(CHSHOBJS) -L. -lshadow $(LIBS)

chsh.lint: $(CHSHSRCS)
	$(LINT) $(LINTFLAGS) $(CHSHSRCS) > chsh.lint

chage:	$(CHAGEOBJS) $(LIBSHADDEP)
	$(CC) -o chage $(LDFLAGS) $(CHAGEOBJS) -L. -lshadow $(LIBS)

chage.lint: $(CHAGESRCS)
	$(LINT) $(LINTFLAGS) -DPASSWD $(CHAGESRCS) > chage.lint

chpasswd: $(CHPASSOBJS) $(LIBSHADDEP)
	$(CC) -o chpasswd $(LDFLAGS) $(CHPASSOBJS) -L. -lshadow $(LIBS)

chpasswd.lint: $(CHPASSSRCS)
	$(LINT) $(LINTFLAGS) $(CHPASSSRCS) > chpasswd.lint

newusers: newusers.o $(LIBSHADDEP)
	$(CC) -o newusers $(LDFLAGS) newusers.o -L. -lshadow $(LIBS)

newusers.lint: newusers.c
	$(LINT) $(LINTFLAGS) newusers.c > newusers.lint
	
id: id.o $(LIBSHADDEP)
	$(CC) -o id $(LDFLAGS) id.o -L. -lshadow $(LIBS)

id.lint: id.c
	$(LINT) $(LINTFLAGS) id.c > id.lint

groups: groups.o $(LIBSHADDEP)
	$(CC) -o groups $(LDFLAGS) groups.o -L. -lshadow $(LIBS)

groups.lint: groups.c
	$(LINT) $(LINTFLAGS) groups.c > groups.lint

useradd: useradd.o copydir.o mkrmdir.o $(LIBSHADDEP)
	$(CC) -o useradd $(LDFLAGS) useradd.o copydir.o mkrmdir.o \
		-L. -lshadow $(LIBS) $(NDIR)

useradd.lint: useradd.c copydir.c mkrmdir.c
	$(LINT) $(LINTFLAGS) useradd.c copydir.c mkrmdir.c > useradd.lint

userdel: userdel.o copydir.o mkrmdir.o $(LIBSHADDEP)
	$(CC) -o userdel $(LDFLAGS) userdel.o copydir.o mkrmdir.o \
		-L. -lshadow $(LIBS) $(NDIR)

userdel.lint: userdel.c copydir.c mkrmdir.c
	$(LINT) $(LINTFLAGS) userdel.c copydir.c mkrmdir.c > userdel.lint

usermod: usermod.o copydir.o mkrmdir.o $(LIBSHADDEP)
	$(CC) -o usermod $(LDFLAGS) usermod.o copydir.o mkrmdir.o \
		-L. -lshadow $(LIBS) $(NDIR)

usermod.lint: usermod.c copydir.c mkrmdir.c
	$(LINT) $(LINTFLAGS) usermod.c copydir.c mkrmdir.c > usermod.lint

groupadd: groupadd.o $(LIBSHADDEP)
	$(CC) -o groupadd $(LDFLAGS) groupadd.o -L. -lshadow $(LIBS)

groupadd.lint: groupadd.c
	$(LINT) $(LINTFLAGS) groupadd.c > groupadd.lint

groupdel: groupdel.o $(LIBSHADDEP)
	$(CC) -o groupdel $(LDFLAGS) groupdel.o -L. -lshadow $(LIBS)

groupdel.lint: groupdel.c
	$(LINT) $(LINTFLAGS) groupdel.c > groupdel.lint

groupmod: groupmod.o $(LIBSHADDEP)
	$(CC) -o groupmod $(LDFLAGS) groupmod.o -L. -lshadow $(LIBS)

groupmod.lint: groupmod.c
	$(LINT) $(LINTFLAGS) groupmod.c > groupmod.lint

pwd.h.m4:
	[ -f s.pwd.h.m4 ] && get -t -r$(RELEASE) s.pwd.h.m4

pwd.h: pwd.h.m4
	m4 $(OS) < pwd.h.m4 > pwd.h

logoutd: logoutd.o $(LIBSHADDEP)
	$(CC) -o logoutd $(LDFLAGS) logoutd.o -L. -lshadow

logoutd.lint: logoutd.c
	$(LINT) $(LINTFLAGS) logoutd.c > logoutd.lint

sulog.o: config.h

susetup.c: setup.c
	cp setup.c susetup.c

susetup.o: config.h susetup.c pwd.h
	$(CC) -c $(CFLAGS) -DSU susetup.c

scologin: scologin.o
	$(CC) -o scologin $(LDFLAGS) scologin.o -lsocket

passwd.o: config.h shadow.h pwd.h pwauth.h
lmain.o: config.h lastlog.h faillog.h pwd.h pwauth.h
smain.o: config.h lastlog.h pwd.h shadow.h pwauth.h
sub.o: pwd.h
setup.o: config.h pwd.h
mkrmdir.o: config.h
utmp.o: config.h
mail.o: config.h
motd.o: config.h
age.o: config.h pwd.h
log.o: config.h lastlog.h pwd.h
shell.o: config.h
entry.o: config.h shadow.h pwd.h
hushed.o: config.h pwd.h
valid.o: config.h pwd.h
failure.o: faillog.h config.h
faillog.o: faillog.h config.h pwd.h
newgrp.o: config.h shadow.h pwd.h
mkpasswd.o: config.h shadow.h pwd.h
gpmain.o: config.h pwd.h
chfn.o: config.h pwd.h
chsh.o: config.h pwd.h
chage.o: config.h shadow.h pwd.h
pwconv.o: config.h shadow.h
pwunconv.o: config.h shadow.h pwd.h
chpasswd.o: config.h shadow.h pwd.h
id.o: pwd.h
newusers.o: config.h shadow.h pwd.h
dpmain.o: dialup.h
useradd.o: config.h shadow.h pwd.h pwauth.h
userdel.o: config.h shadow.h pwd.h pwauth.h
usermod.o: config.h shadow.h pwd.h pwauth.h
groupadd.o: config.h shadow.h
groupdel.o: config.h shadow.h
groupmod.o: config.h shadow.h
logoutd.o: config.h
sulogin.o: config.h pwauth.h

libshadow.a(shadow.o): shadow.h config.h
libshadow.a(shadowio.o): shadow.h
libshadow.a(grent.o): config.h shadow.h
libshadow.a(sgroupio.o): shadow.h
libshadow.a(dialup.o): dialup.h
libshadow.a(dialchk.o): dialup.h config.h
libshadow.a(getdef.o): config.h
libshadow.a(pwdbm.o): config.h pwd.h
libshadow.a(spdbm.o): config.h shadow.h
libshadow.a(grdbm.o): config.h
libshadow.a(gshadow.o): config.h
libshadow.a(gsdbm.o): config.h shadow.h
libshadow.a(pwauth.o): config.h pwauth.h
libshadow.a(pwpack.o): config.h pwd.h
libshadow.a(pwent.o): config.h pwd.h
libshadow.a(pwio.o): pwd.h
libshadow.a(getpass.o): config.h
libshadow.a(encrypt.o): config.h
libshadow.a(port.o): port.h
libshadow.a(rad64.o): config.h

clean:
	-rm -f susetup.c *.o a.out core npasswd nshadow *.pag *.dir pwd.h

clobber: clean
	-rm -f $(BINS) *.lint *.L libshadow.a

nuke:	clobber
	-for file in * ; do \
		if [ -f s.$$file -a ! -f p.$$file ] ; then \
			rm -f $$file ;\
		fi ;\
	done

shar:	login.sh.01 login.sh.02 login.sh.03 login.sh.04 login.sh.05 \
	login.sh.06 login.sh.07 login.sh.08 login.sh.09 login.sh.10 \
	login.sh.11 login.sh.12

login.sh.01: $(FILES1) Makefile
	shar -a $(FILES1) > login.sh.01

login.sh.02: $(FILES2) Makefile
	shar -a $(FILES2) > login.sh.02

login.sh.03: $(FILES3) Makefile
	shar -a $(FILES3) > login.sh.03

login.sh.04: $(FILES4) Makefile
	shar -a $(FILES4) > login.sh.04

login.sh.05: $(FILES5) Makefile
	shar -a $(FILES5) > login.sh.05

login.sh.06: $(FILES6) Makefile
	shar -a $(FILES6) > login.sh.06

login.sh.07: $(FILES7) Makefile
	shar -a $(FILES7) > login.sh.07

login.sh.08: $(FILES8) Makefile
	shar -a $(FILES8) > login.sh.08

login.sh.09: $(FILES9) Makefile
	shar -a $(FILES9) > login.sh.09

login.sh.10: $(DOCS1) Makefile
	shar -a $(DOCS1) > login.sh.10

login.sh.11: $(DOCS2) Makefile
	shar -a $(DOCS2) > login.sh.11

login.sh.12: $(FILES_SUN4) $(FILES_SVR4) Makefile
	shar -a $(FILES_SUN4) $(FILES_SVR4) > login.sh.12
