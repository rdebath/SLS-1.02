#
# nfsd/mountd	This package is a user-space "NFS" server.
#
# Version:	@(#)Makefile	1.5	93/04/10
#
# Authors:	Mark A. Shand, May 1988
#		Donald J. Becker, <becker@super.org>
#		Rick Sladkey, <jrs@world.std.com>
#		Orest Zborowski, <obz@raster.Kodak.COM>
#		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
#
#		Copyright 1988 Mark A. Shand
#		This software maybe be used for any purpose provided
#		the above copyright notice is retained.  It is supplied
#		as is, with no warranty expressed or implied.
#

# Select the compiler.
CC		= gcc -Wall -O2
LD		= gcc

# Select any extra libraries.
LDLIBS		= #-lrpclib
LDFLAGS		= -N -s

OPTS		= -DSYSLOG # -DDEBUG -DAUTH_DEBUG -DREAD_ONLY -DRPC_SVC_FG
CFLAGS		= -DANSI $(OPTS)
XDRCFLAGS 	= $(CFLAGS) -Wno-unused

RPCGEN		= rpcgen
RPCSVCDIR	= rpcsvc
#RPCSVCDIR	= /usr/include/rpcsvc
TRANSPORTFLAGS	= -I -s udp -s tcp
#TRANSPORTFLAGS	= -s udp -s tcp

# Installation defines.
PROGS		= nfsd mountd showmount
BININSTALL	= install -m 755
MANINSTALL	= install -m 644
ETCDIR		= /etc
MAN5DIR		= /usr/man/man5
MAN8DIR		= /usr/man/man8
MAN5FILES	= man/exports.5
MAN8FILES	= man/mountd.8 man/nfsd.8 man/showmount.8

CFILES		= nfsd.c mountd.c dispatch.c getattr.c fh.c logging.c \
		  auth_init.c auth_clnt.c showmount.c
HFILES		= nfsd.h fh.h auth.h

XDRFILES	= mount.x nfs_prot.x
GENCFILES	= mount_svc.c mount_xdr.c nfs_prot_xdr.c
GENHFILES	= mount.h nfs_prot.h
GENFILES	= $(XDRFILES) $(GENCFILES) $(GENHFILES)

OBJS		= logging.o fh.o auth_init.o auth_clnt.o
NFSD_OBJS	= nfsd.o dispatch.o getattr.o nfs_prot_xdr.o $(OBJS)
MOUNTD_OBJS	= mountd.o mount_svc.o mount_xdr.o $(OBJS)
SHOWMOUNT_OBJS	= showmount.o mount_xdr.o


.c.o:		$< $(GENFILES)
		$(CC) -c $(CFLAGS) $<

all:		$(PROGS)


nfsd:		$(NFSD_OBJS)
		$(LD) $(LDFLAGS) -o $@ $(NFSD_OBJS) $(LDLIBS)

$(NFSD_OBJS):	$(GENHFILES)

mountd:		$(MOUNTD_OBJS)
		$(LD) $(LDFLAGS) -o $@ $(MOUNTD_OBJS) $(LDLIBS)

$(MOUNTD_OBJS):	$(GENHFILES)

showmount:	$(SHOWMOUNT_OBJS)
		$(LD) $(LDFLAGS) -o $@ $(SHOWMOUNT_OBJS) $(LDLIBS)

$(SHOWMOUNT_OBJS): $(GENHFILES)

nfs_prot.x:
		cp $(RPCSVCDIR)/nfs_prot.x .

mount.x:
		cp $(RPCSVCDIR)/mount.x .

nfs_prot.h:	nfs_prot.x
		$(RPCGEN) -h -o $@ $?

nfs_prot_xdr.c: nfs_prot.x
		$(RPCGEN) -c -o $@ $?

mount.h:	mount.x
		$(RPCGEN) -h -o $@ $?

mount_xdr.c:	mount.x
		$(RPCGEN) -c -o $@ $?

mount_svc.c:	mount.x
		$(RPCGEN) $(TRANSPORTFLAGS) $? | \
			sed 's/svc_run/mountd_init(argc, argv); &/' >$@

# wrong
			#sed 's/main/mount_run/' >$@

nfs_prot_xdr.o:	nfs_prot_xdr.c
		$(CC) $(XDRCFLAGS) -c nfs_prot_xdr.c

mount_xdr.o:	mount_xdr.c
		$(CC) $(XDRCFLAGS) -c mount_xdr.c

install:
		$(BININSTALL) $(PROGS) $(ETCDIR)
		$(MANINSTALL) $(MAN5FILES) $(MAN5DIR)
		$(MANINSTALL) $(MAN8FILES) $(MAN8DIR)

rpcsvc:		$(GENFILES)
		cp -p $(GENFILES) rpcsvc

dep:		$(CFILES) $(HFILES) $(GENFILES)
		sed '/^### Dep/q' Makefile >tmp_make
		$(CC) -M $(CFLAGS) $(CFILES) $(GENCFILES) >> tmp_make
		mv Makefile Makefile~
		mv tmp_make Makefile

clean:
		rm -f *.o *.b *.bak *~ \#* tmp_make $(GENFILES)

clobber:	clean
		rm -f $(PROGS)

### Dependencies: do not delete this line
