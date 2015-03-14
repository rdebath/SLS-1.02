# To make "ext" the default file system type for mount
#  (used when no other type is specified), replace \"minix\" by \"ext\".
DEFAULT_FSTYPE=\"minix\"

# you need rpcgen and libc-4.2 or rpclib to compile in the NFS support
DEFINES = -DHAVE_NFS

# For debugging, uncomment -g in DEBUG and comment out -N in LDFLAGS.
# If you prefer static binaries, add -static to the LDFLAGS.

# debug link options
#DEBUG = -g
#LDFLAGS = $(DEBUG)

# static link options
#LDFLAGS = -N -s -static

# my favorite link options
LDFLAGS = -N -s

CC = gcc -pipe -O2 -Wall -Wstrict-prototypes -Wmissing-prototypes
CFLAGS = $(DEBUG) -DFSTYPE_DEFAULT=$(DEFAULT_FSTYPE) $(DEFINES)

PROGS = mount umount swapon
MAN5 = fstab.5 nfs.5
MAN8 = mount.8 swapon.8

# comment these out if you are are compiling in NFS support
NFS_OBJS = nfsmount.o mount_xdr.o mount_clnt.o
# uncomment this if you don't have libc-4.2 but do have the rpclib
LDLIBS = # -lrpclib
#RPCSVCDIR = /usr/include/rpcsvc
RPCSVCDIR = rpcsvc
GEN_FILES = mount.x mount.h mount_xdr.c mount_clnt.c
# the old rpcgen used to generate K&R style code
#RPCFLAGS = -Wno-strict-prototypes -Wno-missing-prototypes
# the new rpcgen generates lots of unused variables
RPCFLAGS = -Wno-unused 
RPCGEN = rpcgen

all: $(PROGS)

install: $(PROGS)
	install -s $(PROGS) /etc
	install -d /usr/man/cat5
	install -d /usr/man/cat8
	install $(MAN5) /usr/man/cat5
	install $(MAN8) /usr/man/cat8
	(cd /usr/man/cat8 ; ln -sf mount.8 umount.8)
	ln -sf /etc/swapon /etc/swapoff
	(cd /usr/man/cat8 ; ln -sf swapon.8 swapoff.8)

mount: mount.o sundries.o fstab.o $(NFS_OBJS)

umount: umount.o sundries.o fstab.o

swapon:	swapon.o fstab.o

nfsmount.o mount_xdr.o mount_clnt.o: mount.h

nfsmount.o: nfsmount.c
	$(CC) -c $(CFLAGS) $(RPCFLAGS) nfsmount.c

mount_clnt.o: mount_clnt.c
	$(CC) -c $(CFLAGS) $(RPCFLAGS) mount_clnt.c

mount_xdr.o: mount_xdr.c
	$(CC) -c $(CFLAGS) $(RPCFLAGS) mount_xdr.c

mount.h mount_xdr.c mount_clnt.c: mount.x
	rm -f mount.h mount_xdr.c mount_clnt.c
	$(RPCGEN) -h -o mount.h mount.x
	$(RPCGEN) -c -o mount_xdr.c mount.x
	$(RPCGEN) -l -o mount_clnt.c mount.x

mount.x:
	cp $(RPCSVCDIR)/mount.x .

clean:
	rm -f a.out core *~ *.o

clobber: clean
	rm -f $(PROGS) $(GEN_FILES)
