# To make "ext" the default file system type for mount
#  (used when no other type is specified), replace \"minix\" by \"ext\".
DEFAULT_FSTYPE=\"minix\"

# kernels prior to 0.96c-pl1 use a 4 arg mount(2) and don't have msdos or ext
#DEFINES =

# 0.96c-pl1 and later kernels use a 5 arg mount(2) and have msdos and ext fs
#DEFINES = -DHAVE_MOUNT5
# 0.97-pl3 and later kernels also have swapoff(2)
DEFINES = -DHAVE_MOUNT5 -DHAVE_SWAPOFF -DHAVE_NFS

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
CFLAGS = $(DEBUG) -I. -DFSTYPE_DEFAULT=$(DEFAULT_FSTYPE) $(DEFINES)

PROGS = mount umount swapon
MAN5 = fstab.5
MAN8 = mount.8 swapon.8

NFS_OBJS = nfsmount.o mount_xdr.o mount_clnt.o
LOADLIBES = -lrpclib
GEN_FILES = mount.x mount.h mount_xdr.c mount_clnt.c
RPCFLAGS = -Wno-strict-prototypes -Wno-missing-prototypes

all: $(PROGS)

install: $(PROGS)
	install -s $(PROGS) /etc
	install -d /usr/man/cat5
	install -d /usr/man/cat8
	install $(MAN5) /usr/man/cat5
	install $(MAN8) /usr/man/cat8
	(cd /usr/man/cat8 ; ln -fs mount.8 umount.8)
# comment out the two lines below if your kernel version is < 0.97-pl3
	ln -f /etc/swapon /etc/swapoff
	(cd /usr/man/cat8 ; ln -fs swapon.8 swapoff.8)

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
	rpcgen -h -o mount.h mount.x
	rpcgen -c -o mount_xdr.c mount.x
	rpcgen -l -o mount_clnt.c mount.x

mount.x:
	cp /usr/include/rpcsvc/mount.x .

clean:
	rm -f a.out core *~ *.o $(GEN_FILES)

clobber: clean
	rm -f $(PROGS)
