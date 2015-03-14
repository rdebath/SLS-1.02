# Makefile -- Makefile for utile-1.0 Linux utilities
# Created: Sat Dec 26 20:09:40 1992
# Revised: Thu Jan 14 21:16:07 1993 by root
# Copyright 1992 Rickard E. Faith (faith@cs.unc.edu)
#
# $Log$
#

CC=        gcc
CFLAGS=    -O6 -I. -DHAVE_NFS
LDFLAGS=   -s -N

MAN5=      fstab.5 nfs.5

MAN8=      agetty.8 clock.8 ctrlaltdel.8 doshell.8 fdformat.8 fdisk.8 \
	   frag.8 fsck.8 kbdrate.8 makehole.8 mkfs.8 mkswap.8 mount.8 \
	   rdev.8 setfdprm.8 setserial.8 shutdown.8 simpleinit.8 swapon.8 \
	   sync.8 update.8

ETC=       agetty clock ctrlaltdel doshell fdformat fdisk frag fsck \
	   kbdrate makehole mkfs mkswap mount rdev setfdprm setserial \
	   shutdown simpleinit swapon sync umount update

ETC_FILES= fdprm

SPECIAL=   mount.o umount.o swapon.o sundries.o fstab.o

all: $(ETC) $(SPECIAL)

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

$(ETC):
	$(CC) $(LDFLAGS) $^ -o $@

$(SPECIAL):
	$(CC) -c $(CFLAGS) $(LDFLAGS) -DFSTYPE_DEFAULT=\"minix\" \
		$(*F).c -o $@

agetty: agetty.o
clock: clock.o
ctrlaltdel: ctrlaltdel.o
doshell: doshell.o
fdformat: fdformat.o
fdisk: fdisk.o
frag: frag.o
fsck: fsck.o
kbdrate: kbdrate.o
makehole: makehole.o
mkfs: mkfs.o
mkswap: mkswap.o
rdev: rdev.o
setfdprm: setfdprm.o
setroot: setroot.o
setserial: setserial.o
shutdown: shutdown.o
simpleinit: simpleinit.o
sync: sync.o
update: update.o

mount: mount.o sundries.o fstab.o nfsmount.o
umount: umount.o sundries.o fstab.o
swapon: swapon.o fstab.o
sundries.o: sundries.h sundries.c
fstab.o: fstab.h fstab.c

install: all
	install -m 755 $(ETC) /etc
	ln -sf /etc/shutdown /etc/reboot
	ln -sf /etc/shutdown /etc/fastboot
	ln -sf /etc/shutdown /etc/halt
	ln -sf /etc/shutdown /etc/fasthalt
	ln -sf /etc/rdev /etc/swapdev
	ln -sf /etc/rdev /etc/ramsize
	ln -sf /etc/rdev /etc/vidmode
	install -m 755 $(SPECIAL) /etc
	ln -sf /etc/swapon /etc/swapoff
	install -m 644 $(ETC_FILES) /etc
	install -m 644 $(MAN5) /usr/man/man5
	install -m 644 $(MAN8) /usr/man/man8
	ln -sf /usr/man/man8/shutdown.8 /usr/man/man8/reboot.8
	ln -sf /usr/man/man8/shutdown.8 /usr/man/man8/fastboot.8
	ln -sf /usr/man/man8/shutdown.8 /usr/man/man8/halt.8
	ln -sf /usr/man/man8/shutdown.8 /usr/man/man8/fasthalt.8
	ln -sf /usr/man/man8/rdev.8 /usr/man/man8/swapdev.8
	ln -sf /usr/man/man8/rdev.8 /usr/man/man8/ramsize.8
	ln -sf /usr/man/man8/rdev.8 /usr/man/man8/vidmode.8
	ln -sf /usr/man/man8/mount.8 /usr/man/man8/umount.8
	ln -sf /usr/man/man8/swapon.8 /usr/man/man8/swapoff.8
	
clean:
	-rm -f $(ETC) *.o *~
