
all:	Version Image

.EXPORT_ALL_VARIABLES:

#
# Make "config" the default target if there is no configuration file or
# "depend" the target if there is no top-level dependency information.
#
ifeq (.config,$(wildcard .config))
include .config
ifeq (.depend,$(wildcard .depend))
include .depend
else
CONFIGURATION = depend
endif
else
CONFIGURATION = config
endif

ifdef CONFIGURATION
CONFIGURE = dummy
endif

#
# ROOT_DEV specifies the default root-device when making the image.
# This can be either FLOPPY, CURRENT, /dev/xxxx or empty, in which case
# the default of FLOPPY is used by 'build'.
#

ROOT_DEV = CURRENT

#
# uncomment the correct keyboard:
#
# The value of KBDFLAGS should be or'ed together from the following
# bits, depending on which features you want enabled.
#
# The least significant bits control if the following keys are "dead".
# The key is dead by default if the bit is on.
# 0x01 - backquote (`)
# 0x02 - accent acute
# 0x04 - circumflex (^)
# 0x08 - tilde (~)
# 0x10 - dieresis (umlaut)

KEYBOARD = -DKBD_FINNISH -DKBDFLAGS=0
# KEYBOARD = -DKBD_FINNISH_LATIN1 -DKBDFLAGS=0x1F
# KEYBOARD = -DKBD_US -DKBDFLAGS=0
# KEYBOARD = -DKBD_GR -DKBDFLAGS=0
# KEYBOARD = -DKBD_GR_LATIN1 -DKBDFLAGS=0x1F
# KEYBOARD = -DKBD_FR -DKBDFLAGS=0
# KEYBOARD = -DKBD_FR_LATIN1 -DKBDFLAGS=0x1F
# KEYBOARD = -DKBD_UK -DKBDFLAGS=0
# KEYBOARD = -DKBD_DK -DKBDFLAGS=0
# KEYBOARD = -DKBD_DK_LATIN1 -DKBDFLAGS=0x1F
# KEYBOARD = -DKBD_DVORAK -DKBDFLAGS=0
# KEYBOARD = -DKBD_SG -DKBDFLAGS=0
# KEYBOARD = -DKBD_SG_LATIN1 -DKBDFLAGS=0x1F
# KEYBOARD = -DKBD_SF -DKBDFLAGS=0
# KEYBOARD = -DKBD_SF_LATIN1 -DKBDFLAGS=0x1F
# KEYBOARD = -DKBD_NO -DKBDFLAGS=0

#
# If you want to preset the SVGA mode, uncomment the next line and
# set SVGA_MODE to whatever number you want.
# Set it to -DSVGA_MODE=NORMAL_VGA if you just want the EGA/VGA mode.
# The number is the same as you would ordinarily press at bootup.
#

SVGA_MODE=	-DSVGA_MODE=3

#
# standard CFLAGS
#

CFLAGS = -Wall -Wstrict-prototypes -O6 -fomit-frame-pointer

ifdef CONFIG_M486
CFLAGS := $(CFLAGS) -m486
endif

#
# if you want the ram-disk device, define this to be the
# size in blocks.
#

#RAMDISK = -DRAMDISK=512

AS86	=as86 -0 -a
LD86	=ld86 -0

AS	=as
LD	=ld
HOSTCC	=gcc
CC	=gcc -DKERNEL
MAKE	=make
CPP	=$(CC) -E
AR	=ar
STRIP	=strip

ARCHIVES	=kernel/kernel.o mm/mm.o fs/fs.o net/net.o
FILESYSTEMS	=fs/filesystems.a
DRIVERS		=kernel/blk_drv/blk_drv.a kernel/chr_drv/chr_drv.a \
		 kernel/blk_drv/scsi/scsi.a kernel/chr_drv/sound/sound.a
MATH		=kernel/FPU-emu/math.a
LIBS		=lib/lib.a
SUBDIRS		=kernel mm fs net lib

KERNELHDRS	=/usr/src/linux/include

.c.s:
	$(CC) $(CFLAGS) -S -o $*.s $<
.s.o:
	$(AS) -c -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -c -o $*.o $<

Version: dummy
	rm -f tools/version.h

lilo: $(CONFIGURE) Image
	if [ -f /vmlinux ]; then mv /vmlinux /vmlinux.old; fi
	cat Image > /vmlinux
	/etc/lilo/install

config:
	sh Configure < config.in
	mv .config~ .config
	$(MAKE) soundconf

soundconf:
	cd kernel/chr_drv/sound;$(MAKE) config

linuxsubdirs: dummy
	@for i in $(SUBDIRS); do (cd $$i && echo $$i && $(MAKE)) || exit; done

tools/./version.h: tools/version.h

tools/version.h: $(CONFIGURE) Makefile
	@./makever.sh
	@echo \#define UTS_RELEASE \"0.99.pl9-`cat .version`\" > tools/version.h
	@echo \#define UTS_VERSION \"`date +%D`\" >> tools/version.h
	@echo \#define LINUX_COMPILE_TIME \"`date +%T`\" >> tools/version.h
	@echo \#define LINUX_COMPILE_BY \"`whoami`\" >> tools/version.h
	@echo \#define LINUX_COMPILE_HOST \"`hostname`\" >> tools/version.h

Image: $(CONFIGURE) boot/bootsect boot/setup tools/system tools/build
	tools/build boot/bootsect boot/setup tools/system $(ROOT_DEV) > Image
	sync

disk: Image
	dd bs=8192 if=Image of=/dev/fd0

tools/build: $(CONFIGURE) tools/build.c
	$(HOSTCC) $(CFLAGS) \
	-o tools/build tools/build.c

boot/head.o: $(CONFIGURE) boot/head.s

boot/head.s: $(CONFIGURE) boot/head.S include/linux/tasks.h
	$(CPP) -traditional boot/head.S -o boot/head.s

tools/version.o: tools/version.c tools/version.h

init/main.o: $(CONFIGURE) init/main.c
	$(CC) $(CFLAGS) $(PROFILING) -c -o $*.o $<

tools/system:	boot/head.o init/main.o tools/version.o linuxsubdirs
	$(LD) $(LDFLAGS) -M boot/head.o init/main.o tools/version.o \
		$(ARCHIVES) \
		$(FILESYSTEMS) \
		$(DRIVERS) \
		$(MATH) \
		$(LIBS) \
		-o tools/system > System.map

boot/setup: boot/setup.s
	$(AS86) -o boot/setup.o boot/setup.s
	$(LD86) -s -o boot/setup boot/setup.o

boot/setup.s: $(CONFIGURE) boot/setup.S include/linux/config.h Makefile
	$(CPP) -traditional $(SVGA_MODE) $(RAMDISK) boot/setup.S -o boot/setup.s

boot/bootsect.s: $(CONFIGURE) boot/bootsect.S include/linux/config.h Makefile
	$(CPP) -traditional $(SVGA_MODE) $(RAMDISK) boot/bootsect.S -o boot/bootsect.s

boot/bootsect:	boot/bootsect.s
	$(AS86) -o boot/bootsect.o boot/bootsect.s
	$(LD86) -s -o boot/bootsect boot/bootsect.o

zBoot/zSystem: zBoot/*.c zBoot/*.S tools/zSystem
	cd zBoot;$(MAKE)

zImage: $(CONFIGURE) boot/bootsect boot/setup zBoot/zSystem tools/build
	tools/build boot/bootsect boot/setup zBoot/zSystem $(ROOT_DEV) > zImage
	sync

zdisk: zImage
	dd bs=8192 if=zImage of=/dev/fd0

zlilo: $(CONFIGURE) zImage
	cat zImage > /vmlinuz
	/etc/lilo/install


tools/zSystem:	boot/head.o init/main.o tools/version.o linuxsubdirs
	$(LD) $(LDFLAGS) -T 100000 -M boot/head.o init/main.o tools/version.o \
		$(ARCHIVES) \
		$(FILESYSTEMS) \
		$(DRIVERS) \
		$(MATH) \
		$(LIBS) \
		-o tools/zSystem > zSystem.map

fs: dummy
	$(MAKE) linuxsubdirs SUBDIRS=fs

mm: dummy
	$(MAKE) linuxsubdirs SUBDIRS=mm

kernel: dummy
	$(MAKE) linuxsubdirs SUBDIRS=kernel

clean:
	rm -f zImage zSystem.map tools/zSystem
	rm -f Image System.map core boot/bootsect boot/setup \
		boot/bootsect.s boot/setup.s boot/head.s init/main.s
	rm -f init/*.o tools/system tools/build boot/*.o tools/*.o
	for i in zBoot $(SUBDIRS); do (cd $$i && $(MAKE) clean); done

mrproper: clean
	rm -f include/linux/autoconf.h tools/version.h
	rm -f .version .config*
	rm -f .depend `find . -name .depend -print`

backup: mrproper
	cd .. && tar cf - linux | gzip -9 > backup.z
	sync

depend dep:
	touch tools/version.h
	for i in init/*.c;do echo -n "init/";$(CPP) -M $$i;done > .depend~
	for i in tools/*.c;do echo -n "tools/";$(CPP) -M $$i;done >> .depend~
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) dep) || exit; done
	rm -f tools/version.h
	mv .depend~ .depend

ifdef CONFIGURATION
..$(CONFIGURATION):
	@echo
	@echo "You have no" .$(CONFIGURATION) ": running 'make" $(CONFIGURATION)"'"
	@echo
	$(MAKE) $(CONFIGURATION)
	@echo
	@echo "Successful. Try re-making (ignore the error that follows)"
	@echo
	exit 1

dummy: ..$(CONFIGURATION)

else

dummy:

endif
