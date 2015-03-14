#
# Makefile for the kernel block device drivers.
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definition is now inherited from the
# parent makefile.
#

.c.s:
	$(CC) $(CFLAGS) $(RAMDISK) -S $<
.s.o:
	$(AS) -c -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) $(RAMDISK) -c $<

SUBDIRS	= scsi

OBJS = xd.o hd.o ll_rw_blk.o floppy.o ramdisk.o genhd.o

all: blk_drv.a scsisubdirs

blk_drv.a: $(OBJS)
	rm -f blk_drv.a
	$(AR) rcs blk_drv.a $(OBJS)
	sync

scsisubdirs: dummy
	@for i in $(SUBDIRS); do (cd $$i && echo $$i && $(MAKE)) || exit; done

clean:
	rm -f core *.o *.a *.s
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean); done

dep:
	$(CPP) -M *.c > .depend
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) dep); done

xd.o:	
	$(CC) $(CFLAGS) -fno-omit-frame-pointer $(RAMDISK) -c $<

dummy:

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif

