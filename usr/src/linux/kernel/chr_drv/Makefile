#
# Makefile for the kernel character device drivers.
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definitions are now inherited from the
# parent makes..
#

.c.s:
	$(CC) $(CFLAGS) -S $<
.s.o:
	$(AS) -c -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -c $<

SUBDIRS= sound

OBJS  = tty_io.o console.o keyboard.o serial.o \
	tty_ioctl.o pty.o lp.o vt.o mem.o mouse.o \
	busmouse.o psaux.o msbusmouse.o atixlmouse.o

all:	chr_drv.a subdirs

chr_drv.a: $(OBJS)
	$(AR) rcs chr_drv.a $(OBJS)
	sync

console.o: console.c
	$(CC) $(CFLAGS) -c -o console.o console.c

keyboard.o: keyboard.c
	$(CC) $(CFLAGS) $(KEYBOARD) -c -o keyboard.o keyboard.c

subdirs: dummy
	@for i in $(SUBDIRS); do (cd $$i && echo $$i && $(MAKE)) || exit; done

clean:
	rm -f core *.o *.a *.s
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit; done

dep:
	$(CPP) -M $(KEYBOARD) *.c > .depend
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) dep) || exit; done

dummy:

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
