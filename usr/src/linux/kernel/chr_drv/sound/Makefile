# Makefile for the stub version of Linux sound card driver
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definitions are now inherited from the
# parent makes. (hopefully)
#
#

.c.s:
	$(CC) $(CFLAGS) -S $<
.s.o:
	$(AS) -c -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -c $<

OBJS	= sound_stub.o

sound.a: $(OBJS) 
	$(AR) rcs sound.a $(OBJS)
	sync

clean:
	rm -f core *.o *.a

config:
	-@if [ -n "$(CONFIG_SOUND)" ] ;then make notice; fi

notice:
	@echo
	@echo
	@echo "WARNING!	You have attempted to compile the sound driver"
	@echo "		to your kernel. The driver is not included in"
	@echo "		the kernel distribution. You have to install"
	@echo "		the sound driver and to run "make config" again."
	@echo "		Otherwise the sound driver will not be included"
	@echo
	@echo "		The sound driver is available at nic.funet.fi"
	@echo "		in directory pub/OS/Linux/xtra/snd-kit. It "
	@echo "		should be also at the other Linux ftp sites."
	@echo

dep:
	@touch .depend
