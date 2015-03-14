#
# Makefile for the linux networking.
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definition is now in the main makefile...

.c.o:
	$(CC) $(CFLAGS) \
	-c -o $*.o $<
.s.o:
	$(AS) -o $*.o $<
.c.s:
	$(CC) $(CFLAGS) \
	-S -o $*.s $<

OBJS	= sock.o tcp.o ip.o timer.o we.o arp.o udp.o eth.o Space.o loopback.o \
	  icmp.o protocols.o raw.o pack_type.o dev.o packet.o

tcpip.a: $(OBJS)
	$(AR) rcs tcpip.a $(OBJS)

subdirs: dummy
	for i in $(SUBDIRS); do (cd $$i; $(MAKE)); done


clean:
	rm -f core *.o *.a *.s

dep:
	$(CPP) -M *.c > .depend

tar:
	tar -cvf /dev/f1 .

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
