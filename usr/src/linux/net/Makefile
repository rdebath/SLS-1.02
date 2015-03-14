#
# Makefile for the linux networking.
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definition is now in the main makefile...

# only these two lines should need to be changed to remove inet sockets.
# (and the tcp/tcpip.o in net.o)

SUBDIRS = tcp

ifdef CONFIG_TCPIP
NET_SUBDIRS = tcp
TCP_ARCHIVE = tcp/tcpip.a
endif

.c.o:
	$(CC) $(CFLAGS) -c $<
.s.o:
	$(AS) -o $*.o $<
.c.s:
	$(CC) $(CFLAGS) -S $<

OBJS	=  socket.o unix.o 

net.o: $(OBJS) subdirs
	$(LD) -r -o net.o $(OBJS) $(TCP_ARCHIVE)


subdirs: dummy
	for i in $(NET_SUBDIRS); do (cd $$i && echo $$i && $(MAKE)) || exit; done

clean:
	rm -f core *.o *.a *.s
	@for i in $(SUBDIRS); do (cd $$i && echo $$i && $(MAKE) clean) || exit; done

dep:
	$(CPP) -M *.c > .depend
	@for i in $(SUBDIRS); do (cd $$i && echo $$i && $(MAKE) dep) || exit; done

dummy:

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif

