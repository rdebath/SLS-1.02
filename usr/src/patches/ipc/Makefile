#
# Makefile for the linux ipc.
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definition is now in the main makefile...


SUBDIRS = 
.c.o:
	$(CC) $(CFLAGS) $(BFLAGS) -c $<
.s.o:
	$(AS) -o $*.o $<
.c.s:
	$(CC) $(CFLAGS) -S $<

OBJS	=  msg.o sem.o shm.o util.o 

ipc.o: $(OBJS) subdirs
	$(LD) -r -o ipc.o $(OBJS) 


subdirs: dummy
	for i in $(SUBDIRS); do (cd $$i && echo $$i && $(MAKE)) || exit; done

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

