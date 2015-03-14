#
# Makefile for the linux kernel.
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
# Note 2! The CFLAGS definitions are now in the main makefile...

.S.s:
	$(CPP) -traditional $< -o $*.s
.c.s:
	$(CC) $(CFLAGS) -S $<
.s.o:
	$(AS) -c -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -c $<

SUBDIRS	= 

OBJS  = kdebug.o i386-pinsn.o

debugger.o: $(OBJS)
	$(LD) -r -o debugger.o $(OBJS)
	sync

clean:
	rm -f core *.o *.a tmp_make sys_call.s
	for i in *.c;do rm -f `basename $$i .c`.s;done
	for i in $(SUBDIRS); do (cd $$i; $(MAKE) clean); done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	for i in *.c;do $(CPP) -M $$i;done >> tmp_make
	cp tmp_make Makefile
	for i in $(SUBDIRS); do (cd $$i; $(MAKE) dep); done

dummy:

### Dependencies:
