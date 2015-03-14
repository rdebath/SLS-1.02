#
# Makefile for the linux sysv ipc user programs.
#
BINDIR= /usr/local/bin/
SYSBINDIR= /usr/local/bin/
LIBDIR= /usr/lib/

CC = gcc 
CPP = gcc -E $(CFLAGS) 
CFLAGS= -O6 # -g 
LDFLAGS= -N -s #-g
LOADLIBES= libipc.a
.c.o:  
	$(CC) $(CFLAGS) -c $<

SCRIPTS= msgloop swaptst
SYSPROGS= ipcs ipcrm
PROGS=  shmget shmctl shmop shmat shmfork shmswap semget semctl semop \
	msgget msgctl msgtst philos swapat
all: libipc.a $(SYSPROGS) $(PROGS)

libipc.a: libipc.o
	$(AR) -rcs libipc.a libipc.o

libipc.o: libipc.c
	$(CC) $(CFLAGS) -c -Wall $<
ipcs.o: ipcs.c libipc.a
	$(CC) $(CFLAGS) -c -Wall $<
ipcrm.o: ipcrm.c libipc.a
	$(CC) $(CFLAGS) -c -Wall $<


install: all
	cp $(PROGS) $(BINDIR)
	cp $(SYSPROGS) $(SYSBINDIR)
	cd $(SYSBINDIR); \
		chown bin.bin $(SYSPROGS); \
		chmod 0755 $(SYSPROGS)
	cp $(SCRIPTS) $(BINDIR)
	cp libipc.a $(LIBDIR)
	cd $(BINDIR); \
		chown bin.bin $(PROGS) $(SCRIPTS); \
		chmod 0755 $(PROGS) $(SCRIPTS)
	cd $(LIBDIR); chown root.root libipc.a
clean:
	rm -f *.o *.a $(PROGS) $(SYSPROGS) tmp_make
dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	$(CPP) -M *.c >> tmp_make
	cp tmp_make Makefile

### Dependencies:
