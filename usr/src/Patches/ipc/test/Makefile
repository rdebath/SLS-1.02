#
# Makefile for the linux sysv ipc user programs.
#
BINDIR= /usr/local/bin/
LIBDIR= /usr/lib/
CC = gcc  
CFLAGS= -O6
LDFLAGS= -N -s 
LOADLIBES= libipc.a
.c.o:
	$(CC) $(CFLAGS) -c $<

SCRIPTS= msgloop
SYSPROGS= ipcs ipcrm
PROGS=  shmget shmctl shmop semget semctl semop \
	msgget msgctl msgtst philos 
all: first sysprogs progs

first : libipc.a
sysprogs: first $(SYSPROGS)
progs: first $(PROGS)

libipc.a: libipc.o
	$(AR) -rcs libipc.a libipc.o
install: all
	cp $(PROGS) $(SYSPROGS) $(BINDIR)
	cp $(SCRIPTS) $(BINDIR)
	cp libipc.a $(LIBDIR)
	cd $(BINDIR); chown bin.bin $(PROGS) $(SYSPROGS) $(SCRIPTS); \
	    chmod 0755 $(PROGS) $(SCRIPTS); \
	    chmod 0754 $(SYSPROGS)
	chown root.root $(LIBDIR)libipc.a
clean:
	rm -f *.o *.a $(PROGS) $(SYSPROGS)
dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	$(CPP) -M *.c >> tmp_make
	cp tmp_make Makefile

### Dependencies:
