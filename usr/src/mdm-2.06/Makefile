#
# Makefile for the LINUX Modem Server Version 2
#
# Compile used is GNU C
#
CC = gcc
#
# -DDEBUG for misc debugging to be sent to STDERR/logging device
# -DLOGDEV = "/dev/tty8" for logging output to be sent to a Virtual TTY 
# -DLOGFILE = "/usr/adm/modem.log" for logging output to be sent to a logfile
# -DCMGET if you have TIOCMGET function (Newer kernals) (97pl4 on I believe)
# (Not implemented yet) -DMTMP if you want /etc/mtmp to be updated
# (Not implemented yet) -DUTMP if you want /etc/utmp to be updated
#
EXTRA = '-DLOGDEV="/dev/tty8"' '-DLOGFILE="/usr/adm/modem.log"'  
EXTRA2 = -Wall # -DLINESTAT -DCMGET -DUTMP
#
# Optimisers..
#
OPTIM = -O6 -m486 -fomit-frame-pointer -s -N
#
# Debug options
#
DEBUG =# -DDEBUG #-g
#

CFLAGS = $(EXTRA) $(EXTRA2) $(OPTIM) $(DEBUG)

CFILES = modem.c fgetsraw.c strupr.c fgetstr.c flog.c setup.c
OFILES = modem.o fgetsraw.o strupr.o fgetstr.o flog.o setup.o
HFILES = modem.h

all: modem

modem: $(OFILES)
	$(CC) $(CFLAGS) -o modem $(OFILES)

install: modem
	install -s -g system -o root -m 110 modem /etc
	install -d /etc/default
	install -g system -o root -m 644 modem.cfg /etc/default/modem
	install -g system -o root -m 644 ttys1.cfg /etc/default/ttys1

clean:
	rm -f *.o modem

tar:	$(CFILES) $(HFILES)
	tar -cvf mdm.tar *.c *.h COPY* Makefile modem.cfg ttys1.cfg README version BUGS

shar:	$(CFILES) $(HFILES)
	shar *.c *.h COPY* Makefile modem.cfg ttys1.cfg README version BUGS >mdm.shar

# dep
modem.o: modem.c modem.h
fgetsraw.o: fgetsraw.c
strupr.o: strupr.c
fgetstr.o: fgetstr.c
flog.o: flog.c
setup.o: setup.c modem.h
