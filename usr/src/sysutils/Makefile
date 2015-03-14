#
# Makefile for various Linux system utilities
#
# Jim Winstead Jr. (jwinstea@fenris.claremont.edu)

CC=	gcc -O3
CFLAGS=	-Wall
LDFLAGS=-N -s

etc=	update
bin=	ed fdformat fdisk fsck kill mkfs mkswap setfdprm sync
usr.bin=doshell frag makehole more setroot setserial \
	setterm

PROGS=	$(etc) $(bin) $(usr.bin)

all:	$(PROGS)

more:	more.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o more $< -ltermcap

setterm:setterm.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o setterm $< -ltermcap

install::	$(etc)
	install -s $(etc) /etc

install::	$(bin)
	install -s $(etc) /bin

install::	$(usr.bin)
	install -s $(usr.bin) /usr/bin

install::	more.help
	install more.help /usr/lib

clean:
	rm -f core *~ $(PROGS)
