#
# Makefile for Minicom - fill in your system
#

# Take these flags for the PC Minix ACK compiler
# LFLAGS	= -i
# CFLAGS	= -D_V7 -D_MINIX -D_POSIX_SOURCE -D_ACK 
# LIBS	=
# CC	= cc
# O	= s
# chmem	= chmem

# Take these flags for Bruce Evans' bcc
#LFLAGS	= -i -s
#CFLAGS	= -D_V7 -D_MINIX -D_POSIX_SOURCE #-DDEBUG
#LIBS	=
#CC	= bcc
#O	= o
#chmem	= chmem
## Rule for Minix make & bcc's .o files
#.c.$(O):
#	$(CC) $(CFLAGS) -c $<

# Take these flags for a 68000 Minix
#LFLAGS	= -s
#CFLAGS	= -D_V7 -D_MINIX -D_POSIX_SOURCE
#LIBS	=
#CC	= cc
#O	= o
#chmem	= chmem

# Take these flags for BSD 4.3 (Apollo Domain/OS)
#LFLAGS	= -s
#CFLAGS	= -D_BSD43 -D_SELECT #-DDEBUG
#LIBS	= -ltermcap
#CC	= cc
#O	= o
#chmem	= /bin/true

# Take these flags for a SysV system (sysv/HPUX/Linux)
LFLAGS	= -s
#CFLAGS	= -D_SYSV -D_POSIX_SOURCE # Generic Sysv (R4)
#CFLAGS	= -D_POSIX_SOURCE -D_HPUX_SOURCE -D_SYSV -D_SELECT # HPUX
CFLAGS	= -Wall -D_POSIX_SOURCE -D_SYSV -D_SELECT -fwritable-strings # Sysv with select(),eg Linux
#CFLAGS = -D_POSIX_SOURCE -D_SYSV -D_SVR2 # Sysv R2, eg UnixPC
LIBS	= -ltermcap
CC	= gcc -O6
O	= o
chmem	= /bin/true

# Take these flags for Mark Williams' Coherent
#LFLAGS	= -s
#CFLAGS	= -D_V7 -D_COHERENT
#LIBS	= -lterm
#CC	= cc
#O	= o
#chmem	= /bin/true

# Where to install things (must be the same as in "config.h")
BINDIR	= /usr/local/bin
ETCDIR	= /usr/local/etc

# Nothing should have to change beneath this line

SRCS	= minicom.c vt100.c config.c help.c updown.c \
	  fastsystem.c dial.c window.c wkeys.c ipc.c main.c \
	  keyserv.c windiv.c script.c sysdep1.c sysdep2.c rwconf.c

HDRS	= minicom.h window.h keyserv.h charmap.h config.h \
	  configsym.h patchlevel.h

OTHERS  = Makefile Install Readme Todo unixlogin saralogin scriptdemo \
	  Manual.scr Manual.mnc Readme.rzsz Readme.Coh minicom.users \
	  mc History

MOBJS	= minicom.$(O) vt100.$(O) config.$(O) help.$(O) updown.$(O) \
	  fastsystem.$(O) dial.$(O) window.$(O) wkeys.$(O) ipc.$(O) \
	  windiv.$(O) sysdep1.$(O) sysdep2.$(O) rwconf.$(O) main.$(O)

KOBJS	= keyserv.$(O) wkeys.$(O) sysdep2.$(O)

SOBJS	= script.$(O)

all:		minicom keyserv runscript

minicom:	$(MOBJS)
		$(CC) $(LFLAGS) -o minicom $(MOBJS) $(LIBS)
		@$(chmem) =40000 minicom

keyserv:	$(KOBJS)
		$(CC) -o keyserv $(LFLAGS) $(KOBJS) $(LIBS)
		@$(chmem) =8000 keyserv

runscript:	$(SOBJS)
		$(CC) -o runscript $(LFLAGS) $(SOBJS)
		@$(chmem) =8192 runscript 

script.$(O):	script.c

keyserv.$(O):	keyserv.c $(HDRS)

minicom.$(O):	minicom.c $(HDRS)

main.$(O):	main.c $(HDRS)

windiv.$(O):	windiv.c $(HDRS)

vt100.$(O):	vt100.c $(HDRS)

config.$(O):	config.c $(HDRS)

fastsystem.$(O): fastsystem.c $(HDRS)

dial.$(O):	dial.c $(HDRS)

help.$(O):	help.c $(HDRS)

updown.$(O):	updown.c $(HDRS)

window.$(O):	window.c $(HDRS)

wkeys.$(O):	wkeys.c $(HDRS)

ipc.$(O):	ipc.c $(HDRS)

sysdep1.$(O):	sysdep1.c $(HDRS)

sysdep2.$(O):	sysdep2.c $(HDRS)

rwconf.$(O):	rwconf.c $(HDRS)

shar:
		@echo Creating shar archive..
		@shar $(SRCS) $(HDRS) $(OTHERS) > minicom.sh
		@echo Compressing..
		@rm -f minicom.sh.Z
		@compress -b13 minicom.sh
		@rm -f minicom.sh

tar:
		@echo Creating tar archive..
		@tar cvf minicom.tar $(SRCS) $(HDRS) $(OTHERS)
		@echo Compressing..
		@rm -f minicom.tar.Z
		@compress -b13 minicom.tar
		@rm -f minicom.tar

lzh:
		@echo Creating lharc archive..
		@rm -f minicom.lzh
		@lharc a minicom.lzh $(SRCS) $(HDRS) $(OTHERS)

${BINDIR}/minicom: minicom
		@echo Installing minicom in ${BINDIR}
		@cp minicom ${BINDIR}
		@chown root ${BINDIR}/minicom
		@chgrp bin ${BINDIR}/minicom
		@chmod 4755 ${BINDIR}/minicom

${BINDIR}/runscript: runscript
		@echo Installing runscript in ${BINDIR}
		@cp runscript ${BINDIR}/runscript
		@chown bin ${BINDIR}/runscript
		@chgrp bin ${BINDIR}/runscript
		@chmod 755 ${BINDIR}/runscript

${ETCDIR}/keyserv: keyserv
		@echo Installing keyserv in ${ETCDIR}
		@cp keyserv ${ETCDIR}
		@chown bin ${ETCDIR}/keyserv
		@chgrp bin ${ETCDIR}/keyserv
		@chmod 755 ${ETCDIR}/keyserv

install:	${BINDIR}/minicom ${BINDIR}/runscript ${ETCDIR}/keyserv
		@echo Minicom has been installed.
		@echo You must now run \"minicom -s\" to adapt minicom to
		@echo your system.
		@echo You may also want to create a \"minicom.users\" file \
in ${ETCDIR}.

clean:
		rm -f *.$(O) minicom keyserv runscript

