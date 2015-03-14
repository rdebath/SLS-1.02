#				PCOMM
#			for a generic Unix box

#for Linux version
CURSES = -lcurses -ltermcap
TERMLIB = -ltermcap
BSD = # tty_ucb.o
TTY = tty_att.o
BIN = /usr/local/bin
DEFAULT_DIR = /usr/local/lib/pcomm
CFLAGS = -O -DDEFAULT_DIR=\"$(DEFAULT_DIR)\"
LDFLAGS =

#for 80286 versions of SCO Xenix
#CFLAGS = -Od -DM_TERMINFO -Mle2 -LARGE
#LDFLAGS = -SEG 1000 -F 5000 -Mle2
#CURSES = -ltinfo -lx
#TERMLIB = -ltinfo -lx

#for 80286 versions of Microsoft Xenix 3.0/3.5
#CFLAGS = -O -Mme2 -Dresetterm=xresetterm -DXENIX_3
#LDFLAGS = -s -Mm -F 5120

#CFLAGS =
#LDFLAGS =
#LDFLAGS = -s
SHAR = shar -a
#BIN = /usr/local/bin

#for old curses(3) or Berkeley systems
#CURSES = -lcurses -ltermcap
#TERMLIB = -ltermcap
#CURSES = -lcurses
#TERMLIB = -lcurses

#for systems without getcwd(3) or getopt(3)
#GETCWD = getcwd.o
#GETOPT = getopt.o

#for System V or Berkeley worlds
#BSD = -DBSD
#TTY = tty_ucb.o
#BSD =
#TTY = tty_att.o

PCOMM = $(GETCWD) $(GETOPT) $(TTY) admin.o chg_dir.o curses.o d_delete.o \
	d_lib.o d_manual.o d_menu.o d_print.o d_prompt.o d_revise.o \
	data_log.o di_delay.o di_win.o dial.o e_lib.o expand.o help.o \
	info.o init.o list_dir.o ls_menu.o m_lib.o macro.o main.o \
	n_shell.o p_lib.o passthru.o pexit.o port.o redial.o s_axfer.o \
	s_extrnl.o s_gen.o s_menu.o s_modem.o s_prompt.o s_term.o \
	s_tty.o screen.o script.o st_line.o strings.o terminal.o x_ascii.o \
	x_batch.o x_extrnl.o x_menu.o x_rcv.o x_send.o x_win.o xmodem.o

INPUT =	input.o vcs.o

all:	pcomm pcomm_input waitfor matches modem_break

pcomm:	$(PCOMM)
	$(CC) $(LDFLAGS) $(PCOMM) -o pcomm $(CURSES)

pcomm_input:	$(INPUT)
	$(CC) $(LDFLAGS) $(INPUT) -o pcomm_input $(TERMLIB)

waitfor:
	$(CC) $(CFLAGS) $(BSD) $(LDFLAGS) waitfor.c -o waitfor

matches:
	$(CC) $(CFLAGS) $(LDFLAGS) matches.c -o matches

modem_break:
	$(CC) $(CFLAGS) $(BSD) $(LDFLAGS) modem_break.c -o modem_break

install:	all
	cp pcomm $(BIN)
	cp pcomm_input $(BIN)
	cp waitfor $(BIN)
	cp matches $(BIN)
	cp modem_break $(BIN)

install_config:
	-mkdir $(DEFAULT_DIR)
	cp ./Config/* $(DEFAULT_DIR)

clean:
	-rm pcomm pcomm_input waitfor matches modem_break *.o

lint:
	lint -p -Dlint admin.c chg_dir.c curses.c d_delete.c d_lib.c \
	d_manual.c d_menu.c d_print.c d_prompt.c d_revise.c data_log.c \
	di_delay.c di_win.c dial.c e_lib.c expand.c getcwd.c getopt.c \
	help.c info.c init.c list_dir.c ls_menu.c m_lib.c macro.c main.c \
	n_shell.c p_lib.c passthru.c pexit.c port.c redial.c s_axfer.c \
	s_extrnl.c s_gen.c s_menu.c s_modem.c s_prompt.c s_term.c s_tty.c \
	screen.c script.c st_line.c strings.c terminal.c tty_att.c \
	x_ascii.c x_batch.c x_extrnl.c x_menu.c x_rcv.c x_send.c x_win.c \
	xmodem.c
	lint -p -Dlint input.c vcs.c
	lint -p -Dlint $(BSD) waitfor.c
	lint -p -Dlint matches.c
	lint -p -Dlint $(BSD) modem_break.c

shar:
	$(SHAR) Doc > pcomm_sh.1
	$(SHAR) Makefile Matches.1 Modem_break.1 Pcomm.1 Pcomm.dial_dir \
	Pcomm.extrnl Pcomm.modem Pcomm.param Readme Release.notes Sample \
	Unixpc.shar Waitfor.1 > pcomm_sh.2
	$(SHAR) admin.c chg_dir.c config.h curses.c d_delete.c d_lib.c \
	d_manual.c d_menu.c d_print.c d_prompt.c d_revise.c data_log.c \
	di_delay.c > pcomm_sh.3
	$(SHAR) di_win.c dial.c dial_dir.h e_lib.c expand.c extrnl.h \
	getcwd.c getopt.c help.c info.c init.c input.c list_dir.c \
	ls_menu.c > pcomm_sh.4
	$(SHAR) m_lib.c macro.c main.c matches.c misc.h modem.h \
	modem_break.c n_shell.c p_lib.c param.h passthru.c pexit.c \
	port.c > pcomm_sh.5
	$(SHAR) redial.c s_axfer.c s_extrnl.c s_gen.c s_menu.c s_modem.c \
	s_prompt.c s_term.c s_tty.c screen.c script.c st_line.c status.h \
	strings.c > pcomm_sh.6
	$(SHAR) terminal.c tty_att.c tty_ucb.c vcs.c vcs.h waitfor.c \
	x_ascii.c x_batch.c x_extrnl.c > pcomm_sh.7
	$(SHAR) x_menu.c x_rcv.c x_send.c x_win.c xmodem.c xmodem.h \
	> pcomm_sh.8

admin.o:	config.h dial_dir.h param.h
chg_dir.o:	config.h misc.h
curses.o:	config.h misc.h
d_delete.o:	config.h dial_dir.h misc.h param.h
d_lib.o:	dial_dir.h param.h
d_manual.o:	config.h dial_dir.h misc.h
d_menu.o:	config.h dial_dir.h misc.h param.h
d_print.o:	config.h dial_dir.h misc.h
d_prompt.o:	config.h dial_dir.h misc.h
d_revise.o:	config.h dial_dir.h misc.h param.h
data_log.o:	config.h misc.h param.h status.h
di_delay.o:	config.h misc.h param.h
di_win.o:	config.h dial_dir.h misc.h modem.h param.h
dial.o:		config.h dial_dir.h misc.h modem.h param.h
e_lib.o:	extrnl.h
expand.o:	config.h
help.o:		config.h misc.h
init.o:		config.h misc.h status.h
input.o:	config.h misc.h status.h vcs.h
ls_menu.o:	config.h dial_dir.h misc.h param.h
m_lib.o:	modem.h
macro.o:	config.h misc.h param.h
main.o:		config.h dial_dir.h extrnl.h misc.h modem.h param.h status.h
n_shell.o:	config.h
p_lib.o:	param.h
passthru.o:	config.h misc.h
pexit.o:	config.h dial_dir.h misc.h param.h status.h
port.o:		config.h dial_dir.h modem.h
redial.o:	config.h dial_dir.h misc.h
s_axfer.o:	config.h misc.h param.h
s_extrnl.o:	config.h extrnl.h misc.h
s_gen.o:	config.h misc.h param.h
s_menu.o:	config.h misc.h
s_modem.o:	config.h misc.h modem.h
s_prompt.o:	config.h misc.h
s_term.o:	config.h misc.h param.h status.h
s_tty.o:	config.h misc.h modem.h
screen.o:	config.h param.h status.h
script.o:	config.h dial_dir.h status.h
st_line.o:	config.h dial_dir.h misc.h modem.h param.h status.h
strings.o:	config.h
terminal.o:	config.h dial_dir.h misc.h modem.h param.h status.h xmodem.h
tty_att.o:	dial_dir.h param.h
tty_ucb.o:	dial_dir.h param.h
vcs.o:		config.h vcs.h
x_ascii.o:	config.h misc.h param.h
x_batch.o:	config.h misc.h xmodem.h
x_extrnl.o:	config.h
x_menu.o:	config.h extrnl.h misc.h xmodem.h
x_rcv.o:	config.h dial_dir.h misc.h xmodem.h
x_send.o:	config.h dial_dir.h misc.h xmodem.h
x_win.o:	config.h dial_dir.h misc.h xmodem.h
xmodem.o:	config.h misc.h param.h xmodem.h

waitfor:	waitfor.c
matches:	matches.c
modem_break:	modem_break.c
