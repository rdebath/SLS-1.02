/*
 * Start the terminal dialogue, fork the input routine, watch for the
 * hot key so we can execute an option.
 */

#include <stdio.h>
#include <curses.h>
#include <signal.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "modem.h"
#include "param.h"
#include "status.h"
#include "xmodem.h"

#ifdef BSD
#include <sys/file.h>
#else /* BSD */
#include <fcntl.h>
#endif /* BSD */

#ifdef UNIXPC
#include <sys/phone.h>
#endif /* UNIXPC */

static int pid = -1;

terminal(extra_dir, input_status)
char *extra_dir;
int input_status;
{
	extern int fd;
	int i, j, k, cr_lf, script;
	char c, lf=10, *str_rep(), *keymac, *memset();
	void help_screen(), line_set(), n_shell(), load_vs(), send_str();
	void release_port(), do_input(), list_dir(), pexit(), zap_vs();
	void st_line(), chg_dir(), screen_dump(), input_off(), suspend();
	void info(), term_mode(), macro(), do_script();

					/* if starting out in command mode */
	if (!input_status) {
		erase();
		refresh();
		st_line("");
	}
					/* put stdin/stdout in terminal mode */
	resetterm();
	term_mode();
	cr_lf = !strcmp(param->cr_out, "CR/LF");

	if (input_status) {
		do_script(extra_dir);
		do_input();
	}

	while (1) {
		read(0, &c, 1);
		c &= 0x7f;
					/* is it the hot key? */
		if (c == param->hot) {
					/* suspend input */
			input_status = 0;
			suspend(TRUE);
			script = 0;

			/*
			 * Put the terminal in the curses mode, load the
			 * virtual screen and add the status line at the bottom.
			 */
			fixterm();
			load_vs();
			st_line("");
#ifndef OLDCURSES
			keypad(stdscr, TRUE);
#endif /* OLDCURSES */
			i = wgetch(stdscr);
					/* map an additional hot key to -1 */
			if (i == param->hot)
				i = -1;

			keymac = "";
					/* look for options */
			k = -1;
			switch (i) {
				case -1:	/* 2 "hots" means send 1 */
					k = param->hot;
					break;
				case '0':	/* help screen */
					help_screen(param->ascii_hot);
					break;
				case 'd':
				case 'D':	/* dialing directory */
					if (dial_menu())
						input_status = dial_win();
					script = input_status;
					break;
				case 'r':
				case 'R':	/* redial */
					if (redial())
						input_status = dial_win();
					script = input_status;
					break;
				case 'm':
				case 'M':	/* keyboard macros */
					macro();
					break;
				case 'p':
				case 'P':	/* line settings */
					if (ls_menu())
						line_set();
					break;
				case 'x':
				case 'X':	/* exit */
					pexit();
					break;
				case '4':	/* Unix gateway */
					n_shell();
					break;
				case 'i':
				case 'I':	/* program info screen */
					info(MANUAL_CLEAR);
					break;
				case 's':	/* setup menu */
				case 'S':
					input_status = setup_menu();
					break;
				case 'c':	/* clear the screen */
				case 'C':
					zap_vs();
					erase();
#ifdef SHAREDMEM
					if (pid == -1) {
						for (j=0; j<LINES; j++)
							memset(status->vs[j], ' ', COLS);
					}
#endif /* SHAREDMEM */
					break;
				case 'b':
				case 'B':	/* change directory */
					chg_dir();
					break;
				case 'e':
				case 'E':	/* toggle duplex */
					if (dir->duplex[dir->d_cur] == 'F')
						dir->duplex[dir->d_cur] = 'H';
					else
						dir->duplex[dir->d_cur] = 'F';

						/* show changes */
					st_line("");
					k = wait_key(stdscr, 2);
					break;
				case 'h':
				case 'H':	/* hang up phone */
					release_port(VERBOSE);
					input_off();
					break;
				case 'l':
				case 'L':	/* toggle printer */
					status->print = status->print ? 0 : 1;
#ifndef SHAREDMEM
					if (pid != -1)
						kill(pid, SIGUSR2);
#endif /* SHAREDMEM */
						/* show changes */
					st_line("");
					k = wait_key(stdscr, 2);
					break;
				case '3':	/* toggle CR - CR/LF */
					if (!strcmp(param->cr_in, "CR")) {
						param->cr_in = str_rep(param->cr_in, "CR/LF");
						status->add_lf = 1;
					}
					else {
						param->cr_in = str_rep(param->cr_in, "CR");
						status->add_lf = 0;
					}
#ifndef SHAREDMEM
					input_off();
					input_status++;
#endif /* SHAREDMEM */
						/* show changes */
					st_line("");
					k = wait_key(stdscr, 2);
					break;
				case '7':	/* break key */
					if (fd != -1)
						tty_break(fd);

					st_line("   break");
					break;
#ifndef OLDCURSES
				case KEY_UP:
#endif /* OLDCURSES */
				case 'u':
				case 'U':	/* send files */
					input_status = xfer_menu(UP_LOAD);
					break;
#ifndef OLDCURSES
				case KEY_DOWN:
				case '\n':
#endif /* OLDCURSES */
				case 'n':
				case 'N':	/* receive files */
					input_status = xfer_menu(DOWN_LOAD);
					break;
				case 't':
				case 'T':
					input_status = pass_thru();
					break;
				case 'f':
				case 'F':	/* list directory */
					list_dir();
					break;
				case 'g':	/* screen dump */
				case 'G':
					screen_dump();
					st_line(" screen dump");
					k = wait_key(stdscr, 2);
					break;
				case '1':	/* data logging */
					input_status = data_logging();
					break;
				case '2':	/* toggle log */
					if (!strcmp(status->log_path, "NOT_DEFINED")) {
						beep();
						st_line(" no log file");
						k = wait_key(stdscr, 2);
						break;
					}
					status->log = status->log ? 0 : 1;
#ifndef SHAREDMEM
					if (pid != -1)
						kill(pid, SIGUSR1);
#endif /* SHAREDMEM */
						/* show changes */
					st_line("");
					k = wait_key(stdscr, 2);
					break;
				/*
				 * The following are the keyboard macros
				 * corresponding to the shifted number keys.
				 * (Too many keys... [control] [A] [shift] [1]
				 * is hardly a shortcut!)
				 */
				case '!':
					keymac = param->mac_1;
					break;
				case '@':
					keymac = param->mac_2;
					break;
				case '#':
					keymac = param->mac_3;
					break;
				case '$':
					keymac = param->mac_4;
					break;
				case '%':
					keymac = param->mac_5;
					break;
				case '^':
					keymac = param->mac_6;
					break;
				case '&':
					keymac = param->mac_7;
					break;
				case '*':
					keymac = param->mac_8;
					break;
				case '(':
					keymac = param->mac_9;
					break;
				case ')':
					keymac = param->mac_0;
					break;
				default:
					fputc(BEL, stderr);
					break;
			}

			/*
			 * Repaint the stdscr (if we are already talking),
			 * get the stdin/stdout out of the curses mode and
			 * into the terminal mode.
			 */
			if (fd != -1) {
				touchwin(stdscr);
				refresh();
			}
			resetterm();
			term_mode();

			/*
			 * Some of the output processing options have to be
			 * faked...  Unfortunately, adding a LF to CR on
			 * output is one of them.
			 */
			cr_lf = !strcmp(param->cr_out, "CR/LF");

					/* run the auto-login script */
			if (script)
				do_script(extra_dir);

					/* re-start input routine */
			if (input_status)
				do_input();
			else
				suspend(FALSE);

					/* send the macro */
			if (*keymac != '\0')
				send_str(keymac, FAST);
			/*
			 * If you pressed a key during one of the sleeping
			 * periods (typically the delay to see the status
			 * line change), let the keyboard value fall thru
			 * to the write() below.
			 */
			if (k == -1)
				continue;
			c = k;
		}
					/* ignore errors if fd == -1 */
		write(fd, &c, 1);
					/* map cr to cr_lf? */
		if (c == '\r' && cr_lf)
			write(fd, &lf, 1);
	}
}

/*
 * Fire up the input routine...
 */

void
do_input()
{
	extern int fd;
	void error_win();
	char first[(sizeof(int)*8)+1];
#ifdef SHAREDMEM
	extern int shm_id;
#else /* SHAREDMEM */
	char add_lf[2], log[2], print[2], dup_fd[3];
#endif /* SHAREDMEM */
					/* if no TTY, or already on */
	if (pid != -1 || fd == -1)
		return;

	status->fd = fd;
	status->add_lf = !strcmp(param->cr_in, "CR/LF");

#ifdef SHAREDMEM
	sprintf(first, "%d", shm_id);
#else /* SHAREDMEM */
	sprintf(first, "%d", status->fd);
	sprintf(dup_fd, "%d", status->dup_fd);
	sprintf(add_lf, "%d", status->add_lf);
	sprintf(log, "%d", status->log);
	sprintf(print, "%d", status->print);
#endif /* SHAREDMEM */

					/* fork the input routine */
	if (!(pid = fork())) {
#ifdef BSD
		setpgrp(0, getpid());
#else /* BSD */
		setpgrp();
#endif /* BSD */

#ifdef SETUGID
		setuid(getuid());
		setgid(getgid());
#endif /* SETUGID */

#ifdef SHAREDMEM
		execlp("pcomm_input", "pcomm_input", first, (char *) 0);
#else /* SHAREDMEM */
		execlp("pcomm_input", "pcomm_input", first, dup_fd, add_lf,
		 log, print, status->log_path, status->vs_path, (char *) 0);
#endif /* SHAREDMEM */
		error_win(1, "Cannot find (or execute) the 'pcomm_input' program", "");
	}

	return;
}

/*
 * shut it down...
 */

void
input_off()
{
	SIG_TYPE (*cstat)();

	if (pid != -1) {
		/*
		 * This serves to periodically clean up the process table
		 */
		cstat = signal(SIGCLD, SIG_IGN);
		kill(pid, SIGTERM);
		pid = -1;
		signal(SIGCLD, cstat);
	}
	return;
}

/*
 * Hang up the phone but remain in the Pcomm command state.  Uses the
 * hang_up string only, does *not* drop the DTR!
 */

void
hang_up(verbose)
int verbose;
{
	extern int fd;
	void send_str(), st_line(), line_set();
	char buf[80], *strcpy(), *ttyname();
	unsigned int sleep();
					/* sanity checking */
	if (modem == NULL)
		return;
					/* anything to hang up? */
	if (modem->m_cur == -1 || fd == -1)
		return;

	if (verbose)
		st_line("disconnecting");
					/* special case for OBM */
	if (!strcmp(modem->mname[modem->m_cur], "OBM")) {
#ifdef UNIXPC
		ioctl(fd, PIOCDISC);
		/*
		 * The PIOCDISC ioctl screws up the file descriptor!!!
		 * No other phone(7) ioctl can fix it.  Whatever it does,
		 * it seems to escape detection with PIOCGETA and TCGETA.
		 * The best I can do is close the port and start over.
		 */
		strcpy(buf, ttyname(fd));
		close(fd);
		fd = open(buf, O_RDWR|O_NDELAY);
		line_set();
		fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) & ~O_NDELAY);
#endif /* UNIXPC */
	}
	else {
		send_str(modem->hang_up[modem->m_cur], SLOW);

		/*
		 * Some modems do "damage" to the tty driver when they hang
		 * up by flashing the modem control lines on the port.  The
		 * following is some witchcraft designed to put the driver
		 * back the way it was.
		 */
		sleep(1);
		close(open(ttyname(fd), O_RDWR));
	}

	if (verbose)
		st_line("");
	return;
}

/*
 * Suspend or un-suspend the input routine.  The argument is used in
 * non-shared memory configurations to give the vs_path file a fighting
 * chance of being written to disk before load_vs() reads it.
 */

/* ARGSUSED */
void
suspend(on)
int on;
{
	unsigned int sleep();

	if (pid == -1)
		return;
	kill(pid, SIGINT);

#ifndef SHAREDMEM
	if (on)
		sleep(1);
#endif /* SHAREDMEM */

	return;
}
