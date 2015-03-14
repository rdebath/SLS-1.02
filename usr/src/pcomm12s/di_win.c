/*
 * The dialing window routines.
 */

#define MAX_PASS	25

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "modem.h"
#include "param.h"

/*
 * The dialing window.  Its job is to kill the input routine, get a port,
 * cycle thru the entries in the queue, while interpreting both the
 * user's requests and the modem's responses.  A non-zero return code
 * means we're ready to fire up the input routine.
 */

int
dial_win()
{
	extern int rc_index, fd;
	WINDOW *di_win, *newwin();
	int i, j, key, want_out, pass, tic, baud;
	long now, time();
	char *tbuf, *ctime(), *str, cr=13, *read_codes();
	void disp_queue(), dial_it(), delay_times(), input_off();
	void error_win(), line_set(), hang_up(), zap_vs(), log_calls();
	void st_line();
	unsigned int sleep();
					/* are we already talking? */
	input_off();
	hang_up(VERBOSE);

	touchwin(stdscr);
	refresh();

	if (get_port())
		return(0);
	/*
	 * If the phone number points to NULL, then either you're on a
	 * direct line, or you want to do the dialing yourself.
	 */
	if (*dir->number[dir->q_num[0]] == '\0') {
					/* check LD permission */
		if (limit_ld(0))
			return(0);
					/* can't talk directly to OBM */
		if (!strcmp(modem->mname[modem->m_cur], "OBM")) {
			error_win(0, "Can't access the On Board Modem directly",
			 "You must use the automatic dialing feature");
			return(0);
		}

		zap_vs();
		touchwin(stdscr);
		clear();
		printw("Connected to /dev/%s at %d baud...\n", modem->tty[modem->t_cur], dir->baud[dir->d_cur]);
		refresh();
		return(1);
	}

	di_win = newwin(17, 70, 3, 5);
					/* the basic window */
	mvwattrstr(di_win, 1, 20, A_BOLD, "D I A L I N G       W I N D O W");
	horizontal(di_win, 2, 0, 70);
	mvwaddstr(di_win, 4, 23, "System name:");
	mvwaddstr(di_win, 5, 23, "Pass number:");
	mvwaddstr(di_win, 6, 14, "Elapse time this try:");
	mvwaddstr(di_win, 7, 13, "Time at start of dial:");
	mvwaddstr(di_win, 8, 9, "Time at start of this try:");
	mvwaddstr(di_win, 9, 16, "Connect delay time:");
	mvwaddstr(di_win, 10, 17, "Redial delay time:");
	mvwaddstr(di_win, 11, 24, "Script/TTY:");
	mvwaddstr(di_win, 12, 16, "Result of last try:");

	mvwaddstr(di_win, 14, 3, "<SPACE>: Recycle");
	mvwaddstr(di_win, 14, 22, "<DEL>: Remove from queue");
	mvwaddstr(di_win, 14, 49, "E: Change delays");

					/* the start time */
	time(&now);
	tbuf = ctime(&now);
	tbuf[19] = '\0';
	mvwaddstr(di_win, 7, 36, &tbuf[11]);

	mvwprintw(di_win, 9, 36, "%-4d", param->c_delay);
	mvwprintw(di_win, 10, 36, "%-4d", param->r_delay);

	box(di_win, VERT, HORZ);
	mvwaddstr(di_win, 16, 24, " Press <ESC> to abort ");

	pass = 0;
	i = 0;
	want_out = 0;
	while (!want_out && pass <= MAX_PASS) {
		key = -1;
		pass++;
					/* update the d_cur variable */
		dir->d_cur = dir->q_num[i];
					/* check LD permission */
		if (limit_ld(i)) {
			want_out++;
			break;
		}
					/* get a port */
		if (get_port()) {
			want_out++;
			break;
		}
					/* fill in the window */
		disp_queue(di_win, dir->d_cur, pass);

		/*
		 * The actual dial routine.  The "i" is the index into the
		 * queue, not the entry number.  Returns immediately without
		 * waiting for a carrier.
		 */
		dial_it(i);
		tty_flush(fd, 0);

		/*
		 * Here we do a time-slice between reading the result codes
		 * from the modem and reading the keyboard.  The one second
		 * granularity won't be too accurate, but who cares?
		 */
		tic = 0;
		rc_index = 0;
		while (tic < param->c_delay) {
			if ((str = read_codes()) == NULL) {
				mvwprintw(di_win, 6, 36, "%-4d", ++tic);
				wrefresh(di_win);
			}
			else {
				/*
				 * A return code that converts to an number
				 * that is less than 300 is probably an error
				 * message.
				 */
				baud = atoi(str);
				if (baud < 300) {
					mvwprintw(di_win, 12, 36, "%-20.20s", str);
					wmove(di_win, 12, 36);
					wrefresh(di_win);
					break;
				}
					/* we're connected */
				beep();
				clear_line(di_win, 12, 36, TRUE);
				wattrstr(di_win, A_BLINK, "CONNECTED");
				wmove(di_win, 12, 36);
				wrefresh(di_win);
				wait_key(di_win, 2);
				delwin(di_win);

				/*
				 * Did the modem sync at a different baud
				 * rate than what we expected?
				 */
				if (dir->baud[dir->d_cur] != baud) {
					if (can_sync(baud)) {
						dir->baud[dir->d_cur] = baud;
						line_set();
					}
				}

				zap_vs();
				touchwin(stdscr);
				clear();
				printw("Connected to %s at %d baud...\n",
				 dir->name[dir->d_cur], dir->baud[dir->d_cur]);
				refresh();

					/* log the call */
				log_calls(i);
				return(1);
			}
			if (tic == param->c_delay)
				break;
					/* ok... try the keyboard */
			if ((key = wait_key(di_win, 1)) != -1)
				break;

			mvwprintw(di_win, 6, 36, "%-4d", ++tic);
			wrefresh(di_win);
		}
		/*
		 * If the modem did not return a code, then we need to
		 * stop it.  Sending a CR will stop most modems cold,
		 * except of course for the OBM...
		 */
		if (str == NULL) {
			if (!strcmp(modem->mname[modem->m_cur], "OBM"))
				hang_up(QUIET);
			else
				write(fd, &cr, 1);
			sleep(2);
		}
					/* if we get here, no key was pressed */
		if (key == -1) {
			clear_line(di_win, 6, 14, TRUE);
			mvwaddstr(di_win, 6, 27, "Pausing:");
					/* no return code? */
			if (str == NULL) {
				clear_line(di_win, 12, 36, TRUE);
				waddstr(di_win, "TIMED OUT");
				wmove(di_win, 12, 36);
			}
					/* do the pause */
			tic = 0;
			while (tic < param->r_delay) {
				if ((key = wait_key(di_win, 1)) != -1)
					break;
				mvwprintw(di_win, 6, 36, "%-4d", ++tic);
				wrefresh(di_win);
			}
			clear_line(di_win, 6, 14, TRUE);
			waddstr(di_win, "Elapse time this try:");
		}
		mvwaddstr(di_win, 6, 36, "0   ");
					/* Process the keystroke */
		switch (key) {
			case ' ':	/* next in the queue */
				clear_line(di_win, 12, 36, TRUE);
				waddstr(di_win, "RECYCLED");
				wmove(di_win, 12, 36);
				wrefresh(di_win);
				/* fall thru... */
			case -1:	/* no key was pressed */
				i++;
				if (i > NUM_QUEUE)
					i = 0;
				if (dir->q_num[i] == -1)
					i = 0;
				break;
			case DEL:	/* <DEL> key, remove from queue */
				if (dir->q_num[1] == -1) {
					beep();
					clear_line(di_win, 12, 36, TRUE);
					waddstr(di_win, "NO MORE ENTRIES");
					wmove(di_win, 12, 36);
					wrefresh(di_win);
					wait_key(di_win, 3);
					break;
				}
				clear_line(di_win, 12, 36, TRUE);
				waddstr(di_win, "ENTRY DELETED");
				wmove(di_win, 12, 36);
				wrefresh(di_win);
				wait_key(di_win, 3);

					/* compact the queue */
				for (j=i; j<NUM_QUEUE-1; j++)
					dir->q_num[j] = dir->q_num[j+1];
				dir->q_num[NUM_QUEUE-1] = -1;

				if (dir->q_num[i] == -1)
					i = 0;
				break;
			case 'e':
			case 'E':	/* change delay time */
				delay_times();
				touchwin(di_win);
				mvwprintw(di_win, 9, 36, "%-4d", param->c_delay);
				mvwprintw(di_win, 10, 36, "%-4d", param->r_delay);
				break;
			case ESC:	/* <ESC> key */
				beep();
				clear_line(di_win, 12, 36, TRUE);
				wattrstr(di_win, A_BLINK, "DIAL ABORTED");
				wmove(di_win, 12, 36);
				wrefresh(di_win);
				wait_key(di_win, 3);
				want_out++;
				break;
			default:
				beep();
				break;
		}
	}
					/* clean up and go home */
	werase(di_win);
	wrefresh(di_win);
	delwin(di_win);
	if (!want_out)
		error_win(0, "Exceeded the maximum number number of dialing attempts", "");
	return(0);
}

/*
 * Display what info we know at this time.
 */

static void
disp_queue(win, entry, pass)
WINDOW *win;
int entry, pass;
{
	long now, time();
	char *tbuf, *ctime();
	void st_line();
					/* redo the status line */
	st_line("");
					/* system name */
	clear_line(win, 4, 36, TRUE);
	waddstr(win, dir->name[entry]);
					/* pass number */
	mvwprintw(win, 5, 36, "%-4d", pass);
					/* time of this call */
	time(&now);
	tbuf = ctime(&now);
	tbuf[19] = '\0';
	mvwaddstr(win, 8, 36, &tbuf[11]);
					/* the script field */
	clear_line(win, 11, 36, TRUE);
	waddstr(win, dir->script[entry]);

	wmove(win, 12, 36);
	wrefresh(win);
	return;
}

/*
 * Determine if the modem can detect the synchronization of the connected
 * baud rate.  We check the modem database and see if the connect string
 * is unique.  A non-zero return code means the modem can sync.
 */

static int
can_sync(baud)
int baud;
{
	int i;
	char *str;
					/* feature disabled? */
	if (modem->auto_baud[modem->m_cur] != 'Y')
		return(0);
					/* re-construct the string */
	switch (baud) {
		case 300:
			str = modem->con_3[modem->m_cur];
			break;
		case 1200:
			str = modem->con_12[modem->m_cur];
			break;
		case 2400:
			str = modem->con_24[modem->m_cur];
			break;
		case 4800:
			str = modem->con_48[modem->m_cur];
			break;
		case 9600:
			str = modem->con_96[modem->m_cur];
			break;
		case 19200:
			str = modem->con_192[modem->m_cur];
			break;
		default:
			return(0);
	}

	if (*str == '\0')
		return(0);
					/* test "str" against all others */
	i = 0;
	if (!strcmp(str, modem->con_3[modem->m_cur]))
		i++;
	if (!strcmp(str, modem->con_12[modem->m_cur]))
		i++;
	if (!strcmp(str, modem->con_24[modem->m_cur]))
		i++;
	if (!strcmp(str, modem->con_48[modem->m_cur]))
		i++;
	if (!strcmp(str, modem->con_96[modem->m_cur]))
		i++;
	if (!strcmp(str, modem->con_192[modem->m_cur]))
		i++;
					/* should match only itself */
	if (i == 1)
		return(1);
	return(0);
}
