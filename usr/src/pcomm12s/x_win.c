/*
 * Display the file transfer window, and invoke the transfer protocol.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "xmodem.h"

void
xfer_win(list, up, type)
char *list;
int up, type;
{
	extern int fd;
	WINDOW *xf_win, *newwin();
	int ack_error, fast;
	void xmodem_mode(), input_off(), line_set(), st_line();

	touchwin(stdscr);
	refresh();
	st_line("");

	xf_win = newwin(15, 44, 2, 30);
	/*
	 * This window should be in the non-blocking mode, so we can
	 * scan the keyboard for input while transferring a file.
	 */
	nodelay(xf_win, TRUE);
					/* basic window stuff */
	mvwaddstr(xf_win, 2, 14, "Protocol:");
	mvwaddstr(xf_win, 3, 13, "File name:");
	mvwaddstr(xf_win, 4, 13, "File size:");
	mvwaddstr(xf_win, 5, 4, "Error check method:");
	mvwaddstr(xf_win, 6, 5, "Est transfer time:");
	mvwaddstr(xf_win, 7, 11, "Block count:");
	mvwaddstr(xf_win, 8, 6, "Percent complete:");
	mvwaddstr(xf_win, 9, 5, "Bytes transferred:");
	mvwaddstr(xf_win, 10, 5, "Errors this block:");
	mvwaddstr(xf_win, 11, 5, "Total error count:");
	mvwaddstr(xf_win, 12, 10, "Last message: NONE");
	box(xf_win, VERT, HORZ);

	if (up)
		mvwattrstr(xf_win, 0, 17, A_BOLD, " Uploading ");
	else
		mvwattrstr(xf_win, 0, 16, A_BOLD, " Downloading ");

	mvwaddstr(xf_win, 14, 11, " Press <ESC> to abort ");
	wrefresh(xf_win);
					/* fix up the terminal mode */
	input_off();
	xmodem_mode(fd);

	/*
	 * Is your terminal slower than the xfer baud rate?  For example:
	 * I'm at home with my PC and 1200 baud modem; I call my system
	 * at work so I can use their 2400 baud modems to call some other
	 * system.  In this case, I don't wanna spend too much time updating
	 * my screen at 1200 baud, when I'm transferring the file at 2400 baud.
	 */
	fast = 0;

	if (my_speed() >= dir->baud[dir->d_cur])
		fast++;

	if (up)
		ack_error = send_xmodem(xf_win, list, type, fast);
	else
		ack_error = rcv_xmodem(xf_win, list, type, fast);

	nodelay(xf_win, FALSE);
					/* prompt for a key on errors */
	if (ack_error) {
		beep();
		clear_line(xf_win, 13, 9, TRUE);
		wattrstr(xf_win, A_BOLD, "Press any key to continue");
		wrefresh(xf_win);
		wgetch(xf_win);
	}
	werase(xf_win);
	wrefresh(xf_win);
	delwin(xf_win);
					/* undo what xmodem_mode() did */
	line_set();
	return;
}
