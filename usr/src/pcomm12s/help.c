/*
 * Display the help screen.  Press any key to continue.  If the ascii_hot
 * string is more than 4 characters wide, this screen will look silly.
 * Maybe one day, this will also contain full page descriptions of each
 * command.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"

void
help_screen(hot)
char *hot;
{
	extern int fd;
	WINDOW *h_win, *newwin();

	h_win = newwin(17, 80, 0, 0);

	mvwattrstr(h_win, 1, 29, A_BOLD, "P C O M M       H E L P\n");
	horizontal(h_win, 2, 0, 80);
	mvwattrstr(h_win, 4, 0, A_BOLD, "       Major Functions          Utility Functions         File Functions\n\n");
#ifdef OLDCURSES
	mvwprintw(h_win,  6,  2, "Dialing Directory.%4.4s-D  Program Info ....%4.4s-I  Send Files ....%4.4s-U", hot, hot, hot);
	mvwprintw(h_win,  7,  2, "Auto Redial ......%4.4s-R  Setup Screen ....%4.4s-S  Receive Files .%4.4s-N", hot, hot, hot);
#else /* OLDCURSES */
	mvwprintw(h_win,  6,  2, "Dialing Directory.%4.4s-D  Program Info ....%4.4s-I  Send Files ....%4.4s-<up>", hot, hot, hot);
	mvwprintw(h_win,  7,  2, "Auto Redial ......%4.4s-R  Setup Screen ....%4.4s-S  Receive Files .%4.4s-<down>", hot, hot, hot);
#endif /* OLDCURSES */
	mvwprintw(h_win,  8,  2, "Keyboard Macros ..%4.4s-M  Change Directory.%4.4s-B  Pass Thru Mode.%4.4s-T", hot, hot, hot);
	mvwprintw(h_win,  9,  2, "Line Settings ....%4.4s-P  Clear Screen ....%4.4s-C  Directory .....%4.4s-F", hot, hot, hot);
	mvwprintw(h_win, 10,  2, "Exit Pcomm .......%4.4s-X  Toggle Duplex ...%4.4s-E  Screen Dump ...%4.4s-G", hot, hot, hot);
	mvwprintw(h_win, 11,  2, "Unix Gateway .....%4.4s-4  Hang Up Phone ...%4.4s-H  Start Data Log.%4.4s-1", hot, hot, hot);
	mvwprintw(h_win, 12, 28, "Printer On/Off ..%4.4s-L  Toggle Log ....%4.4s-2", hot, hot);
	mvwprintw(h_win, 13, 28, "Toggle CR-CR/LF .%4.4s-3", hot);
	mvwprintw(h_win, 14, 28, "Break Key .......%4.4s-7", hot);

	box(h_win, VERT, HORZ);
	mvwaddstr(h_win, 16, 26, " Press any key to continue ");
	wrefresh(h_win);

	wgetch(h_win);
	if (fd == -1) {
		werase(h_win);
		wrefresh(h_win);
	}
	delwin(h_win);
	return;
}
