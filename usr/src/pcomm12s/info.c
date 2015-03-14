/*
 * Display the initial welcome screen (to include all of the proper
 * acknowledgements).  Press any key to continue.
 */

#define VERSION	"1.2.4"
#define DATE	"23 Jun 89"

#include <stdio.h>
#include <curses.h>

void
info(auto_clear)
int auto_clear;
{
	extern int fd;
	WINDOW *w_win, *newwin();
	char buf[80];
					/* display the welcome screen */
	w_win = newwin(23, 80, 0, 0);
	mvwaddstr(w_win, 3, 18, "PPPPPP    CCCC    OOOO    MM   MM   MM   MM");
	mvwaddstr(w_win, 4, 18, "P    P   C       O    O   M M M M   M M M M");
	mvwaddstr(w_win, 5, 18, "PPPPPP   C       O    O   M  M  M   M  M  M");
	mvwaddstr(w_win, 6, 18, "P        C       O    O   M     M   M     M");
	mvwaddstr(w_win, 7, 18, "P         CCCC    OOOO    M     M   M     M");

	sprintf(buf, ">>> Pcomm Version %s <<<", VERSION);
	mvwaddstr(w_win, 10, (80-strlen(buf))/2, buf);
	sprintf(buf, "Release date: %s", DATE);
	mvwaddstr(w_win, 11, (80-strlen(buf))/2, buf);

	mvwaddstr(w_win, 13, 8, "Pcomm is a public domain telecommunication program for Unix that");
	mvwaddstr(w_win, 14, 8, "is designed to operate similar to the MSDOS program, ProComm.");
	mvwaddstr(w_win, 15, 8, "ProComm (TM) is copyrighted by Datastorm Technologies, Inc.");
	mvwaddstr(w_win, 19, 45, "Emmet P. Gray");
	mvwaddstr(w_win, 20, 45, "...!uunet!uiucuxc!fthood!egray");
	wmove(w_win, 22, 79);
	wrefresh(w_win);
					/* delay so you can read the herald */
	if (auto_clear)
		wait_key(w_win, 5);
	else
		wgetch(w_win);

	if (fd == -1) {
		werase(w_win);
		wrefresh(w_win);
	}
	delwin(w_win);
	return;
}
