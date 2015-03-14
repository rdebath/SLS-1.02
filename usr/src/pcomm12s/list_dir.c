/*
 * Do a shell escape with the "ls" command.  Additional command line options
 * are allowed at run time.
 */

#define LS_CMD "ls -aC"

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"

void
list_dir()
{
	extern int fd;
	WINDOW *ls_win, *newwin();
	FILE *pfp, *n_popen();
	int lines, oops;
	char *ans, *cwd, *getcwd(), buf[200], *get_str();

	ls_win = newwin(6, 70, 8, 5);

	cwd = getcwd(buf, 200);

	mvwprintw(ls_win, 2, 4, "Current directory: %s", cwd);
	mvwaddstr(ls_win, 3, 4, "File spec (wildcards allowed): ");
	box(ls_win, VERT, HORZ);

	mvwattrstr(ls_win, 0, 3, A_BOLD, " List Directory ");
	wmove(ls_win, 3, 35);
	wrefresh(ls_win);

	if ((ans = get_str(ls_win, 80, "", "\n")) == NULL) {
		if (fd == -1) {
			werase(ls_win);
			wrefresh(ls_win);
		}
		delwin(ls_win);
		return;
	}
					/* popen() an ls */
	sprintf(buf, "%s %s", LS_CMD, ans);
	pfp = n_popen(buf, "r");
					/* make a bigger window */
	werase(ls_win);
	wrefresh(ls_win);
	delwin(ls_win);
	ls_win = newwin(LINES-1, COLS, 0, 0);
	touchwin(ls_win);
					/* a crude kind of paging */
	oops = 0;
	lines = 0;
	while (fgets(buf, BUFSIZ, pfp) != NULL) {
		waddstr(ls_win, buf);
		lines++;
		if (lines == LINES-2) {
			lines = 0;
			mvwaddstr(ls_win, LINES-2, 28, "Press any key for more");
			wrefresh(ls_win);
			if (wgetch(ls_win) == ESC) {
				oops++;
				break;
			}
			werase(ls_win);
			wrefresh(ls_win);
		}
	}
	n_pclose(pfp);

	if (!oops) {
		mvwaddstr(ls_win, LINES-2, 25, "Press any key to continue");
		wrefresh(ls_win);
		wgetch(ls_win);
	}
	if (fd == -1) {
		werase(ls_win);
		wrefresh(ls_win);
	}
	delwin(ls_win);
	return;
}
