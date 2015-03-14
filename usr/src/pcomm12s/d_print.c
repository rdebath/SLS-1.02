/*
 * The print option of the dialing directory.  A carriage return will
 * send the dialing directory to the print spool program, otherwise the
 * selected file will be used.
 */

#define MAX_STRING	80

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"

void
print_dir()
{
	FILE *fp, *popen(), *my_fopen();
	WINDOW *p_win, *newwin();
	char *ans, *file, *e_get_str(), buf[100], *expand();
	int is_printer, i;
	void error_win();
	unsigned int sleep();

	p_win = newwin(5, 54, 0, 26);

	mvwaddstr(p_win, 2, 3, "Print to: (printer)");
	box(p_win, VERT, HORZ);
	wmove(p_win, 2, 13);
	wrefresh(p_win);

	/*
	 * This is a special version of get_str() that looks at the
	 * first character to see if it should erase the default answer
	 * already on the screen.
	 */
	if ((ans = e_get_str(p_win, 80)) == NULL) {
					/* erase because it overlaps dm_win */
		werase(p_win);
		wrefresh(p_win);
		delwin(p_win);
		return;
	}
	file = expand(ans);
	is_printer = 0;
					/* the default (printer) */
	if (*file == '\0') {
		if (!(fp = popen(LPRINT, "w"))) {
			sprintf(buf, "\"%s\"", LPRINT);
			error_win(0, "Can't open printer program", buf);
			werase(p_win);
			wrefresh(p_win);
			delwin(p_win);
			return;
		}
		is_printer++;
	}
					/* the requested file */
	else {
		/*
		 * Check to see if the file already exists (and if you
		 * have write permission too).  Currently only allows
		 * you to bail out or overwrite the file (no append).
		 */
		switch(can_write(file)) {
			case DENIED:
				sprintf(buf, "\"%s\"", file);
				error_win(0, "No write permission on file", buf);
				werase(p_win);
				wrefresh(p_win);
				delwin(p_win);
				return;
			case OK_BUT_EXISTS:
				werase(p_win);
				mvwprintw(p_win, 2, 3, "File \"%s\" already exists!", file);
				beep();
				box(p_win, VERT, HORZ);
				if (!yes_prompt(p_win, 3, 3, A_BOLD, "Overwrite")) {
					werase(p_win);
					wrefresh(p_win);
					delwin(p_win);
					return;
				}
				/* fall thru */
			case WRITE_OK:
				if (!(fp = my_fopen(file, "w"))) {
					sprintf(buf, "\"%s\"", file);
					error_win(0, "Can't open file", buf);
					werase(p_win);
					wrefresh(p_win);
					delwin(p_win);
					return;
				}
				break;
		}
	}

	werase(p_win);
	mvwaddstr(p_win, 2, 13, "Printing Pcomm directory");
	box(p_win, VERT, HORZ);
	wrefresh(p_win);

	/*
	 * Only prints up to the end of the physical file, not the entire
	 * structure.  I gave some thought about not printing empty entries,
	 * but...
	 */
	for (i=1; i<=dir->d_entries; i++)
		fprintf(fp, "%4d- %-20.20s %18.18s  %5d-%c-%d-%d  %c  %-14.14s\n",
		 i, dir->name[i], dir->number[i], dir->baud[i], dir->parity[i],
		 dir->dbits[i], dir->sbits[i], dir->duplex[i], dir->script[i]);

	if (is_printer)
		pclose(fp);
	else {
					/* a dramatic delay... */
		sleep(1);
		fclose(fp);
	}

	werase(p_win);
	wrefresh(p_win);
	delwin(p_win);
	return;
}

/*
 * Get a string from a window but erase the line first.
 */

static char *
e_get_str(win, num)
WINDOW *win;
int num;
{
	int count, x, y, done_it;
	char ans;
	static char buf[MAX_STRING];

	done_it = 0;
	count = 0;
	while ((ans = wgetch(win)) != '\r') {
					/* do our own backspace */
		if (ans == BS || ans == DEL) {
			if (!count) {
				beep();
				continue;
			}
			count--;
			buf[count] = '\0';
			getyx(win, y, x);
			x--;
			wmove(win, y, x);
			waddch(win, (chtype) ' ');
			wmove(win, y, x);
			wrefresh(win);
			continue;
		}
					/* exceeded the max? */
		if (count >= num || count >= MAX_STRING) {
			beep();
			continue;
		}
					/* illegal character? */
		if (ans == '\n') {
			beep();
			continue;
		}
					/* an <ESC> anywhere in the string */
		if (ans == ESC)
			return(NULL);
					/* erase the default answer */
		if (!done_it) {
			waddstr(win, "         ");
			wmove(win, 2, 13);
			wrefresh(win);
			done_it++;
		}

		buf[count] = ans;
		waddch(win, (chtype) ans);
		wrefresh(win);
		count++;
	}
	buf[count] = '\0';
	return(buf);
}
