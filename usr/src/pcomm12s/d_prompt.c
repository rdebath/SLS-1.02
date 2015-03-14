/*
 * Prompt for directory entry changes.  Copies the original values in
 * case you change your mind half way thru.  A non-zero return code means
 * the entry was changed.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"

int
prompt_lib(win, i)
WINDOW *win;
int i;
{
	extern int xmc;
	extern char *null_ptr;
	int n, baud, dbits, sbits, spot;
	static int valid_baud[6] = {300, 1200, 2400, 4800, 9600, 19200};
	static char *valid_parity[3] = {"Even", "Odd", "None"};
	char *ans, *get_str(), c, temp, name[40], number[40], script[40];
	char parity, duplex, *str_rep(), *strcpy(), buf[40];
	void free_ptr();
					/* make copies */
	strcpy(name, dir->name[i]);
	strcpy(number, dir->number[i]);
	baud = dir->baud[i];
	parity = dir->parity[i];
	dbits = dir->dbits[i];
	sbits = dir->sbits[i];
	duplex = dir->duplex[i];
	strcpy(script, dir->script[i]);
					/* display original values */
	werase(win);
	mvwprintw(win, 2, 5, "%-20.20s %18.18s  %5d-%c-%d-%d  %c  %-14.14s\n",
	 dir->name[i], dir->number[i], dir->baud[i], dir->parity[i],
	 dir->dbits[i], dir->sbits[i], dir->duplex[i], dir->script[i]);
	box(win, VERT, HORZ);

					/* prompt for name */
	mvwaddstr(win, 4, 4, "Name: ");
	wrefresh(win);

	if ((ans = get_str(win, 20, "", ";\n")) == NULL)
		return(0);
	if (*ans != '\0') {
		strcpy(name, ans);
		mvwaddstr(win, 2, 5, "                    ");
		wrefresh(win);
		mvwattrstr(win, 2, 5, A_BOLD, name);
	}
					/* prompt for number */
	clear_line(win, 4, 4, TRUE);
	waddstr(win, "Number: ");
	wrefresh(win);

	if ((ans = get_str(win, 18, "", ";\n")) == NULL)
		return(0);
	if (*ans != '\0') {
		strcpy(number, ans);
		mvwaddstr(win, 2, 26, "                  ");
		wrefresh(win);
		/*
		 * Should be right justified, but we don't wanna have
		 * the attribute turned on for blanks.
		 */
		spot = 26 + 18 - strlen(number);
		mvwattrstr(win, 2, spot, A_BOLD, number);
	}
					/* template for next few */
	clear_line(win, 4, 4, TRUE);
	mvwaddstr(win, 4, 31, "(Any key to change, <CR> to accept)");

	/*
	 * These next few prompts display a series of choices and allow
	 * the user to hit <CR> to accept the currently showing value
	 * or any other key to see the next choice.  The first value
	 * displayed is always the current value.
	 */
					/* choose from baud menu */
	for (n=0; n<6; n++) {
		if (valid_baud[n] == baud)
			break;
	}
	mvwprintw(win, 4, 4, "Baud: %-5d", valid_baud[n]);
	wmove(win, 4, 10);
	wrefresh(win);

	while ((c = wgetch(win)) != '\r') {
		if (c == ESC)
			return(0);
		n = (n == 5) ? 0 : n+1;
		mvwprintw(win, 4, 4, "Baud: %-5d", valid_baud[n]);
		wmove(win, 4, 10);
		wrefresh(win);
	}
	if (baud != valid_baud[n]) {
		baud = valid_baud[n];
		sprintf(buf, "%5d", baud);
		if (xmc > 0) {
			sprintf(buf, "%5d-%c-%d-%d", baud, parity, dbits, sbits);
			mvwaddstr(win, 2, 46, "           ");
			wrefresh(win);
		}
		mvwattrstr(win, 2, 46, A_BOLD, buf);
	}
					/* choose from parity menu */
	for (n=0; n<3; n++) {
		if (*valid_parity[n] == parity)
			break;
	}
	mvwprintw(win, 4, 4, "Parity: %-5.5s", valid_parity[n]);
	wmove(win, 4, 12);
	wrefresh(win);

	while ((c = wgetch(win)) != '\r') {
		if (c == ESC)
			return(0);
		n = (n == 2) ? 0 : n+1;
		mvwprintw(win, 4, 4, "Parity: %-5.5s", valid_parity[n]);
		wmove(win, 4, 12);
		wrefresh(win);
	}
	if (parity != *valid_parity[n]) {
		parity = *valid_parity[n];
		if (xmc > 0) {
			sprintf(buf, "%5d-%c-%d-%d", baud, parity, dbits, sbits);
			mvwaddstr(win, 2, 46, "           ");
			wrefresh(win);
			mvwattrstr(win, 2, 46, A_BOLD, buf);
		}
		else
			mvwattrch(win, 2, 52, A_BOLD, parity);
	}
					/* choose from data bits menu */
	n = dbits;
	mvwprintw(win, 4, 4, "Data Bits: %d    ", n);
	wmove(win, 4, 15);
	wrefresh(win);

	while ((c = wgetch(win)) != '\r') {
		if (c == ESC)
			return(0);
		n = (n == 8) ? 7 : 8;
		mvwprintw(win, 4, 4, "Data Bits: %d    ", n);
		wmove(win, 4, 15);
		wrefresh(win);
	}
	if (dbits != n) {
		dbits = n;
		if (xmc > 0) {
			sprintf(buf, "%5d-%c-%d-%d", baud, parity, dbits, sbits);
			mvwaddstr(win, 2, 46, "           ");
			wrefresh(win);
			mvwattrstr(win, 2, 46, A_BOLD, buf);
		}
		else
			mvwattrnum(win, 2, 54, A_BOLD, dbits);
	}
					/* choose from stop bits menu */
	n = sbits;
	mvwprintw(win, 4, 4, "Stop Bits: %d    ", n);
	wmove(win, 4, 15);
	wrefresh(win);

	while ((c = wgetch(win)) != '\r') {
		if (c == ESC)
			return(0);
		n = (n == 2) ? 1 : 2;
		mvwprintw(win, 4, 4, "Stop Bits: %d    ", n);
		wmove(win, 4, 15);
		wrefresh(win);
	}
	if (sbits != n) {
		sbits = n;
		if (xmc > 0) {
			sprintf(buf, "%5d-%c-%d-%d", baud, parity, dbits, sbits);
			mvwaddstr(win, 2, 46, "           ");
			wrefresh(win);
			mvwattrstr(win, 2, 46, A_BOLD, buf);
		}
		else
			mvwattrnum(win, 2, 56, A_BOLD, sbits);
	}
					/* choose from duplex menu */
	temp = duplex;
	mvwprintw(win, 4, 4, "Duplex: %c    ", temp);
	wmove(win, 4, 12);
	wrefresh(win);

	while ((c = wgetch(win)) != '\r') {
		if (c == ESC)
			return(0);
		temp = (temp == 'F') ? 'H' : 'F';
		mvwprintw(win, 4, 4, "Duplex: %c    ", temp);
		wmove(win, 4, 12);
		wrefresh(win);
	}
	if (duplex != temp) {
		duplex = temp;
		mvwattrch(win, 2, 59, A_BOLD, duplex);
	}
					/* prompt for script or TTY */
	clear_line(win, 4, 4, TRUE);
	waddstr(win, "Script name (or TTY): ");
	wrefresh(win);

	if ((ans = get_str(win, 14, "", ";\n")) == NULL)
		return(0);

	if (*ans != '\0') {
		strcpy(script, ans);
		mvwaddstr(win, 2, 62, "              ");
		wrefresh(win);
		mvwattrstr(win, 2, 62, A_BOLD, script);
	}
					/* store 'em for real */

	if (!strcmp(name, " ")) {
		free_ptr(dir->name[i]);
		dir->name[i] = null_ptr;
	}
	else
		dir->name[i] = str_rep(dir->name[i], name);

	if (!strcmp(number, " ")) {
		free_ptr(dir->number[i]);
		dir->number[i] = null_ptr;
	}
	else
		dir->number[i] = str_rep(dir->number[i], number);

	dir->baud[i] = baud;
	dir->parity[i] = parity;
	dir->dbits[i] = dbits;
	dir->sbits[i] = sbits;
	dir->duplex[i] = duplex;

	if (!strcmp(script, " ")) {
		free_ptr(dir->script[i]);
		dir->script[i] = null_ptr;
	}
	else
		dir->script[i] = str_rep(dir->script[i], script);

	return(1);
}
