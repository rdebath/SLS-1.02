/*
 * The delete option of the dialing directory.  Prompts for saving
 * changes to disk.  A non-zero return code means that entries were deleted.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "param.h"

int
delete()
{
	extern char *null_ptr;
	WINDOW *d_win, *newwin();
	int i, first, last;
	void free_ptr();

	d_win = newwin(6, 32, 10, 15);

	mvwaddstr(d_win, 2, 2, "Delete entry:     thru:");
	box(d_win, VERT, HORZ);
	wmove(d_win, 2, 16);
	wrefresh(d_win);
					/* get the first of the range */
	while ((first = get_num(d_win, 3)) != -1) {
		if (first > 0 && first <= NUM_DIR)
			break;
		mvwaddstr(d_win, 2, 16, "   ");
		wmove(d_win, 2, 16);
		beep();
		wrefresh(d_win);
	}
	if (first == -1) {
		delwin(d_win);
		return(0);
	}
					/* get the last of the range */
	wmove(d_win, 2, 26);
	wrefresh(d_win);
	while ((last = get_num(d_win, 3)) != -1) {
		if ((first <= last && last <= NUM_DIR) || last == 0)
			break;
		mvwaddstr(d_win, 2, 26, "   ");
		wmove(d_win, 2, 26);
		beep();
		wrefresh(d_win);
	}
	if (last == -1) {
		delwin(d_win);
		return(0);
	}
					/* if "last" omitted, echo "first" */
	if (!last) {
		last = first;
		mvwprintw(d_win, 2, 26, "%d", first);
		wrefresh(d_win);
	}
					/* save to disk? */
	if (yes_prompt(d_win, 3, 2, A_BOLD, "Are you sure")) {
					/* delete from the physical file */
		if (del_dir(first, last)) {
			touchwin(d_win);
			wrefresh(d_win);
		}
		/*
		 * We don't really care if del_dir() fails because we still
		 * change the version in memory.
		 */
		for (i=first; i<=last; i++) {
			free_ptr(dir->name[i]);
			free_ptr(dir->number[i]);
			free_ptr(dir->script[i]);
			dir->name[i] = null_ptr;
			dir->number[i] = null_ptr;
			dir->baud[i] = param->d_baud;
			dir->parity[i] = param->d_parity;
			dir->dbits[i] = param->d_dbits;
			dir->sbits[i] = param->d_sbits;
			dir->duplex[i] = *param->d_duplex;
			dir->script[i] = null_ptr;
		}
		delwin(d_win);
		return(1);
	}
	delwin(d_win);
	return(0);
}
