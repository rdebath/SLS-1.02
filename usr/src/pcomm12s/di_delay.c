/*
 * Prompt for new delay times during a dialing session.  Also, prompts
 * if changes should be saved to disk.  Dialing is suspended during
 * this routine.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "param.h"

void
delay_times()
{
	WINDOW *dt_win, *newwin();
	int cdelay, rdelay;

	dt_win = newwin(9, 45, 7, 15);

	mvwprintw(dt_win, 2, 4, "Current connect delay time: %d", param->c_delay);
	mvwprintw(dt_win, 3, 4, "Current redial delay time: %d", param->r_delay);
	mvwaddstr(dt_win, 5, 4, "New connect delay: ");
	mvwaddstr(dt_win, 6, 4, "New redial delay: ");
	box(dt_win, VERT, HORZ);

	mvwattrstr(dt_win, 0, 3, A_BOLD, " Change delay times ");
	wmove(dt_win, 5, 23);
	wrefresh(dt_win);
					/* get the cdelay number */
	if ((cdelay = get_num(dt_win, 3)) == -1) {
		delwin(dt_win);
		return;
	}
					/* give 'em the current settings */
	if (!cdelay) {
		cdelay = param->c_delay;
		wprintw(dt_win, "%-3d", cdelay);
	}
	else {
					/* some reasonable limit */
		if (cdelay > MAX_CDELAY || cdelay < MIN_CDELAY) {
			beep();
			if (cdelay > MAX_CDELAY)
				cdelay = MAX_CDELAY;
			else
				cdelay = MIN_CDELAY;
			mvwprintw(dt_win, 5, 23, "%-3d", cdelay);
		}
	}
					/* get the rdelay number */
	wmove(dt_win, 6, 22);
	wrefresh(dt_win);
	if ((rdelay = get_num(dt_win, 3)) == -1) {
		delwin(dt_win);
		return;
	}
					/* give 'em the current settings */
	if (!rdelay) {
		rdelay = param->r_delay;
		wprintw(dt_win, "%-3d", rdelay);
	}
	else {
					/* some reasonable limit */
		if (rdelay > MAX_PAUSE || rdelay < MIN_PAUSE) {
			beep();
			if (rdelay > MAX_PAUSE)
				rdelay = MAX_PAUSE;
			else
				rdelay = MIN_PAUSE;
			mvwprintw(dt_win, 6, 22, "%-3d", rdelay);
		}
	}
					/* set 'em */
	param->c_delay = cdelay;
	param->r_delay = rdelay;
					/* save 'em to disk? */
	if (yes_prompt(dt_win, 7, 12, A_BOLD, "Save to disk")) {
		if (up_param()) {
			touchwin(dt_win);
			wrefresh(dt_win);
		}
	}

	delwin(dt_win);
	return;
}
