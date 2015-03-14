/*
 * Display the status line.  Up to now, we've never really cared how
 * large the physical screen was... but now we want the status line
 * on the bottom.
 */

#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "modem.h"
#include "param.h"
#include "status.h"

void
st_line(message)
char *message;
{
	extern int xmc;
	WINDOW *sl_win, *newwin();
	int d, x, y;
	static char *dn[2] = {"FDX", "HDX"};
	static char *ln[2] = {"LOG OFF", "LOG ON"};
	static char *pn[2] = {"PTR OFF", "PTR ON "};
	char buf[80], field_one[15], *cur_tty;

					/* is anybody missing? */
	if (dir == NULL || modem == NULL || param == NULL)
		return;
					/* remember where we parked the car.. */
	getyx(stdscr, y, x);

	sl_win = newwin(1, 80, LINES-1, 0);
					/* duplex message */
	d = 0;
	if (dir->duplex[dir->d_cur] == 'H')
		d++;
					/* the current TTY */
	cur_tty = "No TTY";
	if (modem->t_cur != -1)
		cur_tty = modem->tty[modem->t_cur];

	/*
	 * The philosophy is:  If you press a command sequence that
	 * doesn't generate a window on the screen, then show the user
	 * what's going on in the status line.
	 */
	if (*message == '\0')
		sprintf(field_one, " %4.4s-0 HELP  ", param->ascii_hot);
	else
		sprintf(field_one, " %-13.13s", message);

#ifdef XMC_BROKE
	if (xmc > 0)
		sprintf(buf, "%s | %-9.9s| %s | %5d %c%d%d | %-7.7s | %-7.7s | %-5.5s|%-5.5s",
		 field_one, cur_tty, dn[d], dir->baud[dir->d_cur],
		 dir->parity[dir->d_cur], dir->dbits[dir->d_cur],
		 dir->sbits[dir->d_cur], ln[status->log], pn[status->print],
		 param->cr_in, param->cr_out);
	else
#endif /* XMC_BROKE */
		sprintf(buf, "%s | %-9.9s| %s | %5d %c%d%d | %-7.7s | %-7.7s | %-5.5s| %-5.5s",
		 field_one, cur_tty, dn[d], dir->baud[dir->d_cur],
		 dir->parity[dir->d_cur], dir->dbits[dir->d_cur],
		 dir->sbits[dir->d_cur], ln[status->log], pn[status->print],
		 param->cr_in, param->cr_out);

	if (xmc > 0) {
		touchwin(sl_win);
		werase(sl_win);
		wrefresh(sl_win);
	}
	wattrstr(sl_win, A_STANDOUT, buf);
	wrefresh(sl_win);
					/* go ahead and delete it now */
	delwin(sl_win);
	move(y, x);
	refresh();
	return;
}
