/*
 * Display the ASCII transfer setup, query for changes.  A non-zero return
 * code means something was changed.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "param.h"

int
axfer_setup()
{
	extern char *v_yes[];
	WINDOW *x_win, *newwin();
	int i, ret_code, num;
	char *ans, *menu_prompt(), *str_rep();
	static char *v_cr[4] = {"NONE", "STRIP", "ADD LF", NULL};
	static char *v_lf[4] = {"NONE", "STRIP", "ADD CR", NULL};
	static char *v_delay[4] = {"0", "100", "150", NULL};

	x_win = newwin(23, 80, 0, 0);

	horizontal(x_win, 0, 0, 28);
	mvwattrstr(x_win, 0, 29, A_BOLD, "ASCII Transfer Setup");
	horizontal(x_win, 0, 50, 29);
	mvwaddstr(x_win, 3, 33, "ASCII UPLOAD");
	mvwprintw(x_win, 5, 22, "1) Echo locally ........... %s", param->lecho);
	mvwprintw(x_win, 6, 22, "2) Expand blank lines ..... %s", param->expand);
	mvwprintw(x_win, 7, 22, "3) CR delay (ms) .......... %d", param->cr_delay);
	mvwprintw(x_win, 8, 22, "4) Pace the output ........ %s", param->pace);
	mvwprintw(x_win, 9, 22, "5) CR translation ......... %s", param->cr_up);
	mvwprintw(x_win, 10, 22, "6) LF translation ......... %s", param->lf_up);
	mvwaddstr(x_win, 12, 32, "ASCII DOWNLOAD");
	mvwprintw(x_win, 14, 22, "7) Transfer timeout (sec) . %d", param->timer);
	mvwprintw(x_win, 15, 22, "8) CR translation ......... %s", param->cr_dn);
	mvwprintw(x_win, 16, 22, "9) LF translation ......... %s", param->lf_dn);
	horizontal(x_win, 19, 0, 80);
	mvwattrstr(x_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(x_win, 20, 58, "Press <ESC> to return");
	wmove(x_win, 20, 12);
	touchwin(x_win);
	wrefresh(x_win);
					/* get the option number */
	ret_code = 0;
	while ((i = get_num(x_win, 1)) != -1) {
		switch (i) {
			case 1:
				if ((ans = menu_prompt(x_win, 5, 50, "Echo locally", v_yes)) != NULL) {
					param->lecho = str_rep(param->lecho, ans);
					ret_code++;
				}
				break;
			case 2:
				if ((ans = menu_prompt(x_win, 6, 50, "Expand blank lines", v_yes)) != NULL) {
					param->expand = str_rep(param->expand, ans);
					ret_code++;
				}
				break;
			case 3:
				if ((ans = menu_prompt(x_win, 7, 50, "CR delay (ms)", v_delay)) != NULL) {
					param->cr_delay = atoi(ans);
					ret_code++;
				}
				break;
			case 4:
				if ((ans = menu_prompt(x_win, 8, 50, "Pace the output", v_yes)) != NULL) {
					param->pace = str_rep(param->pace, ans);
					ret_code++;
				}
				break;
			case 5:
				if ((ans = menu_prompt(x_win, 9, 50, "CR translation (upload)", v_cr)) != NULL) {
					param->cr_up = str_rep(param->cr_up, ans);
					ret_code++;
				}
				break;
			case 6:
				if ((ans = menu_prompt(x_win, 10, 50, "LF translation (upload)", v_lf)) != NULL) {
					param->lf_up = str_rep(param->lf_up, ans);
					ret_code++;
				}
				break;
			case 7:
				if ((num = num_prompt(x_win, 14, 50, "Transfer timeout", "(in seconds)")) != -1) {
					if (num > MAX_TIMER || num < MIN_TIMER) {
						beep();
					/* some reasonable range of values */
						if (num < MIN_TIMER)
							num = MIN_TIMER;
						else
							num = MAX_TIMER;
						mvwaddstr(x_win, 14, 50, "   ");
						wrefresh(x_win);
						mvwattrnum(x_win, 14, 50, A_BOLD, num);
						wrefresh(x_win);
					}
					param->timer = num;
					ret_code++;
				}
				break;
			case 8:
				if ((ans = menu_prompt(x_win, 15, 50, "CR translation (download)", v_cr)) != NULL) {
					param->cr_dn = str_rep(param->cr_dn, ans);
					ret_code++;
				}
				break;
			case 9:
				if ((ans = menu_prompt(x_win, 16, 50, "LF translation (download)", v_lf)) != NULL) {
					param->lf_dn = str_rep(param->lf_dn, ans);
					ret_code++;
				}
				break;
			default:
				beep();
		}
		mvwaddch(x_win, 20, 12, (chtype) ' ');
		clear_line(x_win, 21, 0, FALSE);
		clear_line(x_win, 22, 0, FALSE);
		wmove(x_win, 20, 12);
		wrefresh(x_win);
	}
	delwin(x_win);
	return(ret_code);
}
