/*
 * Display the general setup, query for changes.  A non-zero return code
 * means something was changed.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "param.h"

int
gen_setup()
{
	extern char *v_yes[];
	WINDOW *g_win, *newwin();
	int i, num, ret_code;
	char c, *ans, *str_prompt(), *menu_prompt(), chr_prompt();
	char *str_rep();
	void line_set();
	static char *v_abort[3] = {"KEEP", "DELETE", NULL};

	g_win = newwin(23, 80, 0, 0);

	horizontal(g_win, 0, 0, 32);
	mvwattrstr(g_win, 0, 33, A_BOLD, "General Setup");
	horizontal(g_win, 0, 47, 32);
	mvwprintw(g_win, 3, 22, "1) Default log file ....... %s", param->logfile);
	mvwprintw(g_win, 4, 22, "2) Screen dump file ....... %s", param->dumpfile);
	mvwprintw(g_win, 6, 22, "3) Strip high bit  ........ %s", param->strip);
	mvwprintw(g_win, 8, 22, "4) Pause character ........ %c", param->pause_char);
	mvwprintw(g_win, 9, 22, "5) CR character ........... %c", param->cr_char);
	mvwprintw(g_win, 10, 22, "6) CTRL character ......... %c", param->ctrl_char);
	mvwprintw(g_win, 11, 22, "7) ESC character .......... %c", param->esc_char);
	mvwprintw(g_win, 12, 22, "8) Break character ........ %c", param->brk_char);
	mvwprintw(g_win, 14, 22, "9) Aborted downloads ...... %s", param->abort);
	mvwprintw(g_win, 16, 21, "10) Connect delay (sec) .... %d", param->c_delay);
	mvwprintw(g_win, 17, 21, "11) Redial delay (sec) ..... %d", param->r_delay);
	horizontal(g_win, 19, 0, 80);
	mvwattrstr(g_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(g_win, 20, 58, "Press <ESC> to return");
	wmove(g_win, 20, 12);
	touchwin(g_win);
	wrefresh(g_win);
					/* get the option number */
	ret_code = 0;
	while ((i = get_num(g_win, 2)) != -1) {
		switch (i) {
			case 1:
				if ((ans = str_prompt(g_win, 3, 50, "Default log file", "")) != NULL) {
					param->logfile = str_rep(param->logfile, ans);
					ret_code++;
				}
				break;
			case 2:
				if ((ans = str_prompt(g_win, 4, 50, "Default screen dump file", "")) != NULL) {
					param->dumpfile = str_rep(param->dumpfile, ans);
					ret_code++;
				}
				break;
			case 3:
				if ((ans = menu_prompt(g_win, 6, 50, "Strip high bit?", v_yes)) != NULL) {
					param->strip = str_rep(param->strip, ans);
					line_set();
					ret_code++;
				}
				break;
			case 4:
				if ((c = chr_prompt(g_win, 8, 50, "Pause character", "1 second")) != '\0') {
					param->pause_char = c;
					ret_code++;
				}
				break;
			case 5:
				if ((c = chr_prompt(g_win, 9, 50, "CR character", "(carriage return)")) != '\0') {
					param->cr_char = c;
					ret_code++;
				}
				break;
			case 6:
				if ((c = chr_prompt(g_win, 10, 50, "CTRL character", "(control)")) != '\0') {
					param->ctrl_char = c;
					ret_code++;
				}
				break;
			case 7:
				if ((c = chr_prompt(g_win, 11, 50, "ESC character", "(escape)")) != '\0') {
					param->esc_char = c;
					ret_code++;
				}
				break;
			case 8:
				if ((c = chr_prompt(g_win, 12, 50, "Break character", "")) != '\0') {
					param->brk_char = c;
					ret_code++;
				}
			case 9:
				if ((ans = menu_prompt(g_win, 14, 50, "Aborted downloads", v_abort)) != NULL) {
					param->abort = str_rep(param->abort, ans);
					ret_code++;
				}
				break;
			case 10:
				if ((num = num_prompt(g_win, 16, 50, "Connect delay time", "(in seconds)")) != -1) {
					if (num > MAX_CDELAY || num < MIN_CDELAY) {
						beep();
					/* some reasonable range of values */
						if (num < MIN_CDELAY)
							num = MIN_CDELAY;
						else
							num = MAX_CDELAY;
						mvwaddstr(g_win, 16, 50, "   ");
						wrefresh(g_win);
						mvwattrnum(g_win, 16, 50, A_BOLD, num);
						wrefresh(g_win);
					}
					param->c_delay = num;
					ret_code++;
				}
				break;
			case 11:
				if ((num = num_prompt(g_win, 17, 50, "Redial delay time", "(in seconds)")) != -1) {
					if (num > MAX_PAUSE || num < MIN_PAUSE) {
						beep();
					/* some reasonable range */
						if (num < MIN_PAUSE)
							num = MIN_PAUSE;
						else
							num = MAX_PAUSE;
						mvwaddstr(g_win, 17, 50, "    ");
						wrefresh(g_win);
						mvwattrnum(g_win, 17, 50, A_BOLD, num);
						wrefresh(g_win);
					}
					param->r_delay = num;
					ret_code++;
				}
				break;
			default:
				beep();
		}
		mvwaddstr(g_win, 20, 12, "  ");
		clear_line(g_win, 21, 0, FALSE);
		clear_line(g_win, 22, 0, FALSE);
		wmove(g_win, 20, 12);
		wrefresh(g_win);
	}
	delwin(g_win);
	return(ret_code);
}
