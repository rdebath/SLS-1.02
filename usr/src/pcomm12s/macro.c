/*
 * The keyboard macro feature.  Displays (and prompts for editing) the
 * macros assigned to the shifted number keys.  Prompts for saving
 * changes to disk.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "param.h"

void
macro()
{
	extern int fd;
	WINDOW *ma_win, *newwin();
	int ans, changed;
	char *mac, *str_rep(), *mac_prompt();

	ma_win = newwin(18, 65, 2, 15);
	mvwattrstr(ma_win, 1, 25, A_BOLD, "Keyboard Macros");
	horizontal(ma_win, 2, 0, 65);
	mvwprintw(ma_win, 4, 0, " %4.4s-!  %-50.50s\n", param->ascii_hot, param->mac_1);
	wprintw(ma_win, " %4.4s-@  %-50.50s\n", param->ascii_hot, param->mac_2);
	wprintw(ma_win, " %4.4s-#  %-50.50s\n", param->ascii_hot, param->mac_3);
	wprintw(ma_win, " %4.4s-$  %-50.50s\n", param->ascii_hot, param->mac_4);
	wprintw(ma_win, " %4.4s-%%  %-50.50s\n", param->ascii_hot, param->mac_5);
	wprintw(ma_win, " %4.4s-^  %-50.50s\n", param->ascii_hot, param->mac_6);
	wprintw(ma_win, " %4.4s-&  %-50.50s\n", param->ascii_hot, param->mac_7);
	wprintw(ma_win, " %4.4s-*  %-50.50s\n", param->ascii_hot, param->mac_8);
	wprintw(ma_win, " %4.4s-(  %-50.50s\n", param->ascii_hot, param->mac_9);
	wprintw(ma_win, " %4.4s-)  %-50.50s\n", param->ascii_hot, param->mac_0);
	mvwaddstr(ma_win, 15, 5, "Macro key to revise:");
	box(ma_win, VERT, HORZ);
					/* on the bottom line */
	mvwaddstr(ma_win, 17, 21, " Press <ESC> to continue ");
	wmove(ma_win, 15, 26);
	wrefresh(ma_win);

	changed = 0;

	while ((ans = wgetch(ma_win)) != ESC) {
		switch (ans) {
			case '!':	/* shifted 1 */
				if ((mac = mac_prompt(ans, param->mac_1)) != NULL) {
					param->mac_1 = str_rep(param->mac_1, mac);
					clear_line(ma_win, 4, 9, TRUE);
					mvwattrstr(ma_win, 4, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '@':	/* shifted 2 */
				if ((mac = mac_prompt(ans, param->mac_2)) != NULL) {
					param->mac_2 = str_rep(param->mac_2, mac);
					clear_line(ma_win, 5, 9, TRUE);
					mvwattrstr(ma_win, 5, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '#':	/* shifted 3 */
				if ((mac = mac_prompt(ans, param->mac_3)) != NULL) {
					param->mac_3 = str_rep(param->mac_3, mac);
					clear_line(ma_win, 6, 9, TRUE);
					mvwattrstr(ma_win, 6, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '$':	/* shifted 4 */
				if ((mac = mac_prompt(ans, param->mac_4)) != NULL) {
					param->mac_4 = str_rep(param->mac_4, mac);
					clear_line(ma_win, 7, 9, TRUE);
					mvwattrstr(ma_win, 7, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '%':	/* shifted 5 */
				if ((mac = mac_prompt(ans, param->mac_5)) != NULL) {
					param->mac_5 = str_rep(param->mac_5, mac);
					clear_line(ma_win, 8, 9, TRUE);
					mvwattrstr(ma_win, 8, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '^':	/* shifted 6 */
				if ((mac = mac_prompt(ans, param->mac_6)) != NULL) {
					param->mac_6 = str_rep(param->mac_6, mac);
					clear_line(ma_win, 9, 9, TRUE);
					mvwattrstr(ma_win, 9, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '&':	/* shifted 7 */
				if ((mac = mac_prompt(ans, param->mac_7)) != NULL) {
					param->mac_7 = str_rep(param->mac_7, mac);
					clear_line(ma_win, 10, 9, TRUE);
					mvwattrstr(ma_win, 10, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '*':	/* shifted 8 */
				if ((mac = mac_prompt(ans, param->mac_8)) != NULL) {
					param->mac_8 = str_rep(param->mac_8, mac);
					clear_line(ma_win, 11, 9, TRUE);
					mvwattrstr(ma_win, 11, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case '(':	/* shifted 9 */
				if ((mac = mac_prompt(ans, param->mac_9)) != NULL) {
					param->mac_9 = str_rep(param->mac_9, mac);
					clear_line(ma_win, 12, 9, TRUE);
					mvwattrstr(ma_win, 12, 9, A_BOLD, mac);
					changed++;
				}
				break;
			case ')':	/* shifted 0 */
				if ((mac = mac_prompt(ans, param->mac_0)) != NULL) {
					param->mac_0 = str_rep(param->mac_0, mac);
					clear_line(ma_win, 13, 9, TRUE);
					mvwattrstr(ma_win, 13, 9, A_BOLD, mac);
					changed++;
				}
				break;
			default:
				beep();
				break;
		}
		touchwin(ma_win);
		wmove(ma_win, 15, 26);
		wrefresh(ma_win);
	}
					/* if something changed */
	if (changed) {
					/* save to disk? */
		if (yes_prompt(ma_win, 15, 30, A_BOLD, "Save to disk")) {
			if (up_param()) {
				touchwin(ma_win);
				wrefresh(ma_win);
			}
		}
	}
	if (fd == -1) {
		werase(ma_win);
		wrefresh(ma_win);
	}
	delwin(ma_win);
	return;
}

/*
 * Sounds like McDonalds doesn't it?  Actually, it opens a new window
 * and prompts for the new macro.  Returns a pointer to the new string.
 * Since it uses get_str(), the return value points to a static area.
 */

static char *
mac_prompt(key, string)
char key, *string;
{
	extern char *null_ptr;
	WINDOW *mp_win, *newwin();
	char *new, *get_str();

	mp_win = newwin(6, 65, 8, 0);
	mvwprintw(mp_win, 2, 3, "%4.4s-%c  %-50.50s", param->ascii_hot, key, string);
	mvwaddstr(mp_win, 3, 5, "New : ");
	box(mp_win, VERT, HORZ);
	wrefresh(mp_win);

	if ((new = get_str(mp_win, 50, "", "\n")) != NULL) {
					/* if CR, return NULL */
		if (*new == '\0')
			new = NULL;
					/* if space, change to null_ptr */
		if (!strcmp(new, " "))
			new = null_ptr;
	}

	werase(mp_win);
	wrefresh(mp_win);
	delwin(mp_win);
	return(new);
}
