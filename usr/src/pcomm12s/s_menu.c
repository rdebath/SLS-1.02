/*
 * Display the setup menu, prompts for a bunch of other menus.  A non-zero
 * return code means we have to restart the input routine.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"

int
setup_menu()
{
	extern int fd, xmc;
	WINDOW *s_win, *newwin();
	char *ans, *get_str();
	int param_flag, modem_flag, ext_flag, ret_code;
	void top_line();

	s_win = newwin(23, 80, 0, 0);

	top_line(s_win);
	mvwaddstr(s_win, 3, 29, "1) TTY Setup");
	mvwaddstr(s_win, 5, 29, "2) Modem Setup");
	mvwaddstr(s_win, 7, 29, "3) Terminal Setup");
	mvwaddstr(s_win, 9, 29, "4) General Setup");
	mvwaddstr(s_win, 11, 29, "5) ASCII Transfer Setup");
	mvwaddstr(s_win, 13, 29, "6) External Protocol Setup");
	mvwaddstr(s_win, 15, 29, "S) Save setup to disk");
	horizontal(s_win, 19, 0, 80);
	mvwattrstr(s_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(s_win, 20, 58, "  Press <ESC> to exit");
	wmove(s_win, 20, 12);
	touchwin(s_win);
	wrefresh(s_win);

	param_flag = 0;
	modem_flag = 0;
	ext_flag = 0;
	ret_code = 0;
					/* get the options */
	while ((ans = get_str(s_win, 1, "123456Ss", "")) != NULL) {
		if (xmc > 0) {
			clear_line(s_win, 0, 0, FALSE);
			wrefresh(s_win);
		}
		switch (*ans) {
			case '1':
				if (tty_setup())
					modem_flag++;
				break;
			case '2':
				if (modem_setup())
					modem_flag++;
				break;
			case '3':
				/*
				 * term_setup() returns a 1 if something was
				 * changed, and a 2 if the change requires
				 * the input routine to be restarted.
				 */
				if (ret_code = term_setup()) {
					ret_code--;
					param_flag++;
				}
				break;
			case '4':
				if (gen_setup())
					param_flag++;
				break;
			case '5':
				if (axfer_setup())
					param_flag++;
				break;
			case '6':
				if (ext_setup())
					ext_flag++;
				break;
			case 's':
			case 'S':
				if (xmc > 0)
					top_line(s_win);
				/*
				 * Writes to disk are not critical, since
				 * the changes are made in memory.
				 */
				if (param_flag) {
					mvwattrstr(s_win, 22, 27, A_BLINK, "Updating Parameter File");
					wrefresh(s_win);
					wait_key(s_win, 3);
					if (up_param()) {
						touchwin(s_win);
						wrefresh(s_win);
					}
				}
				if (modem_flag) {
					mvwattrstr(s_win, 22, 27, A_BLINK, "Updating Modem Database");
					wrefresh(s_win);
					wait_key(s_win, 3);
					if (up_modem()) {
						touchwin(s_win);
						wrefresh(s_win);
					}
				}
				if (ext_flag) {
					mvwattrstr(s_win, 22, 25, A_BLINK, "Updating External Protocols");
					wrefresh(s_win);
					wait_key(s_win, 3);
					if (up_extrnl()) {
						touchwin(s_win);
						wrefresh(s_win);
					}
				}
				clear_line(s_win, 22, 25, FALSE);
				wrefresh(s_win);
				break;
			default:
				beep();
		}
		touchwin(s_win);
		if (xmc > 0)
			top_line(s_win);

		mvwaddch(s_win, 20, 12, (chtype) ' ');
		wmove(s_win, 20, 12);
		wrefresh(s_win);
	}
	if (fd == -1) {
		werase(s_win);
		wrefresh(s_win);
	}
	delwin(s_win);
	return(ret_code);
}

/*
 * Put the top line on the window.
 */

void
top_line(win)
WINDOW *win;
{
	clear_line(win, 0, 0, FALSE);
	wrefresh(win);
	horizontal(win, 0, 0, 33);
	mvwattrstr(win, 0, 34, A_BOLD, "Setup Menu");
	horizontal(win, 0, 45, 34);
	wrefresh(win);
	return;
}
