/*
 * Display the terminal setup, query for changes.  A return code of 1
 * means something was changed, 2 means we have to kill and restart
 * the input routine.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "param.h"
#include "status.h"

int
term_setup()
{
	WINDOW *t_win, *newwin();
	int i, num, ret_code;
	char *ans, *str_rep(), *str_prompt(), *menu_prompt();
	void input_off(), line_set();
	static char *v_crio[3] = {"CR", "CR/LF", NULL};
	static char *v_duplex[3] = {"FULL", "HALF", NULL};
	static char *v_flow[3] = {"NONE", "XON/XOFF", NULL};

	t_win = newwin(23, 80, 0, 0);

	horizontal(t_win, 0, 0, 32);
	mvwattrstr(t_win, 0, 33, A_BOLD, "Terminal Setup");
	horizontal(t_win, 0, 48, 32);
	mvwprintw(t_win, 4, 22, "1) Hot key (decimal) ...... %d", param->hot);
	mvwprintw(t_win, 6, 22, "2) ASCII version of hot ... %s", param->ascii_hot);
	mvwprintw(t_win, 9, 22, "3) Duplex ................. %s", param->d_duplex);
	mvwprintw(t_win, 11, 22, "4) Flow control ........... %s", param->flow);
	mvwprintw(t_win, 13, 22, "5) CR translation (in) .... %s", param->cr_in);
	mvwprintw(t_win, 15, 22, "6) CR translation (out) ... %s", param->cr_out);
	horizontal(t_win, 19, 0, 80);
	mvwattrstr(t_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(t_win, 20, 58, "Press <ESC> to return");
	wmove(t_win, 20, 12);
	touchwin(t_win);
	wrefresh(t_win);
					/* get the option number */
	ret_code = 0;
	while ((i = get_num(t_win, 1)) != -1) {
		switch (i) {
			case 1:
				if ((num = num_prompt(t_win, 4, 50, "Hot key", "decimal code for the hot key")) != -1) {
					param->hot = num;
					ret_code = 1;
				}
				break;
			case 2:
				if ((ans = str_prompt(t_win, 6, 50, "ASCII version of hot key", "(printable version)")) != NULL) {
					param->ascii_hot = str_rep(param->ascii_hot, ans);
					ret_code = 1;
				}
				break;
			case 3:
				if ((ans = menu_prompt(t_win, 9, 50, "Duplex", v_duplex)) != NULL) {
					param->d_duplex = str_rep(param->d_duplex, ans);
					ret_code = 1;
				}
				break;
			case 4:
				if ((ans = menu_prompt(t_win, 11, 50, "Flow control", v_flow)) != NULL) {
					param->flow = str_rep(param->flow, ans);
					line_set();
					ret_code = 1;
				}
				break;
			case 5:
				if ((ans = menu_prompt(t_win, 13, 50, "CR translation (in)", v_crio)) != NULL) {

					/*
					 * the "add lf to cr" function is
					 * performed by the input routine
					 */
					param->cr_in = str_rep(param->cr_in, ans);
					status->add_lf = !strcmp(ans, "CR/LF");
#ifdef SHAREDMEM
					ret_code = 1;
#else /* SHAREDMEM */
					input_off();
					ret_code = 2;
#endif /* SHAREDMEM */
				}
				break;
			case 6:
				if ((ans = menu_prompt(t_win, 15, 50, "CR translation (out)", v_crio)) != NULL) {
					param->cr_out = str_rep(param->cr_out, ans);
					ret_code = 1;
				}
				break;
			default:
				beep();
		}
		mvwaddch(t_win, 20, 12, (chtype) ' ');
		clear_line(t_win, 21, 0, FALSE);
		clear_line(t_win, 22, 0, FALSE);
		wmove(t_win, 20, 12);
		wrefresh(t_win);
	}
	delwin(t_win);
	return(ret_code);
}
