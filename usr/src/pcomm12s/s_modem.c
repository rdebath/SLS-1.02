/*
 * Display the modem setup, query for changes.  A non-zero return code
 * means something was changed.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "modem.h"

int
modem_setup()
{
	WINDOW *mo_win, *newwin();
	int i, j, ret_code, mod_prompt();
	char *ans, *str_rep(), *str_prompt(), *menu_prompt();
	void disp_modem();
	extern char *v_yn[];
					/* the current modem */
	j = 0;
	if (modem->m_cur != -1)
		j = modem->m_cur;

	mo_win = newwin(23, 80, 0, 0);

	horizontal(mo_win, 0, 0, 33);
	mvwattrstr(mo_win, 0, 34, A_BOLD, "Modem Setup");
	horizontal(mo_win, 0, 46, 34);
					/* display the current settings */
	disp_modem(mo_win, j);
	horizontal(mo_win, 19, 0, 80);
	mvwattrstr(mo_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(mo_win, 20, 58, "Press <ESC> to return");
	wmove(mo_win, 20, 12);
	touchwin(mo_win);
	wrefresh(mo_win);
					/* get the option number */
	ret_code = 0;
	while ((i = get_num(mo_win, 2)) != -1) {
		switch (i) {
			case 1:
				j = mod_prompt(mo_win);
				break;
			case 2:
				if ((ans = str_prompt(mo_win, 3, 39, "Modem init string", "sent to the modem once")) != NULL) {
					modem->init[j] = str_rep(modem->init[j], ans);
					ret_code++;
				}
				break;
			case 3:
				if ((ans = str_prompt(mo_win, 4, 39, "Dialing command", "")) != NULL) {
					modem->dial[j] = str_rep(modem->dial[j], ans);
					ret_code++;
				}
				break;
			case 4:
				if ((ans = str_prompt(mo_win, 5, 39, "Dialing cmd suffix", "typically the <CR> character")) != NULL) {
					modem->suffix[j] = str_rep(modem->suffix[j], ans);
					ret_code++;
				}
				break;
			case 5:
				if ((ans = str_prompt(mo_win, 6, 39, "Hang up string", "")) != NULL) {
					modem->hang_up[j] = str_rep(modem->hang_up[j], ans);
					ret_code++;
				}
				break;
			case 6:
				if ((ans = menu_prompt(mo_win, 7, 39, "Auto Baud detect", v_yn)) != NULL) {
					modem->auto_baud[j] = *ans;
					ret_code++;
				}
				break;
			case 7:
				if ((ans = str_prompt(mo_win, 8, 39, "300 baud connect string", "")) != NULL) {
					modem->con_3[j] = str_rep(modem->con_3[j], ans);
					ret_code++;
				}
				break;
			case 8:
				if ((ans = str_prompt(mo_win, 9, 39, "1200 baud connect string", "")) != NULL) {
					modem->con_12[j] = str_rep(modem->con_12[j], ans);
					ret_code++;
				}
				break;
			case 9:
				if ((ans = str_prompt(mo_win, 10, 39, "2400 baud connect string", "")) != NULL) {
					modem->con_24[j] = str_rep(modem->con_24[j], ans);
					ret_code++;
				}
				break;
			case 10:
				if ((ans = str_prompt(mo_win, 11, 39, "4800 baud connect string", "")) != NULL) {
					modem->con_48[j] = str_rep(modem->con_48[j], ans);
					ret_code++;
				}
				break;
			case 11:
				if ((ans = str_prompt(mo_win, 12, 39, "9600 baud connect string", "")) != NULL) {
					modem->con_96[j] = str_rep(modem->con_96[j], ans);
					ret_code++;
				}
				break;
			case 12:
				if ((ans = str_prompt(mo_win, 13, 39, "19200 baud connect string", "")) != NULL) {
					modem->con_192[j] = str_rep(modem->con_192[j], ans);
					ret_code++;
				}
				break;
			case 13:
				if ((ans = str_prompt(mo_win, 14, 39, "No connect string 1", "")) != NULL) {
					modem->no_con1[j] = str_rep(modem->no_con1[j], ans);
					ret_code++;
				}
				break;
			case 14:
				if ((ans = str_prompt(mo_win, 15, 39, "No connect string 2", "")) != NULL) {
					modem->no_con2[j] = str_rep(modem->no_con2[j], ans);
					ret_code++;
				}
				break;
			case 15:
				if ((ans = str_prompt(mo_win, 16, 39, "No connect string 3", "")) != NULL) {
					modem->no_con3[j] = str_rep(modem->no_con3[j], ans);
					ret_code++;
				}
				break;
			case 16:
				if ((ans = str_prompt(mo_win, 17, 39, "No connect string 4", "")) != NULL) {
					modem->no_con4[j] = str_rep(modem->no_con4[j], ans);
					ret_code++;
				}
				break;
			default:
				beep();
		}
					/* clear the previous prompts */
		mvwaddstr(mo_win, 20, 12, "   ");
		clear_line(mo_win, 21, 0, FALSE);
		clear_line(mo_win, 22, 0, FALSE);
		wmove(mo_win, 20, 12);
		wrefresh(mo_win);
	}
	delwin(mo_win);
	return(ret_code);
}

/*
 * Prompts for the modem name.  The user selects the currently showing
 * choice by hitting a carriage return.  Returns the modem entry number.
 * DOES NOT change the value of modem->m_cur.
 */

static int
mod_prompt(win)
WINDOW *win;
{
	char ans;
	int i;
					/* print prompt lines */
	mvwaddstr(win, 22, 0, "Press any key to change, or <CR> to accept");
	mvwaddstr(win, 21, 0, "Modem name: ");
					/* show current choice */
	i = 0;
	if (modem->m_cur != -1)
		i = modem->m_cur;
	mvwprintw(win, 21, 12, "%-30.30s", modem->mname[i]);
	wmove(win, 21, 12);
	wrefresh(win);
					/* show the choices one at a time */
	while ((ans = wgetch(win)) != '\r') {
		i++;
		if (*modem->mname[i] == '\0')
			i = 0;
		if (ans == ESC)
			return(0);
		mvwprintw(win, 21, 12, "%-30.30s", modem->mname[i]);
		wmove(win, 21, 12);
		wrefresh(win);
	}
					/* display the new values */
	disp_modem(win, i);
					/* display the name in bold */
	clear_line(win, 2, 39, FALSE);
	wrefresh(win);
	mvwattrstr(win, 2, 39, A_BOLD, modem->mname[i]);
	mvwprintw(win, 2, 25, "(%d of %d) ", i+1, modem->m_entries);

	return(i);
}

/*
 * Show the current settings for the given modem entry number.
 */

static void
disp_modem(w, i)
WINDOW *w;
int i;
{
	mvwprintw(w, 2, 11, "1) Modem name ............. %-39.39s", modem->mname[i]);
	mvwprintw(w, 2, 25, "(%d of %d) ", i+1, modem->m_entries);
	mvwprintw(w, 3, 11, "2) Modem init string ...... %-39.39s", modem->init[i]);
	mvwprintw(w, 4, 11, "3) Dialing command ........ %-39.39s", modem->dial[i]);
	mvwprintw(w, 5, 11, "4) Dialing cmd suffix ..... %-39.39s", modem->suffix[i]);
	mvwprintw(w, 6, 11, "5) Hang up string ......... %-39.39s", modem->hang_up[i]);
	mvwprintw(w, 7, 11, "6) Auto baud detect ....... %c", modem->auto_baud[i]);
	mvwprintw(w, 8, 11, "7) 300 baud connect ....... %-39.39s", modem->con_3[i]);
	mvwprintw(w, 9, 11, "8) 1200 baud connect ...... %-39.39s", modem->con_12[i]);
	mvwprintw(w, 10, 11, "9) 2400 baud connect ...... %-39.39s", modem->con_24[i]);
	mvwprintw(w, 11, 10, "10) 4800 baud connect ...... %-39.39s", modem->con_48[i]);
	mvwprintw(w, 12, 10, "11) 9600 baud connect ...... %-39.39s", modem->con_96[i]);
	mvwprintw(w, 13, 10, "12) 19200 baud connect ..... %-39.39s", modem->con_192[i]);
	mvwprintw(w, 14, 10, "13) No connect string 1 .... %-39.39s", modem->no_con1[i]);
	mvwprintw(w, 15, 10, "14) No connect string 2 .... %-39.39s", modem->no_con2[i]);
	mvwprintw(w, 16, 10, "15) No connect string 3 .... %-39.39s", modem->no_con3[i]);
	mvwprintw(w, 17, 10, "16) No connect string 4 .... %-39.39s", modem->no_con4[i]);
	return;
}
