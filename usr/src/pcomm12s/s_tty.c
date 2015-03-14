/*
 * Display the TTY setup, query for changes.  A non-zero return code
 * means something was changed.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "modem.h"

int
tty_setup()
{
	WINDOW *tt_win, *newwin();
	char message[80], *str, *get_str();
	int num, i, j, ret_code;
	void disp_tty(), create_modem(), del_modem(), error_win();
	void del_tty();

	tt_win = newwin(23, 80, 0, 0);

	horizontal(tt_win, 0, 0, 34);
	mvwattrstr(tt_win, 0, 35, A_BOLD, "TTY Setup");
	horizontal(tt_win, 0, 45, 34);
	mvwaddstr(tt_win, 2, 22, "TTY name");
	mvwaddstr(tt_win, 2, 37, "Modem name");
	mvwaddstr(tt_win, 2, 51, "Init speed");
					/* display the current TTY list */
	disp_tty(tt_win);
					/* prompt for options */
	mvwaddstr(tt_win, 15, 20, "A) Add a TTY entry");
	mvwaddstr(tt_win, 16, 20, "D) Delete a TTY entry");
	horizontal(tt_win, 19, 0, 80);
	mvwattrstr(tt_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(tt_win, 20, 58, "Press <ESC> to return");
	wmove(tt_win, 20, 12);
	touchwin(tt_win);
	wrefresh(tt_win);
					/* get the option */
	ret_code = 0;
	while ((str = get_str(tt_win, 2, "01234356789AaDd", "")) != NULL) {
		switch(*str) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				i = atoi(str);
					/* if beyond t_entries */
				if (i > modem->t_entries) {
					beep();
					break;
				}

					/* change the entry  */
				if (tty_prompt(tt_win, i-1)) {

					/* requires modem update? */
					create_modem(modem->tname[i-1]);
					del_modem();
					ret_code++;
				}
				break;
			case 'a':
			case 'A':	/* add an entry */
				if (modem->t_entries == NUM_TTY) {
					sprintf(message, "\"%s\"", modem->m_path);
					error_win(0, "No empty TTY slots in modem/TTY database", message);
					break;
				}
					/* prompt for info */
				j = modem->t_entries;
				if (tty_prompt(tt_win, j)) {

					/* add modem entry? */
					modem->t_entries++;
					create_modem(modem->tname[j]);
					ret_code++;
				}
				break;
			case 'd':
			case 'D':	/* delete an entry */
				mvwaddstr(tt_win, 21, 0, "Entry number to delete: ");
				wrefresh(tt_win);
				while ((num = get_num(tt_win, 4)) != -1) {
					/* valid range */
					if (!num || num>modem->t_entries) {
						beep();
						mvwaddstr(tt_win, 21, 24, "   ");
						wmove(tt_win, 21, 24);
						wrefresh(tt_win);
						continue;
					}
					del_tty(num-1);
					del_modem();

					/* show the new list */
					disp_tty(tt_win);
					ret_code++;
					break;
				}
				break;
			default:
				beep();
				break;
		}
		mvwaddstr(tt_win, 20, 12, "  ");
		clear_line(tt_win, 21, 0, FALSE);
		clear_line(tt_win, 22, 0, FALSE);
		wmove(tt_win, 20, 12);
		wrefresh(tt_win);
	}
	delwin(tt_win);
	return(ret_code);
}

/*
 * Display the current TTY list.  No scrolling yet, so if your NUM_TTY is
 * greater than ten, this routine will need some work.
 */

static void
disp_tty(win)
WINDOW *win;
{
	int i;

	for (i=0; i<NUM_TTY; i++)
		mvwprintw(win, i+4, 20, "%2d) %-14.14s %-14.14s  %d\n",
		 i+1, modem->tty[i], modem->tname[i], modem->init_sp[i]);
	return;
}

/*
 * Prompt the user for the TTY database info.  A non-zero return code means
 * something was changed.  The second argument is the zero based index.
 */

static int
tty_prompt(win, i)
WINDOW *win;
int i;
{
	char *ans, t_tty[80], t_tname[80], *str_prompt(), *menu_prompt();
	char *str_rep(), *strcpy();
	static char *v_baud[8] = {"0", "300", "1200", "2400", "4800", "9600",
	 "19200", NULL};
					/* get temp TTY */
	if ((ans = str_prompt(win, i+4, 24, "TTY name", "")) == NULL)
		return(0);

	strcpy(t_tty, ans);
	clear_line(win, 21, 0, FALSE);

					/* get temp tname */
	if ((ans = str_prompt(win, i+4, 39, "Modem name", "")) == NULL)
		return(0);

	strcpy(t_tname, ans);
	clear_line(win, 21, 0, FALSE);

					/* get maximum baud */
	if ((ans = menu_prompt(win, i+4, 55, "Init speed", v_baud)) == NULL)
		return(0);

	wrefresh(win);
					/* store 'em for real */
	modem->tty[i] = str_rep(modem->tty[i], t_tty);
	modem->tname[i] = str_rep(modem->tname[i], t_tname);
	modem->init_sp[i] = atoi(ans);

	return(1);
}

/*
 * Delete a TTY entry.  Since the list must be contiguous, we collapse the
 * list to cover the hole we made.
 */

static void
del_tty(i)
int i;
{
	extern char *null_ptr;
	int j;
	char *str_rep();
	void free_ptr();
					/* collapse the list */
	for (j=i; j<modem->t_entries-1; j++) {
		modem->tty[j] = str_rep(modem->tty[j], modem->tty[j+1]);
		modem->tname[j] = str_rep(modem->tname[j], modem->tname[j+1]);
		modem->init_sp[j] = modem->init_sp[j+1];
	}
	j = modem->t_entries-1;
					/* zap the entry */
	free_ptr(modem->tty[j]);
	free_ptr(modem->tname[j]);
	modem->tty[j] = null_ptr;
	modem->tname[j] = null_ptr;
	modem->init_sp[j] = 0;
					/* update the count */
	modem->t_entries--;
	if (modem->t_cur >= modem->t_entries)
		modem->t_cur = -1;
	return;
}
