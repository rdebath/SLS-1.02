/*
 * Prompting routines used in the setup menus.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"

/*
 * Prompt for a string at line 21 (with optional line 22 for additional
 * information).  Display the new string in bold at its original location
 * in the menu.  Used in virtually all of the *_setup() routines.  Since
 * it uses get_str(), the return value points to a static area.
 */

char *
str_prompt(win, y, x, p1, p2)
WINDOW *win;
int y, x;
char *p1, *p2;
{
	extern char *null_ptr;
	char *ans, *get_str();
					/* print first prompt last */
	mvwaddstr(win, 22, 0, p2);
	mvwaddstr(win, 21, 0, p1);
	waddstr(win, ": ");
	wrefresh(win);

	if ((ans = get_str(win, 80, "", "\n")) == NULL)
		return(NULL);
					/* check the value */
	if (!strcmp(ans, " "))
		ans = null_ptr;
					/* display the value in bold */
	clear_line(win, y, x, FALSE);
	wattrstr(win, A_BOLD, ans);

	return(ans);
}

/*
 * Same as above, except we return a single character.
 */

char
chr_prompt(win, y, x, p1, p2)
WINDOW *win;
int y, x;
char *p1, *p2;
{
	char *ans, *get_str();
					/* print first prompt last */
	mvwaddstr(win, 22, 0, p2);
	mvwaddstr(win, 21, 0, p1);
	waddstr(win, ": ");
	wrefresh(win);

	if ((ans = get_str(win, 1, "", "\n")) == NULL)
		return('\0');
					/* display the value in bold */
	mvwaddstr(win, y, x, "  ");
	wrefresh(win);
	mvwattrstr(win, y, x, A_BOLD, ans);

	return(*ans);
}

/*
 * Same as above, except that it prompts for a three digit number.
 */

int
num_prompt(win, y, x, p1, p2)
WINDOW *win;
int y, x;
char *p1, *p2;
{
	int i;
					/* print first prompt last */
	mvwaddstr(win, 22, 0, p2);
	mvwaddstr(win, 21, 0, p1);
	waddstr(win, ": ");
	wrefresh(win);

	if ((i = get_num(win, 3)) == -1)
		return(-1);
					/* display the value in bold */
	mvwaddstr(win, y, x, "    ");
	wrefresh(win);
	mvwattrnum(win, y, x, A_BOLD, i);
					/* return the number */
	return(i);
}

/*
 * Prompts for a selection from a menu.  We display the prompt lines,
 * and show the choices one at a time.  The user selects the currently
 * showing choice by hitting a carriage return.  Unlike the similar
 * routines in d_prompt(), the first choice shown is not necessarily
 * the current.
 */

char *v_yes[3] = {"YES", "NO", NULL};
char *v_yn[3] = {"Y", "N", NULL};

char *
menu_prompt(win, y, x, p, menu)
WINDOW *win;
int y, x;
char *p, *menu[];
{
	char ans;
	int i, cy, cx;
					/* print first prompt last */
	mvwaddstr(win, 22, 0, "Press any key to change, or <CR> to accept");
	mvwaddstr(win, 21, 0, p);
	waddstr(win, ": ");
					/* show first choice */
	i = 0;
	getyx(win, cy, cx);
	mvwprintw(win, cy, cx, "%-30.30s", menu[i]);
	wmove(win, cy, cx);
	wrefresh(win);
					/* show the choices one at a time */
	while ((ans = wgetch(win)) != '\r') {
		i++;
		if (menu[i] == NULL)
			i = 0;
		if (ans == ESC)
			return(NULL);
		mvwprintw(win, cy, cx, "%-30.30s", menu[i]);
		wmove(win, cy, cx);
		wrefresh(win);
	}
					/* display the value in bold */
	clear_line(win, y, x, FALSE);
	wattrstr(win, A_BOLD, menu[i]);
					/* return the value */
	return(menu[i]);
}
