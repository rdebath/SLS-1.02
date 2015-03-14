/*
 * Display the external protocol setup, query for changes.  A non-zero
 * return code means something was changed.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "extrnl.h"
#include "misc.h"

int
ext_setup()
{
	extern char *null_ptr;
	WINDOW *ext_win, *newwin();
	int i, ret_code;
	char *str, *get_str();
	void disp_ext();

	ext_win = newwin(23, 80, 0, 0);

	horizontal(ext_win, 0, 0, 27);
	mvwattrstr(ext_win, 0, 28, A_BOLD, "External Protocol Setup");
	horizontal(ext_win, 0, 52, 27);
	mvwaddstr(ext_win, 3, 36, "UPLOAD");
	mvwaddstr(ext_win, 5, 8, "Name");
	mvwaddstr(ext_win, 5, 21, "Command line");
	mvwaddstr(ext_win, 5, 54, "Requires file list?");
	mvwaddstr(ext_win, 10, 35, "DOWNLOAD");
	mvwaddstr(ext_win, 12, 8, "Name");
	mvwaddstr(ext_win, 12, 21, "Command line");
	mvwaddstr(ext_win, 12, 54, "Requires file list?");
					/* display the current list */
	disp_ext(ext_win);

	horizontal(ext_win, 19, 0, 80);
	mvwattrstr(ext_win, 20, 0, A_BOLD, "OPTION ==> ");
	mvwaddstr(ext_win, 20, 58, "Press <ESC> to return");
	wmove(ext_win, 20, 12);
	touchwin(ext_win);
	wrefresh(ext_win);
					/* get the option */
	ret_code = 0;
	while ((str = get_str(ext_win, 1, "1234356", "")) != NULL) {
		switch(*str) {
			case '1':
				if (ext_prompt(ext_win, 1, 0, 6))
					ret_code++;
				break;
			case '2':
				if (ext_prompt(ext_win, 1, 1, 7))
					ret_code++;
				break;
			case '3':
				if (ext_prompt(ext_win, 1, 2, 8))
					ret_code++;
				break;
			case '4':
				if (ext_prompt(ext_win, 0, 0, 13))
					ret_code++;
				break;
			case '5':
				if (ext_prompt(ext_win, 0, 1, 14))
					ret_code++;
				break;
			case '6':
				if (ext_prompt(ext_win, 0, 2, 15))
					ret_code++;
				break;
		}
		mvwaddstr(ext_win, 20, 12, "  ");
		clear_line(ext_win, 21, 0, FALSE);
		clear_line(ext_win, 22, 0, FALSE);
		wmove(ext_win, 20, 12);
		wrefresh(ext_win);
	}
	/*
	 * Recalculate the number of entries.  Please notice that if you
	 * create an empty entry (a hole), all entries after that are ignored.  
	 * The software doesn't compact the holes out.. you're on your own.
	 */
	if (ret_code) {
		for (i=0; i<3; i++) {
			if (extrnl->name[1][i] == null_ptr)
				break;
		}
		extrnl->up_entries = i;

		for (i=0; i<3; i++) {
			if (extrnl->name[0][i] == null_ptr)
				break;
		}
		extrnl->dn_entries = i;
	}
	delwin(ext_win);
	return(ret_code);
}

/*
 * Display the current list of external file transfer programs.
 */

static void
disp_ext(win)
WINDOW *win;
{
	int i, up, entry, line;

	up = 1;
	line = 6;
	for (i=0; i<6; i++) {
		if (i < 3)
			entry = i;
		else {
			up = 0;
			entry = i-3;
			line = 10;
		}
		mvwprintw(win, i+line, 5, "%d) %-12.12s %-40.40s  %c\n",
		 i+1, extrnl->name[up][entry], extrnl->command[up][entry],
		 extrnl->prompt[up][entry]);
	}
	return;
}

/*
 * Prompt for the info in the database.  A non-zero return code means
 * that something was changed.  To delete the line, you enter a single
 * space character at the first prompt.
 */

static int
ext_prompt(win, up, entry, line)
WINDOW *win;
int up, entry, line;
{
	extern char *v_yn[], *null_ptr;
	char *ans, t_name[80], t_command[80], *str_prompt(), *str_rep();
	char *strcpy(), *menu_prompt();

					/* get temp name */
	if ((ans = str_prompt(win, line, 8, "Protocol name", "")) == NULL)
		return(0);

	strcpy(t_name, ans);
	clear_line(win, 21, 0, FALSE);
					/* are we zapping the line */
	if (ans == null_ptr) {
		extrnl->name[up][entry] = null_ptr;
		extrnl->command[up][entry] = null_ptr;
		extrnl->prompt[up][entry] = 'N';
		return(1);
	}
					/* get temp command */
	if ((ans = str_prompt(win, line, 21, "Command line", "")) == NULL)
		return(0);

	strcpy(t_command, ans);
	clear_line(win, 21, 0, FALSE);

					/* get temp prompt */
	if ((ans = menu_prompt(win, line, 63, "Does it require a file list", v_yn)) == NULL)
		return(0);

	wrefresh(win);
					/* store 'em for real */
	extrnl->name[up][entry] = str_rep(extrnl->name[up][entry], t_name);
	extrnl->command[up][entry] = str_rep(extrnl->command[up][entry], t_command);
	extrnl->prompt[up][entry] = *ans;

	return(1);
}
