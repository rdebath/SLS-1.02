/*
 * Open a window to prompt for a new directory.  Checks to see you have
 * search permission.
 */

#include <curses.h>
#include "config.h"
#include "misc.h"

void
chg_dir()
{
	extern int fd;
	WINDOW *ch_win, *newwin();
	char *ans, *dir, *expand(), *cwd, *getcwd(), cwdbuf[200];
	char *get_str();

	cwd = getcwd(cwdbuf, 200);

	ch_win = newwin(6, 70, 5, 5);

	mvwprintw(ch_win, 2, 4, "Current directory: %s", cwd);
	mvwaddstr(ch_win, 3, 4, "New directory: ");
	box(ch_win, VERT, HORZ);

	mvwattrstr(ch_win, 0, 3, A_BOLD, " Change directory ");
	wmove(ch_win, 3, 19);
	wrefresh(ch_win);
					/* get the answer */
	while ((ans = get_str(ch_win, 80, "", " \t\n")) != NULL) {
					/* a CR means no change */
		if (*ans == '\0')
			break;
					/* expand the input */
		dir = expand(ans);
					/* if you have search permission */
		if (!access(dir, 1)) {
			if (!chdir(dir))
				break;
		}
		beep();
		mvwattrstr(ch_win, 4, 15, A_BOLD, "No such directory or no access permission");
		wrefresh(ch_win);
		wait_key(ch_win, 3);
					/* clean up the mess */
		clear_line(ch_win, 3, 19, TRUE);
		clear_line(ch_win, 4, 14, TRUE);
		wmove(ch_win, 3, 19);
		wrefresh(ch_win);
	}
	if (fd == -1) {
		werase(ch_win);
		wrefresh(ch_win);
	}
	delwin(ch_win);
	return;
}

#ifdef BSD
/*
 * Get the current working directory, AT&T style.  Well... not really, it
 * doesn't handle a NULL pointer for the buffer.
 */

/* ARGSUSED */
char *
getcwd(buf, len)
char *buf;
int len;
{
	char *getwd();

	return(getwd(buf));
}
#endif /* BSD */
