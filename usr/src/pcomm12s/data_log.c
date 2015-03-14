/*
 * Open a window to prompt for a path name to be used for the data logging
 * feature.  Also turns on the data logging.  A non-zero return code means
 * we need to restart the input routine.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "param.h"
#include "status.h"

int
data_logging()
{
	extern int fd;
	int ret_code;
	WINDOW *dl_win, *newwin();
	char *ans, *path, *expand(), *get_str(), *strcpy();
	void input_off();

	dl_win = newwin(6, 70, 5, 5);

	mvwprintw(dl_win, 2, 4, "Default log file: %s", param->logfile);
	mvwaddstr(dl_win, 3, 4, "New log file: ");
	box(dl_win, VERT, HORZ);

	mvwattrstr(dl_win, 0, 3, A_BOLD, " Start Data Logging ");
	wmove(dl_win, 3, 18);
	wrefresh(dl_win);
					/* get the path */
	ret_code = 0;
	while ((ans = get_str(dl_win, PATH, "", " \t\n")) != NULL) {
					/* give 'em the default */
		if (*ans == '\0')
			path = param->logfile;
		else
			path = expand(ans);

					/* test write permission */
		if (can_write(path)) {
			ret_code++;
			break;
		}

		beep();
		mvwattrstr(dl_win, 4, 24, A_BOLD, "No write permission");
		wrefresh(dl_win);
		wait_key(dl_win, 3);
					/* clean up the mess */
		clear_line(dl_win, 3, 18, TRUE);
		clear_line(dl_win, 4, 24, TRUE);
		wmove(dl_win, 3, 18);
		wrefresh(dl_win);
	}
	if (ret_code) {
		strcpy(status->log_path, path);
		status->log = 1;
		/*
		 * Without shared memory, killing and restarting the input
		 * routine is the only way to change the name of the file
		 * that the input routines uses.
		 */
#ifdef SHAREDMEM
		ret_code = 0;
#else /* SHAREDMEM */
		input_off();
#endif /* SHAREDMEM */
	}
	if (fd == -1) {
		werase(dl_win);
		wrefresh(dl_win);
	}
	delwin(dl_win);

	return(ret_code);
}
