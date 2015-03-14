/*
 * Exit Pcomm.  A user requested abort.  There are a lot of things to do
 * before we exit!
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "param.h"
#include "status.h"

#ifdef SHAREDMEM
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif /* SHAREDMEM */

void
pexit()
{
	extern int fd;
	WINDOW *ex_win, *newwin();
	void cleanup(), st_line();

	ex_win = newwin(5, 33, 3, 7);

	box(ex_win, VERT, HORZ);
	mvwattrstr(ex_win, 0, 3, A_BOLD, " Exit ");
	if (yes_prompt(ex_win, 2, 4, A_BLINK, "Exit to Unix")) {
		st_line("   exiting");
		cleanup(0);
	}
	if (fd == -1) {
		werase(ex_win);
		wrefresh(ex_win);
	}
	delwin(ex_win);
	return;
}

/*
 * Do the clean up detail before we exit.  Only the status structure
 * is guaranteed to exit.
 */

void
cleanup(val)
int val;
{
	extern int msg_status;
	void release_port(), input_off(), exit();
	char *ttyname();
#ifdef SHAREDMEM
	extern int shm_id;
	void perror();
#endif /* SHAREDMEM */
					/* kill the input routine */
	input_off();
					/* release the port */
	release_port(QUIET);
					/* zap the virtual screen */
#ifdef SHAREDMEM
	if (shmdt((char *) status) < 0)
		perror("shmdt");
	if (shmctl(shm_id, IPC_RMID, (struct shmid_ds *) NULL) < 0)
		perror("shmctl");
#else /* SHAREDMEM */
	unlink(status->vs_path);
#endif /* SHAREDMEM */

	/*
	 * If we die an un-natural death (such as a SIGHUP on the loss of
	 * the controlling terminal) we won't have a terminal to mess with.
	 */
	if (isatty(0)) {
		touchwin(stdscr);
		clear();
		refresh();
		endwin();
					/* return the TTY chmod */
		chmod(ttyname(0), msg_status);
	}
	exit(val);
}

/*
 * Open a window to display an error message.  Handles both fatal and
 * non-fatal errors
 */

void
error_win(code, line_one, line_two)
int code;
char *line_one, *line_two;
{
	WINDOW *e_win, *newwin();
	void cleanup(), st_line();

					/* make sure we're in curses mode */
	fixterm();
	e_win = newwin(7, 70, 9, 5);
					/* display the nasty note */
	mvwaddstr(e_win, 2, 4, line_one);
	mvwaddstr(e_win, 3, 4, line_two);
	box(e_win, VERT, HORZ);

	if (code) {
		mvwattrstr(e_win, 0, 4, A_BOLD, " Error ");
		mvwattrstr(e_win, 5, 24, A_BLINK, "Press any key to exit");
		wmove(e_win, 5, 46);
	}
	else {
		mvwattrstr(e_win, 0, 4, A_BOLD, " Warning ");
		mvwattrstr(e_win, 5, 22, A_BLINK, "Press any key to continue");
		wmove(e_win, 5, 48);
	}
	beep();
	wrefresh(e_win);

	wgetch(e_win);
	werase(e_win);
	wrefresh(e_win);
	delwin(e_win);

	if (code) {
		st_line("   exiting");
		cleanup(code);
	}
	return;
}
