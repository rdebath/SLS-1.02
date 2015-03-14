/*
 * Spawn a shell with the stdin and stdout swapped with the remote
 * system, ie:
 *                               +----------+
 *    TTYin ------------> stdin  |          |
 *                               |  shell   |
 *    TTYout <---------- stdout  |          |
 *                               +----------+
 *
 * An undocumented feature:  The external protocol gateway
 * can be used to pipe the output of a normal Unix command to the
 * remote system.
 */

#include <stdio.h>
#include <signal.h>
#include <curses.h>
#include <errno.h>
#include "config.h"

#ifdef BSD
#include <sys/file.h>
#else /* BSD */
#include <fcntl.h>
#endif /* BSD */

void
do_extrnl(cmd)
char *cmd;
{
	extern int fd, errno;
	WINDOW *xt_win, *newwin();
	SIG_TYPE (*istat)(), (*qstat)();
	int epid, want_out, sig_status;
	char buf[80], *ttyname(), *strcpy();
	unsigned int sleep();
	void _exit(), input_off();

	input_off();
					/* a full window */
	xt_win = newwin(LINES, COLS, 0, 0);
	touchwin(xt_win);
	wrefresh(xt_win);
					/* out of curses mode */
	resetterm();

	if (!(epid = fork())) {
					/* create a new process group ID */
#ifdef BSD
		setpgrp(0, getpid());
#else /* BSD */
		setpgrp();
#endif /* BSD */
					/* recreate the device name */
		strcpy(buf, ttyname(fd));
		close(fd);
					/* swap the stdin */
		close(0);
#ifdef UNIXPC
/*
 * Some strange things here... The OBM uses the second parameter of 
 * open() to determine if the port should be in the DATA mode or
 * the VOICE mode.  Therefore, we must always open with read and write.
 */
		if (!strncmp(buf, "/dev/ph", 7))
			open(buf, O_RDWR);
		else
#endif /* UNIXPC */
		open(buf, O_RDONLY);
					/* swap the stdout */
		close(1);
#ifdef UNIXPC
		if (!strncmp(buf, "/dev/ph", 7))
			open(buf, O_RDWR);
		else
#endif /* UNIXPC */
		open(buf, O_WRONLY);
#ifdef SETUGID
		setgid(getgid());
		setuid(getuid());
#endif /* SETUGID */
		execl("/bin/sh", "sh", "-c", cmd, (char *) 0);
		_exit(1);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);

	/*
	 * Check the keyboard while the external program is running.  If
	 * the user hits the <ESC> key, then kill the entire process
	 * group associated with the new shell.
	 */
	want_out = 0;
	while(1) {
		switch(wait_key(stdscr, 1)) {
			case -1:	/* timed out */
				break;
			case 27:	/* a user abort */
#ifdef BSD
				killpg(epid, SIGKILL);
#else /* BSD */
				kill(-epid, SIGKILL);
#endif /* BSD */
				want_out++;
				break;
			default:
				beep();
				break;
		}
		if (want_out)
			break;
					/* see if the process it still active */
#ifdef BSD
		if ((kill(epid, 0) == -1) && errno == ESRCH) 
#else /* BSD */
		if ((kill(-epid, 0) == -1) && errno == ESRCH) 
#endif /* BSD */
			break;
	}

	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
					/* back to curses mode */
	sleep(1);
	fixterm();

	clearok(curscr, TRUE);
	werase(xt_win);
	wrefresh(xt_win);
	delwin(xt_win);
	return;
}
