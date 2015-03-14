/*
 * Spawn a shell with file descriptors that look like this:
 *
 *              +---------+
 *   TTYin ---> | input   | ---> screen (& virtual screen)
 *              | program |
 *              +---------+
 *                   |                     +---------+
 *                  pipe ----------> stdin |         |
 *                                         |  shell  |
 *   TTYout <---------------------- stdout |         |
 *                                         +---------+
 *
 * The input program has a routine that duplicates the TTYin and sends it
 * down a pipe.  The other end of the pipe is the stdin to the shell script.
 * This allows the characters to appear on the screen *and* be interpreted
 * by the shell script.
 */

#include <stdio.h>
#include <signal.h>
#include <curses.h>
#include <errno.h>
#include "config.h"
#include "dial_dir.h"
#include "status.h"

#ifdef BSD
#include <sys/file.h>
#else /* BSD */
#include <fcntl.h>
#endif /* BSD */

void
do_script(extra)
char *extra;
{
	extern int fd, errno;
	SIG_TYPE (*istat)(), (*qstat)();
	int epid, dup_pipe[2], want_out, sig_status;
	char buf[80], *ttyname(), *strcpy(), *path, *findfile();
	void _exit(), error_win(), input_off(), do_input();

					/* if empty */
	if (*dir->script[dir->d_cur] == '\0')
		return;
					/* if a device */
	if (chk_script(dir->script[dir->d_cur]))
		return;
					/* find the script file */
	if ((path = findfile(extra, dir->script[dir->d_cur])) == NULL) {
		sprintf(buf, "Can't locate script \"%s\"", dir->script[dir->d_cur]);
		error_win(0, buf, "");
		return;
	}
					/* execute permission ? */
	if (access(path, 1)) {
		sprintf(buf, "\"%s\"", path);
		error_win(0, "No execute permission on script file", buf);
		return;
	}
					/* create a fd for duplicating input */
	if (pipe(dup_pipe) < 0)
		return;

	status->dup_fd = dup_pipe[1];
					/* start input duplication */
	do_input();

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
		dup(dup_pipe[0]);
					/* swap the stdout */
		close(1);
#ifdef UNIXPC
/*
 * Some strange things here... The OBM uses the second parameter of 
 * open() to determine if the the port should be in the DATA mode or
 * the VOICE mode.  Therefore, we must always open with read and write.
 */
		if (!strncmp(buf, "/dev/ph", 7))
			open(buf, O_RDWR);
		else
#endif /* UNIXPC */

		open(buf, O_WRONLY);
#ifdef SETUGID
		setgid(getgid());
		setuid(getuid());
#endif /* SETUGID */

		execl("/bin/sh", "sh", "-c", path, (char *) 0);
		_exit(1);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);

	/*
	 * Check the keyboard while the script is being "played".  If the
	 * user hits the <ESC> key, then kill the entire process group
	 * associated with the new shell.
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
					/* shut down the duplication of input */
	status->dup_fd = -1;

#ifndef SHAREDMEM
	input_off();
#endif /* SHAREDMEM */

	close(dup_pipe[0]);
	close(dup_pipe[1]);
	beep();
	return;
}
