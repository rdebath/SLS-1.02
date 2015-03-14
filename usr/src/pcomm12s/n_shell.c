/*
 * Spawn a "native" shell.  Native means the shell found in the SHELL
 * environmental variable.
 */

#include <stdio.h>
#include <signal.h>
#include <curses.h>
#include "config.h"

void
n_shell()
{
	WINDOW *sh_win, *newwin();
	SIG_TYPE (*istat)(), (*qstat)();
	int sig_status, spid, w;
	char *shell, *shellpath, *getenv(), *strrchr();
	unsigned int sleep();
	void _exit();
					/* a full window */
	sh_win = newwin(LINES, COLS, 0, 0);

	touchwin(sh_win);
	waddstr(sh_win, "Pcomm <=> Unix gateway, use ^D or 'exit' to return\n");
	wrefresh(sh_win);
					/* out of curses mode */
	resetterm();

	shellpath = getenv("SHELL");
	if (shellpath == NULL || *shellpath == '\0')
		shellpath = "/bin/sh";

	if (shell = strrchr(shellpath, '/'))
		shell++;
	else {
		shellpath = "/bin/sh";
		shell = "sh";
	}

	if (!(spid = fork())) {
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
#ifdef SETUGID
		setgid(getgid());
		setuid(getuid());
#endif /* SETUGID */
		execl(shellpath, shell, "-i", (char *) 0);
		_exit(1);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);

	while ((w = wait(&sig_status)) != spid && w != -1)
		;

	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
					/* back to curses mode */
	sleep(1);
	fixterm();

	clearok(curscr, TRUE);
	werase(sh_win);
	wrefresh(sh_win);
	delwin(sh_win);
	return;
}
