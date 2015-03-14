/*
 * Do file name expansion with "native" shell.  Using the native shell
 * (as described in the SHELL environmental variable) allows for csh or
 * ksh abbreviations that sh doesn't recognize.  Returns a pointer to
 * a static area.
 */

#define EXPAND_BUF	2048

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include "config.h"

char *
expand(input)
char *input;
{
	extern char *null_ptr;
	FILE *pfp, *n_popen();
	int last;
	char buf[1024], *strpbrk(), *strcpy();
	static char ans[EXPAND_BUF];

					/* same rules as str_dup() */
	if (input == NULL)
		return(NULL);
	if (*input == '\0')
		return(null_ptr);
					/* any thing to expand? */
	if (!strpbrk(input, "$*{}[]\\?~")) {
		strcpy(ans, input);
		return(ans);
	}
					/* popen an echo */
	sprintf(buf, "echo %s", input);

	pfp = n_popen(buf, "r");
	fgets(ans, EXPAND_BUF, pfp);
	n_pclose(pfp);

	if (!strlen(ans)) {
		strcpy(ans, input);
		return(ans);
	}
	/*
	 * A horrible kludge...  if the last character is not a line
	 * feed, then the csh has returned an error message.  Otherwise
	 * zap the line feed.
	 */
	last = strlen(ans) -1;
	if (ans[last] != '\n') {
		strcpy(ans, input);
		return(ans);
	}
	else
		ans[last] = '\0';

	return(ans);
}

#define	tst(a,b) (*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static int popen_pid[20];

FILE *
n_popen(cmd, mode)
char *cmd, *mode;
{
	int myside, hisside, ppid, p[2];
	char *shellpath, *shell, *flags, *getenv(), *strrchr();
	void _exit();

	if (pipe(p) < 0)
		return NULL;

	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
					/* get the environmental variable */
	shellpath = getenv("SHELL");
	if (shellpath == NULL || *shellpath == '\0')
		shellpath = "/bin/sh";

	if (shell = strrchr(shellpath, '/'))
		shell++;
	else {
		shellpath = "/bin/sh";
		shell = "sh";
	}
					/* fix up the flags */
	if (!strcmp(shell, "csh"))
		flags = "-fc";
	else
		flags = "-c";		/* Korn shell too */

	if (!(ppid = fork())) {
		int stdio;
					/* no error messages please */
		close(2);
		open("/dev/null", O_WRONLY);
#ifdef SETUGID
		setgid(getgid());
		setuid(getuid());
#endif /* SETUGID */
		stdio = tst(0, 1);
		close(myside);
		close(stdio);
		fcntl(hisside, F_DUPFD, stdio);
		close(hisside);
		execl(shellpath, shell, flags, cmd, (char *) 0);
		_exit(1);
	}
	if (ppid == -1) {
		close(myside);
		close(hisside);
		return NULL;
	}

	popen_pid[myside] = ppid;

	close(hisside);
	return(fdopen(myside, mode));
}

n_pclose(ptr)
FILE *ptr;
{
	SIG_TYPE (*hstat)(), (*istat)(), (*qstat)();
	int f, r, sig_status;

	f = fileno(ptr);
	fclose(ptr);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);

	while ((r = wait(&sig_status)) != popen_pid[f] && r != -1)
		;

	if (r == -1)
		sig_status = -1;

	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
	return(sig_status);
}
