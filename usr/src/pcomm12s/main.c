/*
 * Pcomm is a public domain telecommunication program for Unix that
 * is designed to operate similar to the MSDOS program, ProComm.
 * ProComm (TM) is copyrighted by Datastorm Technologies, Inc.
 *
 * Emmet P. Gray			US Army, HQ III Corps & Fort Hood
 * ...!uunet!uiucuxc!fthood!egray	Attn: AFZF-DE-ENV
 *					Directorate of Engineering & Housing
 *					Environmental Management Office
 *					Fort Hood, TX 76544-5057
 *
 *	Release v1.0	12 Mar 88
 *	Release v1.1	21 Aug 88
 *	Release v1.2.0	 4 Feb 89
 *	Patch #1	18 Feb 89
 *	Patch #2	11 Mar 89
 *	Patch #3	11 May 89
 *	Patch #4	23 Jun 89
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <curses.h>
#include <sys/types.h>
#include <sys/stat.h>
#define	MAIN
#include "config.h"
#include "dial_dir.h"
#include "extrnl.h"
#include "misc.h"
#include "modem.h"
#include "param.h"
#include "status.h"

#ifndef OLDCURSES
#include <term.h>
#else /* OLDCURSES */
#ifdef UNIXPC
#include <sgtty.h>
#endif /* UNIXPC */
char tcbuf[1024];
struct sgttyb t_mode, c_mode;
#ifndef linux
#define cbreak crmode
#endif
#endif /* OLDCURSES */

#ifdef SHAREDMEM
int shm_id;
#endif /* SHAREDMEM */

struct DIAL_DIR *dir;
struct EXTRNL *extrnl;
struct MODEM *modem;
struct PARAM *param;
struct STATUS *status;

int fd = -1;				/* file descriptor for port */
int xmc;				/* magic cookie terminal */
int msg_status;				/* read/write permissions on TTY */
char *null_ptr = "";			/* generic null pointer */

main(argc, argv)
int argc;
char *argv[];
{
	extern char *optarg;
	int c, i, code, quit();
	char *mytty, *ttyname(), *term, *getenv(), *sys_name, *str_dup();
	char *extra_dir, buf[80], message[80];
	struct DIAL_DIR *read_dir();
	struct EXTRNL *read_extrnl();
	struct MODEM *read_modem();
	struct PARAM *read_param();
	struct STATUS *init();
	struct stat stbuf;
	void exit(), error_win(), free_ptr();
#ifdef OLDCURSES
	char *tgetstr(), *t, tb[1024];
	t = tcbuf;
#endif /* OLDCURSES */

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, quit);
	signal(SIGHUP, quit);

	sys_name = NULL;
	extra_dir = NULL;
					/* the command line */
	while ((c = getopt(argc, argv, "d:f:")) != EOF) {
		switch (c) {
			case 'd':	/* the extra directory to search */
				extra_dir = str_dup(optarg);
				break;
			case 'f':	/* the short cut into the dialing dir */
				sys_name = str_dup(optarg);
				break;
			case '?':	/* default */
				fprintf(stderr, "Usage: pcomm [-d directory] [-f system name]\n");
				exit(1);
				break;
		}
	}
					/* get terminal type */
	term = getenv("TERM");
	if (term == NULL || *term == '\0') {
		fprintf(stderr, "Windows not supported (TERM not defined)\n");
		exit(1);
	}
					/* see if terminfo entry exists */
#ifdef OLDCURSES
	i = tgetent(tb, term);
#else /* OLDCURSES */
	setupterm(term, 1, &i);
#endif /* OLDCURSES */

	if (i != 1) {
		fprintf(stderr, "Windows not supported (no terminfo data for \"%s\")\n", term);
		exit(1);
	}
					/* minimum screen size */
#ifdef OLDCURSES
	if (tgetnum("co") < 80 || tgetnum("li") < 24) {
#else /* OLDCURSES */
	if (columns < 80 || lines < 24) {
#endif /* OLDCURSES */
		fprintf(stderr, "Windows not supported (minimum 80x24 screen required)\n");
		exit(1);
	}
					/* must have cursor movement */
#ifdef OLDCURSES
	if (tgetstr("cm", &t) == NULL) {
#else /* OLDCURSES */
	if (cursor_address == NULL) {
#endif /* OLDCURSES */
		fprintf(stderr, "Windows not supported (terminal too dumb)\n");
		exit(1);
	}
					/* load magic cookie variable */
#ifdef OLDCURSES
	xmc = tgetnum("sg");
#else /* OLDCURSES */
	xmc = magic_cookie_glitch;
#endif /* OLDCURSES */
					/* ok... now let's go! */
#ifdef OLDCURSES
	ioctl(0, TIOCGETP, &t_mode);
#endif /* OLDCURSES */

	initscr();
	nonl();
	cbreak();
	noecho();
#ifdef XENIX_3
	raw();
#endif /* XENIX_3 */

#ifdef OLDCURSES
	ioctl(0, TIOCGETP, &c_mode);
#endif /* OLDCURSES */

	dir = (struct DIAL_DIR *) NULL;
	extrnl = (struct EXTRNL *) NULL;
	param = (struct PARAM *) NULL;
	modem = (struct MODEM *) NULL;
					/* show the herald, return status */
	status = init(sys_name);
					/* get "msgs" status */
	mytty = ttyname(0);
	stat(mytty, &stbuf);
	msg_status = stbuf.st_mode & 0777;
	chmod(mytty, 0600);

	mvaddstr(12, 31, "Initializing...");
	refresh();
					/* read the support files */
	param = read_param(extra_dir);
	dir = read_dir(extra_dir);
	extrnl = read_extrnl(extra_dir);
	modem = read_modem(extra_dir);

					/* warning about screen size */
	if (LINES > MAX_ROW || COLS > MAX_COL-1)
		error_win(0, "Your screen size exceeds an internal Pcomm limit",
		 "The edges of the screen may contain garbage");

					/* short-cut to dialing window? */
	code = 0;
	if (sys_name != NULL) {
		for (i=1; i<dir->d_entries+1; i++) {
			if (match_ci(dir->name[i], sys_name)) {
				dir->q_num[0] = i;
				dir->d_cur = i;
				break;
			}
		}
					/* if match is found */
		if (dir->q_num[0] != -1)
			code = dial_win();
		else {
			sprintf(buf, "Can't match \"%s\" in dialing directory", sys_name);
			sprintf(message, "file \"%s\"", dir->d_path);
			error_win(0, buf, message);
		}
		free_ptr(sys_name);
	}
					/* start terminal dialogue */
	terminal(extra_dir, code);
	exit(0);
}

/*
 * Something dreadful happened...  Clean up the mess we made with the
 * TTY driver and release the phone line.
 */

int
quit()
{
	void cleanup();

	cleanup(1);
					/* never returns... */
	return(0);
}

/*
 * Check write permission with the real UID and GID.  Returns a 0 on
 * permission denied, 1 on OK, and 2 on OK-but the file already exists.
 */

int
can_write(file)
char *file;
{
	char *p, path[256], *strcpy(), *strrchr();

	strcpy(path, file);
					/* dissect the path component */
	if (p = strrchr(path, '/'))
		*p = '\0';
	else
		strcpy(path, ".");
					/* if it already exists */
	if (!access(file, 0)) {
		if (!access(file, 2))
			return(OK_BUT_EXISTS);
		return(DENIED);
	}
					/* if path is writable */
	if (!access(path, 2))
		return(WRITE_OK);
	return(DENIED);
}

/*
 * Check the read and write permissions before opening a file.  This
 * is a horrible kludge to work around the fact that a lot of systems
 * that claim to be SVID compatible don't treat setuid(2) and setgid(2)
 * properly.  For example, on a Masscomp, you can't flip-flop back and
 * forth between the real and effective UID/GID.
 */

FILE *
my_fopen(file, mode)
char *file, *mode;
{
	FILE *fp;

#ifdef SETUGID
#ifdef SETUID_BROKE
	switch (*mode) {
		case 'a':
		case 'w':
			switch(can_write(file)) {
				case DENIED:
					fp = (FILE *) NULL;
					break;
				case OK_BUT_EXISTS:
					fp = fopen(file, mode);
					break;
				case WRITE_OK:
					fp = fopen(file, mode);
					chown(file, getuid(), getgid());
					break;
			}
			break;
		case 'r':
			if (access(file, 4))
				fp = (FILE *) NULL;
			else
				fp = fopen(file, mode);
			break;
	}
#else /* SETUID_BROKE */
	int euid, egid;

	euid = geteuid();
	egid = getegid();
					/* abdicate the throne */
	setuid(getuid());
	setgid(getgid());

	fp = fopen(file, mode);
					/* put things back */
	setuid(euid);
	setgid(egid);
#endif /* SETUID_BROKE */
	return(fp);
#else /* SETUGID */
	return(fopen(file, mode));
#endif /* SETUGID */
}

/*
 * See if s2 in contained in s1 (case insensitive).  Returns a 1 on yes,
 * and a 0 on no.
 */

match_ci(s1, s2)
char *s1, *s2;
{
	int i;
	char str1[128], str2[128], *strstr();

					/* copy the strings to lower case */
	i = 0;
	while(*s1) {
		if (isupper(*s1))
			str1[i++] = tolower(*s1);
		else
			str1[i++] = *s1;

		if (i >= 127)
			break;
		s1++;
	}
	str1[i] = '\0';

	i = 0;
	while(*s2) {
		if (isupper(*s2))
			str2[i++] = tolower(*s2);
		else
			str2[i++] = *s2;

		if (i >= 127)
			break;
		s2++;
	}
	str2[i] = '\0';
					/* do they match? */
	if (strstr(str1, str2))
		return(1);
	return(0);
}
