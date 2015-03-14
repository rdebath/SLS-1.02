/* pty_bsd.c - routines to allocate ptys - BSD version

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include <stdio.h>		/* tmp for debugging */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include "exp_rename.h"

extern int errno;

#ifdef O_NOCTTY
#define RDWR ((O_RDWR)|(O_NOCTTY))
#else
#define RDWR O_RDWR
#endif

void debuglog();

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

static char	line[] = "/dev/ptyXX";
static char	*tty_type;		/* ptr to char [pt] denoting
					   whether it is a pty or tty */
static char	*tty_bank;		/* ptr to char [p-z] denoting
					   which bank it is */
static char	*tty_num;		/* ptr to char [0-f] denoting
					   which number it is */

static int i_read_errno;/* place to save errno, if i_read() == -1, so it
			   doesn't get overwritten before we get to read it */
static jmp_buf env;		/* for interruptable read() */
static int env_valid = FALSE;	/* whether we can longjmp or not */

/* sigalarm_handler and i_read are here just for supporting the sanity */
/* checking of pty slave devices.  I have only seen this happen on BSD */
/* systems, but it may need to be done to the other pty implementations */
/* as well. */

/* Note that this code is virtually replicated from other code in expect */
/* At some point, I'll dump one, but not until I'm satisfied no other */
/* changes are needed */

/*ARGSUSED*/
static void
sigalarm_handler(n)
int n;		/* unused, for compatibility with STDC */
{
#ifdef REARM_SIG
	signal(SIGALRM,sigalarm_handler);
#endif

	/* check env_valid first to protect us from the alarm occurring */
	/* in the window between i_read and alarm(0) */
	if (env_valid) longjmp(env,1);
}

/* interruptable read */
static int
i_read(fd,buffer,length,timeout)
int fd;
char *buffer;
int length;
int timeout;
{
	int cc = -2;

	/* since setjmp insists on returning 1 upon longjmp(,0), */
	/* longjmp(,2) instead. */

	/* restart read if setjmp returns 0 (first time) or 2. */
	/* abort if setjmp returns 1. */

	alarm(timeout);

	if (1 != setjmp(env)) {
		env_valid = TRUE;
		cc = read(fd,buffer,length);
	}
	env_valid = FALSE;
	i_read_errno = errno;	/* errno can be overwritten by the */
				/* time we return */
	alarm(0);
	return(cc);
}

static void
pty_stty(s,name)
char *s;		/* args to stty */
char *name;		/* name of pty */
{
	int cc;

#define MAX_ARGLIST 10240
	char buf[MAX_ARGLIST];	/* overkill is easier */

	/* tell Saber to shut up over confusion of sprintf return type */
	/*SUPPRESS 701*/
	sprintf(buf,"stty %s < %s > %s",s,name,name);
	debuglog("getptyslave: system(\"%s\") = ",buf);
	cc = system(buf);
	debuglog("%d\n",cc);
}

struct sgttyb exp_tty_original;	/* tty parms that include raw/echo */

int dev_tty;		/* file descriptor to /dev/tty or -1 if none */
int knew_dev_tty;	/* true if we had our hands on /dev/tty at any time */

#define GET_TTYTYPE	0
#define SET_TTYTYPE	1
static void
ttytype(request,fd,s)
int request;
int fd;
char *s;	/* stty args, used only if request == SET_TTYTYPE */
{
	static struct	tchars tc;		/* special characters */
	static struct	ltchars lc;		/* local special characters */
	static struct	winsize win;		/* window size */
	static int	lb;			/* local modes */
	static int	l;			/* line discipline */

	if (request == GET_TTYTYPE) {
		if (-1 == ioctl(fd, TIOCGETP, (char *)&exp_tty_original)
		 || -1 == ioctl(fd, TIOCGETC, (char *)&tc)
		 || -1 == ioctl(fd, TIOCGETD, (char *)&l)
		 || -1 == ioctl(fd, TIOCGLTC, (char *)&lc)
		 || -1 == ioctl(fd, TIOCLGET, (char *)&lb)
		 || -1 == ioctl(fd, TIOCGWINSZ, (char *)&win)) {
			knew_dev_tty = FALSE;
			dev_tty = -1;
		}
	} else {	/* type == SET_TTYTYPE */
		if (knew_dev_tty) {
			(void) ioctl(fd, TIOCSETP, (char *)&exp_tty_original);
			(void) ioctl(fd, TIOCSETC, (char *)&tc);
			(void) ioctl(fd, TIOCSLTC, (char *)&lc);
			(void) ioctl(fd, TIOCLSET, (char *)&lb);
			(void) ioctl(fd, TIOCSETD, (char *)&l);
			(void) ioctl(fd, TIOCSWINSZ, (char *)&win);
		} else {
			/* if running in the background, we have no access */
			/* to a a tty to copy parameters from, so use ones */
			/* supplied by original Makefile */
#ifdef __SABER__
#undef DFLT_STTY
#define DFLT_STTY "sane"
#endif
			debuglog("getptyslave: (default) stty %s\n",DFLT_STTY);
			pty_stty(DFLT_STTY,line);
		}
		if (s) {
			/* give user a chance to override any terminal parms */
			debuglog("getptyslave: (user-requested) stty %s\n",s);
			pty_stty(s,line);
		}
	}
}

static char lock[] = "/tmp/ptylock.XX";	/* XX is replaced by pty id */
static char locksrc[50] = "/tmp/expect.pid"; /* pid is replaced by real pid */
	/* locksrc is used as the link source, i.e., something to link from */

void
init_pty()
{
	tty_type = &line[strlen("/dev/")];
	tty_bank = &line[strlen("/dev/pty")];
	tty_num  = &line[strlen("/dev/ptyp")];

	dev_tty = open("/dev/tty",O_RDWR);

#if experimental
	/* code to allocate force expect to get a controlling tty */
	/* even if it doesn't start with one (i.e., under cron). */
	/* This code is not necessary, but helpful for testing odd things. */
	if (dev_tty == -1) {
		/* give ourselves a controlling tty */
		int master = getptymaster();
		fcntl(master,F_SETFD,1);	/* close-on-exec */
		setpgrp(0,0);
		close(0);
		close(1);
		getptyslave(get_var("stty_init"));
		close(2);
		fcntl(0,F_DUPFD,2);		/* dup 0 onto 2 */
	}
#endif

	knew_dev_tty = (dev_tty != -1);
	if (knew_dev_tty) ttytype(GET_TTYTYPE,dev_tty,(char *)0);
}

/* returns fd of master end of pseudotty */
int
getptymaster()
{
	char *hex;
	struct stat statbuf;
	int master;
	int lfd;	/* locksrc file descriptor */
	int locked = 0;
	time_t current_time;

	void (* func)() = signal(SIGALRM,sigalarm_handler);

	time(&current_time);

	/* recreate locksrc to prevent locks from 'looking old', so that */
	/* they are not deleted (later on in this code) */
	sprintf(locksrc,"/tmp/expect.%d",getpid());
	(void) unlink(locksrc);
	if (-1 == (lfd = creat(locksrc,0777))) {
		debuglog("can't create %s, errno = %d\n",locksrc, errno);
		return(-1);
	}
	close(lfd);

	for (*tty_bank = 'p';; (*tty_bank)++) {
		*tty_num = '0';
		if (stat(line, &statbuf) < 0) break;
		for (hex = "0123456789abcdef";*hex;hex++) {
			int cc;
			char c;
			int slave;

			*tty_num = *hex;

			/* make a lock file to prevent others (for now only */
			/* expects) from allocating pty while we are playing */
			/* with it.  This allows us to rigorously test the */
			/* pty is usable. */

			if (locked) {
				(void) unlink(lock);
				locked = 0;
			}
			sprintf(lock,"/tmp/ptylock.%c%c",*tty_bank,*tty_num);
			if (-1 == (link(locksrc,lock))) {
				/* unlink any real old ones (i.e., > 1 hour) */
				if ((0 == stat(lock,&statbuf)) &&
				    (statbuf.st_mtime+3600 < current_time)) {
					(void) unlink(lock);
				}
				continue;
			}
			locked = 1;

			/* verify no one else is using slave by attempting */
			/* to read eof from master side */
			*tty_type = 'p';
			if (0 > (master = open(line,RDWR))) continue;
			*tty_type = 't';
			if (0 > (slave = open(line,RDWR))) {
				(void) close(master);
				continue;
			}
			(void) close(slave);
			cc = i_read(master,&c,1,10);
			(void) close(master);
			if (!(cc == 0 || cc == -1)) {
				debuglog("%s slave open, skipping\n",line);
				continue;
			}

			/* verify no one else is using master by attempting */
			/* to read eof from slave side */
			*tty_type = 'p';
			if (0 > (master = open(line,RDWR))) continue;
			*tty_type = 't';
			if (0 > (slave = open(line,RDWR))) continue;
			(void) close(master);
			cc = i_read(slave,&c,1,10);
			(void) close(slave);
			if (!(cc == 0 || cc == -1)) {
				debuglog("%s master open, skipping\n",line);
				continue;
			}

			/* seems ok, let's use it */
			*tty_type = 'p';
			if (0 > (master = open(line,RDWR))) continue;
			*tty_type = 't';

			signal(SIGALRM,func);
			(void) unlink(locksrc);
			debuglog("using pty %s\n",line);
			return(master);
		}
	}
	signal(SIGALRM,func);
	unlink(locksrc);
	return(-1);
}

int
getptyslave(stty_args)
char *stty_args;
{
	int slave;

	if (0 > (slave = open(line, O_RDWR))) return(-1);

	/* sanity check - if slave not 0, skip rest of this and return */
	/* to what will later be detected as an error in caller */
	if (0 != slave) return(slave);

	fcntl(0,F_DUPFD,1);	/* duplicate 0 onto 1 to prepare for stty */
	ttytype(SET_TTYTYPE,slave,stty_args);
	(void) unlink(lock);
	return(slave);
}

#if 0
opendevtty()
{
	int fd;
	int p;

	errorlog("--opendevtty--\n");
	errorlog("pid = %d\n",getpid());
	errorlog("pgrp = %d\n",getpgrp(0));
	ioctl(0,TIOCGPGRP,&p);
	errorlog("tgrp(0) = %d\n",p);
	if (-1 == (fd = open("/dev/tty",2))) {
		perror("open"); errorlog("sanity open failed\n");
	} else errorlog("sanity open succeeded\n");
}
#endif
