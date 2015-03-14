/* pty_usg.c - routines to allocate ptys - usg version

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#ifdef HPUX
#include <unistd.h>
#include <sys/ptyio.h>
#endif
#include <sys/file.h>
#ifdef AUX2
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif
#include "exp_tty.h"
#include "exp_rename.h"

void debuglog();

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif
#ifdef UTS
#include <sys/vty.h>
static char	line[MAXPTYNAMELEN];
static char	sline[MAXPTYNAMELEN];
#else
#ifdef HPUX
static char	line[] = "/dev/ptym/ptyXX";
static char	sline[] = "/dev/pty/ttyXX";
static char	*slave_bank;
static char	*slave_num;
#else
static char	line[] = "/dev/ptyXX";
#define sline line
#endif /* !HPUX */
#endif
static char	*tty_type;		/* ptr to char [pt] denoting
					   whether it is a pty or tty */
static char	*tty_bank;		/* ptr to char [p-z] denoting
					   which bank it is */
static char	*tty_num;		/* ptr to char [0-f] denoting
					   which number it is */

static void
pty_stty(s,name)
char *s;		/* args to stty */
char *name;		/* name of pty */
{
#define MAX_ARGLIST 10240
	char buf[MAX_ARGLIST];	/* overkill is easier */

	sprintf(buf,"stty %s < %s > %s",s,name,name);
	system(buf);
}

#ifdef POSIX
struct termios exp_tty_original;
#else
struct	termio exp_tty_original;
#endif

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
	static int is_a_tty;

	if (request == GET_TTYTYPE) {
#ifdef POSIX
		if (-1 == tcgetattr(fd, &exp_tty_original)) {
#else
		if (-1 == ioctl(fd, TCGETA, (char *)&exp_tty_original)) {
#endif
			knew_dev_tty = FALSE;
			dev_tty = -1;
		}
	} else {	/* type == SET_TTYTYPE */
		if (knew_dev_tty) {
#ifdef POSIX
			(void) tcsetattr(fd, TCSADRAIN, &exp_tty_original);
#else
			(void) ioctl(fd, TCSETA, (char *)&exp_tty_original);
#endif
		} else {
			/* if running in the background, we have no access */
			/* to a a tty to copy parameters from, so use ones */
			/* supplied by original Makefile */
			debuglog("getptyslave: (default) stty %s\n",DFLT_STTY);
			pty_stty(DFLT_STTY,sline);
		}
		if (s) {
			/* give user a chance to override any terminal parms */
			debuglog("getptyslave: (user-requested) stty %s\n",s);
			pty_stty(s,sline);
		}
	}
}

void
init_pty()
{
#ifdef HPUX
	static char dummy;
	tty_type = &dummy;
	tty_bank = &line[strlen("/dev/ptym/pty")];
	tty_num = &line[strlen("/dev/ptym/ptyX")];
	slave_bank = &sline[strlen("/dev/pty/tty")];
	slave_num = &sline[strlen("/dev/pty/ttyX")];
#else
	tty_type = &line[strlen("/dev/")];
	tty_bank = &line[strlen("/dev/pty")];
	tty_num  = &line[strlen("/dev/ptyp")];
#endif /* HPUX */
	dev_tty = open("/dev/tty",O_RDWR);
	knew_dev_tty = (dev_tty != -1);
	if (knew_dev_tty) ttytype(GET_TTYTYPE,dev_tty,(char *)0);
}

#ifndef R_OK
/* 3b2 doesn't define these according to jthomas@nmsu.edu. */
#define R_OK 04
#define W_OK 02
#endif

/* returns fd of master end of pseudotty */
int
getptymaster()
{
	char *hex;
	struct stat stat_buf;
	int master;
#ifdef UTS
	master = getpty(line, sline, O_RDWR);
#else
	for (*tty_bank = 'p';; (*tty_bank)++) {
		*tty_num = '0';
		if (stat(line, &stat_buf) < 0) break;
		for (hex = "0123456789abcdef";*hex;hex++) {
			*tty_num = *hex;
			*tty_type = 'p';
			if (0 <= (master = open(line, O_RDWR))) {
#endif
				/* verify slave side is usable */
#ifdef HPUX
				*slave_bank = *tty_bank;
				*slave_num = *tty_num;
#else
				*tty_type = 't';
#endif
				if (access(sline, R_OK|W_OK) != 0) {
					(void) close(master);
#ifdef UTS				
					return(-1);
#else
					continue;
#endif
				}
#ifdef HPUX
				/* Turn on trapping of close, open and */
				/* ioctl requests from the slave.*/
				ioctl(master, TIOCTRAP, &enable);
#endif /* HPUX */
				return(master);
#ifndef UTS
			}
		}
	}
#endif
	return(-1);
}

int
getptyslave(stty_args)
char *stty_args;
{
	int slave;
	if (0 > (slave = open(sline, O_RDWR))) return(-1);

	/* sanity check - if slave not 0, skip rest of this and return */
	/* to what will later be detected as an error in caller */
	if (0 != slave) return(slave);

	fcntl(0,F_DUPFD,1);	/* duplicate 0 onto 1 to prepare for stty */
	ttytype(SET_TTYTYPE,slave,stty_args);
	return(slave);
}
