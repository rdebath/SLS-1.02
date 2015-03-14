/*	xcport.c -- modem interface routines for XC
	This file uses 4-character tabstops
*/

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <signal.h>
#include <termio.h>
#include <errno.h>
#include "xc.h"

#ifdef T6000
#include <sys/ioctl.h>
extern int errno;
#endif

# define LOCKDIR "/usr/spool/uucp"	/* system-dependent */

#ifdef DIDO
# define DIDOPORT

# if DIDO == 22			/* SCO Xenix 2.2 uses ungetty */
#	define UNGETTY "/usr/lib/uucp/ungetty"
#	define UG_NOTENAB	0
#	define UG_ENAB		1
#	define UG_FAIL		2
#	define SIZEOFLOCKFILE sizeof(short)
	static	int	code, retcode, errflag;
# endif /*DIDO==22*/

# if DIDO == 23			/* SCO Xenix 2.3, SCO Unix, other HDB sites */
#	include <utmp.h>
#	ifndef ASCII_PID
#	 define ASCII_PID
#	 define PIDSIZE 10
#	endif
	static int gettypid = -1;
# endif /*DIDO==23*/
#endif /*DIDO*/

#ifndef SIZEOFLOCKFILE
#define SIZEOFLOCKFILE sizeof(int)
#endif

int bitmask = 0xFF,			/* modem port i/o data mask */
	cbaud = B2400;			/* default bps */
short flowflag;				/* modem port i/o data mask */
static int mfd = -1,		/* modem port file descriptor */
		 pid;
static struct termio pmode;	/* modem device control string */
static char port[SM_BUFF],	/* modem port device file string */
		lckname[SM_BUFF];	/* lockfile string */

void xc_setflow(flow)
short flow;
{
	if (flow)
		pmode.c_iflag |= IXON | IXOFF,
		pmode.c_iflag &= ~IXANY;
	else
		pmode.c_iflag &= ~(IXON | IXOFF | IXANY);

	ioctl(mfd, TCSETA, &pmode);
}

char *mport(s) /* get/set port string */
char *s;
{
	if (s != NULLS && mfd == -1)
		if (strncmp("/dev/", s, 5)) {
			strcpy(port, "/dev/");
			strcat(port, s);
		} else
			strcpy(port, s);

	return(port);
}

/*	Get/set the bps of the modem port. If the port hasn't been opened yet,
	just store in pmode for mopen() to use when it opens the port.
*/
mrate(s)
char *s;
{
	if (s != NULLS) {
		switch (atoi(s)) {
		case 300:
			cbaud = B300;
			break;
		case 1200:
			cbaud = B1200;
			break;
		case 2400:
			cbaud = B2400;
			break;
		case 9600:
			cbaud = B9600;
			break;
#ifdef B19200
		case 19200:
			cbaud = B19200;
			break;
#endif
#ifdef B38400
		case 38400:
			cbaud = B38400;
			break;
#endif
		default:
			return(-1);
		}
		pmode.c_cflag &= ~CBAUD;
		pmode.c_cflag |= cbaud;

		ioctl(mfd, TCSETA, &pmode);
	}

	switch (pmode.c_cflag & CBAUD) {
	case B300:
		return(30);
	case B1200:
		return(120);
	case B2400:
		return(240);
	case B9600:
		return(960);
#ifdef B19200
	case B19200:
		return(1920);
#endif
#ifdef B38400
	case B38400:
		return(3840);
#endif
	}
	show(1,"Impossible error in bps");
	return(0);
}

/*	The following routine is used to hang up the modem. This is accomplished
	by setting bps to 0. According to my documentation on termio, setting bps
	to zero will result in DTR not being asserted. This hangs up some (most?)
	modems. If not, the second part of the routine sends the Hayes modem
	"escape" and then a hangup command.
*/
hangup()
{
	show(1,"<< HANGUP >>");

#ifdef DTR_DROPS_CARRIER  
	pmode.c_cflag &= ~CBAUD;
	pmode.c_cflag |= B0;		/* set cbaud 0 (drop DTR) */
	ioctl(mfd, TCSETA, &pmode);

	sleep(1);					/* wait a second */

	pmode.c_cflag &= ~CBAUD;	/* reset bps */
	pmode.c_cflag |= cbaud;
	ioctl(mfd, TCSETA, &pmode);
#endif

#ifdef ATH_HANGS_UP             /* use Hayes command */
	sleep(3);					/* Allow for "escape guard time" */
	send_slowly(ATTEN);			/* Send modem escape command */
	sleep(3);					/* More "escape guard time" */
	send_slowly(HANGUP);		/* Send hangup command */
#endif 
}

#ifdef DIDOPORT
/*	suspend() sends signal to a running getty
			 sets:	gettypid, process number of running getty, if SCO 2.3
					retcode, exit value of 'ungetty', if SCO 2.2
	restart(): restarts getty if it had been running before
*/
#if DIDO == 23
static void suspend()
{
	struct	utmp *t, *getutent();
	char buff[12];
	void endutent();

	strcpy(buff, strrchr(port, '/') +1);
	while ((t = getutent()) != (struct utmp *)0) {
		if (t->ut_type == LOGIN_PROCESS && (!strcmp(buff, t->ut_line))) {
			gettypid = t->ut_pid;	/* get getty PID */
			if (kill(gettypid, SIGUSR1) == -1 && errno != EPERM)
				show(1,"Can't signal getty");
		}
	}
	endutent();
}
static void restart()
{
	if (gettypid != -1)
		kill(gettypid, SIGUSR2);
}
#endif /*DIDO==23*/
#if DIDO == 22
static void suspend()
{
	code=errflag=pid=retcode=0;
	if ((pid = fork()) == 0) {
		execl(UNGETTY, "ungetty", port, NULLS);
		show(1,"ungetty exec error");
		exit(8);
		}
	while (((code = wait(&errflag)) != pid) && code != -1);
	switch ((errflag>>8) & 0xff) {
	case UG_NOTENAB:	/* line acquired: not enabled */
		retcode = UG_NOTENAB;
		break;
	case UG_ENAB:	/* line acquired: need ungetty -r when done */
		retcode = UG_ENAB;
		break;
	case UG_FAIL:		/* could not acquire line */
	case 255:
		exit(8);
	}
}
static void restart()
{
	code=errflag=pid=0;
	if(retcode == UG_ENAB) {
		if ((pid = fork()) == 0) {
			execl(UNGETTY, "ungetty", "-r", port, NULLS);
			exit(8);
		}
	while (((code = wait(&errflag)) != pid) && code != -1)
		;
	}
}
#endif /*DIDO=22*/
#endif /*DIDOPORT*/

/*	Opens the modem port and configures it. If the port string is
	already defined it will use that as the modem port; otherwise it
	gets the environment variable MODEM. Returns SUCCESS or FAILURE.
*/
mopen()
{
	int c;
	char *p;

	if (port[0] == '\0') {
		if ((p = getenv("MODEM")) == NULLS) {
			show(1,
				"Exiting: no modem port specified or present in environment");
			exit(3);
		}
		mport(p);
	}
	if (!lock_tty())
		exit(4);

#ifdef DIDOPORT
	p = port +strlen(port) -1;
	*p = toupper(*p);
	suspend();
#endif

	if ((mfd = open(port, O_RDWR | O_NDELAY)) < 0) {
		sprintf(Msg,"Can't open modem port %s",port);
		S;
		exit(5);
	}

	ioctl(mfd, TCGETA, &pmode);

	pmode.c_cflag &= ~(CBAUD | HUPCL);
	pmode.c_cflag |= CLOCAL | cbaud;
#if DIDO == 23
	pmode.c_cflag |= CTSFLOW | RTSFLOW ;
#endif
	pmode.c_iflag = IGNBRK;
	pmode.c_oflag = pmode.c_lflag = 0;
	pmode.c_cc[VMIN] = 1; 	/* This many chars satisfies reads */
	pmode.c_cc[VTIME] = 0;	/* or in this many tenths of seconds */

	xc_setflow(flowflag);

	c = mfd;
	if ((mfd = open(port, O_RDWR)) < 0) {	/* Reopen line with CLOCAL */
		sprintf(Msg,"Can't re-open modem port %s",port);
		S;
		return FAILURE;
	}
	close(c);

	return SUCCESS;
}

/*	Attach standard input and output to the modem port. This only gets called
	after a fork by the child process, which then exec's a program that uses
	standard i/o for some data transfer protocol. (To put this here is actually
	a kludge, but I wanted to keep the modem-specific stuff in a black box.)
*/
void mattach()
{
	dup2(mfd, 0);	/* close local stdin and connect to port */
	dup2(mfd, 1);	/* close local stdout and connect to port */

	close(mfd);		/* close the old port descriptor */
}

static void alrm()
{ /* do nothing */
}

/*	Get a byte from the modem port within 'seconds' or return -1.
	All data read from the modem are input through this routine.
*/
readbyte(seconds)
unsigned seconds;
{
	static int count = 0;
	static char *p, rxbuf[10240];
	unsigned alarm();

	if (count > 0) {
		count--;
		return(*p++ & 0xff);
	}
	if (seconds) {
		signal(SIGALRM, alrm);
		alarm(seconds);
	}
	if ((count = read(mfd, p = rxbuf, 10240)) < 1)
		return(-1);
	if (seconds)
		alarm(0);

	count--;
	return(*p++ & 0xff);
}

/* Read a byte using bitmask */
read_mbyte(secs)
unsigned secs;
{
	int c;

	return (c = readbyte(secs)) == -1 ? -1 : c & bitmask;
}

/*	Output a byte to the modem port.
	All data sent to the modem are output through this routine.
*/
void sendbyte(ch)
int ch;
{
	char c = ch & 0xff;

	if(write(mfd, &c, 1)<0)
		show(1,"sendbyte: write error!");
}

/* Send a byte using bitmask */
void send_mbyte(ch)
int ch;
{
	sendbyte(ch & bitmask);
}

void send_slowly(s)
char *s;
{
	while (*s) {
		send_mbyte(*s++);
		msecs(35);
	}
}

/* send a modem break */
xmitbrk()
{
	show(1,"<< BREAK >>");
	ioctl(mfd, TCSBRK, 0);
}

/*	lock_tty() returns FAILURE if the lock file exists (and XC will not run).

	unlock_tty() deletes the lock file.

	SCOXENIX 2.3 mods: Steve Manes
	Check for active LCK file and try to delete it

	SCOXENIX 2.2 mods: Jean-Pierre Radley
	As above, using 'ungetty'

	Tandy 6000 mods: Fred Buck
*/

static lock_tty()
{
	int lckfd;
	char *s, buff[12];
#ifdef ASCII_PID
	static char apid[PIDSIZE+2] = { '\0' };
#else
	pid = -1;
#endif

	strcpy(buff, strrchr(port, '/') +1);
	s = buff + strlen(buff) - 1;

#if DIDO == 22
	*s = toupper(*s);
#endif
#if DIDO == 23
	*s = tolower(*s);
#endif

	sprintf(lckname, "%s/LCK..%s", LOCKDIR, buff);

	if (!checkLCK())	/* check LCK file */
		return FAILURE;	/* can't unlock it */

	if ((lckfd = creat(lckname, 0666)) < 0) {
		sprintf(Msg,"Can't create %s", lckname);
		S;
		return FAILURE;
	}

#ifdef ASCII_PID
	sprintf(apid, "%*d\n", PIDSIZE, getpid());
	write(lckfd, apid, PIDSIZE+1);
#else
	pid = getpid();
	write(lckfd, (char *)&pid, SIZEOFLOCKFILE);
#endif

	close(lckfd);
	return SUCCESS;
}

void unlock_tty()
{
	static char byettyxx[50], *byeptr;
	extern char *ttyname();

	sprintf(byettyxx,"BYE%s", strrchr(ttyname(mfd),'/')+1);
	byeptr = getenv(byettyxx);
	if (byeptr != NULLS && *byeptr) {
		show(1,"Sending BYE string to modem");
		send_slowly("\r\r");
		send_slowly(byeptr);
		send_slowly("\n");
	}

	pmode.c_cflag &= ~CLOCAL;
	pmode.c_cflag |= B0 | HUPCL;
	ioctl(mfd, TCSETA, &pmode);
	close(mfd);

	setuid((int)geteuid());
	setgid((int)getegid());
	unlink(lckname);
#ifdef DIDOPORT
	restart();
#endif
	show(1,"Exiting XC");
}

/*	check to see if lock file exists and is still active.
	kill(pid, 0) only works on ATTSV, some BSDs and Xenix
	returns: SUCCESS, or
			FAILURE if lock file active
	added: Steve Manes 7/29/88
*/
checkLCK()
{
	int rc, fd;
#ifdef ASCII_PID
	char alckpid[PIDSIZE+2];
#endif
#if DIDO == 22
	short lckpid = -1;
#else
	int lckpid = -1;
#endif

	if ((fd = open(lckname, O_RDONLY)) == -1) {
		if (errno == ENOENT)
			return SUCCESS;	/* lock file doesn't exist */
		goto unlock;
	}
#ifdef ASCII_PID
	rc = read(fd, (char *)alckpid, PIDSIZE+1);
	close(fd);
	lckpid = atoi(alckpid);
	if (rc != 11) {
#else
	rc = read(fd, (char *)&lckpid, SIZEOFLOCKFILE);
	close(fd);
	if (rc != SIZEOFLOCKFILE) {
#endif
		show(1,"Lock file has bad format");
		goto unlock;
	}

	/* now, send a bogus 'kill' and check the results */
	if (kill(lckpid, 0) == 0 || errno == EPERM) {
		sprintf(Msg,"Lock file process %d on %s is still active - try later",
			lckpid, port);
		S;
		return FAILURE;
	}

unlock:
	if (unlink(lckname) != 0) {
		sprintf(Msg,"Can't unlink %s file", lckname);
		S;
		return FAILURE;
	}
	return SUCCESS;
}
