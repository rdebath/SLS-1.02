/* pty_unicos.c - routines to allocate ptys - for CRAY UNICOS 5.1 and 6.0 */

/*

Original by: Don Libes, NIST, 2/6/90
Hacked for Unicos 5.1 by: Frank Terhaar-Yonkers, US EPA,  1/10/91
Hacked for Unicos 6.0 by: Pete TerMaat, pete@willow.cray.com, 3/27/91

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/fcntl.h>
#if CRAY>=60
#include <sys/termios.h>
#else
#include <sys/termio.h>
#endif /* 60 */
#if CRAY>=70 && defined(_CRAY2)
#include <sys/session.h>
#endif /* 70 */
#include <sys/pty.h>
#include <pwd.h>
#include <utmp.h>
#include <signal.h>
#include "exp_rename.h"

void debuglog();
#if CRAY<60
extern int fork(), execl(), wait();
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

static char	linep[] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
static char	linet[] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
static int	lowpty;
static int	highpty;
static int	realuid;
static int	realgid;
static int 	*ptys;
static char myname[32];
static char hostname[MAXHOSTNAMELEN];

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

struct	termio exp_tty_original;

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
	if (request == GET_TTYTYPE) {
		if (-1 == ioctl(fd, TCGETA, (char *)&exp_tty_original)) {
			knew_dev_tty = FALSE;
			dev_tty = -1;
		}
	} else {	/* type == SET_TTYTYPE */
		if (knew_dev_tty) {
			(void) ioctl(fd, TCSETA, (char *)&exp_tty_original);
		} else {
			/* if running in the background, we have no access */
			/* to a a tty to copy parameters from, so use ones */
			/* supplied by original Makefile */
			debuglog("ttytype: (default) stty %s\n",DFLT_STTY);
			pty_stty(DFLT_STTY,linet);
		}
		if (s) {
			/* give user a chance to override any terminal parms */
			debuglog("ttytype: (user-requested) stty %s\n",s);
			pty_stty(s,linet);
		}
	}
}

void
init_pty()
{
	int npty;
	char *myline;

	lowpty=0;
#ifdef _SC_CRAY_NPTY
	highpty=sysconf(_SC_CRAY_NPTY);
#else
	highpty=128;
#endif /* _SC_CRAY_NPTY */

	ptys = (int *) malloc(sizeof(int)*(highpty+1));
	if (ptys == NULL) {
		fprintf(stderr,"init_pty:  couldn't allocate pty array\n");
		exit(1);
	}
	for (npty = lowpty;npty <= highpty;npty++)
		ptys[npty] = 0;

 	realuid=getuid();	/* get REAL uid */
 	realgid=getgid();	/* get REAL uid */

	dev_tty = open("/dev/tty",O_RDWR);
	knew_dev_tty = (dev_tty != -1);
	if (knew_dev_tty) ttytype(GET_TTYTYPE,dev_tty,(char *)0);

	/*
	 * Acquire (as root) current user name and host.
	 */
	(void) cuserid(myname);
	(void) gethostname(hostname,sizeof(hostname));

	/*
	 * Set the real and effective userids to root using 'setuid'.  Then
	 * set the real and effective userids to the actual user using
	 * 'setreuid'.  This allows using 'seteuid' to go back and forth from
	 * root and the actual userid.  Don't ask me why it works.
	 */
	setuid(0);
	setreuid(realuid,realuid);
}

/* returns fd of master end of pseudotty */
int
getptymaster()
{
	struct stat sb;
	int master;
	int npty;

	debuglog("getptymaster:  lowpty=%d  highpty=%d\n",lowpty,highpty);
	for (npty = lowpty; npty <= highpty; npty++) {
		if (seteuid(0) == -1) {		/* we need to be root! */
			debuglog("getptymaster:  seteuid root errno=%d\n",
				errno);
		}
		(void) sprintf(linep, "/dev/pty/%03d", npty);
		master = open(linep, O_RDWR);

		if (master < 0) {
			debuglog("getptymaster:  open linep=%s errno=%d\n",
				linep,errno);
			continue;
		}

		(void) sprintf(linet, "/dev/ttyp%03d", npty);
		if(stat(linet, &sb) < 0) {
			debuglog("getptymaster:  stat linet=%s errno=%d\n",
				linet,errno);
			(void) close(master);
			continue;
		}
		if (sb.st_uid || sb.st_gid || sb.st_mode != 0600) {
                        if (chown(linet, realuid, realgid) == -1) {
				debuglog("getptymaster:  chown linet=%s errno=%d\n",
					linet,errno);
			}
                        if (chmod(linet, 0600) == -1) {
				debuglog("getptymaster:  chmod linet=%s errno=%d\n",
					linet,errno);
			}
                        (void)close(master);
                        master = open(linep, 2);
                        if (master < 0) {
				debuglog("getptymaster:  reopen linep=%s errno=%d\n",
					linep,errno);
                                continue;
			}
                }
		if (seteuid(realuid) == -1) {	/* back to who we are! */
			debuglog("getptymaster:  seteuid user errno=%d\n",
				errno);
		}
		if (access(linet, R_OK|W_OK) != 0) {
			debuglog("getptymaster:  access linet=%s errno=%d\n",
				linet,errno);
			(void) close(master);
			continue;
		}
		debuglog("getptymaster:  allocated %s\n",linet);
		ptys[npty] = -1;
		return(master);
	}
	if (seteuid(realuid) == -1) {		/* back to who we are! */
		debuglog("getptymaster:  seteuid user errno=%d\n",errno);
	}
	return(-1);
}

int
getptyslave(stty_args)
char *stty_args;
{
	int slave;

	if (0 > (slave = open(linet, O_RDWR))) {
		debuglog("getptyslave:  open linet=%s errno=%d\n",linet,errno);
		return(-1);
	}

	/* sanity check - if slave not 0, skip rest of this and return */
	/* to what will later be detected as an error in caller */
	if (0 != slave) {
		debuglog("getptyslave:  slave fd not 0\n");
		 return(slave);
	}

	fcntl(0,F_DUPFD,1);	/* duplicate 0 onto 1 to prepare for stty */
	ttytype(SET_TTYTYPE,slave,stty_args);
	return(slave);
}

setptyutmp()
{
	struct utmp utmp;

	if (seteuid(0) == -1) {		/* Need to be root */
		debuglog("setptyutmp:  setuid root errno=%d\n",errno);
		return(-1);
	}
	(void) time(&utmp.ut_time);
	utmp.ut_type = USER_PROCESS;
	utmp.ut_pid = getpid();
	strncpy(utmp.ut_user,myname,sizeof(utmp.ut_user));
	strncpy(utmp.ut_host,hostname,sizeof(utmp.ut_host));
	strncpy(utmp.ut_line,linet+5,sizeof(utmp.ut_line));
	strncpy(utmp.ut_id,linet+8,sizeof(utmp.ut_id));
	if (pututline(&utmp) == NULL) {
		debuglog("setptyutmp:  pututline failed\n");
	}
	endutent();
	if (seteuid(realuid) == -1)
		debuglog("setptyutmp:  seteuid user errno=%d\n",errno);
	return(0);
}

setptypid(pid)
int pid;
{
        int npty;

        for (npty = lowpty; npty <= highpty; npty++) {
                if (ptys[npty] < 0) {
                        debuglog("setptypid:  ttyp%03d pid=%d\n",npty,pid);
                        ptys[npty] = pid;
                        break;
                }
        }
}

ttyp_reset()
{
        int npty;

        if (seteuid(0) == -1) {		/* we need to be root! */
                debuglog("ttyp_reset:  seteuid root errno=%d\n",errno);
        }
        for (npty = lowpty; npty <= highpty; npty++) {
                if (ptys[npty] <= 0)
                        continue;

                (void) sprintf(linet, "/dev/ttyp%03d", npty);
                debuglog("ttyp_reset:  resetting %s, killing %d\n",
			linet,ptys[npty]);
                if (chown(linet,0,0) == -1) {
                        debuglog("ttyp_reset: chown %s errno=%d\n",linet,errno);
                }
                if (chmod(linet, 0666) == -1) {
                        debuglog("ttyp_reset: chmod %s errno=%d\n",linet,errno);
                }
                resetptyutmp();
                if (kill(ptys[npty],SIGKILL) == -1) {
                        debuglog("ttyp_reset:  kill pid=%d errno=%d\n",
                                ptys[npty],errno);
                }
        }
        if (seteuid(realuid) == -1) {   /* Back to who we really are */
                debuglog("ttyp_reset:  seteuid user errno=%d\n",errno);
        }
}

resetptyutmp()
{
        struct utmp utmp;

        (void) setutent ();
        /* set up entry to search for */
        (void) strncpy(utmp.ut_id, linet + strlen(linet) - 4,
                 sizeof (utmp.ut_id));
        utmp.ut_type = USER_PROCESS;

        /* position to entry in utmp file */
        if(getutid(&utmp) == NULL) {
                debuglog("resetptyutmp:  no utmp entry for %s\n",linet);
                return(-1);     /* no utmp entry for this line ??? */
        }

        /* set up the new entry */
        strncpy(utmp.ut_name,"",sizeof(utmp.ut_name));
        strncpy(utmp.ut_host,"",sizeof(utmp.ut_host));
        time(&utmp.ut_time);
        utmp.ut_type = DEAD_PROCESS;
        utmp.ut_exit.e_exit = 0;

        /* write out the entry */
        pututline(&utmp);

        /* close the file */
        (void) endutent();
        return(0);
}
