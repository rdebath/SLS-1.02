/*************************************************************************
BBS Modem Server Package Version 2
--------------------------------------------------------------------------

    Copyright (C) 1992  Anthony Rumble

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details. <copying>

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

--------------------------------------------------------------------------
RCS Info

$Header: /home/smilie/bbs/modem/RCS/modem.c,v 2.27 1993/04/12 07:32:50 smilie Exp $

$Log: modem.c,v $
 * Revision 2.27  1993/04/12  07:32:50  smilie
 * updated and tested
 *
 * Revision 2.26  1993/03/28  04:02:19  smilie
 * update
 *
 * Revision 2.25  1993/03/27  13:05:55  anthony
 * update
 *
 * Revision 2.24  1993/03/24  13:54:50  anthony
 * added support for new Linux serial drivers
 *
 * Revision 2.23  1993/03/24  11:00:53  anthony
 * fixed up some warnings
 *
 * Revision 2.22  1993/02/28  19:53:01  smilie
 * added some things for new tty handling
 *
 * Revision 2.21  1993/01/06  20:47:36  smilie
 * disable fork and wait
 * cleaned up a little in the code
 *
 * Revision 2.20  1992/12/06  06:49:16  smilie
 * fixed up some int/char
 *
 * Revision 2.19  1992/12/06  06:43:50  smilie
 * fixed a bug with LOCKED
 *
 * Revision 2.18  1992/10/11  08:51:14  smilie
 * typo
 *
 * Revision 2.17  1992/10/11  08:47:49  smilie
 * added ifdefs for CMGET
 * fixed up carrier detection
 * added carrier detect flag
 * removed SETSID
 *
 * Revision 2.16  1992/10/10  06:00:34  smilie
 * fixed some things up
 *
 * Revision 2.15  1992/10/10  05:43:40  smilie
 * fixed up some problems waiting for conmnect
 *
 * Revision 2.13  1992/10/10  04:48:57  smilie
 * added -V command for version
 *
 * Revision 2.12  1992/10/09  15:29:22  smilie
 * fixed a few things up
 * added carrier detect and DTR control
 *
 * Revision 2.11  1992/10/09  14:50:24  smilie
 * fixed a few other things up
 *
 * Revision 2.10  1992/10/09  14:29:37  smilie
 * fixed up the CR detect
 *
 * Revision 2.9  1992/10/09  13:23:54  smilie
 * fixed up some silly warnings
 *
 * Revision 2.8  1992/10/09  13:18:36  smilie
 * Fixed small bugs from old version
 * added some new options
 * added config items
 *
 * Revision 2.7  1992/09/05  06:28:58  smilie
 * tried to fix the MNP problem
 *
 * Revision 2.3  1992/06/29  10:14:59  smilie
 * fixed a few things
 *
 * Revision 2.2  1992/06/22  05:24:34  smilie
 * fixed a bug with getting connect strings
 *
 * Revision 2.1  1992/06/22  05:20:51  smilie
 * new version of modem server
 *
--------------------------------------------------------------------------

Command Line Parameters...

modem tty speed login

eg:

/bin/modem ttys1 2400 bbs

*************************************************************************/

/* Feature test switches */
#define _POSIX_SOURCE 1
#define _MODEM_C

/* System Headers */
#include <stdio.h>
#include <termios.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <pwd.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

/* Local Headers */
#include "modem.h"

/* Macros */

#define LOG_TTY flog("%u VMIN = %u, VTIME = %u, ICANON = %u, ISIG = %u\n", __LINE__,ti.c_cc[VMIN], ti.c_cc[VTIME], ((ti.c_lflag&ICANON) ? 1 : 0), ((ti.c_lflag&ISIG) ? 1 : 0))

/* File scope variables */

static char modem_rcsid[] = "$Id: modem.c,v 2.27 1993/04/12 07:32:50 smilie Exp $";
#define RCSID modem_rcsid

char mtty[128];		/* Modems full tty path */
char *mlogin;		/* user to login as */
char stty[8];		/* short tty name */

int modem_initialised = 0;	/* If the modem has been initialised yet */

/* External variables */

config_struct CONFIG;	/* Main CONFIG file */

/* External Functions */

/* Structures and unions */

/* Functions */

#ifdef CMGET
/************************************************************************
			GET_PORTSETTINGS
-------------------------------------------------------------------------
*************************************************************************/
int get_portsettings(void)
{
int m;
/**/
ioctl(0, TIOCMGET, &m);
return m;
}
/************************************************************************
			SET_PORTSETTINGS
-------------------------------------------------------------------------
*************************************************************************/
void set_portsettings(int in)
{
/**/
ioctl(0, TIOCMSET, &in);
}
/*************************************************************************
			    CHECK_CARRIER
*************************************************************************/
int check_carrier(void)
{
/**/
return(get_portsettings() & TIOCM_CD);
}
/*************************************************************************
			    SET_DTR
*************************************************************************/
void set_dtr(int dtr)
{
int set;
/**/
set = get_portsettings();
if (!dtr)
	set &= ~TIOCM_DTR;
else
	set |= TIOCM_DTR;
set_portsettings(set);
}
#endif	/* CMGET */
/*************************************************************************
			     WAIT_FOR_CR
*************************************************************************/
int wait_for_cr()
{
struct termios ti, tiold;
int ch;
/**/
tcgetattr(0, &ti);
tcgetattr(0, &tiold);

ti.c_cc[VMIN] = 0;
ti.c_cc[VTIME] = CONFIG.timeout*10;		/* Wait Timeout Period */
ti.c_lflag = 0;
tcsetattr(0, TCSANOW, &ti);

#ifdef DEBUG
flog("CONFIG.timeout = '%hu'\n", CONFIG.timeout);
tcgetattr(0, &ti);
LOG_TTY;
#endif

ch = 0;
while ((ch != 13) && (ch != -1))
	{
	ch = fgetc(stdin);
	#ifdef DEBUG
	flog("Garbage '%d'\n", ch);
	#endif
	#ifdef CMGET
	if (!check_carrier() && CONFIG.carrier)
		{
		flog("Lost Carrier\n");
		set_dtr(0);
		exit(1);
		}
	#endif
	}

#ifdef DEBUG
flog("Ret from GETCR '%d'\n", ch);
#endif
	
tcsetattr(0, TCSANOW, &tiold);
fflush(stdout);

return ch;
}
/************************************************************************
				PARSE_RETS
-------------------------------------------------------------------------
Convert |'s in a string to Carriage Returns.

Returns a STATIC array
*************************************************************************/
char *parse_rets(char *instr)
{
static char ret_str[256];
char *ss = instr;
char *s = ret_str;
/**/
while (*ss)
	{
	if (*ss == '|')
		*s = 13;	/* CR */
	else
		*s = *ss;
	s++; ss++;
	}
return ret_str;
}
/*************************************************************************
				GETREAL
*************************************************************************/
int getreal(int in)
{
/**/
switch(in)
	{
	case 300:return B300;
	case 1200:return B1200;
	case 2400:return B2400;
	case 4800:return B4800;
	case 9600:return B9600;
	case 19200:return B19200;
	case 38400:return B38400;
	}
return B2400;
}
/*************************************************************************
			    INITIALISE_MODEM
*************************************************************************/
int initialise_modem()
{
int stop = FALSE;
char instr[256];
int retry = 0;  
/**/
#ifdef DEBUG
flog("Send to modem '%s'\n", CONFIG.init);
#endif
fprintf(stdout, "%s", parse_rets(CONFIG.init));	/* Send INIT String */
fflush(stdout);					/* Flush TTY */
while (!stop)
	{
	if (fgetsraw(instr, sizeof(instr), stdin, 5))	/* Get return */
		{
		strupr(instr);
		#ifdef DEBUG
		flog("[%s]\n", instr);
		#endif
		if (strncmp(instr, "OK", 2) == 0)
			stop = TRUE;
		}
	else
		{
		retry++;
		if (retry > 10)
			{
			flog("initialise_modem: timeout waiting for 'OK'\n");
			return FALSE;
			}
		flog("Timout initialising modem, retrying..\n");
		#ifdef DEBUG
		flog("Send to modem '%s'\n", CONFIG.init);
		#endif
		fprintf(stdout, "%s", parse_rets(CONFIG.init));	/* Send INIT String */
		fflush(stdout);					/* Flush TTY */
		}
	}
return TRUE;
}
/************************************************************************
			PARSE_MODEM_RETURN
-------------------------------------------------------------------------
Parses the modem return string, returning the speed
and extra.
*************************************************************************/
int parse_modem_return(const char *str, int *spd, char *xtra)
{
int num;
char connect_str[20];
/**/
if (strncasecmp(str, "NO CARRIER", 10) == 0)
	return MDM_NOCARRIER;
else
if (strncasecmp(str, "NO ANSWER", 9) == 0)
	return MDM_NOANSWER;
else
if (strncasecmp(str, "NO DIALTONE", 11) == 0)
	return MDM_NODIALTONE;
else
if (strncasecmp(str, "BUSY", 4) == 0)
	return MDM_BUSY;
else
	{
	strcpy(xtra, "");
	num = sscanf(str, "%s %u/%[^:]", connect_str, spd, xtra);
	if (strcasecmp(connect_str, "CONNECT") == 0)
		{
		if (num == 1)
			{
			*spd = 300;
			return MDM_CONNECT;
			}
		if (num > 1)
			{
			return MDM_CONNECT;
			}
		}
	}
return MDM_UNKNOWN;
}
/*************************************************************************
				SET_SPEED
*************************************************************************/
int set_speed(const char *instr, struct termios *ti)
{
int ret, speed;
char extra[256];
/**/
ret = parse_modem_return(instr, &speed, extra);

if (ret == MDM_UNKNOWN)
	{
	return FALSE;
	}
if (ret != MDM_CONNECT)		/* If we did not get a connect message reinit the modem */
	{
	flog("Re-Initialising modem\n");
	modem_initialised = 0;
	return FALSE;
	}

flog("modem: CONNECT %u%s%s\n", speed, extra[0] ? "/":"", extra);

CONFIG.conspeed = speed;			/* Save ACTUAL Transfer speed */

#ifdef DEBUG
if (CONFIG.locked)
	flog("Modem port locked\n");
else
	flog("Modem port variable\n");
#endif
if (!CONFIG.locked)
	{
	switch(speed)
		{
		case 14400:
			speed = 19200;
			break;
		case 12000:
			speed = 19200;
			break;
 		case 7200:
			speed = 9600;
			break;
		case 4800:
			speed = 9600;
			break;
		}
	CONFIG.speed = speed;
	}

#ifdef DEBUG
flog("DTE speed now = '%u'\n", CONFIG.speed);
#endif
	
return TRUE;
}
/*************************************************************************
				MODEM_ABORT
*************************************************************************/
void modem_abort(int sig)
{
/**/
exit(1);
}
/*************************************************************************
				   MAIN
*************************************************************************/
void main(int argc, char *argv[])
{
struct termios ti;
struct termios t;
char tmpstr[256];
int fd;
int stop = FALSE;
int flags;
/**/

if (strcasecmp(argv[1], "-V") == 0)
	{
	fprintf(stderr, "Linux MODEM Server version %s\nBy Anthony Rumble\n", MDMSERVER_VERSION);
	fprintf(stderr, "Compiled on %s %s\n", __DATE__, __TIME__);
	exit(1);
	}

strcpy(stty, argv[1]);		/* Save Short TTY name */

sleep(2);			/* Wait for rubbish to clear, and DTR to affect modem */

/* Read in Command Line options */

(void)sprintf(mtty, "/dev/%s", argv[1]);
(void)sscanf(argv[2], "%u", &CONFIG.speed);
mlogin = argv[3];

/* Load CONFIG files */
if (!load_config())
	{
	flog("main: ERROR loading config files\n");
	exit(1);
	}

/* Setup Signal Handling */
(void)signal(SIGINT, SIG_IGN);
(void)signal(SIGQUIT, SIG_DFL);
(void)signal(SIGTERM, SIG_DFL);

setsid();

/* Open Serial TTY */
#ifdef DEBUG
flog("Opening '%s'\n", mtty);
#endif
while(((fd = open(mtty, O_RDWR | O_NDELAY)) < 0) && (errno == EBUSY))
	{
	#ifdef DEBUG
	flog("'%s' is EBUSY\n", mtty);
	#endif
	sleep(30);
	}
if (fd<0)
	{
	flog("main: ERROR Opening [%s] Modem Port : %s\n", mtty, strerror(errno));
	exit(1);
	}

if (fd > 0)
	{
	(void)close(0);
	if (dup2(fd, 0) != 0)
		{
		flog("main: ERROR cannot open stdin : %s\n", strerror(errno));
		exit(1);
		}
	}

(void)close(1);
if (dup2(fd, 1) != 1)
		{
		flog("main: ERROR cannot open stdout : %s\n", strerror(errno));
		exit(1);
		}


(void)close(2);					/* Close Current STDERR */
if (dup2(open("/dev/null", O_WRONLY), 2) != 2)	/* Open NULL */
	{
	flog("main: ERROR opening NULL device : %s\n", strerror(errno));
	}

if (fd > 0)
	(void)close(fd);

#ifdef DEBUG
flog("Linux MODEM Server version %s\n", MDMSERVER_VERSION);
flog("Compiled on %s %s\n", __DATE__, __TIME__);
#endif
	
/* Flush any rubbish */

tcflush(0, TCIOFLUSH);
	
/* Setup the terminal */

t.c_cflag = CS8|CREAD|getreal(CONFIG.speed)|CRTSCTS|HUPCL;
t.c_iflag = IXON|IGNBRK|IXOFF;
t.c_oflag = CR0|NL0|TAB0|BS0|FF0|VT0;
t.c_lflag = ICANON;

t.c_cc[VEOF] = _POSIX_VDISABLE;
t.c_cc[VEOL] = _POSIX_VDISABLE;
t.c_cc[VERASE] = 0x08;
t.c_cc[VINTR] = _POSIX_VDISABLE;
t.c_cc[VKILL] = _POSIX_VDISABLE;
t.c_cc[VQUIT] = _POSIX_VDISABLE;
t.c_cc[VSUSP] = _POSIX_VDISABLE;
t.c_cc[VSTART] = 0x11;
t.c_cc[VSTOP] = 0x13;

tcsetattr(0, TCSANOW, &t);

tcgetattr(0, &ti);

#ifdef DEBUG
flog("Modem server started\n");
#endif

#ifdef CMGET
if (check_carrier() && CONFIG.carrier)
	{
	flog("Carrier is present dropping DTR\n");
	set_dtr(0);
	}
#endif
	
sleep(2);		/* Wait for things to clear */
tcflush(0, TCIOFLUSH);	/* Flush any rubbish */
#ifdef CMGET
set_dtr(1);
#endif

#ifdef CMGET
if (check_carrier() && CONFIG.carrier)
	{
	flog("Carrier is STILL present. FATAL ERROR\n");
	exit(1);
	}
#endif
	
#ifdef DEBUG
flog("Initialising modem at %u baud (%s)\n", CONFIG.speed, CONFIG.locked ? "Locked" : "Unlocked");
#endif

flags = fcntl(0, F_GETFL, 0);
(void)fcntl(0, F_SETFL, flags & ~O_NDELAY);

if(!modem_initialised)
	{
	if (!initialise_modem())
		{
		flog("main: \aError initialise modem\n");
		exit(1);
		}
	sleep(1);
	tcflush(0, TCIOFLUSH);
	flog("Modem initialised\n");
	modem_initialised = 1;
	}

(void)close(0);
(void)close(1);
(void)close(2);
setsid();

#ifdef DEBUG
flog("Re-Opening '%s'\n", mtty);
#endif

fd = open(mtty, O_RDWR);
if (fd<0)
	{
	flog("main: ERROR Opening [%s] Modem Port : %s\n", mtty, strerror(errno));
	exit(1);
	}

if (fd > 0)
	{
	close(0);
	if (dup2(fd, 0) != 0)
		{
		flog("main: ERROR cannot open stdin : %s\n", strerror(errno));
		exit(1);
		}
	}

if (dup2(fd, 1) != 1)
		{
		flog("main: ERROR cannot open stdout : %s\n", strerror(errno));
		exit(1);
		}

if (dup2(open("/dev/null", O_WRONLY), 2) != 2)	/* Open NULL */
	{
	flog("main: ERROR opening NULL device : %s\n", strerror(errno));
	}

if (fd > 0)
	(void)close(fd);

/* Flush any rubbish */

tcflush(0, TCIOFLUSH);

/* Setup the terminal */

/*t.c_cflag = CS8|CREAD|getreal(CONFIG.speed)|CRTSCTS|HUPCL;
t.c_iflag = IXON|IGNBRK|IXOFF;
t.c_oflag = CR0|NL0|TAB0|BS0|FF0|VT0;
t.c_lflag = ICANON;

t.c_cc[VEOF] = _POSIX_VDISABLE;
t.c_cc[VEOL] = _POSIX_VDISABLE;
t.c_cc[VERASE] = 0x08;
t.c_cc[VINTR] = _POSIX_VDISABLE;
t.c_cc[VKILL] = _POSIX_VDISABLE;
t.c_cc[VQUIT] = _POSIX_VDISABLE;
t.c_cc[VSUSP] = _POSIX_VDISABLE;
t.c_cc[VSTART] = 0x11;
t.c_cc[VSTOP] = 0x13;*/

tcsetattr(0, TCSANOW, &ti);
#ifdef DEBUG
tcgetattr(0, &ti);
LOG_TTY;
#endif

while (!stop)
	{
/* PUT AN ERROR CATCH IN HERE!!!! */	
	if (!fgetsraw(tmpstr, sizeof(tmpstr), stdin, 2))
		{
		if (errno == EBADF)	
			{
			strcpy(tmpstr, "CONNECT 2400");
			}
		else
			{
			flog("Problem reading from STDIN (%s)", strerror(errno));
			exit(1);
			}
		}

	strupr(tmpstr);				/* Make string uppercase */
	#ifdef DEBUG
	flog("[%s]\n", tmpstr);
	#endif
	if (set_speed(tmpstr, &t))		/* Parse CONNECT string */
		stop = TRUE;
	}

(void)signal(SIGHUP, (void *)modem_abort);
(void)signal(SIGINT, (void *)modem_abort);
(void)signal(SIGQUIT, (void *)modem_abort);
(void)signal(SIGTERM, (void *)modem_abort);
	
/* Connect! Wait for MNP/LAPM rubbish to clear */

wait_for_cr();

tcflush(0, TCIOFLUSH);

t.c_iflag = IGNBRK|ICRNL|IXON|IXOFF;
t.c_oflag = OPOST|ONLCR|NL0|CR0|TAB0|BS0|VT0|FF0;
t.c_lflag = ISIG|ICANON|IEXTEN|ECHO|ECHOE|ECHOK|ECHOCTL|ECHOKE;
t.c_cflag = CS8|HUPCL|CREAD|getreal(CONFIG.speed)|CRTSCTS;

t.c_cc[VEOF] = 0x04;
t.c_cc[VEOL] = 0x0A;
t.c_cc[VERASE] = 0x08;
t.c_cc[VINTR] = 0x03;
t.c_cc[VKILL] = _POSIX_VDISABLE;
t.c_cc[VQUIT] = _POSIX_VDISABLE;
t.c_cc[VSUSP] = 0x1A;
t.c_cc[VSTART] = 0x11;
t.c_cc[VSTOP] = 0x13;

tcsetattr(0, TCSANOW, &t);

flog("Executing login as %s\n", mlogin);

/* Redirect STDERR to STDIN/STDOUT */
dup2(0, 2);

#ifdef DEBUG
tcgetattr(0, &ti);
LOG_TTY;
flog("i%u o%u c%u l%u\n", ti.c_iflag, ti.c_oflag, ti.c_cflag, ti.c_lflag);
#endif


#ifdef FORK_AND_WAIT
if (!(child = fork()))
	{
	if (execlp("/bin/login", "-", mlogin, NULL) == -1)
		exit(1);		
	}
stop = 0;
while (!stop)
	{
	if (kill(child, 0) == -1)
		{
		#ifdef DEBUG
		flog("Child process returned\n");
		#endif
		exit(1);
		}
	#ifdef CMGET
	if (!check_carrier() && CONFIG.carrier)
		{
		flog("Carrier Loss Detected, Child has not returned\n");
		if (kill(child, SIGHUP) == -1)
			{
			flog("Problem sending SIGHUP to child : %s\n", strerror(errno));
			}
		sleep(2);
		if (kill(child, SIGKILL) == -1)
			{
			if (errno == ESRCH)	/* If child exited */
				exit(1);
			flog("Problem sending SIGKILL to child : %s\n", strerror(errno));
			}
		else
			exit(1);
		}
	else
		sleep(30);
	#endif
	}
#else /* FORK_AND_WAIT */
if (execlp("/bin/login", "-", mlogin, NULL) == -1)
	exit(1);
#endif /* FORK_AND_WAIT */
	
}


