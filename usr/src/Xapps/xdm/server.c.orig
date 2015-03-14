/*
 * xdm - display manager daemon
 *
 * $XConsortium: server.c,v 1.5 89/12/06 19:38:39 keith Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

# include	<stdio.h>
# include	<X11/Xlib.h>
# include	<X11/Xos.h>
# include	<sys/signal.h>
# include	<setjmp.h>
# include	<errno.h>
# include	"dm.h"

static receivedUsr1;

extern int  errno;
static serverPause ();

static Display	*dpy;

static SIGVAL
CatchUsr1 ()
{
#ifdef SYSV
    (void) signal (SIGUSR1, CatchUsr1);
#endif
    Debug ("display manager caught SIGUSR1\n");
    ++receivedUsr1;
}

static char *_SysErrorMsg (n)
    int n;
{
    extern char *sys_errlist[];
    extern int sys_nerr;
    char *s = ((n >= 0 && n < sys_nerr) ? sys_errlist[n] : "unknown error");

    return (s ? s : "no such error");
}

StartServer (d)
struct display	*d;
{
    char	**f;
    char	**argv;
    char	arg[1024];
    char	**parseArgs ();
    int		pid;

    Debug ("StartServer for %s\n", d->name);
    receivedUsr1 = 0;
    signal (SIGUSR1, CatchUsr1);
    argv = d->argv;
    switch (pid = fork ()) {
    case 0:
	CleanUpChild ();
	if (d->authorization) {
	    sprintf (arg, "-auth %s", d->authFile);
	    argv = parseArgs (argv, arg);
	}
	if (!argv) {
	    LogError ("StartServer: no arguments\n");
	    sleep ((unsigned) d->openDelay);
	    exit (UNMANAGE_DISPLAY);
	}
	for (f = argv; *f; f++)
	    Debug ("'%s' ", *f);
	Debug ("\n");
	/*
	 * give the server SIGUSR1 ignored,
	 * it will notice that and send SIGUSR1
	 * when ready
	 */
	signal (SIGUSR1, SIG_IGN);
	(void) execv (argv[0], argv);
	LogError ("server %s cannot be executed\n",
			argv[0]);
	sleep ((unsigned) d->openDelay);
	exit (REMANAGE_DISPLAY);
    case -1:
	LogError ("fork failed, sleeping\n");
	return 0;
    default:
	break;
    }
    Debug ("Server Started %d\n", pid);
    d->serverPid = pid;
    if (serverPause ((unsigned) d->openDelay, pid))
	return FALSE;
    return TRUE;
}

/*
 * sleep for t seconds, return 1 if the server is dead when
 * the sleep finishes, 0 else
 */

static jmp_buf	pauseAbort;
static int	serverPauseRet;

static SIGVAL
serverPauseAbort ()
{
    longjmp (pauseAbort, 1);
}

static SIGVAL
serverPauseUsr1 ()
{
    Debug ("display manager paused til SIGUSR1\n");
    ++receivedUsr1;
    longjmp (pauseAbort, 1);
}

static
serverPause (t, serverPid)
unsigned    t;
int	    serverPid;
{
    int		pid;

    serverPauseRet = 0;
    if (!setjmp (pauseAbort)) {
	signal (SIGALRM, serverPauseAbort);
	signal (SIGUSR1, serverPauseUsr1);
#ifdef SYSV
	if (receivedUsr1)
	    alarm ((unsigned) 1);
	else
	    alarm (t);
#else
	if (!receivedUsr1)
	    alarm (t);
	else
	    Debug ("Already received USR1\n");
#endif
	for (;;) {
#ifdef SYSV
	    pid = wait ((waitType *) 0);
#else
	    if (!receivedUsr1)
		pid = wait ((waitType *) 0);
	    else
		pid = wait3 ((waitType *) 0, WNOHANG,
			     (struct rusage *) 0);
#endif
	    if (pid == serverPid ||
		pid == -1 && errno == ECHILD)
	    {
		Debug ("Server dead\n");
		serverPauseRet = 1;
		break;
	    }
#ifndef SYSV
	    if (pid == 0) {
		Debug ("Server alive and kicking\n");
		break;
	    }
#endif
	}
    }
    alarm ((unsigned) 0);
    signal (SIGALRM, SIG_DFL);
    signal (SIGUSR1, CatchUsr1);
    if (serverPauseRet) {
	Debug ("Server died\n");
	LogError ("server unexpectedly died\n");
    }
    return serverPauseRet;
}


/*
 * this code is complicated by some TCP failings.  On
 * many systems, the connect will occasionally hang forever,
 * this trouble is avoided by setting up a timeout to longjmp
 * out of the connect (possibly leaving piles of garbage around
 * inside Xlib) and give up, terminating the server.
 */

static jmp_buf	openAbort;

static SIGVAL
abortOpen ()
{
	longjmp (openAbort, 1);
}

static
GetRemoteAddress (d, fd)
    struct display  *d;
    int		    fd;
{
    char    buf[512];
    int	    len = sizeof (buf);

    if (d->peer)
	free ((char *) d->peer);
    getpeername (fd, (struct sockaddr *) buf, &len);
    d->peerlen = 0;
    if (len)
    {
	d->peer = (struct sockaddr *) malloc (len);
	if (d->peer)
	{
	    bcopy (buf, (char *) d->peer, len);
	    d->peerlen = len;
	}
    }
    Debug ("Got remote address %s %d\n", d->name, d->peerlen);
}

int
WaitForServer (d)
    struct display  *d;
{
    int	    i;

    for (i = 0; i < (d->openRepeat > 0 ? d->openRepeat : 1); i++) {
    	(void) signal (SIGALRM, abortOpen);
    	(void) alarm ((unsigned) d->openTimeout);
    	if (!setjmp (openAbort)) {
	    Debug ("Before XOpenDisplay(%s)\n", d->name);
	    errno = 0;
	    dpy = XOpenDisplay (d->name);
	    (void) alarm ((unsigned) 0);
	    (void) signal (SIGALRM, SIG_DFL);
	    Debug ("After XOpenDisplay(%s)\n", d->name);
	    if (dpy) {
	    	if (d->displayType.location == Foreign)
		    GetRemoteAddress (d, ConnectionNumber (dpy));
	    	RegisterCloseOnFork (ConnectionNumber (dpy));
	    	return 1;
	    } else {
	    	Debug ("OpenDisplay failed %d (%s) on \"%s\"\n",
		       errno, _SysErrorMsg (errno), d->name);
	    }
	    Debug ("waiting for server to start %d\n", i);
	    sleep ((unsigned) d->openDelay);
    	} else {
	    Debug ("hung in open, aborting\n");
	    LogError ("Hung in XOpenDisplay(%s), aborting\n", d->name);
	    (void) signal (SIGALRM, SIG_DFL);
	    break;
    	}
    }
    Debug ("giving up on server\n");
    LogError ("server open failed for %s, giving up\n", d->name);
    return 0;
}

ResetServer (d)
    struct display  *d;
{
    if (dpy && d->displayType.origin != FromXDMCP)
	pseudoReset (dpy);
}

static jmp_buf	pingTime;

static int
PingLost ()
{
    longjmp (pingTime, 1);
}

PingServer (d, alternateDpy)
    struct display  *d;
    Display	    *alternateDpy;
{
    int	    (*oldError)();
    SIGVAL  (*oldSig)();
    int	    oldAlarm;

    if (!alternateDpy)
	alternateDpy = dpy;
    oldError = XSetIOErrorHandler (PingLost);
    oldAlarm = alarm (0);
    oldSig = signal (SIGALRM, PingLost);
    alarm (d->pingTimeout * 60);
    if (!setjmp (pingTime))
    {
	Debug ("Ping server\n");
	XSync (alternateDpy, 0);
    }
    else
    {
	Debug ("Server dead\n");
	alarm (0);
	signal (SIGALRM, SIG_DFL);
	return 0;
    }
    alarm (0);
    signal (SIGALRM, oldSig);
    alarm (oldAlarm);
    Debug ("Server alive\n");
    XSetIOErrorHandler (oldError);
    return 1;
}
