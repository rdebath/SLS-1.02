/*
 * xdm - display manager daemon
 *
 * $XConsortium: server.c,v 1.17 91/09/19 16:26:01 keith Exp $
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

/*
 * server.c - manage the X server
 */

# include	"dm.h"
# include	<X11/Xlib.h>
# include	<X11/Xos.h>
# include	<stdio.h>
# include	<signal.h>
# include	<errno.h>

static receivedUsr1;

extern int  errno;
static serverPause ();

static Display	*dpy;

/* ARGSUSED */
static SIGVAL
CatchUsr1 (n)
    int n;
{
#ifdef SIGNALS_RESET_WHEN_CAUGHT
    (void) Signal (SIGUSR1, CatchUsr1);
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

StartServerOnce (d)
struct display	*d;
{
    char	**f;
    char	**argv;
    char	arg[1024];
    char	**parseArgs ();
    int		pid;

    Debug ("StartServer for %s\n", d->name);
    receivedUsr1 = 0;
    (void) Signal (SIGUSR1, CatchUsr1);
    argv = d->argv;
    switch (pid = fork ()) {
    case 0:
	CleanUpChild ();
	if (d->authFile) {
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
	(void) Signal (SIGUSR1, SIG_IGN);
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

StartServer (d)
struct display *d;
{
    int	i;
    int	ret = FALSE;

    i = 0;
    while (d->serverAttempts == 0 || i < d->serverAttempts)
    {
	if ((ret = StartServerOnce (d)) == TRUE)
	    break;
	sleep (d->openDelay);
	i++;
    }
    return ret;
}

/*
 * sleep for t seconds, return 1 if the server is dead when
 * the sleep finishes, 0 else
 */

static Jmp_buf	pauseAbort;
static int	serverPauseRet;

/* ARGSUSED */
static SIGVAL
serverPauseAbort (n)
    int n;
{
    Longjmp (pauseAbort, 1);
}

/* ARGSUSED */
static SIGVAL
serverPauseUsr1 (n)
    int n;
{
    Debug ("display manager paused til SIGUSR1\n");
    ++receivedUsr1;
    Longjmp (pauseAbort, 1);
}

static
serverPause (t, serverPid)
unsigned    t;
int	    serverPid;
{
    int		pid;

    serverPauseRet = 0;
    if (!Setjmp (pauseAbort)) {
	(void) Signal (SIGALRM, serverPauseAbort);
	(void) Signal (SIGUSR1, serverPauseUsr1);
#ifdef SYSV
	if (receivedUsr1)
	    (void) alarm ((unsigned) 1);
	else
	    (void) alarm (t);
#else
	if (!receivedUsr1)
	    (void) alarm (t);
	else
	    Debug ("Already received USR1\n");
#endif
	for (;;) {
#if defined(SYSV) && defined(X_NOT_POSIX)
	    pid = wait ((waitType *) 0);
#else
	    if (!receivedUsr1)
		pid = wait ((waitType *) 0);
	    else
#ifndef X_NOT_POSIX
		pid = waitpid (-1, (int *) 0, WNOHANG);
#else
		pid = wait3 ((waitType *) 0, WNOHANG,
			     (struct rusage *) 0);
#endif /* X_NOT_POSIX */
#endif /* SYSV */
	    if (pid == serverPid ||
		pid == -1 && errno == ECHILD)
	    {
		Debug ("Server dead\n");
		serverPauseRet = 1;
		break;
	    }
#if !defined(SYSV) || !defined(X_NOT_POSIX)
	    if (pid == 0) {
		Debug ("Server alive and kicking\n");
		break;
	    }
#endif
	}
    }
    (void) alarm ((unsigned) 0);
    (void) Signal (SIGALRM, SIG_DFL);
    (void) Signal (SIGUSR1, CatchUsr1);
    if (serverPauseRet) {
	Debug ("Server died\n");
	LogError ("server unexpectedly died\n");
    }
    return serverPauseRet;
}


/*
 * this code is complicated by some TCP failings.  On
 * many systems, the connect will occasionally hang forever,
 * this trouble is avoided by setting up a timeout to Longjmp
 * out of the connect (possibly leaving piles of garbage around
 * inside Xlib) and give up, terminating the server.
 */

static Jmp_buf	openAbort;

/* ARGSUSED */
static SIGVAL
abortOpen (n)
    int n;
{
	Longjmp (openAbort, 1);
}

#ifdef XDMCP

#ifdef STREAMSCONN
#include <tiuser.h>
#endif

static
GetRemoteAddress (d, fd)
    struct display  *d;
    int		    fd;
{
    char    buf[512];
    int	    len = sizeof (buf);
#ifdef STREAMSCONN
    struct netbuf	netb;
#endif

    if (d->peer)
	free ((char *) d->peer);
#ifdef STREAMSCONN
    netb.maxlen = sizeof(buf);
    netb.buf = buf;
    t_getname(fd, &netb, REMOTENAME);
    len = 8;
    /* lucky for us, t_getname returns something that looks like a sockaddr */
#else
    getpeername (fd, (struct sockaddr *) buf, &len);
#endif
    d->peerlen = 0;
    if (len)
    {
	d->peer = (XdmcpNetaddr) malloc (len);
	if (d->peer)
	{
	    bcopy (buf, (char *) d->peer, len);
	    d->peerlen = len;
	}
    }
    Debug ("Got remote address %s %d\n", d->name, d->peerlen);
}

#endif /* XDMCP */

static int
openErrorHandler (dpy)
    Display *dpy;
{
    LogError ("IO Error in XOpenDisplay\n");
    exit (OPENFAILED_DISPLAY);
}

int
WaitForServer (d)
    struct display  *d;
{
    int	    i;

    for (i = 0; i < (d->openRepeat > 0 ? d->openRepeat : 1); i++) {
    	(void) Signal (SIGALRM, abortOpen);
    	(void) alarm ((unsigned) d->openTimeout);
    	if (!Setjmp (openAbort)) {
	    Debug ("Before XOpenDisplay(%s)\n", d->name);
	    errno = 0;
	    (void) XSetIOErrorHandler (openErrorHandler);
	    dpy = XOpenDisplay (d->name);
#ifdef STREAMSCONN
	    {
		/* For some reason, the next XOpenDisplay we do is
		   going to fail, so we might as well get that out
		   of the way.  There is something broken here. */
		Display *bogusDpy = XOpenDisplay (d->name);
		Debug ("bogus XOpenDisplay %s\n",
		       bogusDpy ? "succeeded" : "failed");
		if (bogusDpy) XCloseDisplay(bogusDpy); /* just in case */
	    }
#endif
	    (void) alarm ((unsigned) 0);
	    (void) Signal (SIGALRM, SIG_DFL);
	    (void) XSetIOErrorHandler ((int (*)()) 0);
	    Debug ("After XOpenDisplay(%s)\n", d->name);
	    if (dpy) {
#ifdef XDMCP
	    	if (d->displayType.location == Foreign)
		    GetRemoteAddress (d, ConnectionNumber (dpy));
#endif
	    	RegisterCloseOnFork (ConnectionNumber (dpy));
		(void) fcntl (ConnectionNumber (dpy), F_SETFD, 0);
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
	    (void) Signal (SIGALRM, SIG_DFL);
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

static Jmp_buf	pingTime;

static void
PingLost ()
{
    Longjmp (pingTime, 1);
}

/* ARGSUSED */
static int
PingLostIOErr (dpy)
    Display *dpy;
{
    PingLost();
}

/* ARGSUSED */
static SIGVAL
PingLostSig (n)
    int n;
{
    PingLost();
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
    oldError = XSetIOErrorHandler (PingLostIOErr);
    oldAlarm = alarm (0);
    oldSig = Signal (SIGALRM, PingLostSig);
    (void) alarm (d->pingTimeout * 60);
    if (!Setjmp (pingTime))
    {
	Debug ("Ping server\n");
	XSync (alternateDpy, 0);
    }
    else
    {
	Debug ("Server dead\n");
	(void) alarm (0);
	(void) Signal (SIGALRM, SIG_DFL);
	XSetIOErrorHandler (oldError);
	return 0;
    }
    (void) alarm (0);
    (void) Signal (SIGALRM, oldSig);
    (void) alarm (oldAlarm);
    Debug ("Server alive\n");
    XSetIOErrorHandler (oldError);
    return 1;
}
