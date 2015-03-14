/*
 * xdm - display manager daemon
 *
 * $XConsortium: dm.c,v 1.34 89/12/19 16:56:09 rws Exp $
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
 * display manager
 */

# include	<stdio.h>
# include	<X11/Xos.h>
# include	<sys/signal.h>
# include	<sys/stat.h>
# include	<errno.h>
# include	<varargs.h>
#ifdef SYSV
#ifndef F_TLOCK
# include	<unistd.h>
#endif
#endif
# include	"dm.h"

extern int	errno;

extern void	exit (), abort ();

static void	RescanServers ();
int		Rescan;
static long	ServersModTime, ConfigModTime;
static SIGVAL	StopAll (), RescanNotify ();
void		StopDisplay ();
static void	RestartDisplay ();

#ifndef NOXDMTITLE
static char *Title;
static int TitleLen;
#endif

main (argc, argv)
int	argc;
char	**argv;
{
    int	oldpid;
#ifndef SYSV
    static SIGVAL	ChildNotify ();
#endif

#ifndef NOXDMTITLE
    Title = argv[0];
    TitleLen = (argv[argc - 1] + strlen(argv[argc - 1])) - Title;
#endif

    /*
     * Step 1 - load configuration parameters
     */
    InitResources (argc, argv);
    SetConfigFileTime ();
    LoadDMResources ();
    /*
     * Only allow root to run in non-debug mode to avoid problems
     */
    if (debugLevel == 0 && getuid() != 0)
    {
	fprintf (stderr, "Only root wants to run %s\n", argv[0]);
	exit (1);
    }
    if (debugLevel == 0 && daemonMode)
	    BecomeDaemon ();
    if (oldpid = StorePid ())
    {
	if (oldpid == -1)
	    LogError ("Can't create/lock pid file %s\n", pidFile);
	else
	    LogError ("Can't lock pid file %s, another xdm is running (pid %d)\n",
		 pidFile, oldpid);
	exit (1);
    }
    InitErrorLog ();
    CreateWellKnownSockets ();
    (void) signal (SIGTERM, StopAll);
    (void) signal (SIGINT, StopAll);
    /*
     * Step 2 - Read /etc/Xservers and set up
     *	    the socket.
     *
     *	    Keep a sub-daemon running
     *	    for each entry
     */
    ScanServers ();
    StartDisplays ();
    (void) signal (SIGHUP, RescanNotify);
#ifndef SYSV
    (void) signal (SIGCHLD, ChildNotify);
#endif
    while (AnyWellKnownSockets() || AnyDisplaysLeft ())
    {
	if (Rescan)
	{
	    RescanServers ();
	    Rescan = 0;
	}
#ifdef SYSV
	WaitForChild ();
#else
	WaitForSomething ();
#endif
    }
    Debug ("Nothing left to do, exiting\n");
}

static SIGVAL
RescanNotify ()
{
    Debug ("Caught SIGHUP\n");
    Rescan = 1;
#ifdef SYSV
    signal (SIGHUP, RescanNotify);
#endif
}

ScanServers ()
{
    char	lineBuf[10240];
    int		len;
    FILE	*serversFile;
    struct stat	statb;
    static DisplayType	acceptableTypes[] =
	    { { Local, Permanent, FromFile },
	      { Foreign, Permanent, FromFile },
	    };

#define NumTypes    (sizeof (acceptableTypes) / sizeof (acceptableTypes[0]))

    if (servers[0] == '/')
    {
	serversFile = fopen (servers, "r");
	if (serversFile == NULL)
 	{
	    LogError ("cannot access servers file %s\n", servers);
	    return;
	}
	if (ServersModTime == 0)
	{
	    fstat (fileno (serversFile), &statb);
	    ServersModTime = statb.st_mtime;
	}
	while (fgets (lineBuf, sizeof (lineBuf)-1, serversFile))
	{
	    len = strlen (lineBuf);
	    if (lineBuf[len-1] == '\n')
		lineBuf[len-1] = '\0';
	    ParseDisplay (lineBuf, acceptableTypes, NumTypes);
	}
	fclose (serversFile);
    }
    else
    {
	ParseDisplay (lineBuf, acceptableTypes, NumTypes);
    }
}

static void
MarkDisplay (d)
struct display	*d;
{
    d->state = MissingEntry;
}

static void
RescanServers ()
{
    Debug ("rescanning servers\n");
    LogInfo ("Rescanning both config and servers files\n");
    ForEachDisplay (MarkDisplay);
    ReinitResources ();
    LoadDMResources ();
    ScanServers ();
    StartDisplays ();
}

SetConfigFileTime ()
{
    struct stat	statb;

    if (stat (config, &statb) != -1)
	ConfigModTime = statb.st_mtime;
}

static
RescanIfMod ()
{
    struct stat	statb;

    if (stat (config, &statb) != -1)
    {
	if (statb.st_mtime != ConfigModTime)
	{
	    Debug ("Config file %s has changed, rereading\n", config);
	    LogInfo ("Rereading configuration file %s\n", config);
	    ConfigModTime = statb.st_mtime;
	    ReinitResources ();
	    LoadDMResources ();
	}
    }
    if (servers[0] == '/' && stat(servers, &statb) != -1)
    {
	if (statb.st_mtime != ServersModTime)
	{
	    Debug ("Servers file %s has changed, rescanning\n", servers);
	    LogInfo ("Rereading servers file %s\n", servers);
	    ServersModTime = statb.st_mtime;
	    ForEachDisplay (MarkDisplay);
	    ScanServers ();
	}
    }
    
}

/*
 * catch a SIGTERM, kill all displays and exit
 */

static SIGVAL
StopAll ()
{
    Debug ("Shutting down entire manager\n");
    DestroyWellKnownSockets ();
    ForEachDisplay (StopDisplay);
#ifdef SYSV
    /* to avoid another one from killing us unceremoniously */
    (void) signal (SIGTERM, StopAll);
    (void) signal (SIGINT, StopAll);
#endif
}

/*
 * notice that a child has died and may need another
 * sub-daemon started
 */

int	ChildReady;

#ifndef SYSV
static SIGVAL
ChildNotify ()
{
    ChildReady = 1;
}
#endif

WaitForChild ()
{
    int		pid;
    struct display	*d;
    waitType	status;
    int		mask;

#ifdef SYSV
    /* XXX classic sysV signal race condition here with RescanNotify */
    if ((pid = wait (&status)) != -1)
#else
    mask = sigblock (sigmask (SIGCHLD) | sigmask (SIGHUP));
    Debug ("signals blocked, mask was 0x%x\n", mask);
    if (!ChildReady && !Rescan)
	sigpause (mask);
    ChildReady = 0;
    sigsetmask (mask);
    while ((pid = wait3 (&status, WNOHANG, (struct rusage *) 0)) > 0)
#endif
    {
	Debug ("Manager wait returns pid: %d\n", pid);
	if (autoRescan)
	    RescanIfMod ();
	if ((d = FindDisplayByPid (pid))) {
	    d->pid = -1;
	    switch (waitVal (status)) {
	    case UNMANAGE_DISPLAY:
		Debug ("Display exited with UNMANAGE_DISPLAY\n");
		StopDisplay (d);
		break;
	    case OBEYSESS_DISPLAY:
		Debug ("Display exited with OBEYSESS_DISPLAY\n");
		if (d->displayType.lifetime != Permanent)
		    StopDisplay (d);
		else
		    RestartDisplay (d, FALSE);
		break;
	    default:
		Debug ("Display exited with unknown status %d\n", waitVal(status));
		LogError ("Unknown session exit code %d from process %d\n",
			  waitVal (status), pid);
		StopDisplay (d);
		break;
	    case OPENFAILED_DISPLAY:
		Debug ("Display exited with OPENFAILED_DISPLAY\n");
		if (d->displayType.origin == FromXDMCP)
		{
		    SendFailed (d, "Cannot open display");
		    StopDisplay (d);
		}
		else
		{
		    if (++d->startTries >= d->startAttempts)
			StopDisplay (d);
		    else
			RestartDisplay (d, TRUE);
		}
		break;
	    case RESERVER_DISPLAY:
		Debug ("Display exited with RESERVER_DISPLAY\n");
		if (d->displayType.origin == FromXDMCP ||
		    d->status == zombie)
		    StopDisplay(d);
		else
		    RestartDisplay (d, TRUE);
		break;
	    case SIGTERM * 256 + 1:
		Debug ("Display exited on SIGTERM\n");
		if (d->displayType.origin == FromXDMCP ||
		    d->status == zombie)
		    StopDisplay(d);
		else
		    RestartDisplay (d, TRUE);
		break;
	    case REMANAGE_DISPLAY:
		Debug ("Display exited with REMANAGE_DISPLAY\n");
		/*
 		 * XDMCP will restart the session if the display
		 * requests it
		 */
		if (d->displayType.origin == FromXDMCP ||
		    d->status == zombie)
		    StopDisplay(d);
		else
		    RestartDisplay (d, FALSE);
		break;
	    }
	}
	else if (d = FindDisplayByServerPid (pid))
	{
	    d->serverPid = -1;
	    switch (d->status)
	    {
	    case zombie:
		Debug ("Zombie server reaped, removing display %s\n", d->name);
		RemoveDisplay (d);
		break;
	    case phoenix:
		Debug ("Phoenix server arises, restarting display %s\n", d->name);
		d->status = notRunning;
		break;
	    case running:
		Debug ("Server for display %s terminated unexpectedly, status %d\n", d->name, waitVal (status));
		LogError ("Server for display %s terminated unexpectedly\n", d->name);
		if (d->pid != -1)
		{
		    Debug ("Terminating session pid %d\n", d->pid);
		    TerminateProcess (d->pid);
		}		
		break;
	    case notRunning:
		Debug ("Server exited for notRunning session on display %s\n", d->name);
		break;
	    }
	}
	else
	{
	    Debug ("Unknown child termination, status %d\n", waitVal (status));
	}
    }
    StartDisplays ();
}

static void
CheckDisplayStatus (d)
struct display	*d;
{
    if (d->displayType.origin == FromFile)
    {
	switch (d->state) {
	case MissingEntry:
	    StopDisplay (d);
	    break;
	case NewEntry:
	    d->state = OldEntry;
	case OldEntry:
	    if (d->status == notRunning)
		StartDisplay (d);
	    break;
	}
    }
}

StartDisplays ()
{
    ForEachDisplay (CheckDisplayStatus);
}

StartDisplay (d)
struct display	*d;
{
    int	pid;
    int	ResourcesLoaded = FALSE;

    Debug ("StartDisplay %s\n", d->name);
    d->startTries = 0;
    if (d->displayType.location == Local)
    {
	LoadDisplayResources (d);
	/* don't bother pinging local displays; we'll
	 * certainly notice when they exit
	 */
	d->pingInterval = 0;
	ResourcesLoaded = TRUE;
    	if (d->authorize)
    	{
	    Debug ("SetServerAuthorization %s, file %s, auth %s\n",
		    d->name, d->authFile, d->authName);
	    SetLocalAuthorization (d);
	    /*
	     * reset the server after writing the authorization information
	     * to make it read the file (for compatibility with old
	     * servers which read auth file only on reset instead of
	     * at first connection)
	     */
	    if (d->serverPid != -1 && d->resetForAuth)
		kill (d->serverPid, SIGHUP);
    	}
	if (d->serverPid == -1 && !StartServer (d))
	{
	    LogError ("Server for display %s can't be started, session disabled\n", d->name);
	    RemoveDisplay (d);
	    return;
	}
    }
    else
    {
	if (d->authorization && d->authFile)
	    SaveServerAuthorization (d, d->authorization);
    }
    switch (pid = fork ())
    {
    case 0:
	CleanUpChild ();
	signal (SIGPIPE, SIG_IGN);
	if (!ResourcesLoaded)
	    LoadDisplayResources (d);
	SetAuthorization (d);
	if (!WaitForServer (d))
	    exit (OPENFAILED_DISPLAY);
	ManageSession (d);
	exit (REMANAGE_DISPLAY);
    case -1:
	break;
    default:
	Debug ("pid: %d\n", pid);
	d->pid = pid;
	d->status = running;
	break;
    }
}

TerminateProcess (pid)
{
    kill (pid, SIGTERM);
#ifdef SIGCONT
    kill (pid, SIGCONT);
#endif
}

/*
 * transition from running to zombie or deleted
 */

void
StopDisplay (d)
    struct display	*d;
{
    if (d->serverPid != -1)
	d->status = zombie; /* be careful about race conditions */
    if (d->pid != -1)
	TerminateProcess (d->pid);
    if (d->serverPid != -1)
	TerminateProcess (d->serverPid);
    else
	RemoveDisplay (d);
}

/*
 * transition from running to phoenix or notRunning
 */

static void
RestartDisplay (d, forceReserver)
    struct display  *d;
    int		    forceReserver;
{
    if (d->serverPid != -1 && (forceReserver || d->terminateServer))
    {
	TerminateProcess (d->serverPid);
	d->status = phoenix;
    }
    else
    {
	d->status = notRunning;
    }
}

static FD_TYPE	CloseMask;
static int	max;

RegisterCloseOnFork (fd)
int	fd;
{
    FD_SET (fd, &CloseMask);
    if (fd > max)
	max = fd;
}

ClearCloseOnFork (fd)
int	fd;
{
    FD_CLR (fd, &CloseMask);
    if (fd == max) {
	while (--fd >= 0)
	    if (FD_ISSET (fd, &CloseMask))
		break;
	max = fd;
    }
}

CloseOnFork ()
{
    int	fd;

    for (fd = 0; fd <= max; fd++)
	if (FD_ISSET (fd, &CloseMask))
	    close (fd);
    FD_ZERO (&CloseMask);
    max = 0;
}

static int  pidFd;
static FILE *pidFilePtr;

StorePid ()
{
    int		oldpid;

    if (pidFile[0] != '\0') {
	pidFd = open (pidFile, 2);
	if (pidFd == -1 && errno == ENOENT)
	    pidFd = creat (pidFile, 0666);
	if (pidFd == -1 || !(pidFilePtr = fdopen (pidFd, "r+")))
	{
	    LogError ("process-id file %s cannot be opened\n",
		      pidFile);
	    return -1;
	}
	if (fscanf (pidFilePtr, "%d", &oldpid) != 1)
	    oldpid = -1;
	fseek (pidFilePtr, 0l, 0);
	if (lockPidFile)
	{
#ifdef SYSV
	    if (lockf (pidFd, F_TLOCK, 0) == -1)
	    {
		if (errno == EACCES)
		    return oldpid;
		else
		    return -1;
	    }
#else
	    if (flock (pidFd, LOCK_EX|LOCK_NB) == -1)
	    {
		if (errno == EWOULDBLOCK)
		    return oldpid;
		else
		    return -1;
	    }
#endif
	}
	close(creat(pidFile, 0666));
	fprintf (pidFilePtr, "%d\n", getpid ());
    }
    return 0;
}

UnlockPidFile ()
{
    if (lockPidFile)
#ifdef SYSV
	lockf (pidFd, F_ULOCK, 0);
#else
	flock (pidFd, LOCK_UN);
#endif
    close (pidFd);
    fclose (pidFilePtr);
}

/*VARARGS*/
SetTitle (va_alist)
va_dcl
{
#ifndef NOXDMTITLE
    char	*p = Title;
    int	left = TitleLen;
    char	*s;
    va_list	args;

    va_start(args);
    *p++ = '-';
    --left;
    while (s = va_arg (args, char *))
    {
	while (*s && left > 0)
	{
	    *p++ = *s++;
	    left--;
	}
    }
    while (left > 0)
    {
	*p++ = ' ';
	--left;
    }
    va_end(args);
#endif	
}
