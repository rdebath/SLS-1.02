/*
 * xdm - display manager daemon
 *
 * $XConsortium: session.c,v 1.28 89/12/19 19:50:18 rws Exp $
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
 * session.c
 */

# include "dm.h"
# include <X11/Xlib.h>
# include <signal.h>
# include <X11/Xatom.h>
# include <setjmp.h>
# include <sys/errno.h>
# include <stdio.h>

extern int  errno;

static int			clientPid;
static struct greet_info	greet;
static struct verify_info	verify;

static jmp_buf	abortSession;

static SIGVAL
catchTerm ()
{
    longjmp (abortSession, 1);
}

static jmp_buf	pingTime;

static SIGVAL
catchAlrm ()
{
    longjmp (pingTime, 1);
}

SessionPingFailed (d)
    struct display  *d;
{
    if (clientPid > 1)
    {
    	AbortClient (clientPid);
    	source (&verify, d->reset);
    }
    SessionExit (d, RESERVER_DISPLAY);
}

extern void	exit ();

/*
 * We need our own error handlers because we can't be sure what exit code Xlib
 * will use, and our Xlib does exit(1) which matches REMANAGE_DISPLAY, which
 * can cause a race condition leaving the display wedged.  We need to use
 * RESERVER_DISPLAY for IO errors, to ensure that the manager waits for the
 * server to terminate.  For other X errors, we should give up.
 */

static
IOErrorHandler (dpy)
    Display *dpy;
{
    extern char *sys_errlist[];
    extern int sys_nerr;
    char *s = ((errno >= 0 && errno < sys_nerr) ? sys_errlist[errno]
						: "unknown error");

    LogError("fatal IO error %d (%s)\n", errno, s);
    exit(RESERVER_DISPLAY);
}

static int
ErrorHandler(dpy, event)
    Display *dpy;
    XErrorEvent *event;
{
    LogError("X error\n");
    if (XmuPrintDefaultErrorMessage (dpy, event, stderr) == 0) return 0;
    exit(UNMANAGE_DISPLAY);
}

ManageSession (d)
struct display	*d;
{
    int			pid;
    Display		*dpy, *InitGreet ();

    Debug ("ManageSession %s\n", d->name);
    (void)XSetIOErrorHandler(IOErrorHandler);
    (void)XSetErrorHandler(ErrorHandler);
    SetTitle(d->name, (char *) 0);
    /*
     * Step 5: Load system default Resources
     */
    LoadXloginResources (d);
    Debug ("name now %s\n", d->name);
    dpy = InitGreet (d);
    if (d->authorization && d->authFile)
    {
	Debug ("Done with authorization file %s, removing\n", d->authFile);
	(void) unlink (d->authFile);
    }
    for (;;) {
	/*
	 * Greet user, requesting name/password
	 */
	Greet (d, &greet);
	/*
	 * Verify user
	 */
	if (Verify (d, &greet, &verify))
		break;
	else
		FailedLogin (d, &greet);
	Debug ("after verify, name %s\n", d->name);
    }
    DeleteXloginResources (d, dpy);
    CloseGreet (d);
    Debug ("Greet loop finished\n");
    /*
     * Run system-wide initialization file
     */
    if (source (&verify, d->startup) != 0)
    {
	Debug ("Startup program %s exited with non-zero status\n",
		d->startup);
	SessionExit (d, OBEYSESS_DISPLAY);
    }
    clientPid = 0;
    if (!setjmp (abortSession)) {
	signal (SIGTERM, catchTerm);
	/*
	 * Start the clients, changing uid/groups
	 *	   setting up environment and running the session
	 */
	if (StartClient (&verify, d, &clientPid)) {
	    Debug ("Client Started\n");
	    /*
	     * Wait for session to end,
	     */
	    for (;;) {
		if (d->pingInterval)
		{
		    if (!setjmp (pingTime))
		    {
			signal (SIGALRM, catchAlrm);
			alarm (d->pingInterval * 60);
			pid = wait ((waitType *) 0);
			alarm (0);
		    }
		    else
		    {
			alarm (0);
		    	if (!PingServer (d, (Display *) NULL))
			    SessionPingFailed (d);
		    }
		}
		else
		{
		    pid = wait ((waitType *) 0);
		}
		if (pid == clientPid)
		    break;
	    }
	} else {
	    LogError ("session start failed\n");
	}
    } else {
	/*
	 * when terminating the session, nuke
	 * the child and then run the reset script
	 */
	AbortClient (clientPid);
    }
    /*
     * run system-wide reset file
     */
    Debug ("Source reset program %s\n", d->reset);
    source (&verify, d->reset);
    SessionExit (d, OBEYSESS_DISPLAY);
}

LoadXloginResources (d)
struct display	*d;
{
    char	cmd[1024];

    if (d->resources[0] && access (d->resources, 4) == 0) {
	if (d->authorization && d->authFile && d->authFile[0]) {
	    sprintf (cmd, "XAUTHORITY=%s %s -display %s -load %s",
			    d->authFile,
			    d->xrdb, d->name, d->resources);
	} else {
	    sprintf (cmd, "%s -display %s -load %s",
			    d->xrdb, d->name, d->resources);
	}
	Debug ("Loading resource file: %s\n", cmd);
	system (cmd);
    }
}

/*ARGSUSED*/
DeleteXloginResources (d, dpy)
struct display	*d;
Display		*dpy;
{
    XDeleteProperty(dpy, RootWindow (dpy, 0), XA_RESOURCE_MANAGER);
}

static jmp_buf syncJump;

static SIGVAL
syncTimeout ()
{
    longjmp (syncJump, 1);
}

SecureDisplay (d, dpy)
struct display	*d;
Display		*dpy;
{
    Debug ("SecureDisplay %s\n", d->name);
    signal (SIGALRM, syncTimeout);
    if (setjmp (syncJump)) {
	LogError ("WARNING: display %s could not be secured\n",
		   d->name);
	SessionExit (d, RESERVER_DISPLAY);
    }
    alarm ((unsigned) d->grabTimeout);
    Debug ("Before XGrabServer %s\n", d->name);
    XGrabServer (dpy);
    if (XGrabKeyboard (dpy, DefaultRootWindow (dpy), True, GrabModeAsync,
		       GrabModeAsync, CurrentTime) != GrabSuccess)
    {
	alarm (0);
	signal (SIGALRM, SIG_DFL);
	LogError ("WARNING: keyboard on display %s could not be secured\n",
		  d->name);
	SessionExit (d, RESERVER_DISPLAY);
    }
    Debug ("XGrabKeyboard succeeded %s\n", d->name);
    alarm (0);
    signal (SIGALRM, SIG_DFL);
    pseudoReset (dpy);
    if (!d->grabServer)
    {
	XUngrabServer (dpy);
	XSync (dpy, 0);
    }
    Debug ("done secure %s\n", d->name);
}

UnsecureDisplay (d, dpy)
struct display	*d;
Display		*dpy;
{
    Debug ("Unsecure display %s\n", d->name);
    if (d->grabServer)
	XUngrabServer (dpy);
    XSync (dpy, 0);
}

SessionExit (d, status)
    struct display  *d;
{
    /* make sure the server gets reset after the session is over */
    if (d->serverPid >= 2)
	kill (d->serverPid, SIGHUP);
    else
	ResetServer (d);
    exit (status);
}

StartClient (verify, d, pidp)
struct verify_info	*verify;
struct display		*d;
int			*pidp;
{
    char	**f, *home, *getEnv ();
    char	*failsafeArgv[2];
    int	pid;

    if (verify->argv) {
	Debug ("StartSession %s: ", verify->argv[0]);
	for (f = verify->argv; *f; f++)
		Debug ("%s ", *f);
	Debug ("; ");
    }
    if (verify->userEnviron) {
	for (f = verify->userEnviron; *f; f++)
		Debug ("%s ", *f);
	Debug ("\n");
    }
    switch (pid = fork ()) {
    case 0:
	CleanUpChild ();
#ifdef NGROUPS

	setgid (verify->groups[0]);
	setgroups (verify->ngroups, verify->groups);
#else
	setgid (verify->gid);
#endif
	setuid (verify->uid);
	SetUserAuthorization (d, verify);
	home = getEnv (verify->userEnviron, "HOME");
	if (home)
		if (chdir (home) == -1) {
			LogError ("No home directory %s for user %s, using /\n",
				  home, getEnv (verify->userEnviron, "USER"));
			chdir ("/");
		}
	if (verify->argv) {
		Debug ("executing session %s\n", verify->argv[0]);
		execve (verify->argv[0], verify->argv, verify->userEnviron);
		LogError ("Session execution failed %s\n", verify->argv[0]);
		Debug ("exec failed\n");
	} else {
		LogError ("Session has no command/arguments\n");
	}
	failsafeArgv[0] = d->failsafeClient;
	failsafeArgv[1] = 0;
	execve (failsafeArgv[0], failsafeArgv, verify->userEnviron);
	exit (1);
    case -1:
	Debug ("StartSession, fork failed\n");
	LogError ("can't start session for %d, fork failed\n", d->name);
	return 0;
    default:
	Debug ("StartSession, fork suceeded %d\n", pid);
	*pidp = pid;
	return 1;
    }
}

static jmp_buf	tenaciousClient;

static SIGVAL
waitAbort ()
{
	longjmp (tenaciousClient, 1);
}

#ifdef SYSV
# include	<ctype.h>
#define killpg(pgrp, sig) kill(-(pgrp), sig)
#endif /* SYSV */

AbortClient (pid)
int	pid;
{
    int	sig = SIGTERM;
#ifdef __STDC__
    volatile int	i;
#else
    int	i;
#endif
    int	retId;
    for (i = 0; i < 4; i++) {
	if (killpg (pid, sig) == -1) {
	    switch (errno) {
	    case EPERM:
		LogError ("xdm can't kill client\n");
	    case EINVAL:
	    case ESRCH:
		return;
	    }
	}
	if (!setjmp (tenaciousClient)) {
	    (void) signal (SIGALRM, waitAbort);
	    (void) alarm ((unsigned) 10);
	    retId = wait ((waitType *) 0);
	    (void) alarm ((unsigned) 0);
	    (void) signal (SIGALRM, SIG_DFL);
	    if (retId == pid)
		break;
	} else
	    signal (SIGALRM, SIG_DFL);
	sig = SIGKILL;
    }
}

int
source (verify, file)
struct verify_info	*verify;
char			*file;
{
    char	*args[4];
    int	pid;
    extern int	errno;
    waitType	result;
    char	*getEnv ();

    Debug ("source %s\n", file);
    if (file[0] && access (file, 1) == 0) {
	switch (pid = fork ()) {
	case 0:
	    CleanUpChild ();
	    if (!(args[0] = getEnv (verify->systemEnviron, "SHELL")))
		    args[0] = "/bin/sh";
	    args[1] = "-c";
	    args[2] = file;
	    args[3] = 0;
	    Debug ("interpreting %s with %s\n", args[2], args[0]);
	    execve (args[0], args, verify->systemEnviron);
	    LogError ("can't execute system shell %s\n", args[0]);
	    exit (1);
	case -1:
	    Debug ("fork failed\n");
	    LogError ("can't fork to execute %s\n", file);
	    return 1;
	    break;
	default:
	    while (wait (&result) != pid)
		    ;
	    break;
	}
	return waitVal (result);
    }
    return 0;
}

int
execute (argv, environ)
char	**argv;
char	**environ;
{
    execve (argv[0], argv, environ);
#ifdef SYSV
    /*
     * shell scripts can't be run in SYSV directly
     */
    if (errno == ENOEXEC) {
	char	program[1024], *e, *p, *optarg;
	FILE	*f;
	char	**newargv, **av;
	int	argc;

	/*
	 * emulate BSD kernel behaviour -- read
	 * the first line; check if it starts
	 * with "#!", in which case it uses
	 * the rest of the line as the name of
	 * program to run.  Else use "/bin/sh".
	 */
	f = fopen (argv[0], "r");
	if (!f)
	    return -1;
	if (fgets (program, sizeof (program) - 1, f) == NULL)
 	{
	    fclose (f);
	    return -1;
	}
	fclose (f);
	e = program + strlen (program) - 1;
	if (*e == '\n')
	    *e = '\0';
	if (!strncmp (program, "#!", 2)) {
	    p = program + 2;
	    while (*p && isspace (*p))
		++p;
	    optarg = p;
	    while (*optarg && !isspace (*optarg))
		++optarg;
	    if (*optarg) {
		*optarg = '\0';
		do
		    ++optarg;
		while (*optarg && isspace (*optarg));
	    } else
		optarg = 0;
	} else {
	    p = "/bin/sh";
	    optarg = 0;
	}
	Debug ("Shell script execution: %s (optarg %s)\n",
		p, optarg ? optarg : "(null)");
	for (av = argv, argc = 0; *av; av++, argc++)
		;
	newargv = (char **) malloc ((argc + (optarg ? 3 : 2)) * sizeof (char *));
	if (!newargv)
	    return -1;
	av = newargv;
	*av++ = p;
	if (optarg)
	    *av++ = optarg;
	while (*av++ = *argv++)
	    ;
	execve (newargv[0], newargv, environ);
    }
#endif
}
