/* $Header: /home/x_cvs/mit/clients/xdm/xdmshell.c,v 1.4 1992/09/16 15:09:22 dawes Exp $ */
/*
 * xdmshell - simple program for running xdm from login
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Jim Fulton, MIT X Consortium
 *
 * This program should probably be setuid to root.  On the macII, it must be
 * run from the console so that getty doesn't get confused about zero-length
 * reads.
 *
 * WARNING:  Make sure that you tailor your Xresources file to have a
 * way of invoking the abort-display() action.  Otherwise, you won't be able
 * bring down X when you are finished.
 */

#include <stdio.h>
#include "dm.h"
#include <errno.h>
extern int errno;
#ifndef linux
extern int sys_nerr;
extern char *sys_errlist[];
#endif /* !linux */

#ifdef macII
#define ON_CONSOLE_ONLY
#endif

#ifdef ON_CONSOLE_ONLY
#include <sys/ioctl.h>
#endif

#ifndef BINDIR
#define BINDIR "/usr/bin/X11"
#endif

/*
 * HP-UX does have vfork, but A/UX doesn't
 */
#if (defined(SYSV) || defined(macII)) && !defined(hpux)
#define vfork() fork()
#endif

char *ProgramName;

static char *SysErrorMsg (n)
    int n;
{
    char *s = (n > 0 && n < sys_nerr) ? sys_errlist[n] : "unknown";
    return (s ? s : "null system error");
}


static int exec_args (filename, args)
    char *filename;
    char **args;
{
    int pid;
    waitType status;

    if (!filename) return -1;

    if (filename[0] != '/') {
	fprintf (stderr, 
	       "%s:  attempt to execute program with relative pathname:  %s\n",
		 ProgramName, filename);
	return -1;
    }

    if (access (filename, X_OK) != 0) return -1;

    switch (pid = vfork ()) {
      case -1:						/* error */
	return -1;
      case 0:    					/* child */
	execv (filename, args);
	_exit (1);
	/* NOTREACHED */
      default:						/* parent */
	while (wait (&status) != pid) ;
    }
    return waitCode (status);
}

static int exec_one_arg (filename, arg)
    char    *filename;
    char    *arg;
{
    char    *argv[3];

    argv[0] = filename;
    argv[1] = arg;
    argv[2] = NULL;
    return exec_args (filename, argv);
}

main (argc, argv)
    int argc;
    char *argv[];
{
    int ttyfd;
    char cmdbuf[256];
    char *args[10];
#ifdef ON_CONSOLE_ONLY
    int consfd;
    int ttypgrp, conspgrp;
    char *ttyName;
    extern char *ttyname();
#endif

    ProgramName = argv[0];

    if (argc > 1) {
	fprintf (stderr, "usage:  %s\r\n", ProgramName);
	exit (1);
    }

    ttyfd = open ("/dev/tty", O_RDWR, 0);
    if (ttyfd < 3) {			/* stdin = 0, stdout = 1, stderr = 2 */
	fprintf (stderr, 
		 "%s:  must be run directly from the console.\r\n",
		 ProgramName);
	exit (1);
    }
#ifdef ON_CONSOLE_ONLY
    if (ioctl (ttyfd, TIOCGPGRP, (char *)&ttypgrp) != 0) {
	fprintf (stderr, "%s:  unable to get process group of /dev/tty\r\n",
		 ProgramName);
	(void) close (ttyfd);
	exit (1);
    }
#endif
    (void) close (ttyfd);
    
#ifdef ON_CONSOLE_ONLY
    ttyName = ttyname (0);
    if (!ttyName || strcmp (ttyName, "/dev/console") != 0) {
	fprintf (stderr, "%s:  must login on /dev/console instead of %s\r\n",
		 ProgramName, ttyName ? ttyName : "non-terminal device");
	exit (1);
    }

    consfd = open ("/dev/console", O_RDWR, 0);
    if (consfd < 3) {			/* stdin = 0, stdout = 1, stderr = 2 */
	fprintf (stderr, "%s:  unable to open /dev/console\r\n",
		 ProgramName);
	exit (1);
    }

    if (ioctl (consfd, TIOCGPGRP, (char *)&conspgrp) != 0) {
	fprintf (stderr,
		 "%s:  unable to get process group of /dev/console\r\n",
		 ProgramName);
	(void) close (consfd);
	exit (1);
    }
    (void) close (consfd);

    if (ttypgrp != conspgrp) {
	fprintf (stderr, "%s:  must be run from /dev/console\r\n", 
		 ProgramName);
	exit (1);
    }
#endif

    /* make xdm run in a non-setuid environment */
    setuid (geteuid());

    /*
     * exec /usr/bin/X11/xdm -nodaemon -udpPort 0
     */
    strcpy (cmdbuf, BINDIR);
    strcat (cmdbuf, "/xdm");
    args[0] = cmdbuf;
    args[1] = "-nodaemon";
    args[2] = "-udpPort";
    args[3] = "0";
    args[4] = NULL;
    if (exec_args (cmdbuf, args) == -1) {
	fprintf (stderr, "%s:  unable to execute %s (error %d, %s)\r\n",
		 ProgramName, cmdbuf, errno, SysErrorMsg(errno));
	exit (1);
    }

#ifdef macII
    strcpy (cmdbuf, BINDIR);
    strcat (cmdbuf, "/Xrepair");
    (void) exec_one_arg (cmdbuf, NULL);
    (void) exec_one_arg ("/usr/bin/screenrestore", NULL);
#endif

#ifdef sun
    strcpy (cmdbuf, BINDIR);
    strcat (cmdbuf, "/kbd_mode");
    (void) exec_one_arg (cmdbuf, "-a");
#endif

    exit (0);
    /*NOTREACHED*/
}

