/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	slave.c
 * ---------------------------------------------------------------------*/

#ident	"@(#)slave.c	26.8	91/09/14 SMI"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <X11/Xlib.h>
#include <signal.h>
#include <stdio.h>
#include "cmdstream.h"

/* ----------------------------------------------------------------------
 *	Local Data
 * ---------------------------------------------------------------------*/
typedef struct {
	char	*program;
	pid_t	pid;
} SlaveInfo;

static SlaveInfo slaveInfo = {
	"olwmslave", 0
};

/* ----------------------------------------------------------------------
 *	Local Forward Declarations
 * ---------------------------------------------------------------------*/
void	SlaveFailure();

/* ----------------------------------------------------------------------
 *	SlaveStart
 * ---------------------------------------------------------------------*/
pid_t
SlaveStart(argv)
	char		**argv;
{
	int		input[2],output[2];
	struct rlimit	rlimit;
	int		fd,maxfd;

	if (pipe(input) == -1) {
		ErrorWarning("Couldn't create input pipe for olwmslave");
		perror("pipe");
		SlaveFailure();
		return -1;
	}
	if (pipe(output) == -1) {
		ErrorWarning("Couldn't create output pipe for olwmslave");
		perror("pipe");
		SlaveFailure();
		(void)close(input[0]);
		(void)close(input[1]);
		return -1;
	}

	slaveInfo.pid = fork();

	switch (slaveInfo.pid) {
	case -1:			/* error */
		(void)close(input[0]);
		(void)close(input[1]);
		(void)close(output[0]);
		(void)close(output[1]);
		SlaveFailure();
		break;
	case 0:				/* Slave */
		dup2(input[0],0);
		dup2(output[1],1);
		if (getrlimit(RLIMIT_NOFILE,&rlimit) == -1)
			maxfd = 0;
		else
			maxfd = rlimit.rlim_cur;	
		for (fd=3; fd<maxfd ; fd++) {
			(void)close(fd);
		}
		argv[0] = slaveInfo.program;
		if (execvp(slaveInfo.program,argv) == -1) {
			SlaveFailure();
			exit(-1);
		}
		break;
	default:			/* parent */
		SetCmdStream(fdopen(output[0],"r"), 	/* reader */
			     fdopen(input[1],"w"));	/* writer */
		break;
	}
	return slaveInfo.pid;
}

/* ----------------------------------------------------------------------
 *	SlaveStop	- call to stop Slave process
 * ---------------------------------------------------------------------*/
void
SlaveStop()
{
	if (slaveInfo.pid == 0)
		return;
	if (kill(slaveInfo.pid,SIGTERM) == -1) {
		/* -- dont really want this error message
		ErrorWarning("Couldn't kill olwmslave");
		perror("kill");
		*/
	}
}

/* ----------------------------------------------------------------------
 *	SlaveStopped	- called when Slave process has died
 * ---------------------------------------------------------------------*/
void
SlaveStopped()
{
	SetCmdStream((FILE *)0,(FILE *)0);
}

/* ----------------------------------------------------------------------
 *	SlaveFailure	- called if Slave process wont/cant start
 * ---------------------------------------------------------------------*/
static void
SlaveFailure()
{
	char	errbuf[256];

	sprintf(errbuf,
		"Couldn't start %s; Help for olwm will not work.",
		slaveInfo.program);
	ErrorWarning(errbuf);
}
