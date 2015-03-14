/* interact (with only one process) - give user keyboard control

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

/* This file exists for deficient versions of UNIX that lack select,
poll, or some other multiplexing hook.  Instead, this code uses two
processes per spawned process.  One sends characters from the spawnee
to the spawner; a second send chars the other way.

This will work on any UNIX system.  The only sacrifice is that it
doesn't support multiple processes.  Eventually, it should catch
SIGCHLD on dead processes and do the right thing.  But it is pretty
gruesome to imagine so many processes to do all this.  If you change
it successfully, please mail back the changes to me.  - Don
*/

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include "tcl.h"
#include "exp_global.h"
#include "exp_command.h"	/* for struct f defs */
#include "exp_event.h"

/*ARGSUSED*/
void
exp_event_disarm(fd)
int fd;
{
}

/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
/*ARGSUSED*/
int
exp_get_next_event(interp,masters, n,master_out,timeout,key)
Tcl_Interp *interp;
int *masters;
int n;			/* # of masters */
int *master_out;	/* 1st event master, not set if none */
int timeout;		/* seconds */
int key;
{
	int m;
	struct f *f;

	if (n > 1) {
		exp_error(interp,"expect not compiled with multiprocess support");
		/* select a different INTERACT_TYPE in Makefile */
		return(TCL_ERROR);
	}

	m = *master_out = masters[0];
	f = fs + m;

	if (f->key != key) {
		f->key = key;
		f->force_read = FALSE;
		return(EXP_DATA_OLD);
	} else if ((!f->force_read) && (f->size != 0)) {
		return(EXP_DATA_OLD);
	}

	return(EXP_DATA_NEW);
}


/* There is no portable way to do sub-second sleeps on such a system, so */
/* do the next best thing (without a busy loop) and fake it: sleep the right */
/* amount of time over the long run.  Note that while "subtotal" isn't */
/* reinitialized, it really doesn't matter for such a gross hack as random */
/* scheduling pauses will easily introduce occasional one second delays. */
void
exp_usleep(usec)
long usec;		/* microseconds */
{
	static subtotal = 0;
	int seconds;

	subtotal += usec;
	if (subtotal < 1000000) return;
	seconds = subtotal/1000000;
	subtotal = subtotal%1000000;
	sleep(seconds);
}

/* set things up for later calls to event handler */
void
exp_init_event()
{
	exp_event_exit = 0;
}
