/* exp_poll.c - poll() interface for Expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

/*

This file contains code designed to work on SVR>=3 systems.  The logic
closely parallels interact_select.c.  If your system supports select,
you may use that file instead, however some SV systems (e.g., HPUX)
implement select incorrectly, so I would recommend using poll.

*/

#include <stdio.h>
#include <errno.h>
#include <poll.h>
#include "exp_global.h"

static struct pollfd *fds;
static int maxfds;

/*ARGSUSED*/
void
exp_event_disarm(fd)
int fd;
{
}

/* this returns a printable representation of a poll revent.  I only have */
/* to handle the ones that occurs when event is set to 0. */
static
char *
bad_poll_type(x)
int x;
{
	static char msg[30];

	switch (x) {
	case POLLERR: return("POLLERR");
	case POLLHUP: return("POLLHUP");
	case POLLNVAL: return("POLLNVAL");
	default:
		sprintf(msg,"unknown poll event (%d)",x);
		return(msg);
	}
}

/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
int
exp_get_next_event(interp,masters, n,master_out,timeout,key)
Tcl_Interp *interp;
int *masters;
int n;
int *master_out;	/* out variable */
int timeout;
int key;
{
	static rr = 0;	/* round robin ptr */

	int msec;	/* milliseconds to wait */
	int i;	/* index into in-array */
	int pc;

	for (i=0;i<n;i++) {
		struct f *f;
		int m;

		rr++;
		if (rr == n) rr = 0;

		m = masters[rr];
		f = fs + m;

		if (f->key != key) {
			f->key = key;
			f->force_read = FALSE;
			*master_out = m;
			return(EXP_DATA_OLD);
		} else if ((!f->force_read) && (f->size != 0)) {
			*master_out = m;
			return(EXP_DATA_OLD);
		}
	}

	msec = ((timeout == -1)?-1:(1000 * timeout));

	if (n > maxfds) {
		if (fds) free((char *)fds);
		fds = (struct pollfd *)malloc(n*sizeof(struct pollfd));
		if (!fds) {
			fds = 0;
			exp_error(interp,"failed to malloc %d pollfds",n);
			return(EXP_TCLERROR);
		}
		maxfds = n;
	}
	
	for (i = 0;i < n;i++) {
		fds[i].fd = masters[i];
		fds[i].events = POLLIN;
		/* apparently, no need to clear revents */
	}

	pc = poll(fds,n,msec);
	if (pc > 0) {
	    for (i=0;i<n;i++) {
		rr++;
		if (rr >= n) rr = 0;	/* ">" catches previous readys that */
				/* used more fds then we're using now */

		if (fds[rr].revents) {
			if (fds[rr].revents & POLLIN) {
				*master_out = masters[rr];
				return(EXP_DATA);
			} else {
				exp_error(interp,"poll[%d].revent = %s",
					bad_poll_type(fds[masters[rr]].revents));
				return(EXP_TCLERROR);
			}
		}
	    }
	} else if (pc == 0) {
		return(EXP_TIMEOUT);
	} else if (errno != EBADF) {
		    /* someone is rotten */
		    for (i=0;i<n;i++) {
			if (fds[i].revents & POLLNVAL) {
				exp_error(interp,"spawn_id %d invalid",masters[i]);
				return(EXP_TCLERROR);
			}
		   }
	} else {
		/* not prepared to handle anything else */
		exp_error(interp,"poll: %s",sys_errlist[errno]);
		return(EXP_TCLERROR);
	}
	/*NOTREACHED*/
}

/* have no idea if this works, but it seems reasonable (given that it is a */
/* hack).  Let me know if it works, or you fix it to make it work - DEL */
/* There is no portable way to do sub-msecond sleeps on such a system, so */
/* do the next best thing (without a busy loop) and fake it: sleep the right */
/* amount of time over the long run.  Note that while "subtotal" isn't */
/* reinitialized, it really doesn't matter for such a gross hack as random */
/* scheduling pauses will easily introduce occasional one second delays. */
void
exp_usleep(usec)
long usec;		/* microseconds */
{
	static subtotal = 0;	/* microseconds */
	int msec;		/* milliseconds */
	struct pollfd pf;	/* some systems need this, even tho unused */

	subtotal += usec;
	/* if less then 1 msec pause, do nothing but remember it */
	if (subtotal < 1000) return;
	msec = subtotal/1000;
	subtotal = subtotal%1000;
	(void) poll(&pf,(unsigned long)0,msec);
}

/* set things up for later calls to event handler */
void
exp_init_event()
{
	maxfds = 5;	/* pick a reasonable number */
	fds = (struct pollfd *)malloc(maxfds * sizeof(struct pollfd));

	exp_event_exit = 0;

}

