/* exp_select.c - select() interface for Expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef _AIX
/* AIX has some unusual definition of FD_SET */
#include <sys/select.h>
#endif

#include "tcl.h"
#include "exp_global.h"
#include "exp_command.h"	/* for struct f defs */
#include "exp_event.h"

#ifndef FD_SET
#define FD_SET(fd,fdset)	(fdset)->fds_bits[0] |= (1<<(fd))
#define FD_CLR(fd,fdset)	(fdset)->fds_bits[0] &= ~(1<<(fd))
#define FD_ZERO(fdset)		(fdset)->fds_bits[0] = 0
#define FD_ISSET(fd,fdset)	(((fdset)->fds_bits[0]) & (1<<(fd)))
#ifndef AUX2
typedef struct fd_set {
	long fds_bits[1];
	/* any implementation so pathetic as to not define FD_SET will just */
	/* have to suffer with only 32 bits worth of fds */
} fd_set;
#endif /* AUX2 */
#endif

static struct timeval zerotime = {0, 0};
static struct timeval anytime = {0, 0};	/* can be changed by user */

static int maxfds;

/*ARGSUSED*/
void
exp_event_disarm(fd)
int fd;
{
}

/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
int
exp_get_next_event(interp,masters, n,master_out,timeout,key)
Tcl_Interp *interp;
int *masters;
int n;			/* # of masters */
int *master_out;	/* 1st event master, not set if none */
int timeout;		/* seconds */
int key;
{
	static rr = 0;	/* round robin ptr */

	int i;	/* index into in-array */
	struct timeval *t;

	fd_set rdrs;
#ifdef HPUX
	fd_set excep;
#else
#define excep (fd_set *)0
#endif

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

	if (timeout >= 0) {
		t = &anytime;
		t->tv_sec = timeout;
	} else {
		t = NULL;
	}

 restart:
	FD_ZERO(&rdrs);
#ifdef HPUX
	FD_ZERO(&excep);
#endif
	for (i = 0;i < n;i++) {
		FD_SET(masters[i],&rdrs);
#ifdef HPUX
		FD_SET(masters[i],&excep);
#endif
	}

	if (-1 == select(maxfds,&rdrs,(fd_set *)0,excep,t)) {
		/* window refreshes trigger EINTR, ignore */
		if (errno == EINTR) goto restart;
		else if (errno == EBADF) {
		    /* someone is rotten */
		    for (i=0;i<n;i++) {
			fd_set suspect;
			FD_ZERO(&suspect);
			FD_SET(masters[i],&suspect);
			if (-1 == select(maxfds,&suspect,
			    		(fd_set *)0,(fd_set *)0,&zerotime)) {
				exp_error(interp,"invalid spawn_id (%d)\r",masters[i]);
				return(EXP_TCLERROR);
			}
		   }
	        } else {
			/* not prepared to handle anything else */
			exp_error(interp,"select: %s\r",sys_errlist[errno]);
			return(EXP_TCLERROR);
		}
	}

	for (i=0;i<n;i++) {
		rr++;
		if (rr >= n) rr = 0;	/* ">" catches previous readys that */
				/* used more fds then we're using now */

		if (FD_ISSET(masters[rr],&rdrs)) {
			*master_out = masters[rr];
			return(EXP_DATA_NEW);
#ifdef HPUX
		} else if (FD_ISSET(masters[rr], &excep)) {
			struct request_info ioctl_info;
			if (ioctl(master,TIOCREQCHECK,&ioctl_info) < 0) {
				debuglog("ioctl error on TIOCREQCHECK: %d", errno);
				break;
			}
			if (ioctl_info.request == TIOCCLOSE) {
				/* eof */
				*master_out = masters[rr];
				return(EXP_EOF);
			}
			if (ioctl(master, TIOCREQSET, &ioctl_info) < 0)
				debuglog("ioctl error on TIOCREQSET after ioctl or open on slave: %d", errno);
#endif /* HPUX */
		}
	}
	return(EXP_TIMEOUT);
}

void
exp_usleep(usec)
long usec;		/* microseconds */
{
	struct timeval t;

	t.tv_sec = usec/1000000L;
	t.tv_usec = usec%1000000L;
	(void) select(1, (fd_set *)0, (fd_set *)0, (fd_set *)0, &t);
}

/* set things up for later calls to event handler */
void
exp_init_event()
{
#ifdef HPUX
	maxfds = sysconf(_SC_OPEN_MAX); /* from Ian Hogg, ian@dms.cdc.com */
#else
	maxfds = getdtablesize();
#endif

	exp_event_exit = 0;
}
