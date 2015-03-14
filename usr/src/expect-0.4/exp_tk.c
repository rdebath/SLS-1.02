/* exp_tk.c - Tk event interface for Expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

/* Notes:
I'm only a little worried because Tk does not check for errno == EBADF
after calling select.  I imagine that if the user passes in a bad file
descriptor, we'll never get called back, and thus, we'll hang forever
- it would be better to at least issue a diagnostic to the user.

Another possible problem: Tk does not do file callbacks round-robin.

Another possible problem: Calling Create/DeleteFileHandler
before/after every Tcl_Eval... in expect/interact could be very
expensive.

*/


#include <stdio.h>
#include <errno.h>
#include "tk.h"

#include "exp_global.h"
#include "exp_command.h"	/* for struct f defs */
#include "exp_event.h"

/* Tk_DoOneEvent will call our filehandler which will set the following vars */
/* enabling us to know where and what kind of I/O we can do */
/*#define EXP_SPAWN_ID_BAD	-1*/
#define EXP_SPAWN_ID_TIMEOUT	-2	/* really indicates a timeout */

static int ready_fd = EXP_SPAWN_ID_BAD;
static int ready_mask;
#ifdef HPUX
static int default_mask = TK_READABLE | TK_EXCEPTION;
#else
static int default_mask = TK_READABLE;
#endif

void exp_filehandler(clientData,mask)
ClientData clientData;
int mask;
{
	ready_fd = (int)clientData;
	ready_mask = mask;
}

/*ARGSUSED*/
void
exp_timehandler(clientData)
ClientData clientData;
{
	ready_fd = EXP_SPAWN_ID_TIMEOUT;
}

#if 0
void
exp_disarm_event_handler(count,fds)
int count;
int *fds;
{
	int i;

	for (i=0;i<count;i++) {
		Tk_DeleteFileHandler(fds[i]);
	}
}


void
exp_arm_event_handlers(count,fds)
int count;
int *fds;
{
	int i;
	int flag = TK_WRITABLE;

#ifdef HPUX
	flag |= TK_EXCEPTION;
#endif

	for (i=0;i<count;i++) {
		Tk_CreateFileHandler(fds[i],flag,exp_filehandler,(ClientData)fds[i]);
	}
}
#endif

void
exp_event_disarm(fd)
int fd;
{
	Tk_DeleteFileHandler(fd);
}

/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
/*ARGSUSED*/
int exp_get_next_event(interp,masters, n,master_out,timeout,key)
Tcl_Interp *interp;
int *masters;
int n;			/* # of masters */
int *master_out;	/* 1st ready master, not set if none */
int timeout;		/* seconds */
int key;
{
	int timer_created = FALSE;
	static rr = 0;	/* round robin ptr */

	int i;	/* index into in-array */
	Tk_TimerToken timetoken;/* handle to Tk timehandler descriptor */

	for (;;) {
		int m;
		struct f *f;

		/* if anything has been touched by someone else, report that */
		/* an event has been received */

		for (i=0;i<n;i++) {
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

		if (!timer_created) {
			if (timeout >= 0) {
				timetoken = Tk_CreateTimerHandler(1000*timeout,
						exp_timehandler,(ClientData)0);
				timer_created = TRUE;
			}
		}

		for (;;) {
			extern int fd_max;
			int j;

			/* make sure no fds are armed that shouldn't be */
			for (j=0;j<=fd_max;j++) {
				if (fs[j].armed) {
					int k;

					for (k=0;k<n;k++) {
						if (masters[k] == j) goto ok;
					}
					Tk_DeleteFileHandler(j);
					fs[j].armed = FALSE;
				}
			ok:	continue;
			}

			/* make sure that all fds that should be armed are */
			for (j=0;j<n;j++) {
				int k = masters[j];

				if (!fs[k].armed) {
					Tk_CreateFileHandler(k,default_mask,
							     exp_filehandler,
							     (ClientData)k);
					fs[k].armed = TRUE;
				}
			}

			Tk_DoOneEvent(0);	/* do any event */
			if (ready_fd == EXP_SPAWN_ID_BAD) continue;
			if (ready_fd == EXP_SPAWN_ID_TIMEOUT) {
				ready_fd = EXP_SPAWN_ID_BAD;
				return(EXP_TIMEOUT);
			}
			*master_out = ready_fd;
			ready_fd = EXP_SPAWN_ID_BAD;


			if (timer_created) Tk_DeleteTimerHandler(timetoken);

#ifdef HPUX
			if (mask & TK_EXCEPTION) {
				struct request_info ioctl_info;
				if (ioctl(fd,TIOCREQCHECK,&ioctl_info) < 0) {
					debuglog("ioctl error on TIOCREQCHECK: %d", errno);
					return;
				}
				if (ioctl_info.request == TIOCCLOSE) {
					if (ioctl(master, TIOCREQSET, &ioctl_info) < 0) {
						debuglog("ioctl error on TIOCREQSET after ioctl or open on slave: %d", errno);
					}
				}
				return(EXP_EOF);
			}
#endif /*HPUX*/
			return(EXP_DATA_NEW);
		}
	}
}

void
exp_usleep(usec)
{
	int j;
	extern int fd_max;

	Tk_CreateTimerHandler(usec/1000,exp_timehandler,(ClientData)0);

	ready_fd = EXP_SPAWN_ID_BAD;

	do {
		/* disarm all file handlers or we'll never get any sleep!! */
		for (j=0;j<fd_max;j++) {
			if (fs[j].armed) {
				Tk_DeleteFileHandler(j);
				fs[j].armed = FALSE;
			}
		}
		
		Tk_DoOneEvent(0);	/* do any event */
	} while (ready_fd != EXP_SPAWN_ID_TIMEOUT);
}

static void
exp_event_exit_real(interp)
Tcl_Interp *interp;
{
	Tcl_Eval(interp,"destroy .",0,(char **)NULL);
}

/* set things up for later calls to event handler */
void
exp_init_event()
{
	exp_event_exit = exp_event_exit_real;
}

