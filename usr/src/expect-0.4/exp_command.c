/* command.c - expect commands, except for interact and expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

expect commands are as follows.  They are discussed further in the man page
and the paper "expect: Curing Those Uncontrollable Fits of Interaction",
Proceedings of the Summer 1990 USENIX Conference, Anaheim, California.

Command		Arguments	Returns			Sets
-------		---------	-------			----
close		[-i spawn_id]
debug		[-f file] expr
disconnect	[status]
exit		[status]
expect_after	patlst body ...
expect_before	patlst body ...
expect[_user]	patlst body ...	string matched		expect_status
getpid				pid
interact	str-body pairs	body return
interpreter			TCL status
log_file	[[-a] file]
log_user	expr
match_max	max match size
overlay		[-] fd-spawn_id pairs
ready		spawn_id set	spawn_ids ready
send		[...]
send_error	[...]
send_user	[...]
spawn		program [...]	pid			spawn_id
strace		level
system		shell command	TCL status
trap		[[arg] siglist]
wait				{pid status} or {-1 errno}

Variable of interest are:

Name		Type	Value			Set by		Default
----		----	-----			------		-------
expect_out	str	string matched		expect cmds
spawn_id	int	currently spawned proc	user/spawn cmd
timeout		int	seconds			user		10
tty_spawn_id	int	spawn_id of /dev/tty	expect itself
user_spawn_id	int	spawn_id of user	expect itself	1
send_slow	int/flt	send -s size/time	user
send_human	5 flts	send -h timing		user

*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/file.h>
#include "exp_tty.h"
#ifdef SYSV4
#include <sys/stropts.h>
#endif
#ifndef M_XENIX
 /* wait.h is not available on SCO XENIX 386. -- pf@artcom0.north.de */
#if !defined(CRAY) || CRAY>=60
#include <sys/wait.h>
#endif
#endif
#include <varargs.h>
#include <errno.h>
/*#include <memory.h> - deprecated - ANSI C moves them into string.h */
#include <signal.h>
#include <math.h>		/* for log/pow computation in send -h */
#include <ctype.h>		/* all this for ispunct! */

#include "tcl.h"
#include "string.h"
#include "exp_rename.h"
#include "exp_global.h"
#include "exp_command.h"
#include "exp_log.h"
#include "exp_event.h"
#include "exp_main.h"

#define SPAWN_ID_VARNAME "spawn_id"

int getptymaster();
int getptyslave();
extern exp_tty tty_current, tty_cooked;

int exp_disconnected = FALSE;	/* whether we are a disconnected process */
int forked = FALSE;		/* whether we are child process */

/* the following are just reserved addresses, to be used as ClientData */
/* args to be used to tell commands how they were called. */
/* The actual values won't be used, only the addresses, but I give them */
/* values out of my irrational fear the compiler might collapse them all. */
static int sendCD_error = 2;	/* called as send_error */
static int sendCD_user = 3;	/* called as send_user */
static int sendCD_proc = 4;	/* called as send or send_spawn */
static int sendCD_log = 5;	/* called as send_log */

struct f *fs = 0;		/* process array (indexed by spawn_id's) */
int fd_max = -1;		/* highest fd */

int expect_key;			/* no need to initialize */

/* Do not terminate format strings with \n!!! */
/*VARARGS*/
void
exp_error(va_alist)
va_dcl
{
	Tcl_Interp *interp;
	char *fmt;
	va_list args;

	va_start(args);
	interp = va_arg(args,Tcl_Interp *);
	fmt = va_arg(args,char *);
	vsprintf(interp->result,fmt,args);
	va_end(args);
}

/* returns f struct if valid or 0 */
struct f *
fd_to_f(interp,fd,msg)
Tcl_Interp *interp;
int fd;
char *msg;
{
	if (fd >= 0 && fd <= fd_max && (fs[fd].flags & FD_VALID)) {
		return(fs+fd);
	}

	exp_error(interp,"%s: invalid spawn id (%d)",msg,fd);
	return(0);
}

#if 0
/* following routine is not current used, but might be later */
/* returns fd or -1 if no such entry */
static int
pid_to_fd(pid)
int pid;
{
	int fd;

	for (fd=0;fd<=fd_max;fd++) {
		if (fs[fd].pid == pid) return(fd);
	}
	return 0;
}
#endif

int
fd_close(interp,fd)
Tcl_Interp *interp;
int fd;
{
	struct f *f = fd_to_f(interp,fd,"close");
	if (!f) return(TCL_ERROR);

/* Ignore close errors.  Some systems are really braindamaged and return */
/* errors for no evident reason.  Anyway, receiving an error upon pty-close */
/* doesn't mean anything anyway as far as I know. */
	close(fd);
	if (f->buffer) {
		free(f->buffer);
		f->buffer = 0;
		f->msize = 0;
		f->size = 0;
		f->printed = 0;
		f->echoed = 0;
		if (f->armed) {
			exp_event_disarm(f-fs);
			f->armed = FALSE;
		}
		free(f->lower);
	}
	f->armed = FALSE;

	if (f->flags & FD_USERWAITED) f->flags = 0;
	else f->flags |= FD_CLOSED;
	return(TCL_OK);
}

static int
fd_new(fd,pid)
int fd;
int pid;
{
	int i, low;
	struct f *newfs;	/* temporary, so we don't lose old fs */

	/* resize table if nec */
	if (fd > fd_max) {
		if (!fs) {	/* no fd's yet allocated */
			if (!(newfs = (struct f *)
				malloc(sizeof(struct f)*(fd+1)))) {
				/* malloc failed */
				return(TCL_ERROR);
			}
			low = 0;
		} else {		/* enlarge fd table */
			if (!(newfs = (struct f *)realloc((char *)fs,
				sizeof(struct f)*(fd+1)))) {
				/* malloc failed */
				return(TCL_ERROR);
			}
			low = fd_max+1;
		}
		fs = newfs;
		fd_max = fd;
		for (i = low; i <= fd_max; i++) { /* init new fd entries */
			struct f *f;

			f = fs+i;
			f->pid = 0;
			f->buffer = 0;
			f->lower = 0;
			f->flags = 0;	/* not FD_VALID */
			f->msize = f->size = 0;
			f->armed = FALSE;
		}
	}
	/* close down old table entry if nec */
	fs[fd].pid = pid;		/* overwrite pid */
	fs[fd].size = 0;		/* forget old data */
					/* but leave msize and buffer as is! */
	fs[fd].printed = 0;
	fs[fd].echoed = 0;
	fs[fd].key = expect_key++;
	fs[fd].force_read = FALSE;
	fs[fd].armed = FALSE;
	fs[fd].umsize = default_match_max;
	fs[fd].flags = FD_VALID;	/* reset all flags */
	return(TCL_OK);
}

void
exp_init_spawn_id_vars(interp)
Tcl_Interp *interp;
{
	Tcl_SetVar(interp,"user_spawn_id",USER_SPAWN_ID_LIT,0);

	/* note that the user_spawn_id is NOT /dev/tty which could */
	/* (at least in theory anyway) be later re-opened on a different */
	/* fd, while stdin might have been redirected away from /dev/tty */

	if (dev_tty != -1) {
		char dev_tty_str[10];
		sprintf(dev_tty_str,"%d",dev_tty);
		Tcl_SetVar(interp,"tty_spawn_id",dev_tty_str,0);
	}
}

void
exp_init_spawn_ids()
{
	fd_new(0,getpid());
	fs[0].flags |= FD_USER;

	if (dev_tty != -1) {
		fd_new(dev_tty,getpid());
	}
}

/* arguments are passed verbatim to execvp() */
/*ARGSUSED*/
static int
cmdSpawn(clientData,interp,argc,argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int slave;
	int pid;
	char **a;
	int ttyfd;
	int master;

	if (argc == 1) {
		exp_error(interp,"usage: spawn program [args]");
		return(TCL_ERROR);
	}

	if (0 > (master = getptymaster())) {
		exp_error(interp,"too many programs spawned? - out of ptys");
		return(TCL_ERROR);
	}

	if (streq("-noecho",argv[1])) {
		/* shift command into next position to get rid of noecho */
		argv[1] = argv[0];
		argv++; argc--;
	} else {
		for (a = argv;*a;a++) {
			Log(0,"%s ",*a);
		}
		nflog("\r\n",0);
	}

	/* much easier to set this, than remember all masters */
	fcntl(master,F_SETFD,1);	/* close on exec */

	if (NULL == (argv[1] = Tcl_TildeSubst(interp,argv[1])))
		return TCL_ERROR;

	if ((pid = fork()) == -1) {
		exp_error(interp,"fork: %s",sys_errlist[errno]);
		return(TCL_ERROR);
	}

	if (pid) {
		/* parent */
		char buf[3];

		if (TCL_ERROR == fd_new(master,pid)) return(TCL_ERROR);

#ifdef CRAY
		setptypid(pid);
#endif

		/* tell user id of new process */
		sprintf(buf,"%d",master);
		Tcl_SetVar(interp,SPAWN_ID_VARNAME,buf,0);

		sprintf(interp->result,"%d",pid);
		debuglog("spawn: returns {%s}\r\n",interp->result);

		return(TCL_OK);
	}

	/* child process - do not return from here!  all errors must exit() */

	if (dev_tty != -1) {
		close(dev_tty);
		dev_tty = -1;
	}

#ifdef CRAY
	(void) close(master);
#endif

#ifdef POSIX
#define DO_SETSID
#endif
#ifdef __convex__
#define DO_SETSID
#endif

#ifdef DO_SETSID
	setsid();
#else
#ifdef SYSV3
#ifndef CRAY
	setpgrp();
#endif /* CRAY */
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	setpgrp(0,0);
/*	setpgrp(0,getpid());*/	/* make a new pgrp leader */
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* SYSV3 */
#endif /* DO_SETSID */
	close(0);
	close(1);
	/* leave 2 around awhile for stderr-related stuff */

	/* since we closed fd 0, open of pty slave must return fd 0 */

	/* since getptyslave may have to run stty, (some of which work on fd */
	/* 0 and some of which work on 1) do the dup's inside getptyslave. */

#define STTY_INIT	"stty_init"
	if (0 > (slave = getptyslave(get_var(STTY_INIT)))) {
		errorlog("open(slave pty): %s\r\n",sys_errlist[errno]);
		exit(-1);
	}
	/* sanity check */
	if (slave != 0) {
		errorlog("getptyslave: slave = %d but expected 0\n",slave);
		return(-1);
	}

#ifdef CRAY
 	(void) setsid();
 	(void) ioctl(0,TCSETCTTY,0);
 	(void) close(0);
 	if (open("/dev/tty", O_RDWR) < 0) {
 		errorlog("open(/dev/tty): %s\r\n",sys_errlist[errno]);
 		exit(-1);
 	}
 	(void) close(1);
 	(void) close(2);
 	(void) dup(0);
 	(void) dup(0);
	setptyutmp();	/* create a utmp entry */

	/* _CRAY2 code from Hal Peterson <hrp@cray.com>, Cray Research, Inc. */
#ifdef _CRAY2
	/*
	 * Interpose a process between expect and the spawned child to
	 * keep the slave side of the pty open to allow time for expect
	 * to read the last output.  This is a workaround for an apparent
	 * bug in the Unicos pty driver on Cray-2's under Unicos 6.0 (at
	 * least).
	 */
	if ((pid = fork()) == -1) {
		errorlog("second fork: %s\r\n",sys_errlist[errno]);
		exit(-1);
	}

	if (pid) {
 		/* Intermediate process. */
		int status;
		int timeout;
		char *t;

		/* How long should we wait? */
		if (t = get_var("pty_timeout"))
			timeout = atoi(t);
		else if (t = get_var("timeout"))
			timeout = atoi(t)/2;
		else
			timeout = 5;

		/* Let the spawned process run to completion. */
 		while (wait(&status) < 0 && errno == EINTR)
			/* empty body */;

		/* Wait for the pty to clear. */
		sleep(timeout);

		/* Duplicate the spawned process's status. */
		if (WIFSIGNALED(status))
			kill(getpid(), WTERMSIG(status));

		/* The kill may not have worked, but this will. */
 		exit(WEXITSTATUS(status));
	}
#endif /* _CRAY2 */
#endif /* CRAY */

	/* by now, fd 0 and 1 point to slave pty, so fix 2 */
	close(2);
	fcntl(0,F_DUPFD,2);	/* duplicate 0 onto 2 */

	/* avoid fflush of cmdfile since this screws up the parents seek ptr */
	/* There is no portable way to fclose a shared read-stream!!!! */
	if (exp_cmdfile && (exp_cmdfile != stdin))
		(void) close(fileno(exp_cmdfile));
	if (logfile) (void) fclose(logfile);
	if (debugfile) (void) fclose(debugfile);
	/* (possibly multiple) masters are closed automatically due to */
	/* earlier fcntl(,,CLOSE_ON_EXEC); */

        (void) execvp(argv[1],argv+1);
	/* Unfortunately, by now we've closed fd's to stderr, logfile and
		debugfile.
	   The only reasonable thing to do is to send back the error as
	   part of the program output.  This will be picked up in an
	   expect or interact command.
	*/
	errorlog("execvp(%s): %s\r\n",argv[1],sys_errlist[errno]);
	exit(-1);
	/*NOTREACHED*/
}

/*ARGSUSED*/
static int
cmdGetpid(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	sprintf(interp->result,"%d",getpid());
	return(TCL_OK);
}

/* returns current master (via out-parameter) */
/* returns f or 0, but note that since fd_to_f calls tcl_error, this */
/* may be immediately followed by a "return(TCL_ERROR)"!!! */
struct f *
exp_update_master(interp,m)
Tcl_Interp *interp;
int *m;
{
	char *s = exp_get_var(interp,SPAWN_ID_VARNAME);
	*m = (s?atoi(s):USER_SPAWN_ID);
	return(fd_to_f(interp,*m,(s?s:USER_SPAWN_ID_LIT)));
}

/*ARGSUSED*/
static int
cmdSystem(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
#define MAX_ARGLIST 10240
	int i;
	char buf[MAX_ARGLIST];
	char *bufp = buf;
	int total_len = 0, arg_len;

	int stty_args_recognized = TRUE;
	int cooked = FALSE;

	if (argc > 2 && streq(argv[1],"stty")) {
		ioctled_devtty = TRUE;

		for (i=2;i<argc;i++) {
			if (streq(argv[i],"raw") ||
			    streq(argv[i],"-cooked")) {
				tty_raw(1);
			} else if (streq(argv[i],"-raw") ||
				   streq(argv[i],"cooked")) {
				cooked = TRUE;
				tty_raw(-1);
			} else if (streq(argv[i],"echo")) {
				tty_echo(1);
			} else if (streq(argv[i],"-echo")) {
				tty_echo(-1);
			} else stty_args_recognized = FALSE;
		}
		/* if unknown args, fall thru and let real stty have a go */
		if (stty_args_recognized) {
#ifdef POSIX
 			if (tcsetattr(dev_tty,TCSADRAIN, &tty_current) == -1) {
#else
#	ifdef SYSV3
		        if (ioctl(dev_tty, TCSETAW, &tty_current) == -1) {
#	else
			if (ioctl(dev_tty, TIOCSETP, &tty_current) == -1) {
#	endif
#endif
			    errorlog("ioctl(user): %s\r\n",sys_errlist[errno]);
			    exp_exit(interp,-1);
			}
			return(TCL_OK);
		}
	}

	for (i = 1;i<argc;i++) {
		total_len += (1 + (arg_len = strlen(argv[i])));
		if (total_len > MAX_ARGLIST) {
			exp_error(interp,"args too long (>=%d chars)",
				total_len);
			return(TCL_ERROR);
		}
		memcpy(bufp,argv[i],arg_len);
		bufp += arg_len;
		/* no need to check bounds, we accted for it earlier */
		memcpy(bufp," ",1);
		bufp += 1;
	}

	*(bufp-1) = '\0';
	i = system(buf);
	debuglog("system(%s) = %d\r\n",buf,i);

	if (!stty_args_recognized) {
		/* find out what weird options user asked for */
#ifdef POSIX
 			if (tcgetattr(dev_tty, &tty_current) == -1) {
#else
#	ifdef SYSV3
	        if (ioctl(dev_tty, TCGETA, &tty_current) == -1) {
#	else
		if (ioctl(dev_tty, TIOCGETP, &tty_current) == -1) {
#	endif
#endif
			errorlog("ioctl(get): %s\r\n",sys_errlist[errno]);
			exp_exit(interp,-1);
		}
		if (cooked) {
			/* find out user's new defn of 'cooked' */
			tty_cooked = tty_current;
		}
	}

	/* Following the style of Tcl_ExecCmd, we can just return the */
	/* raw result (appropriately shifted and masked) to Tcl */
	return(0xff & (i>>8));
}

/* write exactly this many bytes, i.e. retry partial writes */
/* I don't know if this is necessary, but large sends might have this */
/* problem */
/* returns 0 for success, -1 for failure */
static int
exact_write(fd,buffer,rembytes)
int fd;
char *buffer;
int rembytes;
{
	int cc;

	while (rembytes) {
		if (-1 == (cc = write(fd,buffer,rembytes))) return(-1);
		/* if (0 == cc) return(-1); can't happen! */

		buffer += cc;
		rembytes -= cc;
	}
	return(0);
}

struct slow_arg {
	int size;
	long time;		/* microseconds */
};

/* returns -1 failure, 0 if successful */
static int
get_slow_args(interp,x)
Tcl_Interp *interp;
struct slow_arg *x;
{
	float ftime;

	int sc;		/* return from scanf */
	char *s = exp_get_var(interp,"send_slow");
	if (!s) {
		exp_error(interp,"send -s: send_slow has no value");
		return(-1);
	}
	if (2 != (sc = sscanf(s,"%d %f",&x->size,&ftime))) {
		exp_error(interp,"send -s: found %d value(s) in send_slow but need 2",sc);
		return(-1);
	}
	if (x->size <= 0) {
		exp_error(interp,"send -s: size (%d) in send_slow must be positive", x->size);
		return(-1);
	}
	x->time = ftime*1000000L;
	if (x->time <= 0) {
		exp_error(interp,"send -s: time (%f) in send_slow must be larger",ftime);
		return(-1);
	}
	return(0);
}

/* returns 0 for success, -1 for failure */
static int
slow_write(fd,buffer,rembytes,arg)
int fd;
char *buffer;
int rembytes;
struct slow_arg *arg;
{
	while (rembytes > 0) {
		int len;

		len = (arg->size<rembytes?arg->size:rembytes);
		if (0 > exact_write(fd,buffer,len)) return(-1);
		rembytes -= arg->size;
		buffer += arg->size;

		/* skip sleep after last write */
		if (rembytes > 0) exp_usleep(arg->time);
	}
	return(0);
}

struct human_arg {
	float alpha;		/* average interarrival time in seconds */
	float alpha_eow;	/* as above but for eow transitions */
	float c;		/* shape */
	float min, max;
};

/* returns -1 if error, 0 if success */
static int
get_human_args(interp,x)
Tcl_Interp *interp;
struct human_arg *x;
{
	int sc;		/* return from scanf */
	char *s = exp_get_var(interp,"send_human");

	if (!s) {
		exp_error(interp,"send -h: send_human has no value");
		return(-1);
	}
	if (5 != (sc = sscanf(s,"%f %f %f %f %f",
			&x->alpha,&x->alpha_eow,&x->c,&x->min,&x->max))) {
		exp_error(interp,"send -h: found %d value(s) in send_human but need 5",sc);
		return(-1);
	}
	if (x->alpha < 0 || x->alpha_eow < 0) {
		exp_error(interp,"send -h: average interarrival times (%f %f) must be non-negative in send_human", x->alpha,x->alpha_eow);
		return(-1);
	}
	if (x->c <= 0) {
		exp_error(interp,"send -h: variability (%f) in send_human must be positive",x->c);
		return(-1);
	}
	x->c = 1/x->c;

	if (x->min < 0) {
		exp_error(interp,"send -h: minimum (%f) in send_human must be non-negative",x->min);
		return(-1);
	}
	if (x->max < 0) {
		exp_error(interp,"send -h: maximum (%f) in send_human must be non-negative",x->max);
		return(-1);
	}
	if (x->max < x->min) {
		exp_error(interp,"send -h: maximum (%f) must be >= minimum (%f) in send_human",x->max,x->min);
		return(-1);
	}
	return(0);
}

/* Compute random numbers from 0 to 1, for expect's send -h */
/* This implementation sacrifices beauty for portability */
static float
unit_random()
{
	/* current implementation is pathetic but works */
	/* 99991 is largest prime in my CRC - can't hurt, eh? */
	return((float)(1+(rand()%99991))/99991.0);
}

void
exp_init_unit_random()
{
	srand(getpid());
}

/* This function is my implementation of the Weibull distribution. */
/* I've added a max time and an "alpha_eow" that captures the slight */
/* but noticable change in human typists when hitting end-of-word */
/* transitions. */
/* returns 0 for success, -1 for failure */
static int
human_write(fd,buffer,arg)
int fd;
char *buffer;
struct human_arg *arg;
{
	char *sp;
	float t;
	float alpha;
	int wc;
	int in_word = TRUE;

	debuglog("human_write: avg_arr=%f/%f  1/shape=%f  min=%f  max=%f\r\n",
		arg->alpha,arg->alpha_eow,arg->c,arg->min,arg->max);

	for (sp = buffer;*sp;sp++) {
		/* use the end-of-word alpha at eow transitions */
		if (in_word && (ispunct(*sp) || isspace(*sp)))
			alpha = arg->alpha_eow;
		else alpha = arg->alpha;
		in_word = !(ispunct(*sp) || isspace(*sp));

		t = (pow(-log((double)unit_random()),arg->c)*alpha)+arg->min;
		if (t>arg->max) t = arg->max;

		/* skip sleep before writing first character */
		if (sp != buffer) exp_usleep((long)(t*1000000));

		wc = write(fd,sp,1);
		if (0 > wc) return(wc);
	}
	return(0);
}


/* I've rewritten this to be unbuffered.  I did this so you could shove */
/* large files through "send".  If you are concerned about efficiency */
/* you should quote all your send args to make them one single argument. */
/*ARGSUSED*/
static int
cmdSend(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int master = -1;
	int master_flag = FALSE;
	int i;
	int wc = 0;	/* if negative, a write has failed */
	extern int is_debugging;
	struct human_arg human_args;
	struct slow_arg slow_args;
#define SEND_STYLE_PLAIN	0
#define SEND_STYLE_HUMAN	1
#define SEND_STYLE_SLOW		2
#define SEND_STYLE_NONE		3
	int send_style = SEND_STYLE_PLAIN;
	int want_cooked = TRUE;

	argv++;
	argc--;
	while (argc) {
		if (streq(*argv,"-i")) {
			argc--; argv++;
			if (argc==0) {
				exp_error(interp,"-i requires following spawn_id");
				return(TCL_ERROR);
			}
			master = atoi(*argv);
			master_flag = TRUE;
			argc--; argv++;
			continue;
		} else if (streq(*argv,"-h")) {
			argc--; argv++;
			if (-1 == get_human_args(interp,&human_args))
				return(TCL_ERROR);
			send_style = SEND_STYLE_HUMAN;
		} else if (streq(*argv,"-s")) {
			argc--; argv++;
			if (-1 == get_slow_args(interp,&slow_args))
				return(TCL_ERROR);
			send_style = SEND_STYLE_SLOW;
		} else if (streq(*argv,"-raw")) {
			argc--; argv++;
			want_cooked = FALSE;
		} else break;
	}

	if (clientData == &sendCD_user) master = 1;
	else if (clientData == &sendCD_error) master = 2;
	else if ((clientData != &sendCD_log) && !master_flag) {
		if (0 == update_master(&master)) return(TCL_ERROR);
		/* true if called as Send with user_spawn_id */
		if (is_user(master)) {
			clientData = &sendCD_user;
			master = 1;
		}
	}

#define send_to_stdout	(clientData == &sendCD_user)
#define send_to_stderr	(clientData == &sendCD_error)
#define send_to_proc	(clientData == &sendCD_proc)
#define send_to_log	(clientData == &sendCD_log)

/*debuglog("master = %d  send_to_proc = %d\r\n",master,send_to_proc);*/

	if (send_to_log) send_style = SEND_STYLE_NONE;
	if (send_to_proc) want_cooked = FALSE;
	if (send_to_proc) debuglog("send: sent {");
	/* if the closing brace doesn't appear, that's because an error was */
	/* encountered before we could send it */

	for (i = 0;i<argc;i++) {
		if (i != 0) {
			/* wedge ' ' between other args.  If you don't want */
			/* this, make it all into one arg! */
			if (send_to_proc) {
				debuglog(" ");
			} else {
/*				if (send_to_stderr) fwrite(" ",1,1,stderr);*/
				if (debugfile) fwrite(" ",1,1,debugfile);
				if ((send_to_stdout && logfile_all) || logfile)
					fwrite(" ",1,1,logfile);
			}
			switch (send_style) {
			case SEND_STYLE_SLOW:
				wc = slow_write(master," ",1,&slow_args);
				break;
			case SEND_STYLE_HUMAN:
				wc = human_write(master," ",&human_args);
				break;
			case SEND_STYLE_NONE:
				break;
			default:
				wc = exact_write(master," ",1);
				break;
			}
		}
		if (wc >= 0) {
			int len = strlen(argv[i]);
			if (send_to_proc) {
				debuglog("%s",dprintify(argv[i]));
			} else {
/*				if (send_to_stderr)
					fwrite(argv[i],1,len,stderr);*/
				if (debugfile)
					fwrite(argv[i],1,len,debugfile);
				if ((send_to_stdout && logfile_all) || logfile)
					fwrite(argv[i],1,len,logfile);
			}

			if (want_cooked) argv[i] = exp_cook(argv[i],&len);

			switch (send_style) {
			case SEND_STYLE_SLOW:
				wc = slow_write(master,argv[i],len,&slow_args);
				break;
			case SEND_STYLE_HUMAN:
				wc = human_write(master,argv[i],&human_args);
				break;
			case SEND_STYLE_NONE:
				break;
			default:
				wc = exact_write(master,argv[i],len);
				break;
			}
		}
		if (wc < 0) {

/* actually, I think close is the wrong thing to do here */
#if 0
			int save_errno = errno;

			/* write failed, so close fd.  But if close fails */
			/* describe write error rather than close */
			if (send_to_proc) fd_close(interp,master);
#endif
			exp_error(interp,"write(spawn_id=%d): %s",
					master,sys_errlist[errno]);
			return(TCL_ERROR);
		}
	}
	if (send_to_proc) debuglog("} to spawn id %d\r\n",master);

	return(TCL_OK);
}

void
cmdLogFile_usage(interp)
Tcl_Interp *interp;
{
	exp_error(interp,"usage: log_file [[-a] file]");
}

/*ARGSUSED*/
static int
cmdLogFile(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	/* when this function returns, we guarantee that if logfile_all */
	/* is TRUE, then logfile is non-zero */

	logfile_all = FALSE;

	argv++; argc--;
	while (argc) {
		if (0 != strcmp(*argv,"-a")) break;
		argc--;argv++;
		logfile_all = TRUE;
	}

	/* note that logfile_all may be TRUE here, even if logfile is zero */

	if (argc > 1) {
		/* too many arguments */
		cmdLogFile_usage(interp);
		if (!logfile) logfile_all = FALSE;
		return(TCL_ERROR);
	}

	if (argc == 0) {
		if (logfile_all) {
			cmdLogFile_usage(interp);
			if (!logfile) logfile_all = FALSE;
			return(TCL_ERROR);
		} else if (logfile) {
			fclose(logfile);
			logfile = 0;
		/*SUPPRESS 530*/
		} else {
			/* asked to close file but not open, ignore */
			/* exp_error(interp,"log not open"); */
			/* return(TCL_ERROR); */
		}
	} else {
		if (logfile) fclose(logfile);
		if (*argv[0] == '~') {
			argv[0] = Tcl_TildeSubst(interp, argv[0]);
			if (argv[0] == NULL) return(TCL_ERROR);
		}

		if (NULL == (logfile = fopen(argv[0],"a"))) {
			exp_error(interp,"%s: %s",argv[0],sys_errlist[errno]);
			logfile_all = FALSE;
			return(TCL_ERROR);
		}
/*new*/		setbuf(logfile,(char *)0);
	}
	return(TCL_OK);
}

/*ARGSUSED*/
static int
cmdLogUser(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc != 2) {
		exp_error(interp,"usage: log_user expr");
		return(TCL_ERROR);
	}

	if (0 == atoi(argv[1])) loguser = FALSE;
	else loguser = TRUE;
	return(TCL_OK);
}

void
cmdDebug_usage(interp)
Tcl_Interp *interp;
{
	exp_error(interp,"usage: debug [-f file] expr");
}

/*ARGSUSED*/
static int
cmdDebug(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int fopened = FALSE;

	argv++;
	argc--;
	while (argc) {
		if (!streq(*argv,"-f")) break;
		argc--;argv++;
		if (argc < 1) {
			cmdDebug_usage(interp);
			return(TCL_ERROR);
		}
		if (debugfile) fclose(debugfile);
		if (*argv[0] == '~') {
			argv[0] = Tcl_TildeSubst(interp, argv[0]);
			if (argv[0] == NULL) return(TCL_ERROR);
		}
		if (NULL == (debugfile = fopen(*argv,"a"))) {
			exp_error(interp,"%s: %s",*argv,sys_errlist[errno]);
			return(TCL_ERROR);
		}
		setbuf(debugfile,(char *)0);
		fopened = TRUE;
		argc--;argv++;
	}

	if (argc != 1) {
		cmdDebug_usage(interp);
		return(TCL_ERROR);
	}

	/* if no -f given, close file */
	if (fopened == FALSE && debugfile) {
		fclose(debugfile);
		debugfile = 0;
	}

	exp_is_debugging = atoi(*argv);
	return(TCL_OK);
}

/*ARGSUSED*/
static int
cmdExit(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int value = 0;

	if (argc > 2) {
		exp_error(interp,"usage: exit [status]");
		return(TCL_ERROR);
	}

	if (argc == 2) {
		if (Tcl_GetInt(interp, argv[1], &value) != TCL_OK) {
			return TCL_ERROR;
		}
	}

	exp_exit(interp,value);
	/*NOTREACHED*/
}

/*ARGSUSED*/
static int
cmdClose(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int m = -1;

	if (argc == 1) {
		if (update_master(&m) == 0) return(TCL_ERROR);
	} else if (streq(argv[1],"-i")) {
		if (argc != 3) {
			exp_error(interp,"usage: close [-i spawn_id]");
			return(TCL_ERROR);
		}
		m = atoi(argv[2]);
	}

	if (m != -1) return(fd_close(interp,m));

	/* doesn't look like our format, it must be a Tcl-style file handle */
	/* we're so lucky that the formats are easily distinguishable */
	/* Historical note: we used "close" in Tcl long before there was a */
	/* builtin by the same name. */

	/* don't know what is formally correct as 1st arg, but I see the */
	/* code doesn't use it anyway */
	return(Tcl_CloseCmd(clientData,interp,argc,argv));
}

/*ARGSUSED*/
static void
tcl_tracer(clientData,interp,level,command,cmdProc,cmdClientData,argc,argv)
ClientData clientData;
Tcl_Interp *interp;
int level;
char *command;
int (*cmdProc)();
ClientData cmdClientData;
int argc;
char *argv[];
{
	int i;

	/* come out on stderr, by using errorlog */
	errorlog("%2d",level);
	for (i = 0;i<level;i++) nferrorlog("  ",0/*ignored - satisfy lint*/);
	errorlog("%s\r\n",command);
}

/*ARGSUSED*/
static int
cmdTrace(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	static int trace_level = 0;
	static Tcl_Trace trace_handle;

	if (argc != 2) {
		exp_error(interp,"usage: trace level");
		return(TCL_ERROR);
	}
	/* tracing already in effect, undo it */
	if (trace_level > 0) Tcl_DeleteTrace(interp,trace_handle);

	/* get and save new trace level */
	trace_level = atoi(argv[1]);
	if (trace_level > 0)
		trace_handle = Tcl_CreateTrace(interp,
				trace_level,tcl_tracer,(ClientData)0);
	return(TCL_OK);
}

static char *wait_usage = "usage: wait [-i spawn_id]";

/*ARGSUSED*/
static int
cmdWait(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int master;
	struct f *f;
	/* if your C compiler bombs here, define NO_PID_T in Makefile */
#ifdef NO_PID_T
	int pid = 0;	/* 0 indicates no error occurred (yet) */
#else
	pid_t pid = 0;	/* ditto */
#endif

	argc--; argv++;

	if (argc == 0) {
		if (0 == update_master(&master)) return(TCL_ERROR);
	} /* else if (streq(argv,"-pid")) {
		argv++;
		pid = atoi(argv++);
		*/ /* search through fs for one with right pid */ /*
		master = pid_to_fd(pid);
	} */ else if (streq(argv[0],"-i")) {
		if (argc != 2) {
			exp_error(interp,wait_usage);
			return(TCL_ERROR);
		}
		master = atoi(argv[1]);
	} else {
		exp_error(interp,wait_usage);
		return(TCL_ERROR);
	}

	if (!(f = fd_to_f(interp,master,"wait"))) return(TCL_ERROR);

	if (!(f->flags & FD_SYSWAITED)) {
#ifdef NOWAITPID
		int status;
		for (;;) {
			int i;

			pid = wait(&status);
			if (pid == f->pid) break;
			/* oops, wrong pid */
			for (i=0;i<=fd_max;i++) {
				if (fs[i].pid == pid) break;
			}
			if (i>fd_max) {
				debuglog("wait found unknown pid %d\r\n",pid);
				continue;	/* drop on floor */
			}
			fs[i].flags |= FD_SYSWAITED;
			fs[i].wait = status;
		}
		f->wait = status;
#else
		pid = waitpid(f->pid,&f->wait,0);
#endif
		f->flags |= FD_SYSWAITED;
	}
	if (f->flags & FD_CLOSED) f->flags = 0;
	f->flags |= FD_USERWAITED;
#ifndef WEXITSTATUS
#define WEXITSTATUS(x) x
#endif
	/* return {pid status} or {-1 errno} */
	/* non-portable assumption that pid_t can be printed with %d */
	sprintf(interp->result,"%d %d",f->pid,
		(pid == -1)?errno:WEXITSTATUS(f->wait));
	return(TCL_OK);
}

/*ARGSUSED*/
static int
cmdFork(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int pid;

	if (argc > 1) {
		exp_error(interp,"usage: fork");
		return(TCL_ERROR);
	}

	if (0 == (pid = fork())) forked = TRUE;

	sprintf(interp->result,"%d",pid);
	debuglog("fork: returns {%s}\r\n",interp->result);
	return(TCL_OK);
}

/*ARGSUSED*/
static int
cmdDisconnect(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int ttyfd;

	if (argc > 1) {
		exp_error(interp,"usage: disconnect");
		return(TCL_ERROR);
	}

	if (exp_disconnected) {
		exp_error(interp,"already disconnected");
		return(TCL_ERROR);
	}
	if (!forked) {
		exp_error(interp,"can only disconnect child process");
		return(TCL_ERROR);
	}
	exp_disconnected = TRUE;

	/* ignore hangup signals generated by testing ptys in getptymaster */
	/* and other places */
	signal(SIGHUP,SIG_IGN);

	/* freopen(,,stdin) prevents confusion between send/expect_user */
	/* accidentally mapping to a real spawned process after a disconnect */
	/* A better solution might be to put code in the _user routines to */
	/* check that a disconnect has occured. */
	/* freopen(,,stderr) saves error checking in error/log routines. */
	freopen("/dev/null","r",stdin);
	freopen("/dev/null","w",stdout);
	freopen("/dev/null","w",stderr);

#ifdef DO_SETSID
	setsid();
#else
#ifdef SYSV3
	/* put process in our own pgrp, and lose controlling terminal */
#ifdef sysV88
	/* With setpgrp first, child ends up with closed stdio */
	/* according to Dave Schmitt <daves@techmpc.csg.gss.mot.com> */
	if (fork()) exit(0);
	setpgrp();
#else
	setpgrp();
	/*signal(SIGHUP,SIG_IGN); moved out to above */
	if (fork()) exit(0);	/* first child exits (as per Stevens, */
	/* UNIX Network Programming, p. 79-80) */
	/* second child process continues as daemon */
#endif
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	setpgrp(0,0);
/*	setpgrp(0,getpid());*/	/* put process in our own pgrp */
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		/* zap controlling terminal if we had one */
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* SYSV3 */
#endif /* DO_SETSID */
	return(TCL_OK);
}

/*ARGSUSED*/
static int
cmdOverlay(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int newfd, oldfd;
	int dash_name = 0;
	char *command;

	argc--; argv++;
	while (argc) {
		if (*argv[0] != '-') break;	/* not a flag */
		if (streq(*argv,"-")) {		/* - by itself */
			argc--; argv++;
			dash_name = 1;
			continue;
		}
		newfd = atoi(argv[0]+1);
		argc--; argv++;
		if (argc == 0) {
			exp_error(interp,"overlay -i requires additional argument");
			return(TCL_ERROR);
		}
		oldfd = atoi(argv[0]);
		argc--; argv++;
		debuglog("overlay: mapping fd %d to %d\r\n",oldfd,newfd);
		if (oldfd != newfd) (void) dup2(oldfd,newfd);
		else debuglog("warning: overlay: old fd == new fd (%d)\r\n",oldfd);
	}
	if (argc == 0) {
		exp_error(interp,"need program name");
		return(TCL_ERROR);
	}
	command = argv[0];
	if (dash_name) {
		argv[0] = malloc(1+strlen(command));
		/* check for malloc failure */
		sprintf(argv[0],"-%s",command);
	}

	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
        (void) execvp(command,argv);
	errorlog("execvp(%s): %s\r\n",argv[0],sys_errlist[errno]);
	exit(-1);
	/*NOTREACHED*/
}

#if 0
/*ARGSUSED*/
int
cmdReady(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	char num[4];	/* can hold up to "999 " */
	char buf[1024];	/* can easily hold 256 spawn_ids! */
	int i, j;
	int *masters, *masters2;
	int timeout = get_timeout();

	if (argc < 2) {
		exp_error(interp,"usage: ready spawn_id1 [spawn_id2 ...]");
		return(TCL_ERROR);
	}

	if (0 == (masters = (int *)malloc((argc-1)*sizeof(int)))) {
		exp_error(interp,"malloc(%d spawn_id's)",(argc-1));
		return(TCL_ERROR);
	}
	if (0 == (masters2 = (int *)malloc((argc-1)*sizeof(int)))) {
		exp_error(interp,"malloc(%d spawn_id's)",(argc-1));
		return(TCL_ERROR);
	}

	for (i=1;i<argc;i++) {
		j = atoi(argv[i]);
		if (!fd_to_f(interp,j,"ready")) {
			free(masters);
			return(TCL_ERROR);
		}
		masters[i-1] = j;
	}
	j = i-1;
	if (TCL_ERROR == ready(masters,i-1,masters2,&j,&timeout))
		return(TCL_ERROR);

	/* pack result back into out-array */
	buf[0] = '\0';
	for (i=0;i<j;i++) {
		sprintf(num,"%d ",masters2[i]); /* note extra blank */
		strcat(buf,num);
	}
	free(masters); free(masters2);
	Tcl_Return(interp,buf,TCL_VOLATILE);
	return(TCL_OK);
}
#endif

/*ARGSUSED*/
int
cmdInterpreter(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc != 1) {
		exp_error(interp,"no arguments allowed");
		return(TCL_ERROR);
	}

	exp_interpreter(interp);
	/* errors and ok, are caught by escape() and discarded */
	/* the only thing that actually comes out of escape are */
	/* RETURN, BREAK, and CONTINUE which we all translate to OK */
	return(TCL_OK);
}

/* this command supercede's Tcl's builtin CONTINUE command */
/*ARGSUSED*/
int
cmdContinueExpect(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc == 1) return(TCL_CONTINUE);
	else if (argc == 2) {
		if (streq(argv[1],"-expect")) {
			return(TCL_CONTINUE_EXPECT);
		}
	}
	exp_error(interp,"usage: continue [-expect]\n");
	return(TCL_ERROR);
}

/* this command supercede's Tcl's builtin RETURN command */
/*ARGSUSED*/
int
cmdReturnInter(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int rc = TCL_RETURN;

	if (argc == 1) return(TCL_RETURN);
	if (argc > 3) {
		exp_error(interp,"usage: return [-tcl] [value]\n");
		return(TCL_ERROR);
	}

	if (streq(argv[1],"-tcl")) rc = TCL_RETURN_TCL;

	if (!(rc == TCL_RETURN_TCL && argc == 2)) {
		/* if either return "-tcl expr" or "expr", get the expr */
		Tcl_SetResult(interp,argv[1],TCL_VOLATILE);
	}

	return(rc);
}

void
exp_create_commands(interp)
Tcl_Interp *interp;
{
	extern int cmdInteract();

	extern int cmdExpectVersion();

	extern int cmdPrompt1();
	extern int cmdPrompt2();

	Tcl_CreateCommand(interp,"close",
		cmdClose,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"debug",
		cmdDebug,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"disconnect",
		cmdDisconnect,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"exit",
		cmdExit,(ClientData)0,exp_deleteProc);

	/* following commands are created in exp_init_expect in expect.c */
	/* to prevent unnecessary name space pollution by the ClientData */
	/* argument to Tcl_CreateCommand */
	/* expect, expect_after, expect_before, expect_user */
	/* trap */
	/* matchmax */

	Tcl_CreateCommand(interp,"continue",
		cmdContinueExpect,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"expect_version",
		cmdExpectVersion,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"fork",
		cmdFork,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"getpid",
		cmdGetpid,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"interact",
		cmdInteract,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"interpreter",
		cmdInterpreter,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"log_file",
		cmdLogFile,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"log_user",
		cmdLogUser,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"overlay",
		cmdOverlay,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"prompt1",
		cmdPrompt1,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"prompt2",
		cmdPrompt2,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"return",
		cmdReturnInter,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"send",
		cmdSend,(ClientData)&sendCD_proc,exp_deleteProc);
	/* Tk may wipe out "send" so provide an alias: "send_spawn" */
	Tcl_CreateCommand(interp,"send_spawn",
		cmdSend,(ClientData)&sendCD_proc,exp_deleteProc);
	Tcl_CreateCommand(interp,"send_error",
		cmdSend,(ClientData)&sendCD_error,exp_deleteProc);
	Tcl_CreateCommand(interp,"send_log",
		cmdSend,(ClientData)&sendCD_log,exp_deleteProc);
	Tcl_CreateCommand(interp,"send_user",
		cmdSend,(ClientData)&sendCD_user,exp_deleteProc);
	Tcl_CreateCommand(interp,"spawn",
		cmdSpawn,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"strace",
		cmdTrace,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"system",
		cmdSystem,(ClientData)0,exp_deleteProc);
	Tcl_CreateCommand(interp,"wait",
		cmdWait,(ClientData)0,exp_deleteProc);
}
