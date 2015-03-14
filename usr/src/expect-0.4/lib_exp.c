/* lib_exp.c - top-level functions in the expect C library, libexpect.a

Written by: Don Libes, libes@cme.nist.gov, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <signal.h>
/*#include <memory.h> - deprecated - ANSI C moves them into string.h */
#ifndef NO_STRING_H
#include <string.h>
#endif
#include <varargs.h>
#include <errno.h>
#include "exp_rename.h"
#define EXP_DEFINE_FNS
#include "expect.h"

extern char *sys_errlist[];
extern int errno;

#ifdef NO_MEMCPY
#define memcpy(x,y,len) bcopy(y,x,len)
#endif

/* avoid coliding with Tcl's stdlib.h */
#ifndef _STDLIB
char *malloc();
void exit();
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define EXP_MATCH_MAX	2000
/* public */
char *exp_match = 0;
int exp_match_max = EXP_MATCH_MAX;	/* bytes */
int exp_timeout = 10;			/* seconds */
int exp_autoallocpty = TRUE;		/* if TRUE, we do allocation */
int exp_pty[2];				/* master is [0], slave is [1] */
int exp_pid;
char *exp_stty_init = 0;		/* initial stty args */
int exp_disconnected = FALSE;		/* not disc. from controlling tty */

jmp_buf exp_readenv;		/* for interruptable read() */
int exp_reading = FALSE;	/* whether we can longjmp or not */

extern FILE *debugfile;
extern FILE *logfile;
int logfile_all = FALSE;
int loguser = FALSE;

void debuglog();
int getptymaster();
int getptyslave();
int exp_stringmatch();

#define sysreturn(x)	return(errno = x, -1)

void init_pty();

/* returns fd of master side of pty */
int
exp_spawnv(file,argv)
char *file;
char **argv;
{
	int ttyfd;

	static int first_time = TRUE;

	if (first_time) {
		first_time = FALSE;
		init_pty();
	}

	if (!file || !argv) sysreturn(EINVAL);
	if (!argv[0] || strcmp(file,argv[0])) {
		debuglog("expect: warning: file (%s) != argv[0] (%s)\n",
			file,
			argv[0]?argv[0]:"");
	}

	if (exp_autoallocpty) {
		if (0 > (exp_pty[0] = getptymaster())) sysreturn(ENODEV);
	}
	fcntl(exp_pty[0],F_SETFD,1);	/* close on exec */
	if ((exp_pid = fork()) == -1) return(-1);
	if (exp_pid) {
		/* parent */
		if (!exp_autoallocpty) close(exp_pty[1]);
		return(exp_pty[0]);
	}

	/* child process - do not return from here!  all errors must exit() */

#ifdef POSIX
	setsid();
#else
#ifdef SYSV3
	setpgrp();
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	/* if (exp_disconnected) */
	setpgrp(0,0);/* make a new pgrp leader */
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* SYSV3 */
#endif /* POSIX */
	if (exp_autoallocpty) {
	    close(0);
	    close(1);
	    /* leave 2 around awhile for stderr-related stuff */

	    /* since we closed fd 0, open of pty slave must return fd 0 */
	    if (0 > (exp_pty[1] = getptyslave(exp_stty_init))) {
		fprintf(stderr,"open(slave pty): %s\n",sys_errlist[errno]);
		exit(-1);
	    }
	    /* sanity check */
	    if (exp_pty[1] != 0) {
		fprintf(stderr,"getptyslave: slave = %d but expected 0\n",
								exp_pty[1]);
		exit(-1);
	    }
	} else {
		if (exp_pty[1] != 0) {
			close(0);
			fcntl(exp_pty[1],F_DUPFD,0);
		}
		close(1);
		fcntl(0,F_DUPFD,1);
		close(exp_pty[1]);
	}

	/* by now, fd 0 and 1 point to slave pty, so fix 2 */
	close(2);
	fcntl(0,F_DUPFD,2);	/* duplicate 0 onto 2 */

	/* (possibly multiple) masters are closed automatically due to */
	/* earlier fcntl(,,CLOSE_ON_EXEC); */

        (void) execvp(file,argv);
	/* Unfortunately, by now we've closed fd's to stderr, logfile and
		debugfile.
	   The only reasonable thing to do is to send back the error as
	   part of the program output.  This will be picked up in an
	   expect or interact command.
	*/
	fprintf(stderr,"execvp(%s): %s\n",file,sys_errlist[errno]);
	exit(-1);
	/*NOTREACHED*/
}

/* returns fd of master side of pty */
/*VARARGS*/
int
exp_spawnl(va_alist)
va_dcl
{
	va_list args;
	int i;
	char *arg, **argv;

	va_start(args);
	for (i=0;;i++) {
		arg = va_arg(args,char *);
		if (!arg) break;
	}
	va_end(args);
	if (i == 0) sysreturn(EINVAL);
	if (!(argv = (char **)malloc((i+1)*sizeof(char *)))) sysreturn(ENOMEM);
	va_start(args);
	for (i=0;;i++) {
		argv[i] = va_arg(args,char *);
		if (!argv[i]) break;
	}
	i = exp_spawnv(argv[0],argv+1);
	free((char *)argv);
	return(i);
}

/* remove nulls from s.  Initially, the number of chars in s is c, */
/* not strlen(s).  This count does not include the trailing null. */
/* returns number of nulls removed. */
static int
rm_nulls(s,c)
char *s;
int c;
{
	char *s2 = s;	/* points to place in original string to put */
			/* next non-null character */
	int count = 0;
	int i;

	for (i=0;i<c;i++,s++) {
		if (0 == *s) {
			count++;
			continue;
		}
		if (count) *s2 = *s;
		s2++;
	}
	return(count);
}

/* generate printable versions of random ASCII strings.  This is used by */
/* cmdExpect when -d forces it to print strings it is examining. */
static char *
printify(s)
char *s;
{
	static int destlen = 0;
	static char *dest = 0;
	char *d;		/* ptr into dest */
	unsigned int need;

	/* worst case is every character takes 3 to printify */
	need = strlen(s)*3 + 1;
	if (need > destlen) {
		if (dest) free(dest);
		if (!(dest = malloc(need))) {
			destlen = 0;
			return("malloc failed in printify");
		}
		destlen = need;
	}

	for (d = dest;*s;s++) {
		if (*s == '\r') {
			strcpy(d,"\\r");		d += 2;
		} else if (*s == '\n') {
			strcpy(d,"\\n");		d += 2;
		} else if (*s == '\t') {
			strcpy(d,"\\t");		d += 2;
		} else if ((unsigned)*s < 0x20) { /* unsigned strips parity */
			sprintf(d,"^%c",*s + '@');	d += 3;
		} else if (*s == 0x7f) {
			/* not syntactically correct, but you get the point */
			strcpy(d,"\\7f");		d += 3;
		} else {
			*d = *s;			d += 1;
		}
	}
	*d = '\0';
	return(dest);
}

static int i_read_errno;/* place to save errno, if i_read() == -1, so it
			   doesn't get overwritten before we get to read it */

/*ARGSUSED*/
static void
sigalarm_handler(n)
int n;			/* signal number, unused by us */
{
#ifdef REARM_SIG
	signal(SIGALRM,sigalarm_handler);
#endif

	longjmp(exp_readenv,1);
}

/* interruptable read */
static int
i_read(fd,fp,buffer,length,timeout)
int fd;
FILE *fp;
char *buffer;
int length;
int timeout;
{
	int cc = -2;

	/* since setjmp insists on returning 1 upon longjmp(,0), */
	/* longjmp(,2) instead. */

	alarm(timeout);

	/* restart read if setjmp returns 0 (first time) or 2. */
	/* abort if setjmp returns 1. */
	if (1 != setjmp(exp_readenv)) {
		exp_reading = TRUE;
		if (fd != -1) cc = read(fd,buffer,length);
		else {
			int c;
			c = getc(fp);
			if (c == EOF) {
				if (feof(fp)) cc = 0;
				else cc = -1;
			} else {
				buffer[0] = c;
				cc = 1;
			}
		}
#if 0
		/* can't get fread to return early! */
		else {
			if (!(cc = fread(buffer,1,length,fp))) {
				if (ferror(fp)) cc = -1;
			}
		}
#endif
		i_read_errno = errno;	/* errno can be overwritten by the */
					/* time we return */
	}
	exp_reading = FALSE;

	alarm(0);
	return(cc);
}

static unsigned int bufsiz = 2*EXP_MATCH_MAX;

/* I tried really hard to make the following two functions share the code */
/* that makes the ecase array, but I kept running into a brick wall when */
/* passing var args into the funcs and then again into a make_cases func */
/* I would very much appreciate it if someone showed me how to do it right */

/* takes pairs of args, with a final 0 arg */
/* first element of pair is pattern, 2nd is int returned if pattern matches */
/* returns negative value if error (or EOF/timeout) occurs */
/* some negative values can also have an associated errno */

static int
expectv(fd,fp,ecases)
int fd;
FILE *fp;
struct exp_case *ecases;
{
	int cc = 0;	/* number of chars returned in a single read */
	int rc = 0;	/* number of chars in exp_match[] */
	int oldrc;

	unsigned int new_siz;	/* this is just match_max*2 for efficiency */
	char *new_buf;
	extern char *realloc();
	struct exp_case *ec;	/* points to current ecase */

	time_t current_time;		/* current time (when we last looked)*/
	time_t end_time;		/* future time at which to give up */

	static int first_time = TRUE;

	if (first_time) {
		first_time = FALSE;
		if (!(exp_match = malloc((unsigned)(bufsiz+1))))
			sysreturn(ENOMEM);
	}

	if (!ecases) sysreturn(EINVAL);

	/* get the latest buffer size.  Double the user input for two */
	/* reasons.  1) Need twice the space in case the match */
	/* straddles two bufferfuls, 2) easier to hack the division by */
	/* two when shifting the buffers later on */
	if (bufsiz != (new_siz = 2*exp_match_max)) {
		if (0 == (new_buf = realloc(exp_match,new_siz+1)))
			sysreturn(ENOMEM);
		bufsiz = new_siz;
		exp_match = new_buf;
	}

	exp_match[0] = '\0';
	signal(SIGALRM,sigalarm_handler);

	time(&current_time);
	/* if user sets timeout to 0 (i.e. poll), do the next best */
	/* thing: wait only one second.  Eventually polling should be */
	/* implemented right, but I don't consider it high priority */
	/* at the moment, especially cause select command can do it */
	end_time = current_time + ((exp_timeout == 0)?1:exp_timeout);

	for (;;) {
		/* when buffer fills, copy second half over first and */
		/* continue, so we can do matches over multiple buffers */
		if (rc == bufsiz) {
			memcpy(exp_match,&exp_match[bufsiz/2],bufsiz/2);
			rc = bufsiz/2;
		}

		cc = i_read(fd,fp,&exp_match[rc],bufsiz-rc,
			end_time-current_time);

		if (cc == 0) return(EXP_EOF);		/* normal EOF */
		else if (cc == -1) {			/* abnormal EOF */
			/* ptys produce EIO upon EOF - sigh */
			if (i_read_errno == EIO) {
				/* convert to EOF indication */
				return(EXP_EOF);
			}
			sysreturn(i_read_errno);
		} else if (cc == -2) return(EXP_TIMEOUT);

		oldrc = rc;
		rc += cc;

		if (logfile_all || (loguser && logfile)) {
			fwrite(&exp_match[oldrc],1,cc,logfile);
		}
		if (loguser) fwrite(&exp_match[oldrc],1,cc,stdout);
		if (debugfile) fwrite(&exp_match[oldrc],1,cc,debugfile);

		/* if we wrote to any logs, flush them */
		if (debugfile) fflush(debugfile);
		if (loguser) {
			fflush(stdout);
			if (logfile) fflush(logfile);
		}

		/* remove nulls from input, so we can use C-style strings */
		/* doing it here lets them be sent to the screen, just */
		/*  in case they are involved in formatting operations */
		rc -= rm_nulls(&exp_match[oldrc],cc);
		/* cc should be decremented as well, but since it will not */
		/* be used before being set again, there is no need */
		exp_match[rc] = '\0';

		debuglog("expect: does {%s} match ",printify(exp_match));
		/* pattern supplied */
		for (ec=ecases;ec->pattern;ec++) {
			debuglog("{%s}? ",printify(ec->pattern));
			if (exp_stringmatch(exp_match,ec->pattern)) {
				debuglog("yes\nexp_match is {%s}\n",
							printify(exp_match));
				return(ec->value);
			}
			debuglog("no\n");
		}
	}
}

int
exp_fexpectv(fp,ecases)
FILE *fp;
struct exp_case *ecases;
{
	return(expectv(-1,fp,ecases));
}

int
exp_expectv(fd,ecases)
int fd;
struct exp_case *ecases;
{
	return(expectv(fd,(FILE *)0,ecases));
}

/*VARARGS*/
int
exp_expectl(va_alist)
va_dcl
{
	va_list args;
	int fd;
	struct exp_case *ec, *ecases;
	int i;

	va_start(args);
	fd = va_arg(args,int);
	/* first just count the arg-pairs */
	for (i=0;;i++) {
		if (!va_arg(args,char *)) break;
		va_arg(args,int);	/*COMPUTED, BUT NOT USED*/
	}
	va_end(args);

	if (!(ecases = (struct exp_case *)
				malloc((1+i)*sizeof(struct exp_case))))
		sysreturn(ENOMEM);

	va_start(args);
	va_arg(args,int);		/*COMPUTED, BUT NOT USED*/
	for (ec=ecases;;ec++) {
		if (!(ec->pattern = va_arg(args,char *))) break;
		ec->value = va_arg(args,int);
	}
	va_end(args);
	i = expectv(fd,(FILE *)0,ecases);
	free((char *)ecases);
	return(i);
}

int
exp_fexpectl(va_alist)
va_dcl
{
	va_list args;
	FILE *fp;
	struct exp_case *ec, *ecases;
	int i;

	va_start(args);
	fp = va_arg(args,FILE *);
	/* first just count the arg-pairs */
	for (i=0;;i++) {
		if (!va_arg(args,char *)) break;
		va_arg(args,int);	/*COMPUTED, BUT NOT USED*/
	}
	va_end(args);

	if (!(ecases = (struct exp_case *)
					malloc((1+i)*sizeof(struct exp_case))))
		sysreturn(ENOMEM);

	va_start(args);
	va_arg(args,FILE *);		/*COMPUTED, BUT NOT USED*/
	for (ec=ecases;;ec++) {
		if (!(ec->pattern = va_arg(args,char *))) break;
		ec->value = va_arg(args,int);
	}
	va_end(args);
	i = expectv(-1,fp,ecases);
	free((char *)ecases);
	return(i);
}

/* like popen(3) but works in both directions */
FILE *
exp_popen(program)
char *program;
{
	FILE *fp;
	int ec;

	if (0 > (ec = exp_spawnl("sh","sh","-c",program,(char *)0))) return(0);
	if (!(fp = fdopen(ec,"r+"))) return(0);
	setbuf(fp,(char *)0);
	return(fp);
}

int
exp_disconnect()
{
	int ttyfd;

#ifndef EALREADY
#define EALREADY 37
#endif

	/* presumably, no stderr, so don't bother with error message */
	if (exp_disconnected) sysreturn(EALREADY);
	exp_disconnected = TRUE;

	freopen("/dev/null","r",stdin);
	freopen("/dev/null","w",stdout);
	freopen("/dev/null","w",stderr);

#ifdef POSIX
	setsid();
#else
#ifdef SYSV3
	/* put process in our own pgrp, and lose controlling terminal */
	setpgrp();
	signal(SIGHUP,SIG_IGN);
	if (fork()) exit(0);	/* first child exits (as per Stevens, */
	/* UNIX Network Programming, p. 79-80) */
	/* second child process continues as daemon */
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	setpgrp(0,getpid());	/* put process in our own pgrp */
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		/* zap controlling terminal if we had one */
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* SYSV3 */
#endif /* POSIX */
	return(0);
}
