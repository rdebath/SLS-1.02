/*
**	uufuncs.c
**
**	$Id: uufuncs.c, v 2.1 93/04/01 17:00:00 kris Rel $
**
**	functions needed by uugetty
**
*/

#define UUFUNCS
#define UUGETTY

#include "main.h"
#include "uufuncs.h"


/* forward declarations
*/

sig_t cshangup();


/*
** waitlocks
**
** if there are UUCP style lockfiles around, sleep until
** they disappear
*/

void waitlocks()
{
	debug2(D_RUN, "checking for lockfiles...\n"); 

	if(checklock(lock)) {
		while(checklock(lock)) (void) sleep(30);
		exit(0);
	}
	if((altlock) && checklock(altlock)) {
		while(checklock(altlock)) (void) sleep(30);
		exit(0);
	}
} 


/*
** lockline
**
** lock the main (lock) and alternate (altlock) lines
** set to remove locks on a signal
*/

void lockline()
{
	debug2(D_RUN, "locking the line\n");

	if(makelock(lock) == FAIL) exit(0);
	if((altlock) && (makelock(altlock) == FAIL)) exit(0);

	(void) signal(SIGHUP, rmlocks);
	(void) signal(SIGINT, rmlocks);
	(void) signal(SIGQUIT, rmlocks);
	(void) signal(SIGTERM, rmlocks);
}


/*
** watchlocks
**
** fork a child and monitor the lockfiles
** to stop the child, the parent signals with a hangup
** if lockfiles are detected, the child signals with a hangup
**
** if waitfor is requested, this is taken care of already
*/

void watchlocks()
{
	int ppid;

	if(! (waitfor)) {
		ppid = getpid();
		if(! (chpid = fork())) {

/* this is the child
*/
			signal(SIGHUP, cshangup);

			for(;;) {
				if ((kill(ppid, 0) == FAIL) && errno == ESRCH) {
					debug2(D_RUN, "parent exited, child exiting\n");
					exit(0);
				}
				if(checklock(lock)) break;
				if((altlock) && checklock(altlock)) break;
				sleep(30);
			}

			debug2(D_RUN, "found lockfiles, signaling parent... \n");
			kill(ppid, SIGHUP);
			exit(0);
		}

/* this is the parent
*/

		if(chpid == -1) {
			logerr("couldn't fork, exiting");
			exit(FAIL);
		}
	}
}


/* 
** cshangup
**
** child signal handler for hangup
*/

sig_t cshangup()
{
	debug2(D_RUN, "Child caught HANGUP signal\n");
	exit(0);
}



/*
** makelock
**
** attempt to make a lockfile
** Returns FAIL if lock could not be made (line in use).
*/

int
makelock(name)
char *name;
{
	int fd, pid;
	char *temp, buf[MAXLINE+1];
#ifdef	ASCIIPID
	char apid[16];
#endif	/* ASCIIPID */
	int getpid();
	char *mktemp();

	debug3(D_LOCK, "makelock(%s) called\n", name);

	/* first make a temp file
	 */
	(void) sprintf(buf, LOCK, "TM.XXXXXX");
	if ((fd = creat((temp=mktemp(buf)), 0444)) == FAIL) {
		(void) sprintf(MsgBuf, "cannot create tempfile (%s)", temp);
		logerr(MsgBuf);
		return(FAIL);
	}
	debug3(D_LOCK, "temp = (%s)\n", temp);

	/* put my pid in it
	 */
#ifdef	ASCIIPID
	(void) sprintf(apid, "%09d", getpid());
	(void) write(fd, apid, strlen(apid));
#else
	pid = getpid();
	(void) write(fd, (char *)&pid, sizeof(pid));
#endif	/* ASCIIPID */
	(void) close(fd);

	/* link it to the lock file
	 */
	while (link(temp, name) == FAIL) {
		debug3(D_LOCK, "link(temp,name) failed, errno=%d\n", errno);
		if (errno == EEXIST) {		/* lock file already there */
			if ((pid = readlock(name)) == FAIL)
				continue;
			if ((kill(pid, 0) == FAIL) && errno == ESRCH) {
				/* pid that created lockfile is gone */
				(void) unlink(name);
				continue;
			}
		}
		debug2(D_LOCK, "lock NOT made\n");
		(void) unlink(temp);
		return(FAIL);
	}
	debug2(D_LOCK, "lock made\n");
	(void) unlink(temp);
	return(SUCCESS);
}

/*
** checklock
**
** test for presense of valid lock file
** Returns TRUE if lockfile found, FALSE if not.
*/

boolean
checklock(name)
char *name;
{
	int pid;
	struct stat st;

	debug3(D_LOCK, "checklock(%s) called\n", name);

	if ((stat(name, &st) == FAIL) && errno == ENOENT) {
		debug2(D_LOCK, "stat failed, no file\n");
		return(FALSE);
	}

	if ((pid = readlock(name)) == FAIL) {
		debug2(D_LOCK, "couldn't read lockfile\n");
		return(FALSE);
	}

	if (pid == getpid()) {
		debug2(D_LOCK, 
		  "lockfile belongs to me... damn race conditions\n");
		return(FALSE);
	}

	if ((kill(pid, 0) == FAIL) && errno == ESRCH) {
		debug2(D_LOCK, "no active process has lock, will remove\n");
		(void) unlink(name);
		return(FALSE);
	}

	debug2(D_LOCK, "active process has lock, return(TRUE)\n");
	return(TRUE);
}

/*
** readlock
**
** read contents of lockfile
** Returns pid read or FAIL on error.
*/

int
readlock(name)
char *name;
{
	int fd, pid, n;
#ifdef	ASCIIPID
	char apid[16];
#endif	/* ASCIIPID */

	if ((fd = open(name, O_RDONLY)) == FAIL)
		return(FAIL);

#ifdef	ASCIIPID
	(void) read(fd, apid, sizeof(apid));
	n = sscanf(apid, "%d", &pid);
#else
	(void) read(fd, (char *)&pid, sizeof(pid));
#endif	/* ASCIIPID */

#ifdef  BOTHPID
	if (n != 1){
		(void) close(fd);
		fd = open(name, O_RDONLY);
		(void) read(fd, (char *)&pid, sizeof(pid));
		}
#endif

	(void) close(fd);
	debug3(D_LOCK, "read %d from the lockfile\n", pid);
	return(pid);
}

/*
** rmlocks
**
** remove lockfile(s)
*/

sig_t
rmlocks()
{
	if (altlock != (char *) NULL)
		(void) unlink(altlock);

	(void) unlink(lock);
}

