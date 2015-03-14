/* untcx.c, Version 1.0.5, 5/4/1993 by Stewart Forster */

/************************************************************************/
/*   Copyright (C) 1993 Stewart Forster					*/
/*  This program is free software; you can redistribute it and/or modify*/
/*  it under the terms of the GNU General Public License as published by*/
/*  the Free Software Foundation; either version 2, or (at your option) */
/*  any later version.							*/
/*									*/
/*  This program is distributed in the hope that it will be useful,	*/
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of	*/
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	*/
/*  GNU General Public License for more details.			*/
/*									*/
/*  You should have received a copy of the GNU General Public License	*/
/*  along with this program; if not, write to the Free Software		*/
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.		*/
/************************************************************************/

#include	"config.h"

typedef struct path
{
	char	path[MAXPATHLEN]; /* Pathname to file */
#ifdef UNPACK_IN_PLACE
	int	pid;		/* Process id compressing file */
	int	local;		/* Is file local? */
#endif
	int	atime;		/* Time last known to be busy */
	int	ctime;		/* Inode modification time */
	struct	path	*next;	/* Next structure */
} path;

#if defined(SUNOS)
#define	MAXOPENFILES	4096
#define	PIHASH(a, b)	(((a) + (b)) % MAXOPENFILES)

typedef struct pstat
{
	int	dev;
	int	ino;
	int	cnt;
	struct	pstat	*next;
} pstat;

pstat	parr[MAXOPENFILES];
pstat	*pihash[MAXOPENFILES];
void	update_pstat_info();
#endif

extern	int	errno;
path	*worklist = NULL, *freelist = NULL;


char	logpath[MAXPATHLEN], logtmppath[MAXPATHLEN], lockpath[MAXPATHLEN];

char	cwd[MAXPATHLEN];	/* Save of current working directory */
char	realdir[MAXPATHLEN];	/* Real directory packed executable lives in */
char	execname[MAXPATHLEN];	/* Name of packed/target executable */
char	execpath[MAXPATHLEN];	/* Full path name of packed executable */
char	untcxtmp[MAXPATHLEN];	/* Temporary unpack pathname */
char	tcxtarg[MAXPATHLEN];	/* Target path name of unpacked executable */
char	linkpath[MAXPATHLEN];	/* Pathname of symlink to executable */

void	untcx_and_exec_local(char *, char *, char *[]);
void	untcx_and_exec_nfs(char *, char *, char *, char *[]);
int	try_to_exec(char *, char *, char *[]);
void	check_tcxd_mode();
#ifdef UNPACK_IN_PLACE
SIGTYPE	tcxd_reaper();
#endif
void	just_untcx(char *, char *);
int	scan_logtail(int);
int	is_dir_local(char *);
int	is_tcx(int);
int	dodecode(int, int);
char	*newargs[1024];
#if defined(ULTRIX)
int	usleep(int);
int	usleep_sig();
#endif

int
main(int argc, char *argv[])
{
char	*c;			/* String manipulation pointers */
struct	stat	dostat;	/* Stat buffer */
int	i, isinterp, len;
#ifdef UNPACK_IN_PLACE
int	local;
#endif

	if(argc < 2)
	{
		(void)fprintf(stderr, "Usage: %s filename\n", argv[0]);
		exit(-1);
	}

	/* Re-exec if not root and pass -x along on the command line. */
	/* If it's there when we restart and we're still not root, then */
	/* avoid an exec loop by quitting with a warning. */

	if(geteuid() != 0)
	{
#ifndef LINUX
		if(strcmp(argv[1], "-x") == 0)
#endif
		{
			(void)fprintf(stderr, "Error: untcx is not installed setuid to root!\n");
			(void)fprintf(stderr, "Contact your systems administrator\n");
			exit(-1);
		}

		/* Push -x into argv[1] and shift rest of args along */

		newargs[0] = argv[0];
		newargs[1] = "-x";
		for(i = 1; i < argc; i++) newargs[i+1] = argv[i];
		newargs[i + 1] = (char *)NULL;
		if(execv(PATHUNTCX, newargs) < 0)
			perror("exec");

		/* exec failed! Warn user and quit */

		(void)fprintf(stderr, "Unable to invoke interpreter as root\n");
		exit(-1);
	}

	if(strcmp(argv[1], "-x") == 0)
	{
		for(i = 1; i < (argc - 1); i++) argv[i] = argv[i+1];
		argv[i] = (char *)NULL;
		--argc;
		isinterp = 1;
	}
	else
	if(strcmp(argv[1], "-u") == 0)
	{
		for(i = 1; i < (argc - 1); i++) argv[i] = argv[i+1];
		argv[i] = (char *)NULL;
		--argc;
		isinterp = 0;
	}
	else
#if defined(LINUX)
		isinterp = 1;
#else
	if(getuid() == 0)
		isinterp = 1;
	else
		isinterp = 0;
#endif

	/* Copy argv[1] to argv[0] for obsolete stuff (and IRIX) */

	argv[0] = argv[1];

	/* Setup to catch all undesirable signals */

	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGTSTP, SIG_IGN);
	(void)signal(SIGALRM, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);
	(void)signal(SIGTERM, SIG_IGN);

	/* Set global paths */

	(void)sprintf(logpath, "%s/log", ENFSDIR);
	(void)sprintf(logtmppath, "%s/logtmp", ENFSDIR);
	(void)sprintf(lockpath, "%s/.lock", ENFSDIR);

	/* Check and start tcxd as required */

	check_tcxd_mode();

	/* Go to the uid of invoking user.  Resolve through symbolic links */
	/* as to what the real pathname of the executable is. */

#ifdef AIX
	if(seteuid(getuid()) < 0) { perror("seteuid"); exit(-1); }
#else
	if(setreuid(geteuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif

	/* Grab argv[0] and resolve to full path name via getwd() */

	if(getwd(cwd) == NULL)
	{
		(void)fprintf(stderr, "Get Working Directory Error: %s\n", cwd);
		exit(-1);
	}

	if(*argv[0] == '/')
		(void)strcpy(realdir, argv[0]);
	else
		(void)sprintf(realdir, "%s/%s", cwd, argv[0]);
	for(;;)
	{
		if((c = strrchr(realdir, '/')) == NULL)
		{	/* Should never happen, BUT... */
			(void)fprintf(stderr, "Help! Internal corruption of variables!\n");
			exit(-1);
		}
		c++;
		(void)strcpy(execname, c);
		*c = '\0';

		if(chdir(realdir) < 0)	/* Oops. Failed. Report and quit. */
		{
			perror(realdir);
			exit(-1);
		}

		if(getwd(realdir) == NULL)
		{
			(void)fprintf(stderr, "Get Working Directory Error: %s\n", cwd);
			exit(-1);
		}

		/* Do lstat executable from here. If not a link, go on, else */
		/* read the value of the link, and append it to realdir if */
		/* it doesn't begin with a /, other just copy it to realdir */
		/* and cycle through the loop again. */

		if(lstat(execname, &dostat) < 0)
		{
			perror(execname);
			exit(-1);
		}

		if((dostat.st_mode & S_IFMT) == S_IFLNK)
		{
			if((len = readlink(execname, execpath, MAXPATHLEN)) < 0)
			{
				perror(execname);
				exit(-1);
			}
			execpath[len] = '\0';
			if(execpath[0] == '/')
				(void)strcpy(realdir, execpath);
			else
			{
				(void)strcat(realdir, "/");
				(void)strcat(realdir, execpath);
			}
			continue;
		}

		if(!(dostat.st_mode & S_IFREG))
		{
			(void)fprintf(stderr, "Error: Not an executable file - %s/%s\n", realdir, execname);
			exit(-1);
		}

		/* Now do a statfs on realdir.  If it's on an NFS mounted filesystem we */
		/* will need to unpack to ENFSDIR, otherwise we can unpack it in place, */

#ifdef UNPACK_IN_PLACE
		if((local = is_dir_local(realdir)) < 0)
			exit(-1);
		if(dostat.st_nlink > 1)	/* Unpack to ENFSDIR if file is linked */
			local = 0;
#endif

		break;
	}

	/* Return to the original invocation directory */

	if(chdir(cwd) < 0)
	{
		perror(cwd);
		exit(-1);
	}

#ifdef AIX
	if(seteuid(0) < 0) { perror("seteuid"); exit(-1); }
#else
	if(setreuid(geteuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif

	/* Now check if last components of argv[0] and argv[1] are equal */
	/* If not, we are invoked just to decompress something, not to */
	/* execute it.  Just attempt an uncompress as the user */

	if(isinterp == 0)
	{
#if defined(IRIX) || defined(AIX)
		if(setuid(getuid()) < 0) { perror("setuid"); exit(-1); }
#else
		if(setreuid(getuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif
		(void)sprintf(tcxtarg, "%s/%s", realdir, execname);
		(void)sprintf(untcxtmp, "%s/.untcx.%s", realdir, execname);
		just_untcx(tcxtarg, untcxtmp);
		exit(0);
	}

#ifdef UNPACK_IN_PLACE
	/* Try to do a local execute. If it returns, it means failure. */

	if(local)
	{
		(void)sprintf(tcxtarg, "%s/%s", realdir, execname);
		(void)sprintf(untcxtmp, "%s/.untcx.%s", realdir, execname);
		untcx_and_exec_local(tcxtarg, untcxtmp, &(argv[1]));
	}
#endif

	/* We get this far if file is not local, or the local execution */
	/* failed for some reason like shortage of disk space. Attempt */
	/* to unpack program to /tmp and go from there. */

	for(c = realdir; *c ; c++)
		if(*c == '/')
			*c = '=';
	(void)sprintf(tcxtarg, "%s/%s", ENFSDIR, realdir);
	if(mkdir(tcxtarg, 0777) < 0)
		if(errno != EEXIST)
		{
			perror(tcxtarg);
			exit(-1);
		}
	(void)chmod(tcxtarg, 0777);
	(void)strcat(tcxtarg,"/");
	(void)strcat(tcxtarg, execname);
	(void)sprintf(untcxtmp, "%s/%s/.untcx.%s", ENFSDIR, realdir, execname);

	untcx_and_exec_nfs(argv[0], untcxtmp, tcxtarg, &(argv[1]));

	/* :-(  We only get this far if everything has failed. Exit with failure */

	return(-1);
} /* main */


void
check_tcxd_mode()
{
char	*s, spid[32];
int	infd, logfd, lkfd, lasttime = 0, timer;
FILE	*logfp;
path	*curr, *prev;
struct	stat	sb;	/* Stat buffer */
struct	flock	lck;	/* File lock structure */
int	lastoff;

	/* Check if we're root. If not, go home */

	if(geteuid() != 0)
		return;

	/* Try to create emergency/NFS execute directory and set it's permissions */
	/* It's not important yet if we can't at this stage. */

	(void)mkdir(ENFSDIR, 01777);
	(void)chmod(ENFSDIR, 01777);

	/* Attempt to create ENFSDIR/.lock  If can't, just return. */

	if((lkfd = open(lockpath, O_CREAT | O_RDONLY, 0600)) < 0)
		return;

	/* Attempt to read lock ENFSDIR/.lock If can't, we assume the tcxd */
	/* is already running. Return. */

	lck.l_type = F_RDLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;
	if(fcntl(lkfd, F_SETLK, &lck) < 0)
	{
		(void)close(lkfd);
		return;
	}

	/* Close lkfd and hence the lock on it, fork() and return */

	(void)close(lkfd);
	if(fork() != 0)	/* On parent or fork error, just return */
		return;

	/* No errors to user at this point */

	(void)fclose(stderr); (void)close(2);
	(void)fclose(stdout); (void)close(1);
	(void)open("/dev/null", O_WRONLY);
	(void)open("/dev/null", O_WRONLY);	/* Don't really care */

	/* Make ourselves nice and rooted */

#ifndef AIX
#if defined(IRIX)
	if(setuid(geteuid()) < 0) exit(-1);
#else
	if(setreuid(geteuid(), geteuid()) < 0) exit(-1);
#endif
#endif

	/* Check if we're root again. If not, exit */

	if(geteuid() != 0)
		exit(-1);

	/* Change directory to ENFSDIR to prevent hanging on NFS server crashes */

	if(chdir(ENFSDIR) < 0)
		exit(-1);

	/* Lock the server lock file to prevent more than 1 starting up */

	if((lkfd = open(lockpath, O_WRONLY | O_TRUNC)) < 0)
		exit(-1);
	lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;
	if(fcntl(lkfd, F_SETLK, &lck) < 0)
		exit(-1);

	/* Write our process id to the lock file. Don't really care if fails. */

	(void)sprintf(spid, "%d\n", getpid());
	(void)write(lkfd, spid, strlen(spid));

#ifdef UNPACK_IN_PLACE
	/* setup our child process reaper */

	(void)signal(SIGCHLD, tcxd_reaper);
#endif

	/* Read in log file if it's there in case of crashes */

	if((lastoff = scan_logtail(0)) < 0)
		(void)creat(logpath, 0600);

	/* Loop doing tasks at hand */

	for(lastoff = 0;;)
	{
		for(timer = 0; timer < SCANRATE; timer++, (void)sleep(1))
		{
			if(stat(logpath, &sb) < 0)
				continue;

			/* Don't check if not updated, but do on last time */

			if(sb.st_mtime <= lasttime)
				continue;
			lasttime = sb.st_mtime;

			lastoff = scan_logtail(lastoff);
		}

#if defined(SUNOS)
		update_pstat_info();
#endif

		for(prev = NULL, curr = worklist; curr != NULL; prev = curr, curr = curr->next)
		{
			/* Stat file. If not accessed within LOCALTIMOUT secs */
			/* for a local file, or ENFSTIMEOUT secs for an NFS file */
			/* we consider it a candidate for compression. */

			if(stat(curr->path, &sb) < 0)
			{
				if(lstat(curr->path, &sb) >= 0)
				{
					(void)unlink(curr->path);	/* Was symlink. Unlink it! */
					s = strrchr(curr->path, '/');
					*s = '\0';
					(void)rmdir(curr->path);
				}
				(prev == NULL) ? (worklist = curr->next) : (prev->next = curr->next);
				curr->next = freelist; freelist = curr;
				(prev == NULL) ? (curr = worklist) : (curr = prev);
				if(curr == NULL) break;
				continue;
			}

			if(sb.st_atime > curr->atime)
				curr->atime = sb.st_atime;

#ifdef UNPACK_IN_PLACE
			if(curr->local == 1 && (time(NULL) - curr->atime) < LOCALTIMEOUT)
				continue;
			if(curr->local == 0 && (time(NULL) - curr->atime) < ENFSTIMEOUT)
				continue;
#else
			if((time(NULL) - curr->atime) < ENFSTIMEOUT)
				continue;
#endif

#if defined(IRIX) || defined(DEC) || defined(AIX) || defined(LINUX)
			/* Attempt to open file for writing, if it fails with ETXTBSY */
			/* then update access times and continue, otherwise it is a */
			/* candidate for recompression. */

			if((infd = open(curr->path, O_WRONLY)) < 0)
			{
				if(errno != ETXTBSY)
				{
					if(lstat(curr->path, &sb) >= 0)
					{
						(void)unlink(curr->path);	/* Was symlink. Unlink it! */
						s = strrchr(curr->path, '/');
						*s = '\0';
						(void)rmdir(curr->path);
					}
					(prev == NULL) ? (worklist = curr->next) : (prev->next = curr->next);
					curr->next = freelist; freelist = curr;
					(prev == NULL) ? (curr = worklist) : (curr = prev);
					if(curr == NULL) break;
					continue;
				}
				curr->atime = time(NULL);
				continue;
			}
			(void)close(infd);
#endif

#if defined(SUNOS)
		{
		pstat	*pc;
		int	found = 0;

			for(pc = pihash[PIHASH(sb.st_dev, sb.st_ino)]; pc != NULL; pc = pc->next)
				if(pc->dev == sb.st_dev && pc->ino == sb.st_ino)
				{
					if(pc->cnt < 2) break;
					curr->atime = time(NULL);
					found = 1;
					break;
				}
			if(found) continue;
		}
#endif

#ifdef UNPACK_IN_PLACE
			if(curr->local == -1)	/* Compression completed */
			{
				(prev == NULL) ? (worklist = curr->next) : (prev->next = curr->next);
				curr->next = freelist; freelist = curr;
				(prev == NULL) ? (curr = worklist) : (curr = prev);
				if(curr == NULL) break;
				continue;
			}

			if(curr->pid > 0)
				continue;

			if(curr->local == 1)
			{
				/* Check inode modification times. If target is */
				/* newer than what we know, forget it. */

				if(sb.st_ctime > curr->ctime)
				{
					(prev == NULL) ? (worklist = curr->next) : (prev->next = curr->next);
					curr->next = freelist; freelist = curr;
					(prev == NULL) ? (curr = worklist) : (curr = prev);
					if(curr == NULL) break;
					continue;
				}

				/* Fork and compress program */

				curr->pid = fork();
				if(curr->pid != 0) continue;

				/* Here we must be the child */
				/* Close stdio stuff and reopen to /dev/null */

				(void)close(2);
				(void)close(1);
				(void)close(0);
				(void)open("/dev/null", O_RDONLY);
				(void)open("/dev/null", O_WRONLY);
				(void)open("/dev/null", O_WRONLY);
				(void)execl(PATHTCX, "tcx", curr->path, (char *)0);
				exit(-1);
			}
#endif

			/* At this point, file is in ENFSDIR. Delete it */

			(void)unlink(curr->path);
			s = strrchr(curr->path, '/');
			*s = '\0';
			(void)rmdir(curr->path);

			(prev == NULL) ? (worklist = curr->next) : (prev->next = curr->next);
			curr->next = freelist; freelist = curr;
			(prev == NULL) ? (curr = worklist) : (curr = prev);
			if(curr == NULL) break;
		} /* for */

		/* Update log file in case of crashes */

		lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;

		/* Do one final read in case someone has modified during */
		/* the scan sequence. */

		(void)scan_logtail(lastoff);

		if((logfp = fopen(logtmppath, "w")) == NULL)
			continue;
		for(curr = worklist; curr != NULL; curr = curr->next)
			(void)fprintf(logfp, "%s\n", curr->path);
		(void)fclose(logfp);

		/* Try for 20 times to lock file, sleep 5/100ths secs b/w tries */

		for(timer = 0; timer < 20; timer++, PUSLEEP(50000))
		{
			if((logfd = open(logpath, O_WRONLY)) < 0)
				continue;
			if(fcntl(logfd, F_SETLK, &lck) < 0)
			{	(void)close(logfd); continue;	}

			(void)rename(logtmppath, logpath);
			if(stat(logpath, &sb) < 0)
				lastoff = 0;
			else
				lastoff = sb.st_size;
			(void)close(logfd);
			break;
		}
	} /* for */
} /* check_tcxd_mode */


#ifdef UNPACK_IN_PLACE
SIGTYPE
tcxd_reaper()
{
#if defined(AIX)
int	state;
#else
union wait state;
#endif
int	pid;
path	*curr;

	while((pid = wait3(&state, WNOHANG, 0)) > 0)
		for(curr = worklist; curr != NULL; curr = curr->next)
			if(curr->pid == pid)
			{
				curr->pid = -1;
				if(WIFEXITED(state) && WEXITSTATUS(state) == 0)
					curr->local = -1;
				break;
			}
#if defined(IRIX) || defined(AIX)
	(void)signal(SIGCHLD, tcxd_reaper);
#endif
} /* tcxd_reaper */
#endif


#ifdef UNPACK_IN_PLACE
void
untcx_and_exec_local(char *expath, char *untemp, char *argv[])
{
struct	stat	tostat;	/* Stat buffer to check on lengths */
int	owner, group;
int	perms;		/* Saved permissions to restore on target exec */
int	infd, outfd;	/* In and out file descriptors */
struct	flock	lck;	/* File lock structure */

	/* In this function argv[0] and expath would normally be the same */
	/* BUT, if argv[0] is a symlink to expath, we want to invoke it */
	/* like that, and not via expath. */

	/* Stat executable and grab permissions */

	if(stat(expath, &tostat) < 0)
	{
		perror(argv[0]);
		exit(-1);
	}
	perms = (tostat.st_mode & 0777);
	owner = tostat.st_uid;
	group = tostat.st_gid;

	for(;;PUSLEEP(10000))
	{
		if((infd = open(expath, O_RDWR)) < 0)
		{
			if(errno != ETXTBSY)
			{
				(void)fprintf(stderr, "Cannot write to %s\n", expath);
				exit(-1);
			}
			if((infd = open(expath, O_RDONLY)) < 0)
				continue;
			(void)close(infd);
			if(try_to_exec(expath, argv[0], argv) < 0)
				exit(-1);
			continue;
		}

		lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;
		if(fcntl(infd, F_SETLK, &lck) < 0)
		{
			if(fcntl(infd, F_GETLK, &lck) < 0 || lck.l_type != F_RDLCK)
			{ (void)close(infd); continue; }
			(void)close(infd);
			if(try_to_exec(expath, argv[0], argv) < 0)
				exit(-1);
			continue;
		}

		if(! is_tcx(infd))
		{
			(void)close(infd);
			if(try_to_exec(expath, argv[0], argv) < 0)
				exit(-1);
			continue;
		}

		/* File is in packed format. Unpack it */

		if((outfd = open(untemp, O_WRONLY | O_CREAT | O_EXCL)) < 0)
		{
			if(errno != EEXIST)
			{
				perror(untemp);
				exit(-1);
			}
			if((outfd = open(untemp, O_WRONLY | O_TRUNC)) < 0)
			{
				perror(untemp);
				exit(-1);
			}
		}

		/* Start decompressing executable */

		if(dodecode(infd, outfd) != 0)
		{
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		(void)close(outfd);

		/* Now set execute permissions on the beastie */

		if(chmod(untemp, perms) < 0)
		{
			perror(untemp);
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		if(chown(untemp, owner, group) < 0)
		{
			perror(untemp);
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		/* Rename untemporary file to target executable and close target */

		if(rename(untemp, expath) < 0)
		{
			perror(untemp);
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		(void)close(infd);

		/* Everything OK!  Now go exec the executable. */

		if(try_to_exec(expath, argv[0], argv) < 0)
			exit(-1);
	} /* for */
} /* untcx_and_exec_local */
#endif


void
untcx_and_exec_nfs(char *expath, char *untemp, char *targ, char *argv[])
{
struct	stat	tostat;	/* Stat buffer to check on lengths */
int	mtime;
int	owner, group;
int	perms;		/* Saved permissions to restore on target exec */
int	infd, outfd;	/* In and out file descriptors */
struct	flock	lck;	/* File lock structure */
char	*c;
int	len;

	/* Stat executable and grab permissions */

#ifdef AIX
	if(seteuid(getuid()) < 0) { perror("seteuid"); exit(-1); }
#else
	if(setreuid(geteuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif

	if(stat(expath, &tostat) < 0) { perror(argv[0]); exit(-1); }

	perms = (tostat.st_mode & 0777);
	mtime = tostat.st_mtime;
	owner = tostat.st_uid;
	group = tostat.st_gid;

	/* resolve first stage of argv[0] */

	if(getwd(cwd) == NULL) { (void)fprintf(stderr, "Get Working Directory Error: %s\n", cwd); exit(-1); }

	if(*argv[0] == '/') (void)strcpy(realdir, argv[0]); else (void)sprintf(realdir, "%s/%s", cwd, argv[0]);

	if((c = strrchr(realdir, '/')) == NULL) { (void)fprintf(stderr, "Help! Internal corruption of variables!\n"); exit(-1); }

	c++; (void)strcpy(execname, c); *c = '\0';

	if(chdir(realdir) < 0) { perror(realdir); exit(-1); }

	if(getwd(realdir) == NULL) { (void)fprintf(stderr, "Get Working Directory Error: %s\n", cwd); exit(-1); }

	for(c = realdir; *c; c++)
		if(*c == '/')
			*c = '=';
	(void)sprintf(linkpath, "%s/%s", ENFSDIR, realdir);
	if(mkdir(linkpath, 0777) < 0)
	{
		if(errno != EEXIST) { perror(linkpath); exit(-1); }
	}
	else
		(void)chmod(linkpath, 0777);
	(void)strcat(linkpath,"/");
	(void)strcat(linkpath, execname);

	if(chdir(cwd) < 0) { perror(cwd); exit(-1); }
	
#ifdef AIX
	if(seteuid(0) < 0) { perror("seteuid"); exit(-1); }
#else
	if(setreuid(geteuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif

	/* Statistics done! Now try to unpack and run it! */

	for(;;PUSLEEP(10000))
	{
		if(stat(targ, &tostat) >= 0)
		{
			if(mtime > tostat.st_mtime)
			{
				if(strcmp(linkpath, targ) == 0)
					(void)unlink(linkpath);
				(void)unlink(targ);
			}
			else
			{
				if(strcmp(linkpath, targ) == 0)
				{
					if(try_to_exec(targ, linkpath, argv) < 0) exit(-1); else continue;
				}
				else
				if((len = readlink(linkpath, realdir, MAXPATHLEN)) >= 0 && strncmp(realdir, targ, len) == 0)
				{
					if(try_to_exec(targ, linkpath, argv) < 0) exit(-1); else continue;
				}
				else
				{
					(void)unlink(linkpath);

					if(symlink(targ, linkpath) < 0) { perror(linkpath); exit(-1); }

					if(try_to_exec(targ, linkpath, argv) < 0) exit(-1); else continue;
				}
			}
		}
		else
		{
			/* If symlink to file is there, then get rid of it! */

			if((len = readlink(linkpath, realdir, MAXPATHLEN)) >= 0 && strncmp(realdir, targ, len) == 0)
				(void)unlink(linkpath);
		}

#ifdef AIX
		if(seteuid(getuid()) < 0) { perror("seteuid"); exit(-1); }
#else
		if(setreuid(geteuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif
		if((infd = open(expath, O_RDONLY)) < 0)
		{
			perror(expath);
			exit(-1);
		}
#ifdef AIX
		if(seteuid(0) < 0) { perror("seteuid"); exit(-1); }
#else
		if(setreuid(geteuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif

		if(! is_tcx(infd))
		{
			(void)close(infd);
			if(try_to_exec(expath, argv[0], argv) < 0)
				exit(-1);
			continue;
		}

		/* File is in packed format. Unpack it */

		if((outfd = open(untemp, O_EXCL | O_CREAT | O_WRONLY)) < 0)
		{
			if(errno != EEXIST)
			{
				perror(untemp);
				exit(-1);
			}
			if((outfd = open(untemp, O_WRONLY | O_TRUNC)) < 0)
			{
				perror(untemp);
				exit(-1);
			}
		}

		lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;
		if(fcntl(outfd, F_SETLK, &lck) < 0)
		{
			(void)close(infd);
			(void)close(outfd);
			continue;
		}

		/* Start decompressing executable */

		if(dodecode(infd, outfd) != 0)
		{
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		(void)close(infd);

		/* Now set execute permissions on the beastie */

		if(chmod(untemp, perms) < 0)
		{
			perror(untemp);
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		if(chown(untemp, owner, group) < 0)
		{
			perror(untemp);
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		/* Rename untemporary file to target executable and close target */

		if(rename(untemp, targ) < 0)
		{
			perror(untemp);
			if(unlink(untemp) < 0)
				perror(untemp);
			exit(-1);
		}

		(void)close(outfd);

		/* Now create symlink from linkpath to targ if they are not equal */

		if(strcmp(linkpath, targ) != 0 && symlink(targ, linkpath) < 0)
		{
			perror(linkpath);
			exit(-1);
		}

		/* Everything OK!  Now go exec the executable. */

		if(try_to_exec(targ, linkpath, argv) < 0)
			exit(-1);
	} /* for */
} /* untcx_and_exec_nfs */


void
just_untcx(char *expath, char *untemp)
{
struct	stat	tostat;	/* Stat buffer to check on lengths */
int	owner, group;
int	perms;		/* Saved permissions to restore on target exec */
int	infd, outfd;	/* In and out file descriptors */
struct	flock	lck;	/* File lock structure */

	/* Stat executable and grab permissions */

	if(stat(expath, &tostat) < 0)
	{
		perror(expath);
		exit(-1);
	}
	perms = (tostat.st_mode & 0777);
	owner = tostat.st_uid;
	group = tostat.st_gid;

	if((infd = open(expath, O_RDWR)) < 0)
	{
		perror(expath);
		exit(-1);
	}

	lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;
	if(fcntl(infd, F_SETLK, &lck) < 0)
	{
		perror(expath);
		exit(-1);
	}

	if(! is_tcx(infd))
	{
		(void)fprintf(stderr, "File does not appear to be in tcx format. Aborting\n");
		exit(-1);
	}

	/* File is in packed format. Unpack it */

	if((outfd = open(untemp, O_WRONLY | O_CREAT | O_EXCL)) < 0)
	{
		if(errno != EEXIST)
		{
			perror(untemp);
			exit(-1);
		}
		if((outfd = open(untemp, O_WRONLY | O_TRUNC)) < 0)
		{
			perror(untemp);
			exit(-1);
		}
	}

	/* Start decompressing executable */

	if(dodecode(infd, outfd) != 0)
	{
		if(unlink(untemp) < 0)
			perror(untemp);
		exit(-1);
	}

	(void)close(outfd);

	/* Now set execute permissions on the beastie */

	if(chmod(untemp, perms) < 0)
	{
		perror(untemp);
		if(unlink(untemp) < 0)
			perror(untemp);
		exit(-1);
	}

	if(chown(untemp, owner, group) < 0)
	{
		perror(untemp);
		if(unlink(untemp) < 0)
			perror(untemp);
		exit(-1);
	}

	/* Rename untemporary file to target executable and close target */

	if(rename(untemp, expath) < 0)
	{
		perror(untemp);
		if(unlink(untemp) < 0)
			perror(untemp);
		exit(-1);
	}

	(void)close(infd);
	return;
} /* just_untcx */


int
try_to_exec(char *expath, char *lnpath, char *argv[])
{
int	infd, try;
int	logfd;
FILE	*logfp;
struct	flock	lck;

	if((infd = open(lnpath, O_RDONLY)) < 0)
	{
		if(errno == ENOENT)
		{
			perror(lnpath);
			return -1;
		}
		return 0;
	}

	if(is_tcx(infd))
	{
		(void)close(infd);
		return 0;
	}

	lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;
	for(try = 0; try < 10; try++, PUSLEEP(50000))
	{
		if((logfd = open(logpath, O_WRONLY)) < 0)
			continue;
		if(fcntl(logfd, F_SETLK, &lck) < 0)
		{
			(void)close(logfd);
			continue;
		}

		if((logfp = fopen(logpath, "a+")) == NULL)
		{
			(void)close(logfd);
			continue;
		}

#ifdef UNPACK_IN_PLACE
		(void)fprintf(logfp, "%s\n", expath);
#else
		if(strstr(expath, ENFSDIR) == expath)
			(void)fprintf(logfp, "%s\n", expath);
#endif
		if((strstr(lnpath, ENFSDIR) == lnpath) && (strcmp(lnpath, expath) != 0))
			(void)fprintf(logfp, "%s\n", lnpath);
		(void)fclose(logfp);
		(void)close(logfd);
		break;
	}

#if defined(IRIX) || defined(AIX)
	if(setuid(getuid()) < 0) { perror("setuid"); exit(-1); }
#else
	if(setreuid(getuid(), getuid()) < 0) { perror("setreuid"); exit(-1); }
#endif

	/* Reset signals back to normal to prevent carriage through execv */

	(void)signal(SIGINT, SIG_DFL);
	(void)signal(SIGHUP, SIG_DFL);
	(void)signal(SIGTSTP, SIG_DFL);
	(void)signal(SIGALRM, SIG_DFL);
	(void)signal(SIGQUIT, SIG_DFL);
	(void)signal(SIGTERM, SIG_DFL);

	(void)close(infd);
	if(execv(lnpath, argv) < 0)
		perror(lnpath);
	return -1;
} /* try_to_exec */


#if defined(SUNOS)
void
update_pstat_info()
{
FILE	*fp;
char	line[256], tmp[10];
pstat	*curr;
int	i, pos;
int	maj, min, dev, ino, cnt;

	for(pos = 0 ; pos < MAXOPENFILES; pos++)
		pihash[pos] = (pstat *)NULL;

	if((fp = popen(PSTATI, "r")) == NULL)
		return;
	if(fgets(line, 256, fp) == NULL)
		return;
	for(i = 0; fgets(line, 256, fp) != NULL && i < MAXOPENFILES; i++)
	{
		curr = parr + i;
		strncpy(tmp, line+16, 3);	tmp[3] = '\0';	maj = atoi(tmp);
		strncpy(tmp, line+20, 3);	tmp[3] = '\0';	min = atoi(tmp);
		strncpy(tmp, line+23, 6);	tmp[6] = '\0';	ino = atoi(tmp);
		strncpy(tmp, line+61, 4);	tmp[4] = '\0';	cnt = atoi(tmp);
		dev = maj * 256 + min;
		pos = PIHASH(dev, ino);
		curr->dev = dev;
		curr->ino = ino;
		curr->cnt = cnt;
		curr->next = pihash[pos];
		pihash[pos] = curr;
	} /* while */
	pclose(fp);
} /* update_pstat_info */
#endif


int
scan_logtail(int lastoff)
{
char	newpath[MAXPATHLEN], *s;
int	len;
FILE	*logfp;
path	*curr;
struct	stat	sb;

	if(lastoff < 0)
		lastoff = 0;
	if((logfp = fopen(logpath, "r")) == NULL)
		return -1;
	(void)fseek(logfp, lastoff, SEEK_SET);
	while(fgets(newpath, MAXPATHLEN, logfp) != NULL)
	{
		for(s = newpath; *s; s++) if(*s == '\n') *s = '\0';

		if(lstat(newpath, &sb) < 0)
			continue;

		for(curr = worklist; curr != NULL; curr = curr->next)
			if(strcmp(curr->path, newpath) == 0) break;

		if(curr != NULL)	/* Reset indicators on known file */
		{
			curr->ctime = sb.st_ctime;
#ifdef UNPACK_IN_PLACE
			curr->pid = -1;
			(strstr(newpath, ENFSDIR) == newpath) ? (curr->local = 0) : (curr->local = 1);
#endif
			continue;
		}

		if(freelist != NULL)
		{
			curr = freelist;
			freelist = freelist->next;
		}
		else
			if((curr = (path *)malloc(sizeof(path))) == NULL)
				continue;

		(void)strcpy(curr->path, newpath);
#ifdef UNPACK_IN_PLACE
		curr->pid = -1;
		(strstr(newpath, ENFSDIR) == newpath) ? (curr->local = 0) : (curr->local = 1);
#endif
		curr->atime = sb.st_atime;
		curr->ctime = sb.st_ctime;
		curr->next = worklist;
		worklist = curr;
	} /* while */
	lastoff = ftell(logfp);
	(void)fclose(logfp);
	return lastoff;
} /* scan_logtail */


#ifdef UNPACK_IN_PLACE
int
is_dir_local(char *dir)
{
#if defined(ULTRIX)
struct  fs_data fsbuf;
int	success = 1;     /* Returns 0 on "NOT MOUNTED" */
#else
struct	statfs	fsbuf;	/* Statfs buffer to check local versus NFS executable */
int	success = 0;	
#endif

#if defined(IRIX)
	if(statfs(dir, &fsbuf, (int)(sizeof(struct statfs)), 0) < success)
#else
	if(statfs(dir, &fsbuf) < success)
#endif
		return -1;

	/* NFS Version 2 returns -1 or 0 for both free inodes, and total */
	/* inodes to a client, so return false on this condition. */

#if defined(ULTRIX)
	if((fsbuf.fd_req.gfree <= 0) && (fsbuf.fd_req.gtot <= 0))
#else
	if((fsbuf.f_files <= 0) && (fsbuf.f_ffree <= 0))
#endif
		return 0;
	return 1;
} /* is_dir_local */
#endif


int
is_tcx(int fd)
{
int	i;
unsigned char	c;

	for(i = 0; i < 256; i++)
		if(read(fd, &c, 1) < 1 || c == 0)
			break;
	if((i >= 256) || read(fd, &c, 1) < 1  || c != 76 || read(fd, &c, 1) < 1  || c != 193
	    || read(fd, &c, 1) < 1  || c != 13 || read(fd, &c, 1) < 1  || c != 138 )
		return 0;
	return 1;
} /* is_tcx */


int
dodecode(int infd, int outfd)
{
int	pid;
#if defined(IRIX) || defined(AIX)
int	status;
#else
union wait status;
#endif

	pid = fork();
	if(pid < 0) return -1;
	if(pid == 0)
	{
		if(dup2(infd, 0) < 0) exit(-1);	 /* Attach infd to stdin */
		(void)close(infd);
		if(dup2(outfd, 1) < 0) exit(-1); /* Attach outfd to stdout */
		(void)close(outfd);
#ifdef	UNPACKOPTS
		(void)execl(PATHUNPACK, PATHUNPACK, UNPACKOPTS, (char *)0);
#else
		(void)execl(PATHUNPACK, PATHUNPACK, (char *)0);
#endif
		exit(-1);
	}
	else
		pid = wait(&status);
	return WEXITSTATUS(status);
} /* dodecode */


#ifdef ULTRIX
int
usleep(int usec)
{
struct	itimerval	value, dumb;

	(void)signal(SIGALRM, usleep_sig);

	if(getitimer(ITIMER_REAL, &value) != 0)
	{
		perror("Error: couldn't get timer");
		return(-1);
	}
	value.it_value.tv_sec = value.it_interval.tv_sec = (long) usec / 1000000;
	value.it_value.tv_usec = value.it_interval.tv_usec = (long) usec % 1000000;
        
	if(setitimer(ITIMER_REAL, &value, &dumb) != 0) /* ignore parameter 3 */
	{
		perror("Error: couldn't set timer");
		return(-1);
	}

	(void)sigpause(SIGALRM);

	if(getitimer(ITIMER_REAL, &value) != 0)
	{
		perror("Error: couldn't get timer");
		return(-1);
	}
	usec = (value.it_value.tv_sec - value.it_interval.tv_sec) * 1000000;
	usec += (value.it_value.tv_usec - value.it_interval.tv_usec);
	return usec;
}

int
usleep_sig()
{
} /* usleep_sig */
#endif
