/* tcx.c, Version 1.0.4, 1/4/1993 by Stewart Forster */

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

extern	int	errno;

int	main(int, char *[]);
int	is_tcx(int);
int	doencode(int, int);

#ifdef ULTRIX
int	is_file_local(char *);
#endif

char	execname[MAXPATHLEN];
char	execpath[MAXPATHLEN];
char	dofile[MAXPATHLEN];
char	tofile[MAXPATHLEN];
char	header[MAXHEADERSIZE];

int
main(int argc, char *argv[])
{
struct	stat	dostat;
char	*s;
int	perms;
int	infd, outfd;
int	len;
struct	flock	lck;
unsigned char c;
#ifdef ULTRIX
int	islocal;
#endif

	/* Check to make sure we have an argument */

	if(argc < 2)
	{
		(void)fprintf(stderr, "Usage: %s filename\n", argv[0]);
		exit(-1);
	}

	if(getwd(dofile) == NULL)
	{
		(void)fprintf(stderr, "Get Working Directory Error: %s\n", dofile);
		exit(-1);
	}

	if(*argv[1] == '/')
		(void)strcpy(dofile, argv[1]);
	else
	{
		(void)strcat(dofile, "/");
		(void)strcat(dofile, argv[1]);
	}
	for(;;)
	{
		if((s = strrchr(dofile, '/')) == NULL)
		{
			(void)fprintf(stderr, "Internal corruption of variables!\n");
			exit(-1);
		}
		s++;
		(void)strcpy(execname, s);
		*s = '\0';

		if(chdir(dofile) < 0) { perror(dofile); exit(-1); }

		if(getwd(dofile) == NULL)
		{
			(void)fprintf(stderr, "Get Working Directory Error: %s\n", dofile);
			exit(-1);
		}

		if(lstat(execname, &dostat) < 0) { perror(execname); exit(-1); }

		if((dostat.st_mode & S_IFMT) == S_IFLNK)
		{
			if((len = readlink(execname, execpath, MAXPATHLEN)) < 0)
			{
				perror(execname);	
				exit(-1);
			}
			execpath[len] = '\0';
			if(execpath[0] == '/')
				(void)strcpy(dofile, execpath);
			else
			{
				(void)strcat(dofile, "/");
				(void)strcat(dofile, execpath);
			}
			continue;
		}

		/* If we get here, dofile is no longer pointing at a symlink */

		(void)strcat(dofile, "/");
		(void)strcat(dofile, execname);
		break;
	}

	/* dofile from now on is the FULL pathname to the executable we're tcx-ing */

	/* Just do a normal stat on dofile */

	if(stat(dofile, &dostat) < 0) { perror(dofile); exit(-1); }

	/* Make sure it's a regular file. If not, quit! */

	if(!(dostat.st_mode & S_IFREG))
	{
		(void)fprintf(stderr, "Error: %s is not a regular file\n", dofile);
		exit(-1);
	}

	/* Check permissions on file, must not be setuid or setgid */
	/* Then check to see if it's an executable */

	if(dostat.st_mode & (S_ISUID | S_ISGID))
	{
		(void)fprintf(stderr, "Error: Cannot compress setuid or setgid programs.\n");
		exit(-1);
	}

	if(! (dostat.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)))
	{
		(void)fprintf(stderr, "File does have any execute bits set. Aborting.\n");
		exit(-1);
	}

	if(dostat.st_nlink > 1)
	{
		(void)fprintf(stderr, "File has multiple hard links to it!  These will be destroyed\n");
		(void)fprintf(stderr, "by this operation.  Please turn the hard links into symlinks.\n");
		exit(-1);
	}

#ifdef ULTRIX
	/* Test to see if file we are compressing is local or not */

	if((islocal = is_file_local(dofile)) < 0)
	{
		perror("statfs");
		exit(-1);
	}
#endif

	/* Now open file we are compressing for reading. Quit if can't. */

	if((infd = open(dofile, O_RDONLY)) < 0)
	{
		perror(dofile);
		exit(-1);
	}

	/* Check to make sure file is not already tcx'ed */

	if(is_tcx(infd))
	{
		(void)fprintf(stderr, "%s is already in tcx format!\n", dofile);
		exit(0);
	}

	if(lseek(infd, 0, SEEK_SET) < 0)
	{
		perror("lseek");
		exit(-1);
	}

	/* Open generation file, and try to mimic permissions */
	/* If cannot, warn user and quit */

	(void)strcpy(tofile, dofile);
	s = strrchr(tofile, '/');
	*s = '\0';
	(void)strcat(tofile, "/.tcx.");
	s = strrchr(dofile, '/');
	s++;
	(void)strcat(tofile, s);

	lck.l_type = F_WRLCK; lck.l_whence = 0; lck.l_start = 0; lck.l_len = 0;

	perms = (dostat.st_mode & 0777);
	if(perms & S_IXUSR) perms |= S_IRUSR;
	if(perms & S_IXGRP) perms |= S_IRGRP;
	if(perms & S_IXOTH) perms |= S_IROTH;
	perms |= S_IWUSR;

	/* Attempt to create scratch file */
	/* Ultrix barfs on F_SETLK if file is on an NFS mount. */

	if((outfd = open(tofile, O_EXCL | O_CREAT | O_WRONLY, perms)) < 0)
	{
#ifdef ULTRIX
		if(islocal == 0 || errno != EEXIST)
#else
		if(errno != EEXIST)
#endif
		{
			perror(tofile);
			exit(-1);
		}

		/* Attempt to open and lock file that's there.  If we can't */
		/* lock the file, someone else must be packing it, so quit. */
		/* If we can, it must be bogus and left lying around after a*/
		/* crash, interrupt or something. Delete file and try again.*/

		if((outfd = open(tofile, O_WRONLY)) < 0)
		{
			perror(tofile);
			exit(-1);
		}

		if(fcntl(outfd, F_SETLK, &lck) < 0)
			exit(-1);

		(void)unlink(tofile);	/* Unlink. Don't care if fails yet */
		(void)close(outfd);
		if((outfd = open(tofile, O_EXCL | O_CREAT | O_WRONLY, perms)) < 0)
		{
			perror(tofile);
			exit(-1);
		}
	}

	/* Attempt to lock tofile.  If can't then assume someone else */
	/* is in the process of packing this file, so quit. */
	/* Only try to lock on ULTRIX if file is local.  If not, risk */
	/* the race condition, we have no choice!. */

#ifdef ULTRIX
	if(islocal == 1)
#endif
		if(fcntl(outfd, F_SETLK, &lck) < 0)
			exit(-1);

	/* Do a chmod (Just to be sure - in case of user umask affecting open) */

	if(chmod(tofile, perms) < 0)
	{
		(void)fprintf(stderr, "Cannot set proper permissions on scratch file %s\n", tofile);
		(void)close(infd);
		(void)close(outfd);
		if(unlink(tofile) < 0)
			(void)fprintf(stderr, "Warning: Unable to delete scratch file\n");
		exit(-1);
	}

	if(chown(tofile, dostat.st_uid, dostat.st_gid) < 0)
	{
		(void)fprintf(stderr, "Cannot set proper ownership on scratch file\n");
		(void)close(infd);
		(void)close(outfd);
		if(unlink(tofile) < 0)
			(void)fprintf(stderr, "Warning: Unable to delete scratch file\n");
		exit(-1);
	}

	/* Spit out header and start encoding executable */

	(void)sprintf(header, "#!%s\n", PATHUNTCX);
	if(write(outfd, header, strlen(header)) < 0) { (void)perror("write"); exit(-1); }

	c = 0;	if((write(outfd, &c, 1)) < 0) { (void)perror("write"); exit(-1); }
	c = 76;	if((write(outfd, &c, 1)) < 0) { (void)perror("write"); exit(-1); }
	c = 193; if((write(outfd, &c, 1)) < 0) { (void)perror("write"); exit(-1); }
	c = 13;	if((write(outfd, &c, 1)) < 0) { (void)perror("write"); exit(-1); }
	c = 138; if((write(outfd, &c, 1)) < 0) { (void)perror("write"); exit(-1); }

	if(doencode(infd, outfd) != 0)
	{
		(void)fprintf(stderr, "Compression failed\n");
		if(unlink(tofile) < 0)
			(void)fprintf(stderr, "Warning: Unable to delete scratch file\n");
		exit(-1);
	}

	(void)close(infd);

	if((infd = open(dofile, O_WRONLY)) <= 0)
	{
		perror(dofile);
		if(unlink(tofile) < 0)
			(void)fprintf(stderr, "Warning: Unable to delete scratch file\n");
		exit(-1);
	}

#ifdef ULTRIX
	if(islocal == 1)
#endif
		if(fcntl(infd, F_SETLK, &lck) < 0)
		{
			if(unlink(tofile) < 0)
				(void)fprintf(stderr, "Warning: Unable to delete scratch file\n");
			exit(-1);
		}

	/* Rename() compressed version to original */

	if(rename(tofile, dofile) < 0)
	{
		perror(dofile);
		if(unlink(tofile) < 0)
			(void)fprintf(stderr, "Warning: Unable to delete scratch file\n");
		exit(-1);
	}

	/* Close files and hence locks */

	(void)close(infd);
	(void)close(outfd);

	/* All done! Bye, bye. */

	return(0);
}


int
is_tcx(int fd)
{
int     i;
unsigned char   c;

        for(i = 0; i < MAXHEADERSIZE; i++)
                if(read(fd, &c, 1) < 1 || c == 0)
                        break;
        if((i >= MAXHEADERSIZE) || read(fd, &c, 1) < 1  || c != 76 || read(fd, &c, 1) < 1  || c != 193
            || read(fd, &c, 1) < 1  || c != 13 || read(fd, &c, 1) < 1  || c != 138 )
                return 0;
        return 1;
} /* is_tcx */


int
doencode(int infd, int outfd)
{
int	pid;
#if defined(AIX) || defined(IRIX)
int	status;
#else
union wait status;
#endif

	pid = fork();
	if(pid < 0) return -1;
	if(pid == 0)
	{
		if(dup2(infd, 0) < 0)	exit(-1);	/* Attach infd to stdin */
		(void)close(infd);
		if(dup2(outfd, 1) < 0)	exit(-1);	/* Attach outfd to stdout */
		(void)close(outfd);
#ifdef PACKEROPTS
		(void)execl(PATHPACKER, PATHPACKER, PACKEROPTS, (char *)0);
#else
		(void)execl(PATHPACKER, PATHPACKER, (char *)0);
#endif
		exit(-1);
	}
	else
		pid = wait(&status);
	return WEXITSTATUS(status);
} /* doencode */


#ifdef ULTRIX
int
is_file_local(char *path)
{
struct  fs_data fsbuf;

        if(statfs(path, &fsbuf) < 1)     /* Returns 0 on "NOT MOUNTED" */
                return -1;

        /* NFS Version 2 returns -1 or 0 for both gfree, and gtot */
        /* to a client, so return false on this condition. */

        if((fsbuf.fd_req.gfree <= 0) && (fsbuf.fd_req.gtot <= 0))
                return 0;
	return 1;
} /* is_file_local */
#endif
