/* 
 *  at.c : Put file into atrun queue
 *  Copyright (C) 1993  Thomas Koenig
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define _POSIX_SOURCE 1

/* System Headers */

#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Local headers */

#include "at.h"
#include <getopt.h>

/* Macros */

#ifndef ATJOB_DIR 
#define ATJOB_DIR "/usr/spool/atjobs/"
#endif

#ifndef LFILE
#define LFILE ATJOB_DIR ".lockfile"
#endif

#ifndef ATJOB_MX
#define ATJOB_MX 255
#endif

#define ALARMC 10 /* Number of seconds to wait for timeout */

#define SIZE 255

/* Structures and unions */

/* File scope variables */

static char rcsid[] = "$Id: at.c,v 1.8 1993/01/17 23:09:31 kernel Exp $";

/* External variables */

extern char **environ;
int fcreated;
char *namep;
char atfile[] = ATJOB_DIR "12345678901234";

/* Function declarations */

static void sigc(int signo);
static void alarmc(int signo);
static char *cwdname(void);
static void writefile(time_t runtimer, char queue);
static void list_jobs(void);

/* Signal catching functions */

static void sigc(int signo)
{
/* If the user presses ^C, remove the spool file and exit 
 */
	if (fcreated)
		unlink(atfile);

	exit(EXIT_FAILURE);
}

static void alarmc(int signo)
{
/* Time out after some seconds
 */
	panic("File locking timed out");
}

/* Local functions */

static char *cwdname(void)
{
/* Read in the current directory; the name will be overwritten on
 * subsequent calls.
 */
	static char *ptr = NULL;
	static size_t size = SIZE;

	if (ptr == NULL)
		ptr = (char *) malloc(size);

	while (1)
	{
		if (ptr == NULL)
			panic("Out of memory");

		if (getcwd(ptr,size-1) != NULL)
			return ptr;

		if (errno != ERANGE)
			perr("Cannot get directory");

		free (ptr);
		size += SIZE;
		ptr = malloc(size);
	}
}

static void writefile(time_t runtimer, char queue)
{
/* This does most of the work if at or batch are invoked for writing a job.
 */
	int i;
	char *ap, *ppos;
	struct stat statbuf;
	int fdes, lockdes;
	FILE *fp;
	struct sigaction act;
	char **atenv;
	int ch;
	gid_t gid;
	uid_t uid;
	mode_t cmask;
	struct flock lock;


/* Install the signal handler for SIGINT; terminate after removing the spool file
 * if necessary
 */
	act.sa_handler = sigc;
	act.sa_mask = 0;
	act.sa_flags = 0;

	sigaction(SIGINT,&act,NULL);

	ppos = atfile + strlen(ATJOB_DIR);

/* Loop over all possible file names for running something at this particular
 * time, see if a file is there; the first empty slot at any particular time
 * is used.  Lock the file LFILE first to make sure we're alone when doing this.
 */
	if ((lockdes = open(LFILE,O_WRONLY)) == -1)
		perr("Cannot open lockfile " LFILE);
	
	lock.l_type = F_WRLCK; lock.l_whence = SEEK_SET; lock.l_start = 0;
	lock.l_len = 0;

	act.sa_handler = alarmc;
	act.sa_mask = 0;
	act.sa_flags = 0;

/* Set an alarm so a timeout occurs after ALARMC seconds, in case something
 * is seriously broken.
 */
	sigaction(SIGALRM,&act,NULL);
	alarm(ALARMC);
	fcntl(lockdes,F_SETLKW, &lock);
	alarm(0);

	for(i=0; i<ATJOB_MX; i++)
	{
		sprintf(ppos,"%c%8lx.%2x",queue,
			(unsigned long) (runtimer/60), i);
		for(ap=ppos; *ap != '\0'; ap ++)
			if (*ap == ' ')
				*ap = '0';

		if (stat(atfile, & statbuf) != 0)
		{
			if (errno == ENOENT)
			{
				break;
			}
			else
				perr("Cannot access " ATJOB_DIR);
		}
		
	}

	if (i >= ATJOB_MX)
		panic("Too many jobs already");

/* Create the file. The x bit is only going to be set after it has been
 * completely written out, to make sure it is not ececuted in the meantime.
 * To make sure they do not get deleted, turn off their r bit.  Yes, this
 * is a kluge.
 */

	cmask = umask(S_IRUSR | S_IWUSR | S_IXUSR);
	if ((fdes = creat(atfile, S_IWUSR)) == -1)
		perr("Cannot create atjob file"); 

	
/* We've successfully created the file; let's set the flag so it 
 * gets removed in case of an interrupt or error.
 */
	fcreated = 1;

/* Now we can release the lock, so other people can access it
 */

	lock.l_type = F_UNLCK; lock.l_whence = SEEK_SET; lock.l_start = 0;
	lock.l_len = 0;
	fcntl(lockdes,F_SETLKW,&lock);
	close(lockdes);

	if((fp = fdopen(fdes,"w")) == NULL)
		panic("Cannot reopen atjob file");

	fputs("#! /bin/sh\n\n",fp);

/* Write out the umask at the time of invocation
 */
	fprintf(fp,"umask %lo\n",(unsigned long) cmask);

/* Write out the environment. Anything that may look like a special character to
 * the shell is quoted, except for \n, which is done with a pair of "'s.
 */
	for (atenv= environ; *atenv != NULL; atenv++)
	{
		ap = strchr(*atenv,'=');
		if (ap == NULL)
			ap = *atenv;
		else
			ap++;

		fwrite(*atenv, sizeof(char), ap-*atenv,fp);
		for(;*ap != '\0'; ap++)
		{
			if (*ap == '\n')
				fprintf(fp, "\"\n\"");
			else
			{
				if (! isalnum(*ap))
					fputc('\\', fp);

				fputc(*ap, fp);
			}
		}
		fputc('\n', fp);
	}	

/* Cd to the directory at the time, write out "(" , all the commands the user
 * wants to have, a ")", a redirection of stderr to stdin and a mail
 * command to the user.
 */

	fprintf(fp, "\ncd %s\n(\n", cwdname());
	while((ch = getchar()) != EOF)
		fputc(ch,fp);

	fprintf(fp, "\n) 2>&1 | %s -s \"Output of your job %s\" %s\n",
		MAIL_CMD, ppos, getlogin());

	if (ferror(fp))
		panic("Output error");
	
	if (ferror(stdin))
		panic("Input error");

	fclose(fp);

/* This program is running authorized, so the effective uid and gid will not
 * match the real ones; let's give the file away to the original user.
 */
	uid = getuid();
	gid = getgid();
	if(chown(atfile, uid, gid) != 0)
		perr("Cannot give away file");

/* At long last, let's set the x bit on the file so that it will be marked
 * as ready for execution, but for this, we need to become ourselves again.
 */
	setgid(gid);
	setuid(uid);
	chmod(atfile,S_IRUSR | S_IWUSR | S_IXUSR);
	fprintf(stderr,"Job %s will be executed using /bin/sh\n",ppos);
}

static void list_jobs()
{
/* List all a user's jobs in the queue, by looping through ATJOB_DIR
 */
	uid_t uid;
	DIR *spool;
	struct dirent *dirent;
	struct stat buf;
	struct tm runtime;
	unsigned long ctm;
	char queue;
	time_t runtimer;

	uid = getuid();
	if (chdir(ATJOB_DIR) != 0)
		perr("Cannot change to " ATJOB_DIR);

	if ((spool = opendir(".")) == NULL)
		perr("Cannot open " ATJOB_DIR);

/*	Loop over every file in the directory 
 */
	while((dirent = readdir(spool)) != NULL)
	{
		if (stat(dirent->d_name,&buf) != 0)
			perr("Cannot stat in " ATJOB_DIR);

/*		See it's a regular file and has its x bit turned on and
 *		is executable
 */
		if (!S_ISREG(buf.st_mode) || (buf.st_uid != uid) ||
			!(S_IXUSR & buf.st_mode))
			continue;

		if(sscanf(dirent->d_name,"%c%8lx",&queue,&ctm)!=2)
			continue;

		runtimer = 60*(time_t) ctm;
		runtime = *localtime(&runtimer);
		
		printf("%s %c %d:%.2d %.2d.%.2d.%.4d\n",dirent->d_name, queue,
			runtime.tm_hour, runtime.tm_min, runtime.tm_mday,
			runtime.tm_mon+1,runtime.tm_year + 1900);
	}
}

static void delete_jobs(int argc,char **argv)
{
/* Delete every argument (job - ID) given
 */
	int i;
	uid_t uid;
	struct stat buf;
	

	uid = getuid();
	if (chdir(ATJOB_DIR) != 0)
		perr("Cannot change to " ATJOB_DIR);

	for (i=optind; i < argc; i++)
	{
		if (stat(argv[i],&buf) != 0)
		{
			perr("Job not found" ATJOB_DIR);
		}

		if (buf.st_uid != uid)
		{
			perr("Job not found" ATJOB_DIR);
		}
		if (unlink(argv[i]) != 0)
			perr("Cannot unlink jobfile");
	}

}

/* Global functions */

int main(int argc, char **argv)
{
	int c;
	char queue = 'a';
	int batch = 0;
	time_t runtimer;
	char *auxp;

	namep = argv[0];
	while ((c = getopt(argc,argv,"q:ld")) != EOF)
	{
		switch(c)
		{
		case 'q':
			if (strlen(optarg) > 1)
				usage();

			queue = *optarg;
			if ((!islower(queue)) || (queue > 'l'))
				usage();

			break;
		case 'l':
			list_jobs();
			exit(EXIT_SUCCESS);
		case 'd':
			delete_jobs(argc,argv);
			exit(EXIT_SUCCESS);
		}
	}

	if ((auxp = strrchr(namep, '/')) == NULL)
		batch = (strcmp(namep, "batch") == 0);
	else
		batch = (strcmp(auxp+1, "batch") == 0);

	if (!batch)
		runtimer = parsetime(argc, argv);
	else
	{
		queue = 'b';
		runtimer = time(NULL);
	}

	writefile(runtimer,queue);
	exit(EXIT_SUCCESS);
}
