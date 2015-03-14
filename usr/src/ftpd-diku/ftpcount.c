#include <stdio.h>
#include <errno.h>
#include <malloc.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <ctype.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>
#include <signal.h>

#include "pathnames.h"
#include "extensions.h"

/*************************************************************************/
/* FUNCTION  : parse_time                                                */
/* PURPOSE   : Check a single valid-time-string against the current time */
/*             and return whether or not a match occurs.                 */
/* ARGUMENTS : a pointer to the time-string                              */
/*************************************************************************/

int
parsetime(whattime)
char	*whattime;

{
static char *days[] = { "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa", "Wk" };
long	clock;
struct	tm *curtime;
int		wday, start, stop, ltime, validday, loop, match;

	(void) time(&clock);
	curtime = localtime(&clock);
	wday = curtime->tm_wday;
	validday = 0;
	match = 1;

	while (match && isalpha(*whattime) && isupper(*whattime)) {
		match = 0;
		for (loop = 0; loop < 8; loop++) {
			if (strncmp(days[loop], whattime, 2) == NULL) {
				whattime += 2;
				match = 1;
				if ((wday == loop) | ((loop == 7) && wday && (wday < 6)))
					validday = 1;
			}
		}
	}

	if (strncmp(whattime, "Any", 3) == NULL) {
		validday = 1;
		whattime += 3;
	}

	if (!validday) return(0);

	if (sscanf(whattime, "%d-%d", &start, &stop) == 2) {
		ltime = curtime->tm_min + 100 * curtime->tm_hour;
		if ((start < stop) && ((ltime > start) && ltime < stop)) return(1);
		if ((start > stop) && ((ltime > start) || ltime < stop)) return(1);
	} else return(1);

	return(0);
}

/*************************************************************************/
/* FUNCTION  : validtime                                                 */
/* PURPOSE   : Break apart a set of valid time-strings and pass them to  */
/*             parse_time, returning whether or not ANY matches occurred */
/* ARGUMENTS : a pointer to the time-string                              */
/*************************************************************************/

int
validtime(ptr)
char	 *ptr;

{
char	 *nextptr;
int	good;

	while (1) {
		nextptr = strchr(ptr, '|');
		if (strchr(ptr, '|') == NULL) return(parsetime(ptr));
		*nextptr = '\0';
		good = parsetime(ptr);
		*nextptr++ = '|';  /* gotta restore the | or things get skipped! */
		if (good) return(1);
		ptr = nextptr;
	}
}

acl_getlimit(aclbuf, class)
char	*aclbuf, *class;

{
char	*crptr,
	*ptr,
	linebuf[1024];
int	limit;

	while (*aclbuf != NULL) {
		if (strncasecmp(aclbuf, "limit", 5) == 0) {
			for (crptr = aclbuf; *crptr++ != '\n'; );
			*--crptr = NULL;
			strcpy(linebuf, aclbuf);
			*crptr = '\n';
			(void) strtok(linebuf, " \t");	/* returns "limit" */
			if (strcmp(class, strtok(NULL, " \t")) == 0) {
				limit = atoi(strtok(NULL, " \t"));	/* returns limit <n> */
				if ((ptr = strtok(NULL, " \t")) && validtime(ptr))
					return(limit);
			}
		}
		while (*aclbuf && *aclbuf++ != '\n');
	}

	return(0);

}

acl_countusers(class)
char	*class;

{
int		pidfd,
		count,
		stat,
		which;
char	pidfile[1024];
pid_t	buf[MAXUSERS];

	sprintf(pidfile, _PATH_PIDNAMES, class);
	pidfd = open(pidfile, O_RDONLY, 0644);
	if (pidfd == -1) {
		printf("error opening pid file (%s): %s\n", pidfile,
			strerror(errno));
		return(0);
	}
	lseek(pidfd, 0, L_SET);

	count = 0;
   
	if (read(pidfd, buf, sizeof(buf)) == sizeof(buf)) {
		for (which = 0; which < MAXUSERS; which++)
		if (buf[which]) {
			stat = kill (buf[which], SIGCONT);
			if (((stat == -1) && (errno == EPERM)) || !stat) count++;
		}
	}

	flock(pidfd, LOCK_UN);
	close(pidfd);

	return(count);
}

main(argc, argv)
int		argc;
char	*argv[];

{
FILE	*accessfile, *pidfile;
char	class[80],
		linebuf[1024],
		*aclbuf,
		*myaclbuf,
		*crptr;
int		limit;
struct	stat	finfo;

	if ((accessfile = fopen(_PATH_FTPACCESS, "r")) == NULL) {
		if (errno != ENOENT) perror("ftpcount: could not open() access file");
		exit(1);
	}

	if (stat(_PATH_FTPACCESS, &finfo)) {
		perror("ftpcount: could not stat() access file");
		exit(1);
	}

	if (finfo.st_size == 0) {
		printf("ftpcount: no service classes defined, no usage count kept\n");
		exit(0);
	} else {
		if (!(aclbuf = malloc(finfo.st_size + 1))) {
			perror("ftpcount: could not malloc aclbuf");
			exit(1);
		}
		fread(aclbuf, finfo.st_size, 1, accessfile);
		*(aclbuf+finfo.st_size) = '\0';
	}

	myaclbuf = aclbuf;
	while (*myaclbuf != NULL) {
		if (strncasecmp(myaclbuf, "class", 5) == 0) {
			for (crptr = myaclbuf; *crptr++ != '\n'; );
			*--crptr = NULL;
			strcpy(linebuf, myaclbuf);
			*crptr = '\n';
			(void) strtok(linebuf, " \t");	/* returns "class" */
			strcpy(class, strtok(NULL, " \t")); /* returns class name */
			limit = acl_getlimit(myaclbuf, class, NULL);
			printf("Service class %-20.20s - %3d users (%3d maximum)\n", class,
				acl_countusers(class), limit);
		}
		while (*myaclbuf && *myaclbuf++ != '\n');
	}
}
