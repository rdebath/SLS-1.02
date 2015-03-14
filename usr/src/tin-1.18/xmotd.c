/*
 *  Project   : NNTP (RFC 977) extension
 *  Module    : xmotd.c
 *  Author    : I.Lea
 *  Created   : 26-09-92
 *  Updated   : 27-09-92
 *  Notes     : Add a command to display a motd (message of the day) file
 *              Ideas borrowed from NEWGROUPS nntp command
 *              posted by Tim Iverson to alt.sources in mid'91.
 *  Copyright : (c) Copyright 1992 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "common.h"

#ifdef XMOTD

#include "time.h"

#undef	DEBUG_XMOTD		/* set to define to turn on more debug info */

#ifdef __STDC__
void xmotd (int argc, char *argv[]);
#else
void xmotd ();
#endif

/*
 * Usage: XMOTD date time ["GMT"]
 *
 * Display a motd file if newer than given date and time
 *
 *  This command is NOT documented in RFC977.
 */

void xmotd(argc, argv)
	int		argc;
	char		*argv[];
{
	char		line[NNTP_STRLEN];
	register char	*cp;
	int		i;
	FILE		*fp;
	long		old_date = 0L;
	long		new_date = 0L;
	struct stat	sb;
	
	if (argc < 3) {
printf("%d Usage: XMOTD yymmdd hhmmss [\"GMT\"].\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

#if defined(SYSLOG) && defined(DEBUG_XMOTD)
		syslog(LOG_INFO, "%s xmotd %s %s", hostname, argv[1], argv[2]);
#endif

	fp = fopen(XMOTD_FILE, "r");
	if (fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "xmotd: fopen %s: %m", XMOTD_FILE);
#endif
		printf("%d XMOTD Cannot open %s\r\n", ERR_XMOTD, XMOTD_FILE);
		(void) fflush(stdout);
		return;
	}

	/*	    YYMMDD		    HHMMSS	*/
	if (strlen(argv[1]) != 6 || strlen(argv[2]) != 6) {
		printf("%d Date/time must be in form YYMMDD HHMMSS.\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		(void) fclose(fp);
		return;
	}

	(void) strcpy(line, argv[1]);			/* yymmdd */
	(void) strcat(line, argv[2]);			/* hhmmss */

	new_date = dtol(line);
	if (new_date < 0) {
		printf("%d Invalid date specification.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		(void) fclose(fp);
		return;
	}

	argc -= 3;
	argv += 3;

	if (argc > 0 && !strcasecmp(*argv, "GMT")) { /* We store stuff in GMT */
			++argv;			/* anyway, so this is */
			--argc;			/* a "noop" */
	} else 					/* But that means not GMT */
		new_date = local_to_gmt(new_date);	/* is a definite "op" */

	/*
	 *  stat() motd file and find mtime for comparison
	 */
	if (stat (XMOTD_FILE, &sb) != -1) {
		old_date = sb.st_mtime;
	}
	
#if defined(SYSLOG) && defined(DEBUG_XMOTD)
		syslog(LOG_INFO, "Motd file time=[%ld] request=[%ld]\r\n", 
			old_date, new_date);
#endif

	printf("%d Motd file since %s follows.\r\n", OK_XMOTD, line);

#if defined(SYSLOG) && defined(DEBUG_XMOTD)
		syslog(LOG_INFO, "Motd file since %s %s follows.\r\n", line);
#endif
	if (new_date < old_date) {
		while (fgets(line, sizeof(line), fp) != NULL) {
			if ((cp = index(line, '\n')) != NULL)
				*cp = '\0';
			putline(line);
		}
	}
	putchar('.');
	putchar('\r');
	putchar('\n');
	(void) fflush(stdout);
	(void) fclose(fp);
}

#endif	/* XMOTD */
