/*
 *  Project   : NNTP (RFC 977) extension
 *  Module    : xoverview.c
 *  Author    : I.Lea
 *  Created   : 18-11-92
 *  Updated   : 18-11-92
 *  Notes     : Add a command to retieve Cnews .overview style
 *              index files from the NNTP server so as to save
 *              space on the client.
 *              Ideas borrowed from XTHREAD nntp extension code
 *              posted by Tim Iverson to alt.sources in mid'91.
 *  Copyright : (c) Copyright 1992 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "common.h"

#ifdef XOVERVIEW

#undef	DEBUG_XOVERVIEW		/* set to define to turn on more debug info */

#ifndef MAXPATHLEN
#	define MAXPATHLEN 256
#endif

#ifdef __STDC__
void xoverview (int argc, char *argv[]);
#else
void xoverview ();
#endif

/*
 *  Usage: XOVERVIEW groupname
 *
 *  Retrieve an index file for the specified newsgroup (ie. alt.sources)
 *
 *  This command is NOT documented in RFC977.
 */

void xoverview (argc, argv)
	int	argc;
	char	*argv[];
{
	char	line[NNTP_STRLEN];
	char	buf[MAXPATHLEN];
	char	group[MAXPATHLEN];
	char	index_file[MAXPATHLEN];
	char	*cp, *p;
	FILE	*fp;
	
	/*
	 * "parse" the argument list
	 */
	if (argc == 1) {
		printf("%d Usage: XOVERVIEW group\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	} else {
		strncpy (group, argv[1], sizeof (group)-1);
#if defined(SYSLOG) && defined(DEBUG_XOVERVIEW)
		syslog(LOG_INFO, "%s xoverview %s", hostname, group);
#endif

		strncpy (buf, group, sizeof (buf)-1);
		p = buf;
		while (*p) {
			if (*p == '.') {
				*p = '/';
			}	
			p++;
		}
		sprintf (index_file, "%s/%s/.overview", SPOOLDIR, buf);
		
		if ((fp = fopen(index_file, "r")) == NULL) {
#ifdef SYSLOG
			syslog(LOG_INFO, "%s xoverview cannot open %s (%s)",
				hostname, group, index_file);
#endif
			printf("%d XOVERVIEW Cannot open %s\r\n",
				ERR_XOVERVIEW, group);
			(void) fflush(stdout);
			return;
		}

		printf("%d XOVERVIEW %s index file follows\r\n", 
			OK_XOVERVIEW, group);
		(void) fflush(stdout);
		
		while (fgets(line, sizeof(line), fp) != NULL) {
			if ((cp = index(line, '\n')) != NULL)
				*cp = '\0';
			putline(line);
		}
		(void) fclose(fp);
	
		putline(".");
		(void) fflush(stdout);
	}
}


#endif /* XOVERVIEW */

