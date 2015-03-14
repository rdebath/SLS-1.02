/*
 *  Project   : NNTP (RFC 977) extension
 *  Module    : xuser.c
 *  Author    : I.Lea / T.Iverson (iverson@xstor.com)
 *  Created   : 07-03-92
 *  Updated   : 07-03-92
 *  Notes     : Add a command to log nntp clients username.
 *              Ideas borrowed from XTHREAD nntp extension code
 *              posted by Tim Iverson to alt.sources in mid'91.
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "common.h"

#ifdef XUSER

/*
 *  Usage: XUSER USER
 *
 *  USER    log clients username to nntp logfile
 *
 *  This command is NOT documented in RFC977.
 */

void xuser (argc, argv)
	int	argc;
	char	*argv[];
{
	char	userinfo[NNTP_STRLEN];
	int	i;
	
	/*
	 * "parse" the argument list
	 */
	if (argc == 1) {
		printf("%d Usage: XUSER user\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	} else {
		sprintf (userinfo, "user %s", argv[1]);
		for (i = 2 ; i < argc ; i++) {
			strcat (userinfo,  " ");
			strcat (userinfo,  argv[i]);
		}
	}

#ifdef SYSLOG
		syslog(LOG_INFO, "%s %s", hostname, userinfo);
#endif
}

#endif /* XUSER */

