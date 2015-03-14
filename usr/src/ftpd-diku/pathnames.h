/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)pathnames.h	5.2 (Berkeley) 6/1/90
 */

/* #include <paths.h> */
/*#define PINS*/

#include "support/paths.h"

#define	_PATH_FTPUSERS	"/etc/ftpusers"
#define	_PATH_FTPACCESS	"/etc/ftpaccess"
#define	_PATH_PIDNAMES	"/usr/adm/ftpd/pids-%s"
#define	_PATH_XFERLOG	"/usr/adm/ftpd/xferlog"
 
 #ifdef PINS
 #define	_PATH_PINFILE	"/news/tmp/ftpd/pins"
 #define _PATH_PACKAGES	"/news/tmp/ftpd/packages"
 #endif /* PINS */

#ifndef	_PATH_UTMP
#define _PATH_UTMP      "/etc/utmp"
#define _PATH_WTMP      "/etc/wtmp"
#define _PATH_LASTLOG   "/etc/lastlog"
#endif
