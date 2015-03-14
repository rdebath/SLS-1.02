/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ftw.h	5.2 (Berkeley) 8/3/88
 */

#define	FTW_F		1		/* regular file */
#define	FTW_D		2		/* directory */
#define	FTW_D2		3		/* directory */
#define	FTW_DNR		4		/* unreadable directory */
#define	FTW_NS		5		/* unstatable object */

#define	FTW_CHDIR	0x01		/* use chdir(2) */
#define	FTW_DIRLAST	0x02		/* visit node last */
#define	FTW_SYMLINK	0x04		/* indirect through symbolic links */
