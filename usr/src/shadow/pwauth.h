/*
 * Copyright 1992, John F. Haugh II
 * All rights reserved.
 *
 * Permission is granted to copy and create derivative works for any
 * non-commercial purpose, provided this copyright notice is preserved
 * in all copies of source code, or included in human readable form
 * and conspicuously displayed on all copies of object code or
 * distribution media.
 *
 * This software is provided on an AS-IS basis and the author makes
 * not warrantee of any kind.
 *
 *	@(#)pwauth.h	3.1	14:53:17	7/27/92
 */

/*
#if	__STDC__
int	pw_auth (char * program, char * user, int flag);
#else
int	pw_auth ();
#endif
*/
int	pw_auth ();

#define	PW_SU		1
#define	PW_LOGIN	2
#define	PW_ADD		3
#define	PW_CHANGE	4
#define	PW_DELETE	5

/*
 * Network access
 */

#define	PW_TELNET	6
#define	PW_RLOGIN	7
#define	PW_FTP		8
#define	PW_REXEC	9
