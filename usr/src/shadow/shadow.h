/*
 * Copyright 1988, 1989, 1990, John F. Haugh II
 * All rights reserved.
 *
 * Use, duplication, and disclosure prohibited without
 * the express written permission of the author.
 */

#ifndef	_SHADOW_H
#define	_SHADOW_H

#include <features.h>

__BEGIN_DECLS

/*
 * This information is not derived from AT&T licensed sources.  Posted
 * to the USENET 11/88, and updated 11/90 with information from SVR4.
 *
 *	@(#)shadow.h	3.3	09:06:50	12/7/90
 */

#ifdef	ITI_AGING
typedef	time_t	sptime;
#else
typedef	long	sptime;
#endif

/*
 * Shadow password security file structure.
 */

struct	spwd {
	char	*sp_namp;	/* login name */
	char	*sp_pwdp;	/* encrypted password */
	sptime	sp_lstchg;	/* date of last change */
	sptime	sp_min;		/* minimum number of days between changes */
	sptime	sp_max;		/* maximum number of days between changes */
	sptime	sp_warn;	/* number of days of warning before password
				   expires */
	sptime	sp_inact;	/* number of days after password expires
				   until the account becomes unusable. */
	sptime	sp_expire;	/* days since 1/1/70 until account expires */
	unsigned long	sp_flag; /* reserved for future use */
};

/*
 * Shadow password security file functions.
 */

extern struct	spwd	*getspent __P((void));
extern struct	spwd	*getspnam __P((char *__nam));
extern struct	spwd	*getspent __P((void));
extern struct	spwd	*fgetspent __P((FILE *__F));
extern void	setspent __P((void));
extern void	endspent __P((void));
extern int	putspent __P((const struct spwd *__sp, FILE *__F));

#define  SHADOW "/etc/shadow"

/*
 * Shadow group security file structure
 */

struct	sgrp {
	char	*sg_name;	/* group name */
	char	*sg_passwd;	/* group password */
	char	**sg_adm;	/* group administator list */
	char	**sg_mem;	/* group membership list */
};

/*
 * Shadow group security file functions.
 */

#include <pwd.h>
#include <grp.h>

extern struct	sgrp	*getsggid __P((__gid_t __gid));
extern struct	sgrp	*getsgent __P((void));
extern struct	sgrp	*getsgnam __P((char *__str));
extern struct	sgrp	*fgetsgent __P((FILE *__F));
extern void	setsgent __P((void));
extern void	endsgent __P((void));
extern int	putsgent __P((struct sgrp *_grp, FILE *__F));

#define	GSHADOW	"/etc/gshadow"

#ifdef SHADOW_PWD

extern struct passwd * 	_spgetpwent __P((void));
extern struct passwd *	_spgetpwnam __P((char *__name));
extern struct passwd *	_spgetpwuid __P((uid_t __uid));
extern struct passwd *	_spfgetpwent __P((FILE *__F));
extern struct group * 	_spgetgrent __P((void));
extern struct group *	_spgetgrnam __P((char *__name));
extern struct group *	_spgetgrgid __P((__gid_t __gid));
extern struct group * 	_spfgetgrent __P((FILE *__F));
extern char *		_spcrypt __P((char *__buf,char *__salt));

#define getpwent() 	_spgetpwent()
#define getpwnam(name)	_spgetpwnam(name)
#define getpwuid(uid)	_spgetpwuid(uid)
#define fgetpwent(F)	_spfgetpwent(F)
#define getgrent() 	_spgetgrent()
#define getgrnam(name)	_spgetgrnam(name)
#define getgrgid(gid)	_spgetgrgid(gid)
#define fgetgrent(F)	_spfgetgrent(F)
#define crypt(buf,salt)	_spcrypt(buf,salt)

#endif

__END_DECLS

#endif
