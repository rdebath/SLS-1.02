/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
#ifndef	SYSTEM_H
#define	SYSTEM_H

/* Get common definitions */
#include <allsys.h>

#define	OPSYSNAME	"SUN4_4.3bsd"
#define	sys_sun4_40	1
#define	SYS_NAME	"sun4_40"
#define	SUN_ENV

/* Here follow the overrides for this system. */
#undef	SY_B43
#define	SY_B43	1 /* This system is most like bsd 4.3 */

#ifndef In_Imake

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>

#include <strings.h>

/*  Get open(2) constants */
#include <sys/file.h>

/*  Get struct timeval */
#include <sys/time.h>

#ifndef AMIGA
/* include path for syslog.h */
#include <syslog.h>
#endif

#ifndef VMUNIX
#define	VMUNIX	1
#endif /* VMUNIX */

#define OSI_HAS_SYMLINKS 1

#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))

#define osi_ExclusiveLockNoBlock(fid)	flock((fid), LOCK_EX | LOCK_NB)
#define osi_UnLock(fid)			flock((fid), LOCK_UN)
#define osi_O_READLOCK			O_RDONLY
#define osi_F_READLOCK			"r"

#define	osi_vfork()			vfork()

#define	osi_setjmp  _setjmp
#define	osi_longjmp _longjmp

/* Make a time standard. */
struct osi_Times {unsigned long int Secs; unsigned long int USecs;};
/* Set one of the above with a call to osi_GetTimes(&foo) */
#define osi_GetSecs() time((long int *) 0)
extern void osi_SetZone();
extern char *osi_ZoneNames[];
extern long int osi_SecondsWest;
extern int osi_IsEverDaylight;

/*
 * Put system-specific definitions here
 */

#define HAS_SYSEXITS 1

#endif /* !In_Imake */

#define	NDBM_ENV    1
#define	GETDOMAIN_ENV	1

/* Now follow the site-specific customizations. */
#include <site.h>

#endif	/* SYSTEM_H */
