/* config.h, Version 1.0.3, 5/4/1993 by Stewart Forster */

/************************************************************************/
/*   Copyright (C) 1993 Stewart Forster					*/
/*  This program is free software; you can redistribute it and/or modify*/
/*  it under the terms of the GNU General Public License as published by*/
/*  the Free Software Foundation; either version 2, or (at your option) */
/*  any later version.							*/
/*									*/
/*  This program is distributed in the hope that it will be useful,	*/
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of	*/
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	*/
/*  GNU General Public License for more details.			*/
/*									*/
/*  You should have received a copy of the GNU General Public License	*/
/*  along with this program; if not, write to the Free Software		*/
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.		*/
/************************************************************************/


/************************************************************************/
/*	OS TYPE DEFINITION						*/
/* Define one of the following where appropriate			*/
/************************************************************************/

/* #define	SUNOS		/* */
#define	IRIX		/* */
/* #define	AIX		/* */
/* #define	ULTRIX		/* */
/* #define	LINUX		/* */

/************************************************************************/
/* SUNOS requires the pstat command to be available.  This is part of 	*/
/* the SunOs distribution but fix this following path if it's wrong.	*/
/************************************************************************/

#ifdef SUNOS
#define	PSTATI	"/usr/etc/pstat -i"
#endif

/************************************************************************/
/* PATHUNTCX is the pathname where the untcx executable resides.  Note	*/
/* that this is the interpreter/unpacker and hence must exist with this	*/
/* path, or else the whole system will fail! This path should also be on*/
/* a filesystem which the local system trusts setuid binaries from,	*/
/* since PATHUNTCX must run setuid to root.				*/
/************************************************************************/

#define	PATHUNTCX	"/usr/local/bin/untcx"

/************************************************************************/
/* PATHTCX is the pathname to the tcx executable.  This should NOT be	*/
/* installed setuid root.						*/
/************************************************************************/

#define	PATHTCX		"/usr/local/bin/tcx"

/************************************************************************/
/* PATHPACKER is the pathname to the compression program you wish to use*/
/* with the system. PACKEROPTS is an optional definition of options you	*/
/* wish to pass to the packer program. NB. The compression program must	*/
/* be a `filter', that is, it is capable of reading from stdion and	*/
/* compressing to stdout.						*/
/************************************************************************/

#define	PATHPACKER	"/usr/local/bin/gzip"
/*#define	PACKEROPTS	"-7"			/* Optional */

/************************************************************************/
/* PATHUNPACK and the optional UNPACKOPTS serve a similar purpose to	*/
/* PATHPACKER and PACKEROPTS above.					*/
/************************************************************************/

#define	PATHUNPACK	"/usr/local/bin/gzip"
#define	UNPACKOPTS	"-d"			/* Optional */

/************************************************************************/
/* UNPACK_IN_PLACE instructs the uncrompressor to unpack an executable	*/
/* where it lives if it resides on the local machine.  This is useful	*/
/* on machines with small /tmp's, plus it evenly distributes the disk	*/
/* workload.  It is generally deemed useful to have it turned on.	*/
/* Comment this out if you have a large unpack area for ENFSDIR (below)	*/
/* or if you have very limited CPU resources, since unpacking in place	*/
/* means that untcx will have to recompress the file later, rather than	*/
/* just deleting the temporary one from ENFSDIR.			*/
/************************************************************************/

#define	UNPACK_IN_PLACE	/* */

/************************************************************************/
/* ENFSDIR is the pathname to the directory where emergency (out of disk*/
/* space locally) or NFS mounted executables get unpacked to.		*/
/* On SUNOS, if you are using the "tmpfs", you will have to set ENFSDIR	*/
/* to a "real disk". (A "real disk" may also be an NFS mounted partition*/
/* to which the machine has root access to).  There seems to be some	*/
/* problem with fcntl locks on SUNOS tmpfs.				*/
/************************************************************************/

#define	ENFSDIR		"/tmp/exec"

/************************************************************************/
/* SCANRATE is the interval in seconds which the tcx daemon waits before*/
/* rescanning all files it is currently managing, for recompression in	*/
/* the case of local files, or deletion from ENFSDIR in the case of 	*/
/* emergency or NFS mounted executables.  Note that SCANRATE should	*/
/* probably not be larger than ENFSTIMEOUT or LOCALTIMEOUT defined	*/
/* below, otherwise you will undermine the purpose of those variables.	*/
/************************************************************************/

#define	SCANRATE	60		/* 60 seconds between scans */

/************************************************************************/
/* ENFSTIMEOUT is the least number of seconds of disuse of an executable*/
/* residing in ENFSDIR the tcx daemon will wait for, before it attempts	*/
/* to delete the executable. This should be set quite low if there isn't*/
/* much disk space available in ENFSDIR.				*/
/* On SUNOS, this value only sets in after the inode is timed out of the*/
/* inode cache.  This problem will be addressed in a future release.	*/
/************************************************************************/

#define	ENFSTIMEOUT	300		/* 5 minutes of inactivity */

/************************************************************************/
/* LOCALTIMEOUT is the least number of seconds of disuse of an		*/
/* executable residing locally on the system that the tcx will wait	*/
/* before attempting to repack the executable.				*/
/* On SUNOS, this value only sets in after the inode is timed out of the*/
/* inode cache.  This problem will be addressed in a future release.	*/
/************************************************************************/

#define	LOCALTIMEOUT	3600		/* 1 hour of inactivity */


/************************************************************************/
/************************************************************************/
/* You should not need to edit anything after this point		*/
/************************************************************************/
/************************************************************************/

#define	MAXHEADERSIZE	256
#define	SIGTYPE	int

/* Define PUSLEEP (portable usleep definition)	*/

#if defined(LINUX)
#define	__USE_BSD_SIGNAL
#define	SIGTYPE	void
#endif

#if defined(IRIX)
#define _BSD_SIGNALS
#define	PUSLEEP(x)	(sginap((long)((x)/10000)))
#endif

#if defined(ULTRIX) || defined(SUNOS) || defined(AIX) || defined(LINUX)
#define	PUSLEEP(x)	(usleep(x))	/* usleep code in untcx.c for ULTRIX */
#endif

#ifdef __STDC__
#include	<stdlib.h>
#endif
#include        <unistd.h>
#include        <sys/time.h>
#include        <sys/wait.h>

#if defined(AIX)
#include	<sys/id.h>
#endif

#include        <sys/types.h>

#if defined(ULTRIX) || defined(LINUX)
#include	<sys/param.h>
#include	<sys/mount.h>
#endif

#if defined(IRIX) || defined(AIX)
#include        <sys/statfs.h>
#endif

#if defined(SUNOS) || defined(LINUX)
#include        <sys/vfs.h>
#endif

#include        <sys/stat.h>
#include	<fcntl.h>
#include        <string.h>
#include        <errno.h>
#include        <signal.h>
#include        <stdio.h>

#ifndef	MAXPATHLEN
#define	MAXPATHLEN	1024
#endif
