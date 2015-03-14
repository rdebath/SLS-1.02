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
/* System-independent variables; included at the head of system.h. */

/* The system.h file will override definitions in this file, as appropriate. */
/* OPSYSNAME, sys_xxxxxx, SYS_NAME, UNQUOT_SYSNAME, and
   some XXX_ENV should be defined, also. */
/* Sites wishing to customize this file should do so by editing the ``site.h'' file,
   which, as distributed, is zero-length.  Patches will never be generated for
   the ``site.h'' file. */

/* The only currently supported systems are bsd4.2 and bsd4.3 */
#define	SY_B42	0 /* define for bsd 4.2 */
#define	SY_B43	0 /* define for bsd 4.3 */
/* These are here for System V support. */
#define	SY_U51	0 /* define for SysVR1 */
#define	SY_U52	0 /* define for SysVR2 */
#define	SY_U53	0 /* define for SysVR3 */
/* These are here for AIX support. */
#define	SY_AIX11	0	/* define for AIX 1.1 (e.g. on PS/2) */
#define	SY_AIX12	0	/* define for AIX 1.2 (e.g. on PS/2) */
#define	SY_AIX221	0	/* define for AIX 2.2.1 */
#define	SY_AIX3		0	/* defined for AIX 3 (e.g. on RS/6000) */
#define	SY_AIX31	0	/* defined for AIX 3.1 (e.g. on RS/6000) */

/* Generic bsd vs SysV defines */
#define	SY_B4x	(SY_B42 || SY_B43)
#define	SY_U5x	(SY_U51 || SY_U52 || SY_U53)
#define	SY_AIXx	(SY_AIX11 || SY_AIX12 || SY_AIX221 || SY_AIX3 || SY_AIX31)

/* If your site has a specific place where you would like to place the */
/* AndrewSetup configuration file, #define LOCAL_ANDREW_SETUP_ENV */
/* to be the string naming that file.  For example: */
/* #define LOCAL_ANDREW_SETUP_ENV "/mit/andrew/lib/AndrewSetup" */
/* This location will be searched along the standard locations in */
/* the AndrewSetup search path: /AndrewSetup and /etc/AndrewSetup will */
/* be searched first, then the value of LOCAL_ANDREW_SETUP_ENV, if any, */
/* then the remainder of the standard AndrewSetup search path. */
/* If you would like to have several locations searched, specify them */
/* in order, as follows: */
/* #define LOCAL_ANDREW_SETUP_ENV "first_pathname",\ */
/* "second_pathname",\ */
/* ... */
/* "last_pathname" */
/* #define LOCAL_ANDREW_SETUP_ENV "/rel/common/AndrewSetup" */

/* These configuration flags determine how the Andrew
  software is built. Defining any one of these will cause that
  part of the system to be built (if the sources are available),
  and/or introduce dependencies on that software in other
  parts of the system.

  At the ITC, these flags should all be defined
  (except RELEASE2_ENV, LINKINSTALL_ENV, and
  LOCAL_ANDREW_SETUP_ENV)
  */

/* Defined if building for use with the Andrew File System (Vice) */
/* #define AFS_ENV	1 */

/* Defined if you have version 3.0 of the Andrew File System, including
  the protection server */
/* #define AFS30_ENV 1 */

/* Defined if building ODA and the ODA Translators */
/* #define ODA_ENV 1 */


/* Defined if you want to build the Andrew Message System (AMS) */
#define AMS_ENV	1

/* Defined if building code to deal with AMDS (AMS Delivery System) anywhere */
/* #define AMS_DELIVERY_ENV	1 */

/* Defined if we expect to run AMDS (AMS Delivery System) at this site.  This option affects only the default option values in mail system configuration, in the files andrew/overhead/util/lib/svcconf.c and andrew/overhead/mail/lib/mailconf.c . */
/* #define RUN_AMDS_ENV	1 */

/* Defined for building with the White Pages */
/* #define WHITEPAGES_ENV	1 */


/* WHITEPAGES_ENV can be defined without AMS_DELIVERY_ENV or AFS_ENV */
/* AMS_DELIVERY_ENV and AMS_ENV can NOT be defined without WHITEPAGES_ENV */
#if (defined(AFS_ENV) || defined(AMS_DELIVERY_ENV)) && ! defined(WHITEPAGES_ENV)
#define WHITEPAGES_ENV 1
#endif /* (defined(AFS_ENV) || defined(AMS_DELIVERY_ENV)) && ! defined(WHITEPAGES_ENV) */


/* Defined if you run the Internet domain name resolver */
#define RESOLVER_ENV	1


/* Defined if building for use with Snap (remote messageservers) */
/* #define SNAP_ENV    1 */


/* Defined for building for X11 */
#define X11_ENV	1

/* Define if you're building against an older version of X11 (pre-release 4) */
/* #define PRE_X11R4_ENV	1 */


/* Defined for building the wm window manager */
/* #define WM_ENV	1 */


/* Defined for using andrew malloc */
#define ANDREW_MALLOC_ENV 1


/* Defined for using debugging version of andrew malloc */
/* #define DEBUG_MALLOC_ENV 1 */


/* Make sure we have the Andrew malloc if debugging is desired */
#ifdef DEBUG_MALLOC_ENV
#ifndef ANDREW_MALLOC_ENV
#define ANDREW_MALLOC_ENV 1
#endif /* #ifndef ANDREW_MALLOC_ENV */
#endif /* #ifdef DEBUG_MALLOC_ENV */


/* Defined if you are using the Andrew/CMU printing software */
/* Most people will not be using this */
/* #define ANDREW_PRINTING_ENV 1 */


/* Defined for building a for a release 2 version of X11 */
/* #define RELEASE2_ENV 1 */

/* Software Levels: (Set to highest number you want to build) */
/* ############ FIX COMMENTS HERE ############ */
#define LEVEL_ENV 4
#define MK_BLD_BLKS		1
/* #define MK_BASIC_UTILS	1 */
#define MK_BASIC_INSETS	1
#define MK_HELP		1
#define MK_TEXT_EXT		1
/* #define MK_AUTHORING		1 */
/* #define MK_AUX_UTILS		1 */
/* #define MK_AUX_INSETS	1 */
/* #define MK_EXAMPLES		1 */


/* Defined if you have ditroff */
#define DITROFF_ENV 1


/* Define this if you have the ndbm(3) package */
/* #define NDBM_ENV 1 */

/* Define this if your libc.a provides getdomainname() */
/* #define GETDOMAIN_ENV 1 */

/* Define this if you want to build the contributed software
   (in ./contrib/*). */
/* #define CONTRIB_ENV 1 */

/* Defined to be the default ``ANDREWDIR'' value, where users will see */
/* the final result of the Andrew installation. */
#define DEFAULT_ANDREWDIR_ENV /usr/andrew


/* Defined to be the default ``LOCALDIR'' value, a directory where */
/* some site-specific customizations may be installed */
#define DEFAULT_LOCALDIR_ENV /usr/local


/* Defined if you want to use links when installing the system */
#define LINKINSTALL_ENV 1

/* Defined if you want to build the andrew version of install */
/* Normally set if your system install does not work with our distribution */
/* In particular if your install can not install a file to as another file */
/* but only a file into a directory */

/* #define BUILDANDREWINSTALL_ENV 1 */

/* If you have an old version of Ultrix that doesn't handle disabling of ECHO for pty's then define OLD_ULTRIX */
/* #define OLD_ULTRIX */
