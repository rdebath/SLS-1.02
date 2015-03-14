/*************************************************************************
Linux MODEM Server version 2
--------------------------------------------------------------------------

    Copyright (C) 1992  Anthony Rumble

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details. <copying>

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

--------------------------------------------------------------------------
RCS Info

$Header: /home/smilie/bbs/modem/RCS/modem.h,v 1.8 1993/03/28 04:53:17 smilie Exp $

$Log: modem.h,v $
 * Revision 1.8  1993/03/28  04:53:17  smilie
 * updated version
 *
 * Revision 1.7  1992/12/06  06:54:18  smilie
 * *** empty log message ***
 *
 * Revision 1.6  1992/10/11  08:49:18  smilie
 * added CONFIG.carrier for carrier detection
 *
 * Revision 1.5  1992/10/10  06:10:21  smilie
 * updated version to 2.02a
 *
 * Revision 1.4  1992/10/10  04:49:06  smilie
 * added server version number
 *
 * Revision 1.3  1992/10/09  15:29:45  smilie
 * *** empty log message ***
 *
 * Revision 1.2  1992/10/09  13:44:22  smilie
 * fixed some bugs
 *
 * Revision 1.1  1992/10/09  13:19:08  smilie
 * Initial revision
 *

$Id: modem.h,v 1.8 1993/03/28 04:53:17 smilie Exp $

*************************************************************************/
#ifndef _MODEM_H
#define _MODEM_H

#include <stdio.h>
#include <sys/types.h>

#define MDMSERVER_VERSION	"2.06"

/* fgetsraw.c */
extern int fgetsraw(char *, int, FILE *, int);

/* fgetstr.c */
extern char *fgetstr(char *, int, FILE *);

/* flog.c */
extern void 	flog (const char *, ...);

/* strupr.c */
extern char *strupr(char *);

/* setup.c */
extern int load_config(void);

typedef struct
	{
	int locked;			/* Locked DTE rate */
	int speed;			/* Modem INIT Speed */
	int conspeed;			/* Connected SPEED */
	char init[1024];		/* Initialise String */
	int quiet;			/* If you want QUIET time */
	int carrier;			/* 1 if wanting carrier checking */
	char quiet_str_on[256];		/* QUIET Modem String */
	char quiet_str_off[256];	/* QUIET Modem String */
	int quiet_start;		/* QUIET start hour */
	int quiet_stop;			/* QUIET stop hour */
	cc_t timeout;			/* Wait for CR timeout seconds */
					/* Set this depending on if you have MNP or not */
 	} config_struct;

extern config_struct CONFIG;

extern char stty[8];

/* Modem Return codes */
#define MDM_UNKNOWN	0
#define MDM_NOCARRIER	1
#define MDM_NOANSWER	2
#define MDM_BUSY	3
#define MDM_VOICE	4
#define MDM_NODIALTONE	5
#define MDM_CONNECT	6

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#endif

