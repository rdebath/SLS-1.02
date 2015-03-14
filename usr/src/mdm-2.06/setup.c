/*************************************************************************
Config loading and parsing code
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

$Header: /home/smilie/bbs/modem/RCS/setup.c,v 1.5 1992/12/06 06:52:20 smilie Exp $

$Log: setup.c,v $
 * Revision 1.5  1992/12/06  06:52:20  smilie
 * fixed an int/char
 *
 * Revision 1.4  1992/10/11  08:49:33  smilie
 * added IGNORECARRIER to change CONFIG.carrier flag
 *
 * Revision 1.3  1992/10/09  14:29:49  smilie
 * added the timeout to configuration
 *
 * Revision 1.2  1992/10/09  13:43:36  smilie
 * fixed some warnings
 *
 * Revision 1.1  1992/10/09  13:18:22  smilie
 * Initial revision
 *

*************************************************************************/

/* Feature test switches */
#define _POSIX_SOURCE 1
#define _SETUP_C

/* System Headers */
#include <stdio.h>
#include <string.h>

/* Local Headers */
#include "modem.h"

/* Macros */

/* File scope variables */

static char setup_rcsid[] = "$Id: setup.c,v 1.5 1992/12/06 06:52:20 smilie Exp $";
#define RCSID setup_rcsid

/* External variables */

/* External Functions */

/* Structures and unions */

/* Functions */

/*************************************************************************
				CHECK_CONFIG
*************************************************************************/
int check_config()
{
int ret = 1;
/**/
if (CONFIG.speed == -1)
	{
	flog("CONFIG: no speed. eg/ 'SPEED 19200'\n");
	ret = 0;
	}
if (CONFIG.quiet)
	{
	if (CONFIG.quiet_start == -1)
		{
		flog("CONFIG: no Quiet Start time. eg/ 'QUIET_TIMEON 22'\n");
		ret = 0;
		}
	if (CONFIG.quiet_stop == -1)
		{
		flog("CONFIG: no Quiet Stop time. eg/ 'QUIET_TIMEOFF 2'\n");
		ret = 0;
		}
	if (CONFIG.quiet_str_on[0] == 0)
		{
		flog("CONFIG: no Quiet Start String. eg/ 'QUIET_STRON ATM0|'\n");
		ret = 0;
		}
	if (CONFIG.quiet_str_off[0] == 0)
		{
		flog("CONFIG: no Quiet Stop String. eg/ 'QUIET_STROFF ATM1|'\n");
		ret = 0;
		}
	}

return ret;
}
/************************************************************************
			PARSE_CONFIG_STRING
-------------------------------------------------------------------------
Parses the Config Strings, setting the correct internal
configuration structures
*************************************************************************/
void parse_config_string(const char *tststring)
{
char tmps[100];
/**/
if (strncasecmp(tststring, "SPEED", 5) == 0)
	sscanf(tststring, "%s %u", tmps, &CONFIG.speed);
else
if (strncasecmp(tststring, "LOCKED", 6) == 0)
	CONFIG.locked = 1;
else
if (strncasecmp(tststring, "IGNORECARRIER", 13) == 0)
	CONFIG.carrier = 0;
else
if (strncasecmp(tststring, "INIT", 4) == 0)
	sscanf(tststring, "%s %[^_]", tmps, CONFIG.init);
else
if (strncasecmp(tststring, "QUIET_STRON", 11) == 0)
	{
	sscanf(tststring, "%s %[^_]", tmps, CONFIG.quiet_str_on);
	CONFIG.quiet = 1;
	}
else
if (strncasecmp(tststring, "QUIET_STROFF", 12) == 0)
	{
	sscanf(tststring, "%s %[^_]", tmps, CONFIG.quiet_str_off);
	CONFIG.quiet = 1;
	}	
else
if (strncasecmp(tststring, "QUIET_TIMEON", 12) == 0)
	{
	sscanf(tststring, "%s %u", tmps, &CONFIG.quiet_start);
	CONFIG.quiet = 1;
	}
else
if (strncasecmp(tststring, "QUIET_TIMEOFF", 13) == 0)
	{
	sscanf(tststring, "%s %u", tmps, &CONFIG.quiet_stop);
	CONFIG.quiet = 1;
	}
else
if (strncasecmp(tststring, "TIMEOUT", 7) == 0)
	{
	sscanf(tststring, "%s %hu", tmps, &CONFIG.timeout);
	CONFIG.quiet = 1;
	}	
}
/************************************************************************
			LOAD_ACONFIG
-------------------------------------------------------------------------
Loads a Config given the file name
*************************************************************************/
int load_aconfig(const char *filenam)
{
FILE *fh;
char tmpstr[100];
/**/

if ((fh = fopen(filenam, "rt")) == NULL)
	{
	#ifdef DEBUG
	perror(filenam);
	#endif
	return FALSE;
	}
while(!feof(fh))
	{
	fgetstr(tmpstr, sizeof(tmpstr), fh);
	if ((tmpstr[0] != 0) && (tmpstr[0] != '#'))
		{
		parse_config_string(tmpstr);
		}
	}
fclose(fh);
return TRUE;
}
/*************************************************************************
				LOAD_CONFIG
*************************************************************************/
int load_config()
{
char tmpf[256];
/**/
CONFIG.locked = 0;
CONFIG.speed = -1;
CONFIG.conspeed = 0;
CONFIG.init[0] = 0;
CONFIG.quiet = 0;
CONFIG.quiet_str_on[0] = 0;
CONFIG.quiet_str_off[0] = 0;
CONFIG.quiet_start = -1;
CONFIG.quiet_stop = -1;
CONFIG.timeout = 10;		/* Set CR timeout to 10 seconds */
CONFIG.carrier = 1;		/* Default to carrier checking */

if (!load_aconfig("/etc/default/modem"))
	{
	flog("load_config: ERROR cannot load /etc/default/modem\n");
	exit(1);
	}
sprintf(tmpf, "/etc/default/%s", stty);
(void)load_aconfig(tmpf);

if (check_config())
	return TRUE;

return FALSE;
}


