/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)defaults.c	26.11	91/09/14 SMI"

#ifdef SYSV
#include <sys/types.h>
#include <unistd.h>
#endif

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#include <sys/file.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

char	*getenv();

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "defaults.h"
#include "globals.h"
#include "resources.h"

extern	char	*ProgramName;

/* private data */
static void parseResourceDefaults();
static void parseApplicationDefaults();

/* 
 * GetDefaults - Set up global OlwmDB with built-in default values.
 *		Then, initialize all the appropriate olwm variables
 *		using the information in the OlwmDB
 */
void
GetDefaults(dpy, commandlineDB)
Display		*dpy;
XrmDatabase	commandlineDB;
{
	/* put resource settings from default files into global OlwmDB */
	parseApplicationDefaults();
	parseResourceDefaults( dpy );

	/* merge command line options last into global OlwmDB
	 * notice that commandlineDB is destroyed by this call!
	 */
	if ( commandlineDB != NULL )
		(void) XrmMergeDatabases( commandlineDB, &OlwmDB );

	InitGlobals(dpy);
}


/* 
 * parseApplicationDefaults - get resource settings from application 
 *	default file
 */
static void
parseApplicationDefaults()
{
	XrmDatabase	applicationDB = NULL;
	char		filename[1024];
        char            *openWinPath;
	Bool		notFound = False;

        /* look for application default file */
	if ( (openWinPath = getenv( "OPENWINHOME" )) != NULL )
	{
		(void) strcpy( filename, openWinPath );
		(void) strcat( filename, "/lib/app-defaults/" );
		(void) strcat( filename, ProgramName );
		if ( access( filename, R_OK ) != 0 )
			notFound = True;
	}
        else
		notFound = True;

	if ( notFound )
	{
        	(void) strcpy( filename, "/usr/lib/X11/app-defaults/" );
		(void) strcat( filename, ProgramName );
	}

        applicationDB = XrmGetFileDatabase( filename );
	if ( applicationDB != NULL )
        	(void) XrmMergeDatabases( applicationDB, &OlwmDB );
}

/* 
 * parseResourceDefaults - get resource settings from default user locations.
 *	Look first for server property - if it's there, use it alone.
 *	If it's not there, look for XENVIRONMENT file and use it.
 *	If still no luck, use $HOME/.Xdefaults.
 */
static void
parseResourceDefaults( dpy )
Display	*dpy;
{
	XrmDatabase	serverDB = NULL, homeDB = NULL;
	char		filename[1024];
	unsigned long	nitems, remain;
	char		*resourceString;
	char		*environmentFile;

	/* look for RESOURCE_MANAGER property on root window of screen 0 */
        resourceString = GetWindowProperty(dpy, RootWindow(dpy, 0),
                XA_RESOURCE_MANAGER, 0L, 100000000L,
                /* REMIND what should the length really be ? */
                XA_STRING, 0, &nitems, &remain);
        if ((resourceString != NULL) && (nitems != 0))
	{
		serverDB = XrmGetStringDatabase( resourceString );
		(void) XrmMergeDatabases( serverDB, &OlwmDB );
		XFree( resourceString );
		return;
	}

	/* if unsuccessful, look for user defaults: 
	 * first try XENVIRONMENT, next in $HOME/.Xdefaults 
	 */
	if ( ( (environmentFile = getenv( "XENVIRONMENT" )) == NULL )
	     || ( access( environmentFile, R_OK ) != 0 ) )
	{
		if ( (environmentFile = getenv( "HOME" )) != NULL )
		{
			(void) strcpy( filename, environmentFile );
			(void) strcat( filename, "/" );
			(void) strcat( filename, ".Xdefaults" );
			environmentFile = filename;
		}
	}
	if ( environmentFile != NULL )
	{
		homeDB = XrmGetFileDatabase( environmentFile );
		(void) XrmMergeDatabases( homeDB, &OlwmDB );
	}
	return;
}
