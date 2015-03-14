/*
 * xdm - display manager daemon
 *
 * $XConsortium: resource.c,v 1.29 89/12/15 20:12:26 keith Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * resource.c
 */

# include "dm.h"
# include <X11/Xlib.h>
# include <X11/Xresource.h>
# include <X11/Xmu/CharSet.h>

/* XtOffset() hack for ibmrt BandAidCompiler */

char	*config;

char	*servers;
int	request_port;
int	debugLevel;
char	*errorLogFile;
int	daemonMode;
char	*pidFile;
int	lockPidFile;
char	*remoteAuthDir;
int	autoRescan;
int	removeDomainname;
char	*keyFile;

# define DM_STRING	0
# define DM_INT		1
# define DM_BOOL	2

/*
 * the following constants are supposed to be set in the makefile from
 * parameters set util/imake.includes/site.def (or *.macros in that directory
 * if it is server-specific).  DO NOT CHANGE THESE DEFINITIONS!
 */
#ifndef DEF_SERVER_LINE 
#define DEF_SERVER_LINE ":0 local /usr/bin/X11/X :0"
#endif
#ifndef XRDB_PROGRAM
#define XRDB_PROGRAM "/usr/bin/X11/xrdb"
#endif
#ifndef DEF_SESSION
#define DEF_SESSION "/usr/bin/X11/xterm -ls"
#endif
#ifndef DEF_USER_PATH
#define DEF_USER_PATH ":/bin:/usr/bin:/usr/bin/X11:/usr/ucb"
#endif
#ifndef DEF_SYSTEM_PATH
#define DEF_SYSTEM_PATH "/etc:/bin:/usr/bin:/usr/bin/X11:/usr/ucb"
#endif
#ifndef DEF_SYSTEM_SHELL
#define DEF_SYSTEM_SHELL "/bin/sh"
#endif
#ifndef DEF_FAILSAFE_CLIENT
#define DEF_FAILSAFE_CLIENT "/usr/bin/X11/xterm"
#endif
#ifndef DEF_XDM_CONFIG
#define DEF_XDM_CONFIG "/usr/lib/X11/xdm/xdm-config"
#endif
#ifndef CPP_PROGRAM
#define CPP_PROGRAM "/lib/cpp"
#endif
#ifndef DEF_XDM_AUTH_GEN
#define DEF_XDM_AUTH_GEN "/usr/lib/X11/xdm/xdmauthgen"
#endif
#ifndef DEF_AUTH_FILE
#define DEF_AUTH_FILE	"/usr/lib/X11/xdm/auth-server"
#endif
#ifndef DEF_AUTH_NAME
#define DEF_AUTH_NAME	"MIT-MAGIC-COOKIE-1"
#endif
#ifndef DEF_REMOTE_AUTH_DIR
#define DEF_REMOTE_AUTH_DIR "/usr/lib/X11/xdm"
#endif
#ifndef DEF_AUTH_DIR
#define DEF_AUTH_DIR	"/tmp"
#endif
#ifndef DEF_KEY_FILE
#define DEF_KEY_FILE	"/usr/lib/X11/xdm/Xkeys"
#endif
#define DEF_UDP_PORT	"177"	    /* registered XDMCP port, dont change */

struct dmResources {
	char	*name, *class;
	int	type;
	char	**dm_value;
	char	*default_value;
} DmResources[] = {
"servers",	"Servers", 	DM_STRING,	&servers,
				DEF_SERVER_LINE,
"requestPort",	"RequestPort",	DM_INT,		(char **) &request_port,
				DEF_UDP_PORT,
"debugLevel",	"DebugLevel",	DM_INT,		(char **) &debugLevel,
				"0",
"errorLogFile",	"ErrorLogFile",	DM_STRING,	&errorLogFile,
				"",
"daemonMode",	"DaemonMode",	DM_BOOL,	(char **) &daemonMode,
				"true",
"pidFile",	"PidFile",	DM_STRING,	&pidFile,
				"",
"lockPidFile",	"LockPidFile",	DM_BOOL,	(char **) &lockPidFile,
				"true",
"remoteAuthDir","RemoteAuthDir",DM_STRING,	&remoteAuthDir,
				DEF_REMOTE_AUTH_DIR,
"autoRescan",	"AutoRescan",	DM_BOOL,	(char **) &autoRescan,
				"true",
"removeDomainname","RemoveDomainname",DM_BOOL,	(char **) &removeDomainname,
				"true",
"keyFile",	"KeyFile",	DM_STRING,	&keyFile,
				DEF_KEY_FILE,
};

# define NUM_DM_RESOURCES	(sizeof DmResources / sizeof DmResources[0])

# define boffset(f)	((char *) &(((struct display *) 0)->f) - (char *) 0)

struct displayResources {
	char	*name, *class;
	int	type;
	int	offset;
	char	*default_value;
} DisplayResources[] = {
"resources",	"Resources",	DM_STRING,	boffset(resources),
				"",
"xrdb",		"Xrdb",		DM_STRING,	boffset(xrdb),
				XRDB_PROGRAM,
"cpp",		"Cpp",		DM_STRING,	boffset(cpp),
				CPP_PROGRAM,
"startup",	"Startup",	DM_STRING,	boffset(startup),
				"",
"reset",	"Reset",	DM_STRING,	boffset(reset),
				"",
"session",	"Session",	DM_STRING,	boffset(session),
				DEF_SESSION,
"openDelay",	"OpenDelay",	DM_INT,		boffset(openDelay),
				"15",
"openRepeat",	"OpenRepeat",	DM_INT,		boffset(openRepeat),
				"5",
"openTimeout",	"OpenTimeout",	DM_INT,		boffset(openTimeout),
				"30",
"startAttempts","StartAttempts",DM_INT,		boffset(startAttempts),
				"4",
"pingInterval",	"PingInterval",	DM_INT,		boffset(pingInterval),
				"5",
"pingTimeout",	"PingTimeout",	DM_INT,		boffset(pingTimeout),
				"5",
"terminateServer","TerminateServer",DM_BOOL,	boffset(terminateServer),
				"false",
"userPath",	"Path",		DM_STRING,	boffset(userPath),
				DEF_USER_PATH,
"systemPath",	"Path",		DM_STRING,	boffset(systemPath),
				DEF_SYSTEM_PATH,
"systemShell",	"Shell",	DM_STRING,	boffset(systemShell),
				DEF_SYSTEM_SHELL,
"failsafeClient","FailsafeClient",	DM_STRING,	boffset(failsafeClient),
				DEF_FAILSAFE_CLIENT,
"grabServer",	"GrabServer",	DM_BOOL,	boffset(grabServer),
				"true",
"grabTimeout",	"GrabTimeout",	DM_INT,		boffset(grabTimeout),
				"3",
"authorize",	"Authorize",	DM_BOOL,	boffset(authorize),
				"true",
"authName",	"AuthName",	DM_STRING,	boffset(authName),
				DEF_AUTH_NAME,
"authFile",	"AuthFile",	DM_STRING,	boffset(authFile),
				DEF_AUTH_FILE,
"resetForAuth",	"ResetForAuth",	DM_BOOL,	boffset(resetForAuth),
				"false",
"userAuthDir",	"UserAuthDir",	DM_STRING,	boffset(userAuthDir),
				DEF_AUTH_DIR,
};

# define NUM_DISPLAY_RESOURCES	(sizeof DisplayResources/\
				 sizeof DisplayResources[0])

XrmDatabase	DmResourceDB;

GetResource (name, class, valueType, valuep, default_value)
    char    *name, *class;
    int	    valueType;
    char    **valuep;
    char    *default_value;
{
    char	*type;
    XrmValue	value;
    char	*string, *new_string, *strncpy (), *malloc ();
    char	str_buf[50];
    int	len;

    if (valueType == DM_STRING && *valuep)
	return;

    if (DmResourceDB && XrmGetResource (DmResourceDB,
	name, class,
	&type, &value))
    {
	string = value.addr;
	len = value.size;
    }
    else
    {
	string = default_value;
	len = strlen (string);
    }
    Debug ("%s/%s value %s\n", name, class, string);
    switch (valueType) {
    case DM_STRING:
	new_string = malloc ((unsigned) (len+1));
	if (!new_string) {
		LogOutOfMem ("GetResource");
		return;
	}
	strncpy (new_string, string, len);
	new_string[len] = '\0';
	*(valuep) = new_string;
	break;
    case DM_INT:
	strncpy (str_buf, string, sizeof (str_buf));
	str_buf[sizeof (str_buf)-1] = '\0';
	*((int *) valuep) = atoi (str_buf);
	break;
    case DM_BOOL:
	strncpy (str_buf, string, sizeof (str_buf));
	str_buf[sizeof (str_buf)-1] = '\0';
	XmuCopyISOLatin1Lowered (str_buf, str_buf);
	if (!strcmp (str_buf, "true") ||
	    !strcmp (str_buf, "on") ||
	    !strcmp (str_buf, "yes"))
		*((int *) valuep) = 1;
	else if (!strcmp (str_buf, "false") ||
		 !strcmp (str_buf, "off") ||
		 !strcmp (str_buf, "no"))
		*((int *) valuep) = 0;
	break;
    }
}

XrmOptionDescRec configTable [] = {
{"-server",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-udpPort",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-error",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-resources",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-session",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-debug",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-xrm",	NULL,			XrmoptionSkipArg,	(caddr_t) NULL },
{"-config",	".configFile",		XrmoptionSepArg,	(caddr_t) NULL }
};

XrmOptionDescRec optionTable [] = {
{"-server",	".servers",		XrmoptionSepArg,	(caddr_t) NULL },
{"-udpPort",	".requestPort",		XrmoptionSepArg,	(caddr_t) NULL },
{"-error",	".errorLogFile",	XrmoptionSepArg,	(caddr_t) NULL },
{"-resources",	"*resources",		XrmoptionSepArg,	(caddr_t) NULL },
{"-session",	"*session",		XrmoptionSepArg,	(caddr_t) NULL },
{"-debug",	"*debugLevel",		XrmoptionSepArg,	(caddr_t) NULL },
{"-xrm",	NULL,			XrmoptionResArg,	(caddr_t) NULL },
{"-daemon",	".daemonMode",		XrmoptionNoArg,		"true"         },
{"-nodaemon",	".daemonMode",		XrmoptionNoArg,		"false"        }
};

static int	originalArgc;
static char	**originalArgv;

InitResources (argc, argv)
int	argc;
char	**argv;
{
	XrmInitialize ();
	originalArgc = argc;
	originalArgv = argv;
	ReinitResources ();
}

ReinitResources ()
{
    int	argc;
    char	**a;
    char	**argv;
    XrmDatabase newDB;

    argv = (char **) malloc ((originalArgc + 1) * sizeof (char *));
    if (!argv)
	LogPanic ("no space for argument realloc\n");
    for (argc = 0; argc < originalArgc; argc++)
	argv[argc] = originalArgv[argc];
    argv[argc] = 0;
    if (DmResourceDB)
	XrmDestroyDatabase (DmResourceDB);
    DmResourceDB = XrmGetStringDatabase ("");
    /* pre-parse the command line to get the -config option, if any */
    XrmParseCommand (&DmResourceDB, configTable,
		     sizeof (configTable) / sizeof (configTable[0]),
		     "DisplayManager", &argc, argv);
    GetResource ("DisplayManager.configFile", "DisplayManager.ConfigFile",
		 DM_STRING, &config, DEF_XDM_CONFIG);
    newDB = XrmGetFileDatabase ( config );
    if (newDB)
    {
	if (DmResourceDB)
	    XrmDestroyDatabase (DmResourceDB);
	DmResourceDB = newDB;
    }
    else if (argc != originalArgc)
	LogError ("Can't open configuration file %s\n", config );
    XrmParseCommand (&DmResourceDB, optionTable,
		     sizeof (optionTable) / sizeof (optionTable[0]),
		     "DisplayManager", &argc, argv);
    if (argc > 1)
    {
	LogError ("extra arguments on command line:");
	for (a = argv + 1; *a; a++)
		LogError (" \"%s\"", *a);
	LogError ("\n");
    }
    free (argv);
}

LoadDMResources ()
{
	int	i;
	char	name[1024], class[1024];

	for (i = 0; i < NUM_DM_RESOURCES; i++) {
		sprintf (name, "DisplayManager.%s", DmResources[i].name);
		sprintf (class, "DisplayManager.%s", DmResources[i].class);
		GetResource (name, class, DmResources[i].type,
			      (char **) DmResources[i].dm_value,
			      DmResources[i].default_value);
	}
}

static
CleanUpName (src, dst, len)
char	*src, *dst;
int	len;
{
    while (*src) {
	if (--len <= 0)
		break;
	switch (*src)
	{
	case ':':
	case '.':
	    *dst++ = '_';
	    break;
	default:
	    *dst++ = *src;
	}
	++src;
    }
    *dst = '\0';
}

LoadDisplayResources (d)
struct display	*d;
{
	int	i;
	char	name[1024], class[1024];
	char	dpyName[512], dpyClass[512];

	CleanUpName (d->name, dpyName, sizeof (dpyName));
	CleanUpName (d->class ? d->class : d->name, dpyClass, sizeof (dpyClass));
	for (i = 0; i < NUM_DISPLAY_RESOURCES; i++) {
		sprintf (name, "DisplayManager.%s.%s", 
			dpyName, DisplayResources[i].name);
		sprintf (class, "DisplayManager.%s.%s",
			dpyClass, DisplayResources[i].class);
		GetResource (name, class, DisplayResources[i].type,
			      (char **) (((char *) d) + DisplayResources[i].offset),
			      DisplayResources[i].default_value);
	}
}
