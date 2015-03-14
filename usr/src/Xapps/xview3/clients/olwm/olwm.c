/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)olwm.c	26.42	91/09/14 SMI"

#include <errno.h>
#include <memory.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>

#include <sys/time.h>
#include <sys/types.h>

#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#include "i18n.h"
#include "ollocale.h"
#include "events.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "defaults.h"
#include "resources.h"
#include "globals.h"
#include "group.h"
#include "slots.h"

typedef	void	(*VoidFunc)();

/*
 * Globals
 */
char	*ProgramName;
char	*AppName;		/* strictly last component of ProgramName */
GlobalResourceVariables GRV;
XrmDatabase OlwmDB;


/*
 * Global Quarks.  "Top" refers to the root of the resource name/instance 
 * hierarchy.
 */
XrmQuark TopClassQ;
XrmQuark TopInstanceQ;
XrmQuark OpenWinQ;

/* Current Display */
Display	*DefDpy;


static Display *openDisplay();
static void	parseCommandline();
static void	sendSyncSignal();


/* Note that changes to RMTable instance names must also be made here !! */
static	XrmOptionDescRec	optionTable[] = {
	{ "-display",		".display",	
		XrmoptionSepArg, (caddr_t) NULL },
	{ "-f",			".setInput",
		XrmoptionNoArg, (caddr_t) "follow" },
	{ "-follow",		".setInput", 
		XrmoptionNoArg, (caddr_t) "follow" },
	{ "-c", 		".setInput",	
		XrmoptionNoArg, (caddr_t) "select" },
	{ "-click", 		".setInput",	
		XrmoptionNoArg, (caddr_t) "select" },
	{ "-2d", 		".use3D",	
		XrmoptionNoArg, (caddr_t) "False" },
	{ "-3d", 		".use3D",	
		XrmoptionNoArg, (caddr_t) "True" },
	{ "-parent", 		".reparentOK",	
		XrmoptionNoArg, (caddr_t) "False" },
	{ "-orphans", 		".printOrphans", 
		XrmoptionNoArg, (caddr_t) "True" },
	{ "-all", 		".printAll", 
		XrmoptionNoArg, (caddr_t) "True" },
	{ "-synchronize",	".synchronize",
		XrmoptionNoArg, (caddr_t) "True" },
	{ "-debug",	".printOrphans", XrmoptionNoArg, (caddr_t) "True" },
	{ "-name",		".name",	
		XrmoptionSepArg, (caddr_t) NULL },
	{ "-bg",	"*Background",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-background", "*Background",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-fg",	"*Foreground",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-foreground","*Foreground",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-bd",	"*Background",	
		XrmoptionSepArg, (caddr_t) NULL },
	{ "-bordercolor",	"*Background",	
		XrmoptionSepArg, (caddr_t) NULL },
	{ "-fn",	"*TitleFont",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-font",	"*TitleFont",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-xrm",	NULL,		XrmoptionResArg, (caddr_t) NULL },
	{ "-single",	".singleScreen",XrmoptionNoArg, (caddr_t) "True" },
	{ "-multi",	".singleScreen",XrmoptionNoArg, (caddr_t) "False" },
	{ "-syncpid",   ".syncPid",     XrmoptionSepArg, (caddr_t) NULL },
	{ "-syncsignal",".syncSignal",  XrmoptionSepArg, (caddr_t) NULL },
	{ "-depth",	"*depth",	XrmoptionSepArg, (caddr_t) NULL },
	{ "-visual",	"*visual",	XrmoptionSepArg, (caddr_t) NULL },
#ifdef OW_I18N_L3
        { "-basiclocale", "*basicLocaleCL", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-displaylang", "*displayLangCL", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-inputlang",   "*inputLangCL", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-numeric",     "*numericCL", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-dateFormat",  "*dateFormatCL", XrmoptionSepArg, (caddr_t) NULL },
#endif /* OW_I18N_L3 */
};
#define OPTION_TABLE_ENTRIES (sizeof(optionTable)/sizeof(XrmOptionDescRec))

static void	initWinClasses();

/* Child Process Handling */
void HandleChildSignal();
void ReapChildren();
int DeadChildren = 0;
int SlavePid;

#ifdef ALLPLANES
int allplanes;			/* server supports the ALLPLANES extension */
#endif

#ifdef SHAPE
Bool ShapeSupported;		/* server supports the SHAPE extension */
int  ShapeEventBase;
int  ShapeErrorBase;
#endif


static char	**argVec;

/*
 * main	-- parse arguments, perform initialization, call event-loop
 *
 * The OpenLook window manager was written before the OpenLook
 * spec. was completed.  Because of this, some of it works in
 * an awkward way.  Specifically, the way a window's focus and
 * select state is shown.
 *
 * For example, when a window is focused in click-to-focus mode
 * the header gets highlighted, (black background, white text).
 * As it was written, the titlebar window sits just inside of the 
 * resize corners.  Because the OpenLook spec. requires the header to 
 * be highlighted in line with the resize corners, blacking in the
 * the background of the titlebar window is one pixel short in each
 * direction of being a big enough highlighted area.  We can't make
 * the titlebar bigger because it would then overlap the resize corners.
 * Now that the spec. is complete, OLWM should be restructured.
 *
 * Overview:
 *
 *	Upon startup OLWM reparents all the client windows unless
 *	the user has told it otherwise (-p option).
 * 	OLWM creates a frame which will contain the client window (the pane)
 *  	and the decoration windows (resize corners, titlebar).  The
 *	titlebar window contains the pushpin, if any.
 *
 *	After creating the decorations for the windows OLWM sits
 * 	in a forever loop getting the next event and calling the
 * 	corresponding window's event handling routine.
 *
 *	Each window has associated with it an event handling 
 * 	routine (EventFrame, EventPushPin) which the main EventLoop
 *	will call when an event happens in that window.  The event
 *	handling routine will call the proper routines to move a 
 *	window, create a menu, etc.
 */
main(argc, argv)
	int argc;
	char **argv;
{
	int			ErrorHandler();
	int			ExitOLWM();
	XrmDatabase		commandlineDB = NULL;
	char			*dpystr;

#ifdef OW_I18N_L3
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif
	char			*OpenWinHome;
	char			locale_dir[MAXPATHLEN+1];
	extern char		*getenv();
#endif /* OW_I18N_L3 */

#ifdef MALLOCDEBUG
	malloc_debug(MALLOCDEBUG);
#endif /* MALLOCDEBUG */
#ifdef GPROF_HOOKS
	moncontrol(0);
#endif /* GPROF_HOOKS */

#ifdef OW_I18N_L3
       	/*
       	 * Even in the SUNDAE1.0 (first release) we might need the
       	 * dynamic locale change for window manager, since window
       	 * manager is usually never re-start again in one sesstion.
       	 * But leave static for now.
       	 */
       	/*
       	 * We are setting the locale (issuing the setlocale) by
       	 * EffectOLLC() function, but we need to call setlocale here
       	 * to handle command line argument with certain locale.
       	 * FIX_ME! But may not work well, because we did not touch the
       	 * Xlib function XrmParseCommand().
       	 */
       	if (setlocale(LC_ALL, "") == NULL) {
		/*
		 * FIX_ME: Following message may be misleading,
		 * because setlocale will retrieve more than just a
		 * $LANG environment variable (such as $LC_CTYPE).
		 * Also, later if the resource DB has a locale
		 * infomation, we will use it. Hence, this setting is
		 * may be only for the command line processing.
		 */
		fprintf(stderr, gettext("\
		olwm: Warning: '%s' is invalid locale, using 'C' locale.\n"),
				getenv("LANG"));
		(void)setlocale(LC_ALL,"C");
	}
	if ((OpenWinHome = getenv("OPENWINHOME")) != 0)
		(void)strcpy(locale_dir,OpenWinHome);
	else
		(void)strcpy(locale_dir,"/usr");
	(void)strcat(locale_dir,"/lib/locale");
	bindtextdomain("olwm_messages",locale_dir);
       	textdomain("olwm_messages");
#endif /* OW_I18N_L3 */

	ProgramName = argv[0];
	argVec = argv;

	/* Set up a signal handler so that if we are killed we put
	 * the windows back into a usable state.  Specifically we
	 * need to reset the focus so they will be able to get input.
	 */
	signal(SIGHUP, (VoidFunc)ExitOLWM);
	signal(SIGINT, (VoidFunc)ExitOLWM);
	signal(SIGTERM, (VoidFunc)ExitOLWM);
	signal(SIGCHLD, HandleChildSignal);

	/* initialize the resource manager */
	XrmInitialize();

	/* parse the command line arguments into local tmp DB */
	parseCommandline( &argc, argv, &commandlineDB );

	DefDpy = openDisplay(commandlineDB);

#ifdef ALLPLANES
	{
	    int tmp;
	    allplanes = XAllPlanesQueryExtension(DefDpy, &tmp, &tmp);
	}
#endif /* ALLPLANES */


#ifdef SHAPE
	ShapeSupported = XShapeQueryExtension(DefDpy, &ShapeEventBase,
					      &ShapeErrorBase);
#endif /* SHAPE */

	/* put all resources into global OlwmDB and set olwm variables */
	GetDefaults(DefDpy, commandlineDB);

	/* Initialize the event handling system. */
	InitEvents(DefDpy);
	InitBindings(DefDpy);

	/* Set up the error handling function. */
	XSetErrorHandler(ErrorHandler);

	/* this can be useful for debuging strange client behaivior */
	if (GRV.Synchronize)
		XSynchronize(DefDpy, True);

	/* Initialize the atoms we will need to communicate with
	 * Open Look clients.
	 */
	InitAtoms(DefDpy);

	/* Initialize the database. */
	WIInit( DefDpy );

	/* Initialise all types of window classes */
	initWinClasses(DefDpy);

	/* Initialize client led state */
	InitClientState(DefDpy);

	/* initialise the group module */
	GroupInit();

	/*
	 * Force child processes to disinherit the TCP file descriptor.
	 * This helps shell commands forked and exec'ed from menus
	 * to work properly.
	 */
	if (fcntl(ConnectionNumber(DefDpy), F_SETFD, 1) == -1)
	{
		perror( gettext("olwm: child cannot disinherit TCP fd") );
		exit( -1 );
	}

#ifdef OW_I18N_L3
	EffectOLLC(DefDpy);
#endif /* OW_I18N_L3 */
	/* Init the global menus */
	InitMenus(DefDpy);

	/* init region handling code */
	InitRegions();

	/* Init screen */
	InitScreens(DefDpy);
	GrabKeys(DefDpy, True);
	GrabButtons(DefDpy, True);
	ReparentScreens(DefDpy);
	if (!GRV.FocusFollowsMouse)
	    ClientFocusTopmost(DefDpy, GetFirstScrInfo(), CurrentTime);

	/*
 	 * Start olwmslave - using the same args we got.
	 */
	if (GRV.RunSlaveProcess)
	    SlavePid = SlaveStart(argVec);

	if (GRV.Beep != BeepNever)
	    XBell(DefDpy, 100);

	sendSyncSignal();

	EventLoop( DefDpy );

	/*NOTREACHED*/
}


/* 
 * parseCommandline - parse the command line arguments into a resource
 *	database
 */
static void
parseCommandline( argc, argv, tmpDB )
int		*argc;
char		*argv[];
XrmDatabase	*tmpDB;
{
	char	instName[MAX_NAME];
	char	namestr[MAX_NAME];
	char	*type, *p;
	XrmValue val;

	/* Extract trailing pathname component of argv[0] into AppName. */

	AppName = strrchr(argv[0], '/');
	if (AppName == NULL)
	    AppName = argv[0];
	else
	    ++AppName;

	XrmParseCommand(tmpDB, optionTable, OPTION_TABLE_ENTRIES,
			AppName, argc, argv );

	/*
	 * Initialize root instance and class quarks.  Create the instance
	 * name by first looking up the "name" resource in the command line
	 * database (for the -name option).  If it's not present, use AppName
	 * (the trailing pathname component of argv[0]).  Then, scan it and
	 * replace all illegal characters with underscores.  Note: we don't
	 * use the ctype functions here, because they are internationalized.
	 * In some locales, isalpha() will return true for characters that are
	 * not valid in resource component names.  Thus, we must fall back to
	 * standard character comparisions.
	 *
	 * REMIND: specifying the -name option changes the name with which 
	 * resources are looked up.  But the command line options were put 
	 * into the database using AppName, which is based on argv[0].  Thus, 
	 * specifying -name causes all command-line args to be ignored, which 
	 * is wrong.
	 */

	(void) strcpy(namestr, AppName);
	(void) strcat(namestr, ".name");
	if (XrmGetResource(*tmpDB, namestr, namestr, &type, &val)) {
	    (void) strncpy(instName, (char *)val.addr, MAX_NAME);
	} else {
	    (void) strncpy(instName, AppName, MAX_NAME);
	}

	instName[MAX_NAME-1] = '\0';
	for (p=instName; *p != '\0'; ++p) {
	    if (!(*p >= 'a' && *p <= 'z' ||
		  *p >= 'A' && *p <= 'Z' ||
		  *p >= '0' && *p <= '9' ||
		  *p == '_' || *p == '-')) {
		*p = '_';
	    }
	}
	TopInstanceQ = XrmStringToQuark(instName);
	TopClassQ = XrmStringToQuark("Olwm");
	OpenWinQ = XrmStringToQuark("OpenWindows");

	/* check to see if there are any arguments left unparsed */
	if ( *argc != 1 )
	{
		/* check to see if it's -help */
		if ( argv[1][0] == '-' && argv[1][1] == 'h' )
			usage(gettext("Command line arguments accepted"),gettext("are:"));
		else
			usage(gettext("Unknown argument(s)"), gettext("encountered"));
	}
}


/*
 * openDisplay
 */
static Display *
openDisplay(rdb)
    XrmDatabase rdb;
{
    char namebuf[MAX_NAME];
    char *type;
    XrmValue value;
    char *dpystr = NULL;
    char *envstr;
    Display *dpy;

    (void) strcpy(namebuf, AppName);
    (void) strcat(namebuf, ".display");

    if (XrmGetResource(rdb, namebuf, namebuf, &type, &value)) {
	dpystr = (char *)value.addr;
	envstr = (char *)MemAlloc(8+strlen(dpystr)+1);
	sprintf(envstr, "DISPLAY=%s", dpystr);
	putenv(envstr);
    }

    dpy = XOpenDisplay(dpystr);
    if (dpy == NULL) {
	fprintf(stderr, gettext("%s: cannot connect to %s\n"),
		ProgramName, dpystr);
	exit(1);
    }
    return dpy;
}


/*
 * sendSyncSignal
 *
 * Send a signal to the process named on the command line (if any).  Values
 * for the process id and signal to send are looked up in the resource 
 * database; they are settable with command-line options.  The resources are 
 * looked up with the names
 * 
 *	<appname>.syncPid		process id
 *	<appname>.syncSignal		signal to send (integer)
 *
 * where <appname> is the trailing pathname component of argv[0].
 */
static void
sendSyncSignal()
{
    char *type;
    XrmValue value;
    int pid;
    int sig = SIGALRM;
    int tmp;
    char namebuf[100];

    (void) strcpy(namebuf, AppName);
    (void) strcat(namebuf, ".syncPid");
    if (!XrmGetResource(OlwmDB, namebuf, namebuf, &type, &value))
	return;
    pid = atoi((char *)value.addr);
    if (pid <= 0 || pid > MAXPID)
	return;

    (void) strcpy(namebuf, AppName);
    (void) strcat(namebuf, ".syncSignal");
    if (XrmGetResource(OlwmDB, namebuf, namebuf, &type, &value)) {
	tmp = atoi((char *)value.addr);
	if (tmp > 0 && tmp <= SIGUSR2)
	    sig = tmp;
    }
    (void) kill(pid, sig);
}
 

static void
initWinClasses(dpy)
Display *dpy;
{
	FrameInit(dpy);
	IconInit(dpy);
	ResizeInit(dpy);
	ColormapInit(dpy);
	ButtonInit(dpy);
	BusyInit(dpy);
	MenuInit(dpy);
	PinMenuInit(dpy);
	RootInit(dpy);
	NoFocusInit(dpy);
	PushPinInit(dpy);
	PaneInit(dpy);
	IconPaneInit(dpy);
}


/*
 * Exit -- exit routine called by menus.
 *	Kill all running applications.
 */
Exit(dpy)
Display	*dpy;
{
	extern void *ClientShutdown();
	
	SlaveStop();
	ListApply(ActiveClientList, ClientShutdown, (void *)0);
	XSync(dpy, True);
	exit(0);
	/*NOTREACHED*/
}


static void
cleanup()
{
	extern void *UnparentClient();

	/*
 	 * If DefDpy is NULL then we didn't get to the XOpenDisplay()
	 * so basically there is nothing to clean up so return.
	 */
	if (DefDpy == NULL)
		return;

	/*
	 * Stop olwmslave
 	 */
	SlaveStop();

	/*
	 * destroy all pinned menus
	 */
	DestroyPinnedMenuClients();

	/*
	 * Go through the list of windows.  Unmap all icons that are on the
	 * screen.  Reparent all windows back to the root, suitably offset
	 * according to their window-gravities.  Also remap all non-withdrawn
	 * windows, and remove all Withdrawn windows from the save-set (so
	 * they don't get remapped.  REMIND: We have to do this because
	 * Withdrawn windows are still left reparented inside the frame; this
	 * shouldn't be the case.
	 */
	ListApply(ActiveClientList,UnparentClient,NULL);

	/* Destroy the screens - which will restore input focus, colormap,
	 * and background, etc.
	 */
	DestroyScreens(DefDpy);

	XSync(DefDpy, True);
}


/* Clean up and then re-exec argv. */
int
RestartOLWM()
{
    cleanup();
    execvp(argVec[0], argVec);
    ErrorGeneral("cannot restart");
    /*NOTREACHED*/
}


/* Clean up and then exit. */
int
ExitOLWM()
{
    cleanup();
    exit(0);
}


/*
 * HandleChildSignal - keep track of children that have died
 */
static void
HandleChildSignal()
{
	++DeadChildren;
}

/*
 * ReapChildren - wait() for all dead child processes
 */
void
ReapChildren()
{
#ifdef SYSV
        sigset_t newmask, oldmask;
        int status;
        pid_t pid;
#else
	int oldmask;
	int pid;
	union wait status;
#endif

#ifdef SYSV
        sigemptyset(&newmask);
        sigemptyset(&oldmask);
        sigaddset(&newmask, SIGCHLD);
        sigprocmask(SIG_BLOCK, &newmask, &oldmask);
#else
	oldmask = sigblock(sigmask(SIGCHLD));
#endif
	while (DeadChildren>0)
	{
#ifdef SYSV
                pid = waitpid(-1, &status, WNOHANG);
#else
                pid = wait3(&status, WNOHANG, (struct rusage *)0);
#endif
		/* if it's the slave process then stop its use */
		if (pid == SlavePid) 
			SlaveStopped();

		if (pid == 0)
		{
			/* Removed error message - deemed not useful */
			DeadChildren = 0;
			break;
		}
		if (WIFSTOPPED(status))
		{
			kill(pid, SIGKILL);
		}
		--DeadChildren;
	}
#ifdef SYSV
        sigprocmask(SIG_SETMASK, &oldmask, &oldmask);
#else
        (void) sigsetmask(oldmask);
#endif
}

/*
 * usage(s1, s2)	-- print informative message regarding usage
 */
static
usage(s1, s2)
char	*s1, *s2;
{
    fprintf(stderr, "%s %s\n", s1, s2);
    fprintf(stderr,
	"usage: %s [-2d] [-3d] [-click] [-follow] [-parent]\n", ProgramName);
    fputs(
       /* STRING_EXTRACTION - do not translate "-display" ,"-name"
        * and "-xrm", becuase those are namne of command line option.
        */
	gettext("\t[-display <display>] [-name <classname>] [-xrm <rsrcstring>]\n"),
	stderr);
    fputs("\t[-all] [-debug] [-orphans] [-synchronize] [-single]\n", stderr);
    fputs("\t[-syncpid pid] [-syncsignal signal]\n", stderr);
    exit(1);
}

