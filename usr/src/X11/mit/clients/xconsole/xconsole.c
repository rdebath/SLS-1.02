/*
 * $Header: /home/x_cvs/mit/clients/xconsole/xconsole.c,v 1.6 1992/09/26 17:39:42 dawes Exp $
 * $XConsortium: xconsole.c,v 1.9 91/07/25 14:23:46 rws Exp $
 *   Versiun 1.9.1 - changes for /dev/osm and SVR4 - dwex@mtgzfs3.att.com
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>

#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>

#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Box.h>

#include <X11/Xos.h>
#include <X11/Xfuncs.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>

#if !defined(SEEK_END) && defined(L_XTND)
#define SEEK_END L_XTND
#endif

/* Fix ISC brain damage.  When using gcc fdopen isn't declared in <stdio.h>. */
#if defined(SYSV) && defined(SYSV386) && defined(__STDC__) && defined(ISC)
extern FILE *fdopen(int, char const *);
#endif

static long	TextLength ();

static Widget	top, text;

static XtInputId	input_id;

static FILE    *input;

static Boolean	notified;
static Boolean	iconified;

static Atom	wm_delete_window;
static Atom	mit_console;
#define MIT_CONSOLE_LEN	12
#define MIT_CONSOLE "MIT_CONSOLE_"
static char	mit_console_name[255 + MIT_CONSOLE_LEN + 1] = MIT_CONSOLE;

static struct _app_resources {
    char    *file;
    Boolean stripNonprint;
    Boolean notify;
    Boolean daemon;
    Boolean verbose;
    Boolean exitOnFail;
    Boolean tail;
} app_resources;

#define Offset(field) XtOffsetOf(struct _app_resources, field)

static XtResource  resources[] = {
    {"file",	"File",	    XtRString,	sizeof (char *),
	Offset (file),	XtRString,  "console" },
    {"notify",	"Notify",   XtRBoolean,	sizeof (Boolean),
	Offset (notify), XtRImmediate, (XtPointer)True },
    {"stripNonprint",	"StripNonprint",    XtRBoolean, sizeof (Boolean),
	Offset (stripNonprint), XtRImmediate, (XtPointer)True },
    {"daemon",		"Daemon",	    XtRBoolean,	sizeof (Boolean),
	Offset (daemon), XtRImmediate, (XtPointer)False},
    {"verbose",		"Verbose",	    XtRBoolean,	sizeof (Boolean),
	Offset (verbose),XtRImmediate, (XtPointer)False},
    {"exitOnFail",	"ExitOnFail",    XtRBoolean,	sizeof (Boolean),
	Offset (exitOnFail),XtRImmediate, (XtPointer)False},
    {"tail",		"Tail",	    	XtRBoolean,	sizeof (Boolean),
	Offset (tail), XtRImmediate, (XtPointer)False},
};

#undef Offset

static XrmOptionDescRec options[] = {
    {"-file",	    "*file",		XrmoptionSepArg,    NULL},
    {"-notify",	    "*notify",		XrmoptionNoArg,	    "TRUE"},
    {"-nonotify",   "*notify",		XrmoptionNoArg,	    "FALSE"},
    {"-daemon",	    "*daemon",		XrmoptionNoArg,	    "TRUE"},
    {"-verbose",    "*verbose",		XrmoptionNoArg,	    "TRUE"},
    {"-exitOnFail", "*exitOnFail",	XrmoptionNoArg,	    "TRUE"},
    {"-tail",       "*tail",		XrmoptionNoArg,	    "TRUE"},
};

#ifdef ultrix
#define USE_FILE
#define FILE_NAME   "/dev/xcons"
#endif

#ifndef USE_FILE
#include    <sys/ioctl.h>
#ifdef SVR4
#include    <termios.h>
#include    <sys/stropts.h>		/* for I_PUSH */
#endif

#ifdef TIOCCONS
#define USE_PTY
static int  tty_fd, pty_fd;
static char ttydev[64], ptydev[64];
#endif
#endif

#if (defined(SYSV) || defined(SVR4)) && defined(SYSV386)
#define USE_OSM
#include <signal.h>
static int child_pid;
#endif

static void inputReady ();

static
OpenConsole ()
{
    input = 0;
    if (app_resources.file)
    {
	if (!strcmp (app_resources.file, "console"))
	{
	    struct stat sbuf;
	    /* must be owner and have read/write permission */
	    if (!stat("/dev/console", &sbuf) &&
		(sbuf.st_uid == getuid()) &&
		!access("/dev/console", R_OK|W_OK))
	    {
#ifdef USE_FILE
	    	input = fopen (FILE_NAME, "r");
#endif
#ifdef USE_PTY
		int	on = 1;

		if (get_pty (&pty_fd, &tty_fd, ttydev, ptydev) == 0 &&
		    ioctl (tty_fd, TIOCCONS, (char *) &on) != -1)
		{
		    input = fdopen (pty_fd, "r");
		}
#endif
	    }
#ifdef USE_OSM
	    /* Don't have to be owner of /dev/console when using /dev/osm. */
	    if (!access("/dev/osm", R_OK))
	    {
	        input = fdopen(osm_pipe(), "r");
	    }
#endif
	    if (input && app_resources.verbose)
	    {
		char	*hostname;
		TextAppend (text, "Console log for ", 16);
		hostname = mit_console_name + MIT_CONSOLE_LEN;
		TextAppend (text, hostname, strlen (hostname));
		TextAppend (text, "\n", 1);
	    }
	}
	else
	{
	    input = fopen (app_resources.file, "r");
	    if (app_resources.tail && input)
	        fseek(input, 0L, SEEK_END);
	}
	if (!input)
	{
	    if (app_resources.exitOnFail)
		exit(0);
	    TextAppend (text, "Couldn't open ", 14);
	    TextAppend (text, app_resources.file, strlen (app_resources.file));
	    TextAppend (text, "\n", 1);
	}
    }
    else
	input = stdin;

    if (input)
    {
	input_id = XtAddInput (fileno (input), (XtPointer) XtInputReadMask,
			       inputReady, (XtPointer) text);
    }
}


static
CloseConsole ()
{
    if (input) {
	XtRemoveInput (input_id);
	fclose (input);
    }
#ifdef USE_PTY
    close (tty_fd);
#endif
}

/*ARGSUSED*/
static void
Quit (widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
#ifdef USE_OSM
    kill (child_pid, SIGTERM);
#endif
    exit (0);
}

#ifdef USE_OSM
static void
Killer(int sig)
{
    kill (child_pid, SIGTERM);
    exit (0);
}

static int 
MyIOErrorHandler(Display *disp)
{
    Killer(0);
}
#endif

extern char *malloc (), *realloc ();

static void
Notify ()
{
    Arg	    arglist[1];
    char    *oldName;
    char    *newName;

    if (!iconified || !app_resources.notify || notified)
	return;
    XtSetArg (arglist[0], XtNiconName, &oldName);
    XtGetValues (top, arglist, 1);
    newName = malloc (strlen (oldName) + 3);
    if (!newName)
	return;
    sprintf (newName, "%s *", oldName);
    XtSetArg (arglist[0], XtNiconName, newName);
    XtSetValues (top, arglist, 1);
    free (newName);
    notified = True;
}

/*ARGSUSED*/
static void
Deiconified (widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg	    arglist[1];
    char    *oldName;
    char    *newName;
    int	    oldlen;

    iconified = False;
    if (!app_resources.notify || !notified)
	return;
    XtSetArg (arglist[0], XtNiconName, &oldName);
    XtGetValues (top, arglist, 1);
    oldlen = strlen (oldName);
    if (oldlen >= 2) {
    	newName = malloc (oldlen - 1);
    	if (!newName)
	    return;
    	strncpy (newName, oldName, oldlen - 2);
	newName[oldlen - 2] = '\0';
    	XtSetArg (arglist[0], XtNiconName, newName);
    	XtSetValues (top, arglist, 1);
    	free (newName);
    }
    notified = False;
}

/*ARGSUSED*/
static void
Iconified (widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    iconified = True;
}

/*ARGSUSED*/
static void
Clear (widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    long	    last;
    XawTextBlock    block;

    last = TextLength (text);
    block.ptr = "";
    block.firstPos = 0;
    block.length = 0;
    block.format = FMT8BIT;
    TextReplace (text, 0, last, &block);
}

static XtActionsRec actions[] = {
    "Quit",	    Quit,
    "Iconified",    Iconified,
    "Deiconified",  Deiconified,
    "Clear",	    Clear,
};

static void
stripNonprint (b)
    char    *b;
{
    char    *c;

    c = b;
    while (*b)
    {
	if (isprint (*b) || isspace (*b) && *b != '\r')
	{
	    if (c != b)
		*c = *b;
	    ++c;
	}
	++b;
    }
    *c = '\0';
}

static void
inputReady (w, source, id)
    XtPointer	w;
    int		*source;
    XtInputId	*id;
{
    char    buffer[1025];
    int	    n;

    n = read (*source, buffer, sizeof (buffer) - 1);
    if (n <= 0)
    {
	if (app_resources.tail)
	{
		sleep(1);
		return;
	}
	fclose (input);
	XtRemoveInput (*id);
    }
    Notify ();
    buffer[n] = '\0';
    if (app_resources.stripNonprint)
    {
	stripNonprint (buffer);
	n = strlen (buffer);
    }
    TextAppend ((Widget) text, buffer, n);
}

static Boolean
ConvertSelection (w, selection, target, type, value, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XtPointer *value;
    unsigned long *length;
    int *format;
{
    Display* d = XtDisplay(w);
    XSelectionRequestEvent* req =
	XtGetSelectionRequest(w, *selection, (XtRequestId)NULL);

    if (*target == XA_TARGETS(d)) {
	Atom* targetP;
	Atom* std_targets;
	unsigned long std_length;
	XmuConvertStandardSelection(w, req->time, selection, target, type,
				  (caddr_t*)&std_targets, &std_length, format);
	*value = (XtPointer)XtMalloc(sizeof(Atom)*(std_length + 5));
	targetP = *(Atom**)value;
	*targetP++ = XA_STRING;
	*targetP++ = XA_TEXT(d);
	*targetP++ = XA_LENGTH(d);
	*targetP++ = XA_LIST_LENGTH(d);
	*targetP++ = XA_CHARACTER_POSITION(d);
	*length = std_length + (targetP - (*(Atom **) value));
	bcopy((char*)std_targets, (char*)targetP, sizeof(Atom)*std_length);
	XtFree((char*)std_targets);
	*type = XA_ATOM;
	*format = 32;
	return True;
    }

    if (*target == XA_LIST_LENGTH(d) ||
	*target == XA_LENGTH(d))
    {
    	long * temp;
    	
    	temp = (long *) XtMalloc(sizeof(long));
    	if (*target == XA_LIST_LENGTH(d))
      	  *temp = 1L;
    	else			/* *target == XA_LENGTH(d) */
      	  *temp = (long) TextLength (text);
    	
    	*value = (XtPointer) temp;
    	*type = XA_INTEGER;
    	*length = 1L;
    	*format = 32;
    	return True;
    }
    
    if (*target == XA_CHARACTER_POSITION(d))
    {
    	long * temp;
    	
    	temp = (long *) XtMalloc(2 * sizeof(long));
    	temp[0] = (long) 0;
    	temp[1] = TextLength (text);
    	*value = (XtPointer) temp;
    	*type = XA_SPAN(d);
    	*length = 2L;
    	*format = 32;
    	return True;
    }
    
    if (*target == XA_STRING ||
      *target == XA_TEXT(d) ||
      *target == XA_COMPOUND_TEXT(d))
    {
	extern char *_XawTextGetSTRING();
    	if (*target == XA_COMPOUND_TEXT(d))
	    *type = *target;
    	else
	    *type = XA_STRING;
	*length = TextLength (text);
    	*value = (XtPointer)_XawTextGetSTRING((TextWidget) text, 0, *length);
    	*format = 8;
	/*
	 * Drop our connection to the file; the new console program
	 * will open as soon as it receives the selection contents; there
	 * is a small window where console output will not be redirected,
	 * but I see no way of avoiding that without having two programs
	 * attempt to redirect console output at the same time, which seems
	 * worse
	 */
	CloseConsole ();
    	return True;
    }
    
    if (XmuConvertStandardSelection(w, req->time, selection, target, type,
				    (caddr_t *)value, length, format))
	return True;

    return False;
}

static void
LoseSelection (w, selection)
    Widget  w;
    Atom    *selection;
{
    Quit ();
}

/*ARGSUSED*/
static void
InsertSelection (w, client_data, selection, type, value, length, format)
    Widget	    w;
    XtPointer	    client_data;
    Atom	    *selection, *type;
    XtPointer	    value;
    unsigned long   *length;
    int		    *format;
{
    if (*type != XT_CONVERT_FAIL)
	TextInsert (text, (char *) value, *length);
    XtOwnSelection(top, mit_console, CurrentTime,
		   ConvertSelection, LoseSelection, NULL);
    OpenConsole ();
}


main (argc, argv)
    char    **argv;
{
    Arg arglist[10];
    Cardinal num_args;
    char     *hostname;

    top = XtInitialize ("xconsole", "XConsole", options, XtNumber (options),
			&argc, argv);
    XtGetApplicationResources (top, (XtPointer)&app_resources, resources,
			       XtNumber (resources), NULL, 0);

    if (app_resources.daemon)
	if (fork ()) exit (0);
    XtAddActions (actions, XtNumber (actions));
    
    text = XtCreateManagedWidget ("text", asciiTextWidgetClass,
				  top, NULL, 0);
    
    XtRealizeWidget (top);
    num_args = 0;
    XtSetArg(arglist[num_args], XtNiconic, &iconified); num_args++;
    XtGetValues(top, arglist, num_args);
    if (iconified)
	Iconified ();
    else
	Deiconified ();
    wm_delete_window = XInternAtom(XtDisplay(top), "WM_DELETE_WINDOW",
				   False);
    (void) XSetWMProtocols (XtDisplay(top), XtWindow(top),
                            &wm_delete_window, 1);

    XmuGetHostname (mit_console_name + MIT_CONSOLE_LEN, 255);

    mit_console = XInternAtom(XtDisplay(top), mit_console_name, False);

    if (XGetSelectionOwner (XtDisplay (top), mit_console))
    {
	XtGetSelectionValue(top, mit_console, XA_STRING, InsertSelection,
			    NULL, CurrentTime);
    }
    else
    {
	XtOwnSelection(top, mit_console, CurrentTime,
		       ConvertSelection, LoseSelection, NULL);
	OpenConsole ();
    }
#ifdef USE_OSM
    (void) XSetIOErrorHandler(MyIOErrorHandler);
#endif
    XtMainLoop ();
    return 0;
}

static long TextLength (w)
    Widget  w;
{
    return XawTextSourceScan (XawTextGetSource (w),
			      (XawTextPosition) 0,
 			      XawstAll, XawsdRight, 1, TRUE);
}

TextReplace (w, start, end, block)
    Widget	    w;
    XawTextBlock    *block;
{
    Arg		    arg;
    Widget	    source;
    XawTextEditType edit_mode;

    source = XawTextGetSource (w);
    XtSetArg (arg, XtNeditType, &edit_mode);
    XtGetValues (source, &arg, ONE);
    XtSetArg (arg, XtNeditType, XawtextEdit);
    XtSetValues (source, &arg, ONE);
    XawTextReplace (w, start, end, block);
    XtSetArg (arg, XtNeditType, edit_mode);
    XtSetValues (source, &arg, ONE);
}

TextAppend (w, s, len)
    Widget  w;
    char    *s;
{
    long	    last, current;
    XawTextBlock    block;

    current = XawTextGetInsertionPoint (w);
    last = TextLength (w);
    block.ptr = s;
    block.firstPos = 0;
    block.length = len;
    block.format = FMT8BIT;
    TextReplace (w, last, last, &block);
    if (current == last)
	XawTextSetInsertionPoint (w, last + block.length);
}

TextInsert (w, s, len)
    Widget  w;
    char    *s;
{
    XawTextBlock    block;
    long	    current;

    current = XawTextGetInsertionPoint (w);
    block.ptr = s;
    block.firstPos = 0;
    block.length = len;
    block.format = FMT8BIT;
    TextReplace (w, 0, 0, &block);
    if (current == 0)
	XawTextSetInsertionPoint (w, len);
}

#ifdef USE_PTY
/* This function opens up a pty master and stuffs it's value into pty.
 * If it finds one, it returns a value of 0.  If it does not find one,
 * it returns a value of !0.  This routine is designed to be re-entrant,
 * so that if a pty master is found and later, we find that the slave
 * has problems, we can re-enter this function and get another one.
 */

#include    "../xterm/ptyx.h"

get_pty (pty, tty, ttydev, ptydev)
    int	    *pty, *tty;
    char    *ttydev, *ptydev;
{
#ifdef SVR4
	if ((*pty = open ("/dev/ptmx", O_RDWR)) < 0) {
	    return 1;
	}
	grantpt(*pty);
	unlockpt(*pty);
	strcpy(ttydev, ptsname(*pty));
	if ((*tty = open(ttydev, O_RDWR)) >= 0) {
	    (void)ioctl(*tty, I_PUSH, "ttcompat");
	    return 0;
	}
	if (*pty >= 0)
	    close (*pty);
#else /* !SVR4, need lots of code */
#ifdef USE_GET_PSEUDOTTY
	if ((*pty = getpseudotty (&ttydev, &ptydev)) >= 0 &&
	    (*tty = open (ttydev, O_RDWR)) >= 0)
	    return 0;
	if (*pty >= 0)
	    close (*pty);
#else
	static int devindex, letter = 0;

#if defined(umips) && defined (SYSTYPE_SYSV)
	struct stat fstat_buf;

	*pty = open ("/dev/ptc", O_RDWR);
	if (*pty < 0 || (fstat (*pty, &fstat_buf)) < 0) {
	  return(1);
	}
	sprintf (ttydev, "/dev/ttyq%d", minor(fstat_buf.st_rdev));
	sprintf (ptydev, "/dev/ptyq%d", minor(fstat_buf.st_rdev));
	if ((*tty = open (ttydev, O_RDWR)) >= 0) {
	    /* got one! */
	    return(0);
	}
	close (*pty);
#else /* not (umips && SYSTYPE_SYSV) */
#ifdef CRAY
	for (; devindex < 256; devindex++) {
	    sprintf (ttydev, "/dev/ttyp%03d", devindex);
	    sprintf (ptydev, "/dev/pty/%03d", devindex);

	    if ((*pty = open (ptydev, O_RDWR)) >= 0 &&
		(*tty = open (ttydev, O_RDWR)) >= 0)
	    {
		/* We need to set things up for our next entry
		 * into this function!
		 */
		(void) devindex++;
		return(0);
	    }
	    if (*pty >= 0)
		close (*pty);
	}
#else /* !CRAY */
	strcpy (ttydev, "/dev/ttyxx");
	strcpy (ptydev, "/dev/ptyxx");
	while (PTYCHAR1[letter]) {
	    ttydev [strlen(ttydev) - 2]  = ptydev [strlen(ptydev) - 2] =
		    PTYCHAR1 [letter];

	    while (PTYCHAR2[devindex]) {
		ttydev [strlen(ttydev) - 1] = ptydev [strlen(ptydev) - 1] =
			PTYCHAR2 [devindex];
		if ((*pty = open (ptydev, O_RDWR)) >= 0 &&
		    (*tty = open (ttydev, O_RDWR)) >= 0)
		{
			/* We need to set things up for our next entry
			 * into this function!
			 */
			(void) devindex++;
			return(0);
		}
		if (*pty >= 0)
		    close (*pty);
		devindex++;
	    }
	    devindex = 0;
	    (void) letter++;
	}
#endif /* CRAY else not CRAY */
#endif /* umips && SYSTYPE_SYSV */
#endif /* USE_GET_PSEUDOTTY */
#endif /* SVR4 */
	/* We were unable to allocate a pty master!  Return an error
	 * condition and let our caller terminate cleanly.
	 */
	return(1);
}
#endif

#ifdef USE_OSM
/*
 * On SYSV386 there is a special device, /dev/osm, where system messages
 * are sent.  Problem is that we can't perform a select(2) on this device.
 * So this routine creates a streams-pty where one end reads the device and
 * sends the output to xconsole.
 */
osm_pipe()
{
  int tty;
  char ttydev[64];
    
  if ((tty = open("/dev/ptmx", O_RDWR)) < 0)  return -1;

  grantpt(tty);
  unlockpt(tty);
  strcpy(ttydev, (char *)ptsname(tty));

  if ((child_pid = fork()) == 0) {
    int pty, osm, buf, nbytes;
    char *dummy = malloc(50);

    buf = 0;
    if ((osm = open("/dev/osm1", O_RDONLY)) != -1) {
      while ((nbytes = read(osm, dummy, 50)) > 0)
	buf += nbytes;
      dummy = realloc(dummy, buf);
    }
    pty = open(ttydev, O_RDWR);
    osm = open("/dev/osm", O_RDONLY);
    if (buf)
      read(osm, dummy, buf);
    free(dummy);
    while ((nbytes = read(osm, &buf, sizeof(buf))) >= 0)
      write(pty, &buf, nbytes);
  }
  signal(SIGHUP, Killer);
  signal(SIGINT, Killer);
  signal(SIGTERM, Killer);

  return tty;
}
#endif  /* USE_OSM */
