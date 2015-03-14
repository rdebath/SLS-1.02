/* 
 * main.c --
 *
 *	This file contains the main program for "wish", a windowing
 *	shell based on Tk and Tcl.  It also provides a template that
 *	can be used as the basis for main programs for other Tk
 *	applications.
 *
 * Copyright 1990-1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef lint
static char rcsid[] = "$Header: /user6/ouster/wish/RCS/main.c,v 1.70 92/12/12 16:17:22 ouster Exp $ SPRITE (Berkeley)";
#endif

#include "tkConfig.h"
#include "tkInt.h"

/*
 * Declarations for library procedures:
 */

extern int isatty();

/*
 * Command used to initialize wish:
 */

static char initCmd[] = "source $tk_library/wish.tcl";

/*
 * Global variables used by the main program:
 */

static Tk_Window w;		/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */
static Tcl_Interp *interp;	/* Interpreter for this application. */
static int x, y;		/* Coordinates of last location moved to;
				 * used by "moveto" and "lineto" commands. */
static Tcl_CmdBuf buffer;	/* Used to assemble lines of terminal input
				 * into Tcl commands. */
static int tty;			/* Non-zero means standard input is a
				 * terminal-like device.  Zero means it's
				 * a file. */

/*
 * Command-line options:
 */

int synchronize = 0;
char *fileName = NULL;
char *name = NULL;
char *display = NULL;
char *geometry = NULL;

Tk_ArgvInfo argTable[] = {
    {"-file", TK_ARGV_STRING, (char *) NULL, (char *) &fileName,
	"File from which to read commands"},
    {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
	"Initial geometry for window"},
    {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &display,
	"Display to use"},
    {"-name", TK_ARGV_STRING, (char *) NULL, (char *) &name,
	"Name to use for application"},
    {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
	"Use synchronous mode for display server"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

/*
 * Declaration for "drag&drop" installation
 */
extern void Tk_AddDragDropCmd
    _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin));


/*
 * Declaration for Tcl command procedure to create demo widget.  This
 * procedure is only invoked if SQUARE_DEMO is defined.
 */

extern int Tk_SquareCmd _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int argc, char **argv));

/*
 * Forward declarations for procedures defined later in this file:
 */

static void		DelayedMap _ANSI_ARGS_((ClientData clientData));
static int		LinetoCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static int		MovetoCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));
static void		StructureProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));
static int		DebugCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	Main program for Wish.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done
 *
 * Side effects:
 *	This procedure initializes the wish world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
{
    char *args, *p, *msg;
    char buf[20];
    int result;
    Tk_3DBorder border;

    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    /*
     * Parse command-line arguments.
     */

    if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 0)
	    != TCL_OK) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    if (name == NULL) {
	if (fileName != NULL) {
	    p = fileName;
	} else {
	    p = argv[0];
	}
	name = strrchr(p, '/');
	if (name != NULL) {
	    name++;
	} else {
	    name = p;
	}
    }

    /*
     * Initialize the Tk application and arrange to map the main window
     * after the startup script has been executed, if any.  This way
     * the script can withdraw the window so it isn't ever mapped
     * at all.
     */

    w = Tk_CreateMainWindow(interp, display, name);
    if (w == NULL) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    Tk_SetClass(w, "Tk");
    Tk_CreateEventHandler(w, StructureNotifyMask, StructureProc,
	    (ClientData) NULL);
    Tk_DoWhenIdle(DelayedMap, (ClientData) NULL);
    if (synchronize) {
	XSynchronize(Tk_Display(w), True);
    }
    Tk_GeometryRequest(w, 200, 200);
    border = Tk_Get3DBorder(interp, w, None, "#4eee94");
    if (border == NULL) {
	Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
	Tk_SetWindowBackground(w, WhitePixelOfScreen(Tk_Screen(w)));
    } else {
	Tk_SetBackgroundFromBorder(w, border);
    }
    XSetForeground(Tk_Display(w), DefaultGCOfScreen(Tk_Screen(w)),
	    BlackPixelOfScreen(Tk_Screen(w)));

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".  Also set the "geometry" variable from the geometry
     * specified on the command line.
     */

    args = Tcl_Merge(argc-1, argv+1);
    Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
    ckfree(args);
    sprintf(buf, "%d", argc-1);
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    if (geometry != NULL) {
	Tcl_SetVar(interp, "geometry", geometry, TCL_GLOBAL_ONLY);
    }

    /*
     * Add "drag&drop" facility to interpreter.
     */
	Tk_AddDragDropCmd(interp, w);

    /*
     * Add a few application-specific commands to the application's
     * interpreter.
     */

    Tcl_CreateCommand(interp, "lineto", LinetoCmd, (ClientData) w,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "moveto", MovetoCmd, (ClientData) w,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "debug", DebugCmd, (ClientData) w,
	    (void (*)()) NULL);
#ifdef SQUARE_DEMO
    Tcl_CreateCommand(interp, "square", Tk_SquareCmd, (ClientData) w,
	    (void (*)()) NULL);
#endif

    /*
     * Execute Wish's initialization script, followed by the script specified
     * on the command line, if any.
     */

    result = Tcl_Eval(interp, initCmd, 0, (char **) NULL);
    if (result != TCL_OK) {
	goto error;
    }
    tty = isatty(0);
    if (fileName != NULL) {
	result = Tcl_VarEval(interp, "source ", fileName, (char *) NULL);
	if (result != TCL_OK) {
	    goto error;
	}
	tty = 0;
    } else {
	/*
	 * Commands will come from standard input.  Set up a handler
	 * to receive those characters and print a prompt if the input
	 * device is a terminal.
	 */

	Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData) 0);
	if (tty) {
	    printf("wish: ");
	}
    }
    fflush(stdout);
    buffer = Tcl_CreateCmdBuf();
    (void) Tcl_Eval(interp, "update", 0, (char **) NULL);

    /*
     * Loop infinitely, waiting for commands to execute.  When there
     * are no windows left, Tk_MainLoop returns and we clean up and
     * exit.
     */

    Tk_MainLoop();
    Tcl_DeleteInterp(interp);
    Tcl_DeleteCmdBuf(buffer);
    exit(0);

error:
    msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) {
	msg = interp->result;
    }
    fprintf(stderr, "%s\n", msg);
    Tcl_Eval(interp, "destroy .", 0, (char **) NULL);
    exit(1);
    return 0;			/* Needed only to prevent compiler warnings. */
}

/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;				/* Not used. */
{
    char line[200];
    static int gotPartial = 0;
    char *cmd;
    int result;

    if (fgets(line, 200, stdin) == NULL) {
	if (!gotPartial) {
	    if (tty) {
		Tcl_Eval(interp, "destroy .", 0, (char **) NULL);
		exit(0);
	    } else {
		Tk_DeleteFileHandler(0);
	    }
	    return;
	} else {
	    line[0] = 0;
	}
    }
    cmd = Tcl_AssembleCmd(buffer, line);
    if (cmd == NULL) {
	gotPartial = 1;
	return;
    }
    gotPartial = 0;
    result = Tcl_RecordAndEval(interp, cmd, 0);
    if (*interp->result != 0) {
	if ((result != TCL_OK) || (tty)) {
	    printf("%s\n", interp->result);
	}
    }
    if (tty) {
	printf("wish: ");
	fflush(stdout);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StructureProc --
 *
 *	This procedure is invoked whenever a structure-related event
 *	occurs on the main window.  If the window is deleted, the
 *	procedure modifies "w" to record that fact.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Variable "w" may get set to NULL.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static void
StructureProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    if (eventPtr->type == DestroyNotify) {
	w = NULL;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DelayedMap --
 *
 *	This procedure is invoked by the event dispatcher once the
 *	startup script has been processed.  It waits for all other
 *	pending idle handlers to be processed (so that all the
 *	geometry information will be correct), then maps the
 *	application's main window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The main window gets mapped.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static void
DelayedMap(clientData)
    ClientData clientData;	/* Not used. */
{

    while (Tk_DoOneEvent(TK_IDLE_EVENTS) != 0) {
	/* Empty loop body. */
    }
    if (w == NULL) {
	return;
    }
    Tk_MapWindow(w);
}

/*
 *----------------------------------------------------------------------
 *
 * MoveToCmd and LineToCmd --
 *
 *	This procedures are registered as the command procedures for
 *	"moveto" and "lineto" Tcl commands.  They provide a trivial
 *	drawing facility.  They don't really work right, in that the
 *	drawn information isn't persistent on the screen (it will go
 *	away if the window is iconified and de-iconified again).  The
 *	commands are here partly for testing and partly to illustrate
 *	how to add application-specific commands to Tk.  You probably
 *	shouldn't use these commands in any real scripts.
 *
 * Results:
 *	The procedures return standard Tcl results.
 *
 * Side effects:
 *	The screen gets modified.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
MovetoCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" x y\"", (char *) NULL);
	return TCL_ERROR;
    }
    x = strtol(argv[1], (char **) NULL, 0);
    y = strtol(argv[2], (char **) NULL, 0);
    return TCL_OK;
}
	/* ARGSUSED */
static int
LinetoCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int newX, newY;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" x y\"", (char *) NULL);
	return TCL_ERROR;
    }
    newX = strtol(argv[1], (char **) NULL, 0);
    newY = strtol(argv[2], (char **) NULL, 0);
    Tk_MakeWindowExist(w);
    XDrawLine(Tk_Display(w), Tk_WindowId(w),
	    DefaultGCOfScreen(Tk_Screen(w)), x, y, newX, newY);
    x = newX;
    y = newY;
    return TCL_OK;
}


/*ARGSUSED*/
static void
DebugProc (clientData, interp, level, command, cmdProc, cmdClientData, 
	   argc, argv)
    ClientData clientData;   /* not used */
    Tcl_Interp *interp;	     /* not used */
    int level;		     /* Current level */
    char *command;	     /* Command before substitution */
    int (*cmdProc) ();	     /* not used */
    ClientData cmdClientData;/* not used */
    int argc;
    char **argv;	     /* Command after parsing, but before evaluation */
{
    register int i;

    fprintf(stderr, "%d>\t%s\n\t", level, command);
    for (i = 0; i < argc; i++) {
	fprintf(stderr, "%s ", argv[i]);
    }
    fputs("\n", stderr);
}


/*ARGSUSED*/
static int
DebugCmd (clientData, interp, argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    static Tcl_Trace token;
    static int level;
    int newLevel;

    if (argc > 2) {
	Tcl_AppendResult(interp, "Wrong # arguments: should be \"", argv[0],
			 " ?level?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (argc == 1) {
	char buf[30];

	sprintf(buf, "%d", level);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);
	return TCL_OK;
    }
    if (Tcl_GetInt(interp, argv[1], &newLevel) != TCL_OK) {
	return TCL_ERROR;
    }
    if (newLevel < 0) {
	newLevel = 0;
    }
    if (newLevel == 0 && level > 0) {
	Tcl_DeleteTrace(interp, token);
    }
    if (newLevel > 0 && level == 0) {
	token = Tcl_CreateTrace(interp, newLevel, DebugProc, (ClientData)NULL);
    }
    level = newLevel;
    return TCL_OK;
}

