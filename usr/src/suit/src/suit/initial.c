/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

/* this tells Borland's Turbo C to make the stack bigger. It's annoying and
 * should probably be done via the command line, but this is how they say to
 * do it.  And the system won't run if we don't. */


#include <stdio.h>
#include "privsuit.h"

#ifdef _Windows
/* This prevents the property names from being declared "extern" */
/* The names are declare for real at the end of this file */
#define WINDECLARED
#endif

#define KILOBYTES     *(unsigned)1024

#ifdef IBM_PC
extern unsigned _stklen = 32 KILOBYTES;
#endif

#ifdef MACINTOSH

#define DEFAULT_STACK  (8 KILOBYTES)

PRIVATE void SetMinimumStack (long minSize)
{
#if THINK_C == 1
    extern void SetApplLimit (unsigned char *);
#endif
    long newApplLimit;
    if (minSize > DEFAULT_STACK) {
        newApplLimit = (long) GetApplLimit() - (minSize - DEFAULT_STACK);
        SetApplLimit ((unsigned char *)newApplLimit);
    }
}
#endif

boolean SUITInitialized = FALSE;

/* if the user uses dumped code, this flags us and indicates
   the routine to call instead of reading the .sui file -- pausch
*/
SUIT_functionPointer routineToCallInsteadOfUsingSUIfile = NULL;

/* !!! I have no idea what this routine does. This should most definitely be
 * explored. */

PRIVATE void ShiftPointers (char *argv[], int start, int space, int no_elems)
{
    int i;

    for (i = start; i + space < no_elems; i++)
	argv[i] = argv[i + space];
}



#define DEBUG_OPTION		"-suitdebug"
#define DYNARRAY_DEBUG_OPTION	"-dynarraydebug"
#define HINTS_OPTION		"-suithintsfile"
#define TRACE_OPTION		"-suittrace"
#define SRGP_TRACE      	"-srgptrace"
#define SRGP_DEBUG      	"-srgpdebug"
#define MALLOC_DEBUG    	"-mallocdebug"
#define HELP_OPTION     	"-suithelp"



/* This simply prints out what command line options SUIT accepts. */

void SUIT_printCommandLineOptions (void)
{
    fprintf (stderr, "valid SUIT options are:\n\n");
    fprintf (stderr, "%20s %s\n", DEBUG_OPTION, "prints SUIT debugging code");
    fprintf (stderr, "%20s %s\n", DYNARRAY_DEBUG_OPTION, "prints dynarray debugging code");
    fprintf (stderr, "%20s %s\n", HINTS_OPTION, "followed by file name");
    fprintf (stderr, "%20s %s\n", TRACE_OPTION, "followed by int (trace level)");
    fprintf (stderr, "%20s %s\n", SRGP_TRACE, "turns srgp tracing on");
    fprintf (stderr, "%20s %s\n", SRGP_DEBUG, "turns SRGP debugging on ");
    fprintf (stderr, "%20s %s\n", MALLOC_DEBUG, "turns malloc debugging on ");
    fprintf (stderr, "%20s %s\n", HELP_OPTION, "lists all SUIT options ");

}




#define HEIGHT(R)      (double)(R.top_right.y - R.bottom_left.y)
#define WIDTH(R)       (double)(R.top_right.x - R.bottom_left.x)
#define ABOVE(P,C)     (double)(P.top_right.y - C.top_right.y)
#define BELOW(P,C)     (double)(C.bottom_left.y - P.bottom_left.y)
#define RIGHT(P,C)     (double)(P.top_right.x - C.top_right.x)
#define LEFT(P,C)      (double)(C.bottom_left.x - P.bottom_left.x)

#define ROUND(A)            ( ((A) > 0)? (int) (A + 0.5) : (int) (A - 0.5) )

SUIT_viewport SUIT_adjustForSpringiness (SUIT_viewport oldparent, SUIT_viewport parent,
					 SUIT_viewport oldchild, SUIT_springiness spring)
{
    SUIT_viewport new;
    
    if (spring & RIGHT_SPRINGINESS)
	if (spring & LEFT_SPRINGINESS)
	    if (spring & HORIZONTAL_SPRINGINESS) {
		new.bottom_left.x = ROUND(parent.bottom_left.x +
					  LEFT(oldparent, oldchild) * WIDTH(parent)/WIDTH(oldparent));
		new.top_right.x = ROUND(parent.top_right.x -
					RIGHT(oldparent, oldchild) * WIDTH(parent)/WIDTH(oldparent));
	    } else {
		new.top_right.x = ROUND(parent.top_right.x - (WIDTH (parent) - WIDTH (oldchild)) *
		    RIGHT (oldparent, oldchild) / (WIDTH (oldparent) - WIDTH (oldchild)));
		new.bottom_left.x = ROUND(parent.bottom_left.x + (WIDTH (parent) - WIDTH (oldchild)) *
		    LEFT (oldparent, oldchild) / (WIDTH (oldparent) - WIDTH (oldchild)));
	    }
	else {
	    new.bottom_left.x = ROUND(parent.bottom_left.x + LEFT (oldparent, oldchild));
	    if (spring & HORIZONTAL_SPRINGINESS)
		new.top_right.x = ROUND(parent.top_right.x -
					(parent.top_right.x - new.bottom_left.x) *
					RIGHT(oldparent, oldchild) /
					(WIDTH(oldchild) + RIGHT(oldparent, oldchild)));
	    else
		new.top_right.x = ROUND(new.bottom_left.x + WIDTH (oldchild));
	}
    else {
	new.top_right.x = ROUND(parent.top_right.x - RIGHT (oldparent, oldchild));
	if (spring & LEFT_SPRINGINESS)
	    if (spring & HORIZONTAL_SPRINGINESS)
		new.bottom_left.x = ROUND(parent.bottom_left.x +
					  (new.top_right.x - parent.bottom_left.x) *
		    LEFT(oldparent, oldchild)/(WIDTH(oldchild) + LEFT(oldparent, oldchild)));
	    else
		new.bottom_left.x = ROUND(new.top_right.x - WIDTH (oldchild));
	else if (spring & HORIZONTAL_SPRINGINESS)
	    new.bottom_left.x = ROUND(parent.bottom_left.x + LEFT (oldparent, oldchild));
	else
	    ASSERT(FALSE, (mes, "Invalid horizontal springiness (%d)!\n",spring));
    }
    
    if (spring & ABOVE_SPRINGINESS)
	if (spring & BELOW_SPRINGINESS)
	    if (spring & VERTICAL_SPRINGINESS) {
		new.bottom_left.y = ROUND(parent.bottom_left.y +
					  BELOW(oldparent, oldchild) *
					  HEIGHT(parent) / HEIGHT(oldparent));
		new.top_right.y = ROUND(parent.top_right.y -
					ABOVE(oldparent, oldchild) *
					HEIGHT(parent) / HEIGHT(oldparent));
	    } else {
		new.top_right.y = ROUND(parent.top_right.y - (HEIGHT (parent) - HEIGHT (oldchild)) *
		    ABOVE (oldparent, oldchild) / (HEIGHT (oldparent) - HEIGHT (oldchild)));
		new.bottom_left.y = ROUND(parent.bottom_left.y + (HEIGHT(parent) - HEIGHT(oldchild)) *
		    BELOW (oldparent, oldchild) / (HEIGHT (oldparent) - HEIGHT (oldchild)));
	    }
	else {
	    new.bottom_left.y = ROUND(parent.bottom_left.y + BELOW(oldparent, oldchild));
	    if (spring & VERTICAL_SPRINGINESS)
		new.top_right.y = ROUND(parent.top_right.y -
					(parent.top_right.y - new.bottom_left.y) *
					ABOVE(oldparent, oldchild) /
					(HEIGHT(oldchild) + ABOVE(oldparent, oldchild)));
	    else
		new.top_right.y = ROUND(new.bottom_left.y + HEIGHT (oldchild));
	}
    else {
	new.top_right.y = ROUND(parent.top_right.y - ABOVE (oldparent, oldchild));
	if (spring & BELOW_SPRINGINESS)
	    if (spring & VERTICAL_SPRINGINESS)
		new.bottom_left.y = ROUND(parent.bottom_left.y +
					  (new.top_right.y - parent.bottom_left.y) *
					  BELOW(oldparent, oldchild) /
					  (HEIGHT(oldchild) + BELOW(oldparent, oldchild)));
	    else
		new.bottom_left.y = ROUND(new.top_right.y - HEIGHT(oldchild));
	else if (spring & VERTICAL_SPRINGINESS)
	    new.bottom_left.y = ROUND(parent.bottom_left.y + BELOW(oldparent, oldchild));
	else
	    ASSERT(FALSE, (mes, "Invalid vertical springiness (%d)!\n", spring));
    }
    

    /* check for validity and correct if necessary */
    if (new.bottom_left.x > new.top_right.x)
	new.bottom_left.x = new.top_right.x;
    if (new.bottom_left.y > new.top_right.y)
	new.bottom_left.y = new.top_right.y;
    
    return new;
}



PRIVATE int DealWithResizeEvent (int newWidth, int newHeight)
{
    SUIT_viewport screen;

    SUIT_deluxeSetInteger (NULL, SCREEN_WIDTH, newWidth, GLOBAL);
    SUIT_deluxeSetInteger (NULL, SCREEN_HEIGHT, newHeight, GLOBAL);

    screen.bottom_left.x = screen.bottom_left.y = 0;
    screen.top_right.x = newWidth-1;
    screen.top_right.y = newHeight-1;
    SUIT_setBoolean (global.root, VISIBLE, FALSE); /* don't erase objects where they currently are */
    SUIT_setViewport (global.root, VIEWPORT, screen);
    SRGP_setClipRectangle (screen);
    SUIT_setBoolean (global.root, VISIBLE, TRUE);

    si_repaintScreen ();
    return 0;
}



PRIVATE void InitializeFunctionPtrs (void)
{
    SUIT_registerCallbackPtr ("ChainedPropertyCallback", ChainedPropertyCallback);
}



PRIVATE SUIT_object si_menuCreateBoundedValue (char *name) { return (SUIT_createBoundedValue (name, NULL)); }
PRIVATE SUIT_object si_menuCreateButton (char *name)       { return (SUIT_createButton (name, NULL)); }
PRIVATE SUIT_object si_menuCreateArrowButton (char *name)  { return (SUIT_createArrowButton (name, NULL)); }
PRIVATE SUIT_object si_menuCreateSwitch (char *name)       { return (SUIT_createOnOffSwitch (name, NULL)); }
PRIVATE SUIT_object si_menuCreatePatternChips (char *name) { return (SUIT_createPatternChips (name, NULL)); }
PRIVATE SUIT_object si_menuCreateColorChips (char *name)   { return (SUIT_createColorChips (name, NULL)); }
PRIVATE SUIT_object si_menuCreateTypeInBox(char *name)	   { return (SUIT_createTypeInBox(name, NULL)); } 
PRIVATE SUIT_object si_menuCreateScrollableBox(char *name) { return (SUIT_createScrollableList(name, NULL)); }
PRIVATE SUIT_object si_menuCreateTextEditor(char *name)	   { return (SUIT_createTextEditor(name, NULL)); }
PRIVATE SUIT_object si_menuCreateRadioButtons(char *name)  { return (SUIT_createRadioButtons(name, NULL)); }
PRIVATE SUIT_object si_menuCreateTextBox(char *name)       { return (SUIT_createTextBox (name, "")); }

PRIVATE SUIT_object si_menuCreateBorderlessBBoard(char *name)
{
    SUIT_object bboard = SUIT_createBulletinBoardWithClass (name, "borderless bulletin board");
    SUIT_deluxeSetBoolean (bboard, HAS_BORDER, FALSE, CLASS);
    return bboard;
}


PRIVATE void InitializeWidgetClasses (void)
{
    SUIT_registerClass ("arrow button", si_menuCreateArrowButton, "Arrow buttons, like regular buttons, call functions when they are pressed.  They are provided as a convenience.  For more information see the reference manual.");
    SUIT_registerClass ("bouncing ball", SUIT_createBouncingBall, "Bouncing ball widgets demonstrate animation.  For more information see the reference manual.");
    SUIT_registerClass ("bounded value", si_menuCreateBoundedValue, "Bounded value widgets allow users to select integers or doubles between specified minimum and maximum values.  To contrain the user to integers set the \"granularity\" property to 1.0.  For more information see the reference manual.");
    SUIT_registerClass ("borderless bulletin board", si_menuCreateBorderlessBBoard, "Bulletin boards are meant to be used as containers for other objects.  For more information see the reference manual.");
    SUIT_registerClass ("bulletin board", SUIT_createBulletinBoard, "Bulletin boards are meant to be used as containers for other objects.  For more information see the reference manual.");
    SUIT_registerClass ("button", si_menuCreateButton, "Buttons call functions when they are pressed.  For more information see the reference manual.");
    SUIT_registerClass ("callback function panel", SUIT_createCallbackFunctionPanel, NULL);
    SUIT_registerClass ("clock", SUIT_createClock, "Clocks display the system time from the computer's clock.  For more information see the reference manual.");
    SUIT_registerClass ("color chips", si_menuCreateColorChips, "This widget allows the user to select a color from the available palette of colors.  For more information see the reference manual.");
    SUIT_registerClass ("font panel", SUIT_createFontPanel, "A font panel allows the user to select one of the standard GP fonts.  The \"current value\" property of panel is the font that is currently displayed.  For more information see the reference manual.");
    SUIT_registerClass ("label", SUIT_createLabel, "A label displays a text string; it has no callback function.  By default it displays its name, though you can specify that it display something different by setting the \"label\" property.  For more information see the reference manual.");
    SUIT_registerClass ("menu", SUIT_createVerticalMenu, "This widget is a placard-style menu.  For more information see the reference manual.");
    SUIT_registerClass ("on/off switch", si_menuCreateSwitch, "Switches allow the user to select one of two options.  For more information see the reference manual.");
    SUIT_registerClass ("pattern chips", si_menuCreatePatternChips, "This widget allows the user to select one of the standard GP patterns.  For more information see the reference manual.");
    SUIT_registerClass ("place mat", SUIT_createPlaceMat, "Placemats are meant to be used to for decoration: they have no function.  Most often, they are placed behind other widgets to create a particular visual effect.  For more information see the reference manual.");
    SUIT_registerClass ("polygon", SUIT_createPolygon, "This is the famous polygon widget found in the SUIT tutorial.");
    SUIT_registerClass ("pulldown menu", SUIT_createPullDownMenu, "Pulldown menus appear to be buttons, but when pressed reveal choices underneath.  For more information see the reference manual.");
    SUIT_registerClass ("radio buttons",si_menuCreateRadioButtons, "Radio buttons allow the user to select one (and only one) item from a list of choices.  For more information see the reference manual.");
    SUIT_registerClass ("scrollable list",si_menuCreateScrollableBox, "Scrollable lists allow the user to select one string from a scrolling list of strings.  For more information see the reference manual.");
    SUIT_registerClass ("spring panel", SUIT_createSpringPanel, "Spring panels allow the user to control the way a widget behaves when the widget's parent resizes.  For more information see the reference manual.");
    SUIT_registerClass ("stacker", SUIT_createStacker, "Stackers, like bulletin boards, are meant to contain other widgets.  Stackers arrange their children in horizontal or vertical stacks, depending on the active display style.  For more information see the reference manual.");
    SUIT_registerClass ("text box", si_menuCreateTextBox, "A text box is essentially a multiline label that performs wrapping of the text (no hyphenation, sorry) automatically, depending on the size of the viewport.  For more information see the reference manual.");
    SUIT_registerClass ("text editor", si_menuCreateTextEditor, "This widget is an emacs-like text editor.  For more information see the reference manual.");
    SUIT_registerClass ("type in box", si_menuCreateTypeInBox, "This widget is a one line text entry field.  For more information see the reference manual.");
    SUIT_registerClass ("uva logo", SUIT_createUVALogo, "This widget displays several different logos for the University of Virginia.  It is included in the toolkit because we are insanely proud of our institution.  For more information see the reference manual.");
}



PRIVATE GP_time copyrightStart;


void SUIT_beginDisplay (void)
{
    ENTER (1, (buf, "SUIT_beginDisplay()\n"));

    /* pausch hack: if we're not going to read the .sui file,
       we'll have to instead call the code that we dumped.
       */

    global.interestOn = FALSE;
    if (routineToCallInsteadOfUsingSUIfile == NULL) {
	GP_time now;
	si_readAndProcessHints (global.hintsfile);
	for (now = GP_getCurrentTime(); GP_timeDifference (now, copyrightStart) < 1000;
	     now = GP_getCurrentTime()) ;
    } else
	routineToCallInsteadOfUsingSUIfile();

    hintsFileRead = TRUE;
    global.interestOn = TRUE;
    SUIT_setViewport (global.root, VIEWPORT, SRGP_inquireCanvasExtent(0));
    si_tickleAllViewports (global.root);

    SRGP_allowResize (TRUE);
    SRGP_registerResizeCallback (DealWithResizeEvent);
    if (global.srgp_debug)
	SRGP_disableDebugAids ();

    SUIT_clearScreen ();
    SUIT_performRedisplay();
    LEAVE (1, (buf, "SUIT_beginDisplay\n"));
}


PRIVATE void InitializeGlobals (void)
{
    global.hintsfile = NULL;
    global.debug = FALSE;
    global.srgp_trace = FALSE;
    global.srgp_debug = FALSE;
    global.malloc_debug = FALSE;
    global.propertyEditor = NULL;
    global.nameTable = DynCreate (sizeof (nameTableEntry), 10);
    global.classProperties = DynCreate (sizeof (SUIT_class *), 10);
    global.callbackPtrs = DynCreate (sizeof (callbackPair), 1);
    global.types = DynCreate (sizeof (SUIT_type), 5);
    global.NoCanvas = FALSE;
    global.trapperFunctions = NULL;
    global.propertyEditorIsActive = FALSE;
    global.animated = FALSE;
    global.currentlyPainting = FALSE;
    global.currentlyInHitProcedure = FALSE;
    global.WidgetMenuOn = FALSE;
    global.CustomMenuOn = FALSE;
    global.interestOn = TRUE;
    global.interactiveToolsAvailable = TRUE;

    if ( routineToCallInsteadOfUsingSUIfile != NULL )
	global.interactiveToolsAvailable = FALSE;
    
    srgpHasBegun = FALSE;
    si_initRCString();
}


PRIVATE void CreateProgramName (char *programName)
{
    /* pausch: this hack isn't necessary right, but it works for now. */
    /* pausch: perhaps we should be really translating parms to argc and argv
     * for people */

#ifdef MACINTOSH
#if THINK_C == 1
extern char *PtoCstr (char *);
#endif
    {
	/* pausch: this number is arbitrary: I hope it's big enough */
#define BIG_BUFFER_SIZE 50
	int dummyInt;
	char dummyHandle[BIG_BUFFER_SIZE];	/* just in case they're
						 * writing to it */
	char dummyProgramName[BIG_BUFFER_SIZE];
	
	GetAppParms (dummyProgramName, &dummyInt, (void *) NULL);
	PtoCstr ((char *) dummyProgramName);
	global.programName = SUIT_createRCString (dummyProgramName);
    }
#else
    global.programName = SUIT_createRCString (programName);
#endif
}


PRIVATE void DrawCopyrightNotice (void)
{
    if (routineToCallInsteadOfUsingSUIfile == NULL) { /* we're initializing from an .sui file */
	rectangle r;
	double x;
	char buf[100];
	r = SRGP_inquireCanvasExtent(0);
	GP_pushGraphicsState();
	
	GP_setViewport (SUIT_defViewport(10, r.top_right.y-75, 70, r.top_right.y-15));
	GP_setWindow (GP_defRectangle(-0.1, -0.1, 1.1, 1.1));
	GP_setLineWidth (1);
	GP_rectangleCoord(0.0, 0.0, 1.0, 0.75);
	GP_rectangleCoord(0.25, 0.0, 0.75, 0.6);
	GP_ellipse(GP_defRectangle(0.0, 0.0, 1.0, 1.0));
	GP_line(GP_defPoint(0.0, 0.21), GP_defPoint(1.0, 0.21));
	GP_line(GP_defPoint(0.25, 0.6), GP_defPoint(0.5, 0.75));
	GP_line(GP_defPoint(0.75, 0.6), GP_defPoint(0.5, 0.75));
	GP_line(GP_defPoint(0.0, 0.6), GP_defPoint(1.0, 0.6));
	for (x = 0.35; x <= 0.75; x += 0.1) {
	    GP_line(GP_defPoint(x, 0.6), GP_defPoint(x, 0.21)); }

	GP_setViewport (SUIT_defViewport(80, r.top_right.y-70, 200, r.top_right.y-10));
	GP_setWindow (SUIT_defWindow(0.0,0.0,1.0,1.0));
	GP_setFont (GP_defFont ("times", "", 18.0));
	sprintf (buf, "SUIT, the Simple User Interface Toolkit, version %d.%d",
		 SUIT_VERSION, SUIT_REVISION);
	GP_text (GP_defPoint(0.0,0.6), buf);
	GP_setFont (GP_defFont ("times", "", 14.0));
	GP_text (GP_defPoint(0.0,0.3),"@(copyright) Copyright 1990, 1991, 1992 the University of Virginia.");
	GP_text (GP_defPoint(0.0,0.0),"All rights reserved. Alle Rechte vorbehalten. Tous droits r@'(e)serv@'(e)s.");

	GP_popGraphicsState();
        SRGP_refresh();
	copyrightStart = GP_getCurrentTime();
    }
}



PRIVATE void FinishInitialization (int width, int height, int depth)
{
    si_registerInitialTypes ();
    si_createRoot ();
    si_initializeProperties ();
    
    SRGP_begin (global.programName, width, height, depth, global.srgp_trace);
#if defined(SUN) || defined(RS6000)
    setlinebuf (stdout);
#endif
    srgpHasBegun = TRUE;

    SRGP_setInputMode (LOCATOR, EVENT);
    SRGP_setLocatorButtonMask (LEFT_BUTTON_MASK | MIDDLE_BUTTON_MASK | RIGHT_BUTTON_MASK);
    SRGP_setInputMode (KEYBOARD, EVENT);
    SRGP_setKeyboardProcessingMode (RAW);
    global.sampleMode = FALSE;
    
    InitializeFonts ();
    DrawCopyrightNotice();

    SUIT_deluxeSetInteger(NULL, SCREEN_WIDTH, width, GLOBAL);
    SUIT_deluxeSetInteger(NULL, SCREEN_HEIGHT, height, GLOBAL);

    /* set Root's viewport to be the size of the screen */
    SUIT_setViewport (global.root, VIEWPORT, SRGP_defRectangle (0, 0, SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL), SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL)));
    /* SUIT_makePropertyTemporary (global.root, VIEWPORT, OBJECT); */
    SUIT_lockProperty(global.root, VIEWPORT, OBJECT);

    OpenObjects = DynCreate (sizeof (SUIT_object), 1);
    si_openObject (global.root);
    SelectedObjects = DynCreate (sizeof (SUIT_object), 1);
    
    InitializeFunctionPtrs ();
    InitializeCursors ();
    InitializeColors (SRGP_inquireCanvasDepth());

    si_setUpHelp();
    InitializeWidgetClasses ();
}



PRIVATE void InitializeTrace (void)
{
    trace.tracefile = stdout;
#if defined(SUN) || defined(RS6000)
    setlinebuf (trace.tracefile);
#endif
    trace.tracelevel = 0;
    trace.traceIndent = 0;

}


PRIVATE void realInit (char *programName, int width, int height, int depth)
{
    ENTER (2, (buf, "realInit(%s)\n",programName));
    InitializeGlobals ();
    CreateProgramName (programName);

    global.hintsfile = (char *) SUIT_malloc (strlen (global.programName) +6);
    strcpy (global.hintsfile, "");
    global.hintsfile = si_makeFileName (global.hintsfile, global.programName);
    if ( routineToCallInsteadOfUsingSUIfile == NULL )
	PeekAtHintsFile (&width, &height, &depth);
    FinishInitialization(width, height, depth);
    SUITInitialized = TRUE;
    LEAVE (2, (buf, "realInit(%s)\n",programName));
}


void SUIT_init (char *programName)
{
#ifdef MACINTOSH
    SetMinimumStack (40 KILOBYTES); /* pausch hack: 8/16/92: used to be 15 */
#endif
    InitializeTrace ();
    ENTER (1, (buf, "SUIT_init(%s)\n",programName));
    realInit (programName, 0, 0, 0);
    LEAVE (1, (buf, "SUIT_init(%s)\n",programName));
}



boolean si_SUITInitialized (void)
{
    return SUITInitialized;
}


void SUIT_interiorInitFromCode (char *programName, SUIT_functionPointer suiRoutine, int width, int height, int depth)
{
    InitializeTrace();
    ENTER (1, (buf, "SUIT_interiorInitFromCode(%s)\n",programName));
    routineToCallInsteadOfUsingSUIfile = suiRoutine;
    realInit(programName, width, height, depth);
    LEAVE (1, (buf, "SUIT_interiorInitFromCode(%s)\n",programName));
}


void SUIT_deluxeInit (int *argc, char *argv[])
{
    int i = 1;
    char *dot;
    int	width, height, depth;

#ifdef MACINTOSH
    SUIT_init (argv[0]);  /* There are no command line args in the Mac world. */
    return;
#endif
    InitializeTrace();
    ENTER (1, (buf, "SUIT_deluxeInit()\n"));
    InitializeGlobals ();
    while ((i < *argc) && (*argc >= 2)) {
	if (SUIT_caseInsensitiveMatch (argv[i], DEBUG_OPTION)) {
	    global.debug = TRUE;
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	}
	if (SUIT_caseInsensitiveMatch (argv[i], DYNARRAY_DEBUG)) {
	    SUIT_deluxeSetBoolean (NULL, DYNARRAY_DEBUG, TRUE, GLOBAL);
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	} else if (SUIT_caseInsensitiveMatch (argv[i], SRGP_DEBUG)) {
	    global.srgp_debug = TRUE;
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	}
	/* Just for turning interest on and off, can be taken out when suit
	 * stabilizes */
	else if (SUIT_caseInsensitiveMatch (argv[i], "-interest")) {
	    global.interestOn = FALSE;
	    fprintf(stderr, "running with interests turned off...\n");
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	} else if (SUIT_caseInsensitiveMatch (argv[i], MALLOC_DEBUG)) {
	    global.malloc_debug = TRUE;
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	} else if (SUIT_caseInsensitiveMatch (argv[i], SRGP_TRACE)) {
	    global.srgp_trace = TRUE;
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	} else if (SUIT_caseInsensitiveMatch (argv[i], HELP_OPTION)) {
	    SUIT_printCommandLineOptions ();
	    ShiftPointers (argv, i, 1, *argc);
	    *argc = *argc - 1;
	} else
	    i++;
    }
    i = 1;
    while ((i < *argc) && (*argc >= 3)) {
	if (SUIT_caseInsensitiveMatch (argv[i], HINTS_OPTION)) {
	    if (argv[i + 1][1] != '-') {
		global.hintsfile = (char *) SUIT_malloc (strlen (argv[i + 1]) + 5);
		strcpy (global.hintsfile, argv[i + 1]);
		dot = strrchr (argv[i + 1], (int) '.');
		if (CaseInsensitiveMatch (dot, ".sui") != 0)
		    (void) strcat (global.hintsfile, ".sui");
		ShiftPointers (argv, i, 2, *argc);
		*argc = *argc - 2;
	    } else {
		fprintf (stderr, "\nerror - usage hintsfile /%s 'filename'/\n", HINTS_OPTION);
		exit (0);
	    }
	} else if (SUIT_caseInsensitiveMatch (argv[i], TRACE_OPTION)) {
	    if (argv[i + 1][1] != '-') {
		trace.tracelevel = atoi (argv[i + 1]);
		ShiftPointers (argv, i, 2, *argc);
		*argc = *argc - 2;
	    } else {
		fprintf (stderr, "error - usage tracelevel - %s 'trace-level'\n", TRACE_OPTION);
		exit (0);
	    }
	} else
	    i++;
    }
    CreateProgramName (argv[0]);
    /* if it wasn't specified on the command line: */
    if (global.hintsfile == NULL) {
	/* .sui will replace .exe */
	global.hintsfile = (char *) SUIT_malloc (strlen (global.programName) +6);
	strcpy (global.hintsfile, "");
	global.hintsfile = si_makeFileName (global.hintsfile, global.programName);
    }
    PeekAtHintsFile (&width, &height, &depth);
    FinishInitialization(width, height, depth);
    LEAVE (1, (buf, "SUIT_deluxeInit() [had %d args]\n", *argc));
    SUITInitialized = TRUE;
}



void SUIT_beginStandardApplication (void)
{
    SUIT_beginDisplay ();
    for (;;)
	SUIT_checkAndProcessInput (INDEFINITE);

}

/* Windows Hack! */
#ifdef _Windows
/* These are the property names.  They are declared as variables
   so that they are not declared as static local memory */

char *ACTIVE_DISPLAY 			="active display";
char *ALTERED 			="altered";
char *ANIMATED 			="animated";
char *ANY_KEYSTROKE_TRIGGERS 		="any keystroke triggers";
char *ARROWHEAD_ANGLE 		="arrowhead angle";
char *ARROWHEAD_LENGTH 		="arrowhead length";
char *AXIS_COLOR 			="axis color";
char *BACKGROUND_COLOR 		="background color";
char *BACKWARD_CHAR_KEY 		="backward char key";
char *BALL_SIZE 			="ball size";
char *BALL_X 				="ball x";
char *BALL_Y 				="ball y";
char *BEGINNING_OF_LINE_KEY 		="beginning of line key";
char *BEGINNING_OF_TEXT_KEY 		="beginning of text key";
char *BODY_COLOR 			="body color";
char *BORDER_COLOR 			="border color";
char *BORDER_RAISED 			="border raised";
char *BORDER_TYPE 			="border type";
char *BORDER_WIDTH 			="border width";
char *BUTTON_BACKGROUND_COLOR 	="button background color";
char *BUTTON_FOREGROUND_COLOR 	="button foreground color";
char *BUTTON_PRESSED 			="button pressed";
char *CAB_COLOR 			="cab color";
char *CACHE_USING_CANVAS 		="cache using canvas";
char *CALLBACK_FUNCTION 		="callback function";
char *CAN_BE_OPENED 			="can be opened";
char *CAN_BE_REDIRECTED 		="can be redirected";
char *CAN_COLOR 			="can color";
char *CHAINED_FROM_PROPERTY_TYPE 	="chained from property type";
char *CHAINED_TO_OBJECT 		="chained to object";
char *CHAINED_TO_PROPERTY_TYPE 	="chained to property type";
char *CHAINED_TO_PROPERTY 		="chained to property";
char *CHIP_BORDER 			="chip border";
char *CLIP_TO_VIEWPORT 		="clip to viewport";
char *CONNECTION_CALLBACK_FUNCTION 	="connection callback function";
char *CURRENT_DIRECTORY  		="current directory";
char *CURRENT_FILE                      ="current file";
char *CURRENT_ROW 			="current row";
char *CURRENT_VALUE 			="current value";
char *CURSOR_COLOR 			="cursor color";
char *CURSOR_INDEX 			="cursor index";
char *CURSOR_STYLE 			="cursor style";
char *CUT_BUFFER 			="cut buffer";
char *DARKEN_BACKGROUND                 ="darken background";
char *DEFAULT_OBJECT_HEIGHT 		="default object height";
char *DEFAULT_OBJECT_WIDTH 		="default object width";
char *DELETE_CHAR_KEY 		="delete char key";
char *DELETE_ENTIRE_LINE_KEY 		="delete entire line key";
char *DIRECTION 			="direction";
char *DISABLED_COLOR 			="disabled color";
char *DISABLED 			="disabled";
char *DONE_CALLBACK_FUNCTION 		="done callback function";
char *DONE_EDITING_KEY 		="done editing key";
char *DRAW_AXIS_LINES 		="draw axis lines";
char *DRAW_BORDER_ON_INSIDE 		="draw border on inside";
char *DRAW_FILLED 			="draw filled";
char *DYNARRAY_DEBUG 			="dynarray debug";
char *END_OF_LINE_KEY 		="end of line key";
char *END_OF_TEXT_KEY 		="end of text key";
char *EXPAND_NAME 			="expand name";
char *FILE_FILTER 			="file filter";
char *FILL_TILE 			="fill tile";
char *FILLED 				="filled";
char *FINISHED     			="finished";
char *FONT 				="font";
char *FOREGROUND_COLOR 		="foreground color";
char *FORWARD_CHAR_KEY 		="forward char key";
char *FRAME_NAME 			="frame name";
char *FROM 				="from";
char *GOT_A_MOUSE_DOWN 		="got a mouse down";
char *GRAB_RETURN_KEY 		="grab return key";
char *GRANULARITY 			="granularity";
char *HAS_ARROW_HEAD 			="has arrow head";
char *HAS_ARROW 			="has arrow";
char *HAS_BACKGROUND 			="has background";
char *HAS_BORDER 			="has border";
char *HAS_RIM 			="has rim";
char *HAS_SECOND_HAND 		="has second hand";
char *HAS_TICK_MARKS 			="has tick marks";
char *HEAD_X 				="head x";
char *HEAD_Y 				="head y";
char *HELP_MESSAGE 			="help message";
char *HIGHLIGHT_BLOCK 		="highlight block";
char *HIGHLIGHT_COLOR 		="highlight color";
char *HOTKEY 				="hotkey";
char *INCREASE_CLOCKWISE 		="increase clockwise";
char *INPUT_SEQUENCE 			="input sequence";
char *INSIDE_COLOR 			="inside color";
char *INTERACTIVELY_CREATED 		="interactively created";
char *INTERMEDIATE_FEEDBACK 		="intermediate feedback";
char *JUSTIFICATION 			="justification";
char *KILL_LINE_KEY 			="kill line key";
char *LABEL_NAME 			="label name";
char *LABEL 				="label";
char *LAST_DRAWN_CURRENT_VALUE 	="last drawn current value";
char *LINE_SPACING 			="line spacing";
char *LINE_WIDTH 			="line width";
char *LIST 				="list";
char *MARGIN 				="margin";
char *MARK_COLOR 			="mark color";
char *MARK_END_INDEX 			="mark end index";
char *MARK_INDEX 			="mark index";
char *MAXIMIZED 			="maximized";
char *MAXIMUM_VALUE 			="maximum value";
char *MILITARY_TIME 			="military time";
char *MINIMIZED 			="minimized";
char *MINIMUM_VALUE 			="minimum value";
char *MINIMUM_VIEWPORT 		="minimum viewport";
char *MOVE_HEAD 			="move head";
char *NEEDLE_COLOR 			="needle color";
char *NEXT_LINE_KEY 			="next line key";
char *NEWLINE_KEY                       ="newline key";
char *NUMBER_OF_CHILDREN 		="number of children";
char *NUMBER_OF_CONNECTIONS 		="number of connections";
char *NUMBER_OF_LINES 		="number of lines";
char *NUMBER_OF_SIDES 		="number of sides";
char *OPEN_LINE_KEY 			="open line key";
char *OUTLINE_COLOR 			="outline color";
char *PERCENT_FULL 			="percent full";
char *PIXELS_PER_SECOND 		="pixels per second";
char *PREVIOUS_TIME 			="previous time";
char *PREVIOUS_VALUE 			="previous value";
char *PREVIOUS_LINE_KEY 		="previous line key";
char *RAISED 				="raised";
char *READ_ONLY 			="read only";
char *REDUCE_NAME 			="reduce name";
char *REPAINT_KEY 			="repaint key";
char *RESTORED 			="restored";
char *ROUNDING_FACTOR 		="rounding factor";
char *SCREEN_DEPTH 			="screen depth";
char *SCREEN_HEIGHT 			="screen height";
char *SCREEN_WIDTH 			="screen width";
char *SCROLL_POSITION 		="scroll position";
char *SCROLL_DOWN_KEY 		="scroll down key";
char *SCROLL_UP_KEY 			="scroll up key";
char *SCROLLER 			="scroller";
char *SET_MARK_KEY 			="set mark key";
char *SHADOW_THICKNESS 		="shadow thickness";
char *SHOW_TEMPORARY_PROPERTIES 	="show temporary properties";
char *SHRINK_TO_FIT 			="shrink to fit";
char *SLIDING 			="sliding";
char *SPACING_GAP 			="spacing gap";
char *SPRINGINESS 			="springiness";
char *START_ANGLE 			="start angle";
char *STARTING_LINE			="starting line";
char *SUIT_PROPERTY_EDITOR 		="SUIT property editor";
char *SUIT_SYSTEM_FONT 		="SUIT system font";
char *TAB_KEY                           ="tab key";
char *TAB_LENGTH 			="tab length";
char *TAIL_X 				="tail x";
char *TAIL_Y 				="tail y";
char *TEXT_COLOR 			="text color";
char *TEXT_SPACING 			="text spacing";
char *TILE_COLOR 			="tile color";
char *TILE_EXTENTS 			="tile extents";
char *TO 				="to";
char *VALID_COLORS 			="valid colors";
char *VALIDATION_FUNCTION 		="validation function";
char *VECTOR_X 			="vector X";
char *VECTOR_Y 			="vector Y";
char *VERTICAL_MENU_VIEWPORT 		="vertical menu viewport";
char *VIEWPORT 			="viewport";
char *VISIBLE_WITHIN_PROPERTY_EDITOR 	="visible within property editor";
char *VISIBLE 			="visible";
char *WALLPAPER 			="wallpaper";
char *WALLPAPER_FUNCTION 		="wallpaper function";
char *WHEEL_COLOR 			="wheel color";
char *WINDOW 				="window";
char *WIPE_BLOCK_KEY 			="wipe block key";
char *YANK_KEY 			="yank key";

#endif
