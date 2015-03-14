/* 
 * tkColor.c --
 *
 *	This file maintains a database of color values for the Tk
 *	toolkit, in order to avoid round-trips to the server to
 *	map color names to pixel values.
 *
 * Copyright 1990 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef lint
static char rcsid[] = "$Header: /user6/ouster/wish/RCS/tkColor.c,v 1.21 92/12/16 11:57:53 ouster Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include "tkConfig.h"
#include "tk.h"
#include "tkInt.h"

/*
 * A two-level data structure is used to manage the color database.
 * The top level consists of one entry for each color name that is
 * currently active, and the bottom level contains one entry for each
 * pixel value that is still in use.  The distinction between
 * levels is necessary because the same pixel may have several
 * different names.  There are two hash tables, one used to index into
 * each of the data structures.  The name hash table is used when
 * allocating colors, and the pixel hash table is used when freeing
 * colors.
 */

/*
 * One of the following data structures is used to keep track of
 * each color that this module has allocated from the X display
 * server.  These entries are indexed by two hash tables defined
 * below:  nameTable and valueTable.
 */

#define COLOR_MAGIC ((unsigned int) 0xc6140277)

typedef struct TkColor {
    XColor color;		/* Information about this color. */
    unsigned int magic;		/* Used for quick integrity check on this
				 * structure.   Must always have the
				 * value COLOR_MAGIC. */
    Screen *screen;		/* Screen where this color is valid.  Used
				 * to delete it. */
    Colormap colormap;		/* Colormap from which this entry was
				 * allocated. */
    int refCount;		/* Number of uses of this structure. */
    Tcl_HashTable *tablePtr;	/* Hash table that indexes this structure
				 * (needed when deleting structure). */
    Tcl_HashEntry *hashPtr;	/* Pointer to hash table entry for this
				 * structure. (for use in deleting entry). */
} TkColor;

/*
 * Hash table for name -> TkColor mapping, and key structure used to
 * index into that table:
 */

static Tcl_HashTable nameTable;
typedef struct {
    Tk_Uid name;		/* Name of desired color. */
    Colormap colormap;		/* Colormap from which color will be
				 * allocated. */
    Display *display;		/* Display for colormap. */
} NameKey;

/*
 * Hash table for value -> TkColor mapping, and key structure used to
 * index into that table:
 */

static Tcl_HashTable valueTable;
typedef struct {
    int red, green, blue;	/* Values for desired color. */
    Colormap colormap;		/* Colormap from which color will be
				 * allocated. */
    Display *display;		/* Display for colormap. */
} ValueKey;

static int initialized = 0;	/* 0 means static structures haven't been
				 * initialized yet. */

/*
 * Forward declarations for procedures defined in this file:
 */

static void		ColorInit _ANSI_ARGS_((void));

/*
 *----------------------------------------------------------------------
 *
 * Tk_GetColor --
 *
 *	Given a string name for a color, map the name to a corresponding
 *	XColor structure.
 *
 * Results:
 *	The return value is a pointer to an XColor structure that
 *	indicates the red, blue, and green intensities for the color
 *	given by "name", and also specifies a pixel value to use to
 *	draw in that color in window "tkwin".  If an error occurs,
 *	then NULL is returned and an error message will be left in
 *	interp->result.
 *
 * Side effects:
 *	The color is added to an internal database with a reference count.
 *	For each call to this procedure, there should eventually be a call
 *	to Tk_FreeColor, so that the database is cleaned up when colors
 *	aren't in use anymore.
 *
 *----------------------------------------------------------------------
 */

XColor *
Tk_GetColor(interp, tkwin, colormap, name)
    Tcl_Interp *interp;		/* Place to leave error message if
				 * color can't be found. */
    Tk_Window tkwin;		/* Window in which color will be used. */
    Colormap colormap;		/* Map from which to allocate color.  None
				 * means use current colormap for tkwin. */
    Tk_Uid name;		/* Name of color to allocated (in form
				 * suitable for passing to XParseColor). */
{
    NameKey nameKey;
    Tcl_HashEntry *nameHashPtr;
    int new;
    TkColor *tkColPtr;
    XColor color;

    if (!initialized) {
	ColorInit();
    }

    /*
     * First, check to see if there's already a mapping for this color
     * name.
     */

    nameKey.name = name;
    if (colormap == None) {
	colormap = Tk_Colormap(tkwin);
    }
    nameKey.colormap = colormap;
    nameKey.display = Tk_Display(tkwin);
    nameHashPtr = Tcl_CreateHashEntry(&nameTable, (char *) &nameKey, &new);
    if (!new) {
	tkColPtr = (TkColor *) Tcl_GetHashValue(nameHashPtr);
	tkColPtr->refCount++;
	return &tkColPtr->color;
    }

    /*
     * The name isn't currently known.  Map from the name to a pixel
     * value.  There are several tricks here:
     *
     * 1. Call XAllocNamedColor rather than XParseColor for non-# names:
     *    this saves a server round-trip for those names.
     * 2. If the display is supposed to be treated as mono, then look
     *    up the color value and translate to mono.
     * 3. If a color allocation fails, then reset the display's color model
     *    to mono, generate an error message, and retry with the mono
     *    approach.
     */

    if (*name != '#') {
	XColor screen;

	if (Tk_GetColorModel(tkwin) != TK_COLOR) {
	    if (XLookupColor(Tk_Display(tkwin), colormap, name, &color,
		    &screen) == 0) {
		badColorName:
		Tcl_AppendResult(interp, "unknown color name \"",
			name, "\"", (char *) NULL);
		Tcl_DeleteHashEntry(nameHashPtr);
		return (XColor *) NULL;
	    }
	    goto useMonoApproximation;
	}

	if (XAllocNamedColor(Tk_Display(tkwin), colormap, name,
		&screen, &color) == 0) {
	    char message[120];

	    /*
	     * Couldn't allocate the color.  Try translating the name to
	     * a color value, to see whether the problem is a bad color
	     * name or a full colormap.  If the colormap is full, then
	     * put the display into mono mode and
	     */

	    if (XLookupColor(Tk_Display(tkwin), colormap, name, &color,
		    &screen) == 0) {
		goto badColorName;
	    }
	    outOfColors:
	    Tk_SetColorModel(tkwin, TK_MONO);
	    Tcl_AppendResult(interp, "no more colors left in colormap; ",
		    "changing screen's color model to monochrome",
		    (char *) NULL);
	    sprintf(message, "\n    (while allocating color \"%.50s\")", name);
	    Tcl_AddErrorInfo(interp, message);
	    Tk_BackgroundError(interp);
	    goto useMonoApproximation;
	}
    } else {
	if (XParseColor(Tk_Display(tkwin), colormap, name, &color) == 0) {
	    Tcl_AppendResult(interp, "invalid color name \"", name,
		    "\"", (char *) NULL);
	    Tcl_DeleteHashEntry(nameHashPtr);
	    return (XColor *) NULL;
	}
	if (Tk_GetColorModel(tkwin) != TK_COLOR) {
	    useMonoApproximation:
	    if ((color.red + color.green + color.blue) > (3*65535)/2) {
		color.pixel = WhitePixelOfScreen(Tk_Screen(tkwin));
	    } else {
		color.pixel = BlackPixelOfScreen(Tk_Screen(tkwin));
	    }
	} else if (XAllocColor(Tk_Display(tkwin), colormap, &color) == 0) {
	    goto outOfColors;
	}
    }

    /*
     * Now create a new TkColor structure and add it to nameTable.
     */

    tkColPtr = (TkColor *) ckalloc(sizeof(TkColor));
    tkColPtr->color = color;
    tkColPtr->magic = COLOR_MAGIC;
    tkColPtr->screen = Tk_Screen(tkwin);
    tkColPtr->colormap = colormap;
    tkColPtr->refCount = 1;
    tkColPtr->tablePtr = &nameTable;
    tkColPtr->hashPtr = nameHashPtr;
    Tcl_SetHashValue(nameHashPtr, tkColPtr);

    return &tkColPtr->color;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_GetColorByValue --
 *
 *	Given a desired set of red-green-blue intensities for a color,
 *	locate a pixel value to use to draw that color in a given
 *	window.
 *
 * Results:
 *	The return value is a pointer to an XColor structure that
 *	indicates the closest red, blue, and green intensities available
 *	to those specified in colorPtr, and also specifies a pixel
 *	value to use to draw in that color in window "tkwin".  If an
 *	error occurs, then NULL is returned and an error message will
 *	be left in interp->result.
 *
 * Side effects:
 *	The color is added to an internal database with a reference count.
 *	For each call to this procedure, there should eventually be a call
 *	to Tk_FreeColor, so that the database is cleaned up when colors
 *	aren't in use anymore.
 *
 *----------------------------------------------------------------------
 */

XColor *
Tk_GetColorByValue(interp, tkwin, colormap, colorPtr)
    Tcl_Interp *interp;		/* Place to leave error message if
				 * color can't be found. */
    Tk_Window tkwin;		/* Window in which color will be used. */
    Colormap colormap;		/* Map from which to allocate color.  None
				 * means use current colormap for tkwin. */
    XColor *colorPtr;		/* Red, green, and blue fields indicate
				 * desired color. */
{
    ValueKey valueKey;
    Tcl_HashEntry *valueHashPtr;
    int new;
    TkColor *tkColPtr;

    if (!initialized) {
	ColorInit();
    }

    /*
     * First, check to see if there's already a mapping for this color
     * name.
     */

    valueKey.red = colorPtr->red;
    valueKey.green = colorPtr->green;
    valueKey.blue = colorPtr->blue;
    if (colormap == None) {
	colormap = Tk_Colormap(tkwin);
    }
    valueKey.colormap = colormap;
    valueKey.display = Tk_Display(tkwin);
    valueHashPtr = Tcl_CreateHashEntry(&valueTable, (char *) &valueKey, &new);
    if (!new) {
	tkColPtr = (TkColor *) Tcl_GetHashValue(valueHashPtr);
	tkColPtr->refCount++;
	return &tkColPtr->color;
    }

    /*
     * The name isn't currently known.  Find a pixel value for this
     * color and add a new structure to valueTable.
     */

    tkColPtr = (TkColor *) ckalloc(sizeof(TkColor));
    tkColPtr->color.red = valueKey.red;
    tkColPtr->color.green = valueKey.green;
    tkColPtr->color.blue = valueKey.blue;
    if (Tk_GetColorModel(tkwin) != TK_COLOR) {
	useMonoApproximation:
	if ((tkColPtr->color.red + tkColPtr->color.green
		+ tkColPtr->color.blue) > (3*65535)/2) {
	    tkColPtr->color.pixel = WhitePixelOfScreen(Tk_Screen(tkwin));
	} else {
	    tkColPtr->color.pixel = BlackPixelOfScreen(Tk_Screen(tkwin));
	}
    } else {
	if (XAllocColor(Tk_Display(tkwin), colormap, &tkColPtr->color) == 0) {
	    Tk_SetColorModel(tkwin, TK_MONO);
	    Tcl_AppendResult(interp, "no more colors left in colormap; ",
		    "changing screen's color model to monochrome",
		    (char *) NULL);
	    Tcl_AddErrorInfo(interp,
		    "\n    (while allocating color in Tk_GetColorByValue)");
	    Tk_BackgroundError(interp);
	    goto useMonoApproximation;
	}
    }
    tkColPtr->magic = COLOR_MAGIC;
    tkColPtr->screen = Tk_Screen(tkwin);
    tkColPtr->colormap = colormap;
    tkColPtr->refCount = 1;
    tkColPtr->tablePtr = &valueTable;
    tkColPtr->hashPtr = valueHashPtr;
    Tcl_SetHashValue(valueHashPtr, tkColPtr);

    return &tkColPtr->color;
}

/*
 *--------------------------------------------------------------
 *
 * Tk_NameOfColor --
 *
 *	Given a color, return a textual string identifying
 *	the color.
 *
 * Results:
 *	If colorPtr was created by Tk_GetColor, then the return
 *	value is the "string" that was used to create it.
 *	Otherwise the return value is a string that could have
 *	been passed to Tk_GetColor to allocate that color.  The
 *	storage for the returned string is only guaranteed to
 *	persist up until the next call to this procedure.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

char *
Tk_NameOfColor(colorPtr)
    XColor *colorPtr;		/* Color whose name is desired. */
{
    register TkColor *tkColPtr = (TkColor *) colorPtr;
    static char string[20];

    if ((tkColPtr->magic == COLOR_MAGIC)
	    && (tkColPtr->tablePtr == &nameTable)) {
	return ((NameKey *) tkColPtr->hashPtr->key.words)->name;
    }
    sprintf(string, "#%4x%4x%4x", colorPtr->red, colorPtr->green,
	    colorPtr->blue);
    return string;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_FreeColor --
 *
 *	This procedure is called to release a color allocated by
 *	Tk_GetColor.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The reference count associated with colorPtr is deleted, and
 *	the color is released to X if there are no remaining uses
 *	for it.
 *
 *----------------------------------------------------------------------
 */

void
Tk_FreeColor(colorPtr)
    XColor *colorPtr;		/* Color to be released.  Must have been
				 * allocated by Tk_GetColor or
				 * Tk_GetColorByValue. */
{
    register TkColor *tkColPtr = (TkColor *) colorPtr;
    Visual *visual;
    Screen *screen = tkColPtr->screen;

    /*
     * Do a quick sanity check to make sure this color was really
     * allocated by Tk_GetColor.
     */

    if (tkColPtr->magic != COLOR_MAGIC) {
	panic("Tk_FreeColor called with bogus color");
    }

    tkColPtr->refCount--;
    if (tkColPtr->refCount == 0) {

	/*
	 * Careful!  Don't free black or white, since this will
	 * make some servers very unhappy.  Also, there is a bug in
	 * some servers (such Sun's X11/NeWS server) where reference
	 * counting is performed incorrectly, so that if a color is
	 * allocated twice in different places and then freed twice,
	 * the second free generates an error (this bug existed as of
	 * 10/1/92).  To get around this problem, ignore errors that
	 * occur during the free operation.
	 */

	visual = DefaultVisualOfScreen(screen);
	if ((visual->class != StaticGray) && (visual->class != StaticColor)
		&& (tkColPtr->color.pixel != BlackPixelOfScreen(screen))
		&& (tkColPtr->color.pixel != WhitePixelOfScreen(screen))) {
	    Tk_ErrorHandler handler;

	    handler = Tk_CreateErrorHandler(DisplayOfScreen(screen),
		    -1, -1, -1, (Tk_ErrorProc *) NULL, (ClientData) NULL);
	    XFreeColors(DisplayOfScreen(screen), tkColPtr->colormap,
		    &tkColPtr->color.pixel, 1, 0L);
	    Tk_DeleteErrorHandler(handler);
	}
	Tcl_DeleteHashEntry(tkColPtr->hashPtr);
	tkColPtr->magic = 0;
	ckfree((char *) tkColPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ColorInit --
 *
 *	Initialize the structure used for color management.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Read the code.
 *
 *----------------------------------------------------------------------
 */

static void
ColorInit()
{
    initialized = 1;
    Tcl_InitHashTable(&nameTable, sizeof(NameKey)/sizeof(int));
    Tcl_InitHashTable(&valueTable, sizeof(ValueKey)/sizeof(int));
}
