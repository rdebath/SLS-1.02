/* 
 * tkCursor.c --
 *
 *	This file maintains a database of read-only cursors for the Tk
 *	toolkit.  This allows cursors to be shared between widgets and
 *	also avoids round-trips to the X server.
 *
 * Copyright 1990-1992 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef lint
static char rcsid[] = "$Header: /user6/ouster/wish/RCS/tkCursor.c,v 1.13 92/12/14 11:18:11 ouster Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include "tkConfig.h"
#include "tkInt.h"

/*
 * One of the following data structures exists for each cursor that is
 * currently active.  Each structure is indexed with two hash tables
 * defined below.  One of the tables is idTable, and the other is either
 * nameTable or dataTable, also defined below.
 * .
 */

typedef struct {
    Cursor cursor;		/* X identifier for cursor. */
    Display *display;		/* Display for which cursor is valid. */
    int refCount;		/* Number of active uses of cursor. */
    Tcl_HashTable *otherTable;	/* Second table (other than idTable) used
				 * to index this entry. */
    Tcl_HashEntry *hashPtr;	/* Entry in otherTable for this structure
				 * (needed when deleting). */
} TkCursor;

/*
 * Hash table to map from a textual description of a cursor to the
 * TkCursor record for the cursor, and key structure used in that
 * hash table:
 */

static Tcl_HashTable nameTable;
typedef struct {
    Tk_Uid name;		/* Textual name for desired cursor. */
    Display *display;		/* Display for which cursor will be used. */
} NameKey;

/*
 * Hash table to map from a collection of in-core data about a
 * cursor (bitmap contents, etc.) to a TkCursor structure:
 */

static Tcl_HashTable dataTable;
typedef struct {
    char *source;		/* Cursor bits. */
    char *mask;			/* Mask bits. */
    unsigned int width, height;	/* Dimensions of cursor (and data
				 * and mask). */
    int xHot, yHot;		/* Location of cursor hot-spot. */
    Tk_Uid fg, bg;		/* Colors for cursor. */
    Display *display;		/* Display on which cursor will be used. */
} DataKey;

/*
 * Hash table that maps from <display + cursor id> to the TkCursor structure
 * for the cursor.  This table is used by Tk_FreeCursor.
 */

static Tcl_HashTable idTable;
typedef struct {
    Display *display;		/* Display for which cursor was allocated. */
    Cursor cursor;		/* Cursor identifier. */
} IdKey;

static int initialized = 0;	/* 0 means static structures haven't been
				 * initialized yet. */

/*
 * The table below is used to map from the name of a cursor to its
 * index in the official cursor font:
 */

static struct CursorName {
    char		*name;
    unsigned int	shape;
} cursorNames[] = {
    {"X_cursor",		XC_X_cursor},
    {"arrow",			XC_arrow},
    {"based_arrow_down",	XC_based_arrow_down},
    {"based_arrow_up",		XC_based_arrow_up},
    {"boat",			XC_boat},
    {"bogosity",		XC_bogosity},
    {"bottom_left_corner",	XC_bottom_left_corner},
    {"bottom_right_corner",	XC_bottom_right_corner},
    {"bottom_side",		XC_bottom_side},
    {"bottom_tee",		XC_bottom_tee},
    {"box_spiral",		XC_box_spiral},
    {"center_ptr",		XC_center_ptr},
    {"circle",			XC_circle},
    {"clock",			XC_clock},
    {"coffee_mug",		XC_coffee_mug},
    {"cross",			XC_cross},
    {"cross_reverse",		XC_cross_reverse},
    {"crosshair",		XC_crosshair},
    {"diamond_cross",		XC_diamond_cross},
    {"dot",			XC_dot},
    {"dotbox",			XC_dotbox},
    {"double_arrow",		XC_double_arrow},
    {"draft_large",		XC_draft_large},
    {"draft_small",		XC_draft_small},
    {"draped_box",		XC_draped_box},
    {"exchange",		XC_exchange},
    {"fleur",			XC_fleur},
    {"gobbler",			XC_gobbler},
    {"gumby",			XC_gumby},
    {"hand1",			XC_hand1},
    {"hand2",			XC_hand2},
    {"heart",			XC_heart},
    {"icon",			XC_icon},
    {"iron_cross",		XC_iron_cross},
    {"left_ptr",		XC_left_ptr},
    {"left_side",		XC_left_side},
    {"left_tee",		XC_left_tee},
    {"leftbutton",		XC_leftbutton},
    {"ll_angle",		XC_ll_angle},
    {"lr_angle",		XC_lr_angle},
    {"man",			XC_man},
    {"middlebutton",		XC_middlebutton},
    {"mouse",			XC_mouse},
    {"pencil",			XC_pencil},
    {"pirate",			XC_pirate},
    {"plus",			XC_plus},
    {"question_arrow",		XC_question_arrow},
    {"right_ptr",		XC_right_ptr},
    {"right_side",		XC_right_side},
    {"right_tee",		XC_right_tee},
    {"rightbutton",		XC_rightbutton},
    {"rtl_logo",		XC_rtl_logo},
    {"sailboat",		XC_sailboat},
    {"sb_down_arrow",		XC_sb_down_arrow},
    {"sb_h_double_arrow",	XC_sb_h_double_arrow},
    {"sb_left_arrow",		XC_sb_left_arrow},
    {"sb_right_arrow",		XC_sb_right_arrow},
    {"sb_up_arrow",		XC_sb_up_arrow},
    {"sb_v_double_arrow",	XC_sb_v_double_arrow},
    {"shuttle",			XC_shuttle},
    {"sizing",			XC_sizing},
    {"spider",			XC_spider},
    {"spraycan",		XC_spraycan},
    {"star",			XC_star},
    {"target",			XC_target},
    {"tcross",			XC_tcross},
    {"top_left_arrow",		XC_top_left_arrow},
    {"top_left_corner",		XC_top_left_corner},
    {"top_right_corner",	XC_top_right_corner},
    {"top_side",		XC_top_side},
    {"top_tee",			XC_top_tee},
    {"trek",			XC_trek},
    {"ul_angle",		XC_ul_angle},
    {"umbrella",		XC_umbrella},
    {"ur_angle",		XC_ur_angle},
    {"watch",			XC_watch},
    {"xterm",			XC_xterm},
    {NULL,			0}
};

/*
 * Font to use for cursors:
 */

#ifndef CURSORFONT
#define CURSORFONT "cursor"
#endif

/*
 * Forward declarations for procedures defined in this file:
 */

static void		CursorInit _ANSI_ARGS_((void));

/*
 *----------------------------------------------------------------------
 *
 * Tk_GetCursor --
 *
 *	Given a string describing a cursor, locate (or create if necessary)
 *	a cursor that fits the description.
 *
 * Results:
 *	The return value is the X identifer for the desired cursor,
 *	unless string couldn't be parsed correctly.  In this case,
 *	None is returned and an error message is left in interp->result.
 *	The caller should never modify the cursor that is returned, and
 *	should eventually call Tk_FreeCursor when the cursor is no longer
 *	needed.
 *
 * Side effects:
 *	The cursor is added to an internal database with a reference count.
 *	For each call to this procedure, there should eventually be a call
 *	to Tk_FreeCursor, so that the database can be cleaned up when cursors
 *	aren't needed anymore.
 *
 *----------------------------------------------------------------------
 */

Cursor
Tk_GetCursor(interp, tkwin, string)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    Tk_Window tkwin;		/* Window in which cursor will be used. */
    Tk_Uid string;		/* Description of cursor.  See manual entry
				 * for details on legal syntax. */
{
    NameKey nameKey;
    IdKey idKey;
    Tcl_HashEntry *nameHashPtr, *idHashPtr;
    register TkCursor *cursorPtr;
    int new;
    Cursor cursor;
    int argc;
    char **argv = NULL;
    Pixmap source = None;
    Pixmap mask = None;

    if (!initialized) {
	CursorInit();
    }

    nameKey.name = string;
    nameKey.display = Tk_Display(tkwin);
    nameHashPtr = Tcl_CreateHashEntry(&nameTable, (char *) &nameKey, &new);
    if (!new) {
	cursorPtr = (TkCursor *) Tcl_GetHashValue(nameHashPtr);
	cursorPtr->refCount++;
	return cursorPtr->cursor;
    }

    /*
     * No suitable cursor exists.  Parse the cursor name into fields
     * and create a cursor, either from the standard cursor font or
     * from bitmap files.
     */

    if (Tcl_SplitList(interp, string, &argc, &argv) != TCL_OK) {
	goto error;
    }
    if (argc == 0) {
	badString:
	Tcl_AppendResult(interp, "bad cursor spec \"", string, "\"",
		(char *) NULL);
	goto error;
    }
    if (argv[0][0] != '@') {
	XColor fg, bg;
	int maskIndex;
	register struct CursorName *namePtr;
	TkDisplay *dispPtr;

	/*
	 * The cursor is to come from the standard cursor font.  If one
	 * arg, it is cursor name (use black and white for fg and bg).
	 * If two args, they are name and fg color (ignore mask).  If
	 * three args, they are name, fg, bg.  Some of the code below
	 * is stolen from the XCreateFontCursor Xlib procedure.
	 */

	if (argc > 3) {
	    goto badString;
	}
	for (namePtr = cursorNames; ; namePtr++) {
	    if (namePtr->name == NULL) {
		goto badString;
	    }
	    if ((namePtr->name[0] == argv[0][0])
		    && (strcmp(namePtr->name, argv[0]) == 0)) {
		break;
	    }
	}
	maskIndex = namePtr->shape + 1;
	if (argc == 1) {
	    fg.red = fg.green = fg.blue = 0;
	    bg.red = bg.green = bg.blue = 65535;
	} else {
	    if (XParseColor(nameKey.display,
		    DefaultColormapOfScreen(Tk_Screen(tkwin)),
		    argv[1], &fg) == 0) {
		Tcl_AppendResult(interp, "invalid color name \"", argv[1],
			"\"", (char *) NULL);
		goto error;
	    }
	    if (argc == 2) {
		bg.red = bg.green = bg.blue = 0;
		maskIndex = namePtr->shape;
	    } else {
		if (XParseColor(nameKey.display,
			DefaultColormapOfScreen(Tk_Screen(tkwin)),
			argv[2], &bg) == 0) {
		    Tcl_AppendResult(interp, "invalid color name \"", argv[2],
			    "\"", (char *) NULL);
		    goto error;
		}
	    }
	}
	dispPtr = ((TkWindow *) tkwin)->dispPtr;
	if (dispPtr->cursorFont == None) {
	    dispPtr->cursorFont = XLoadFont(nameKey.display, CURSORFONT);
	    if (dispPtr->cursorFont == None) {
		interp->result = "couldn't load cursor font";
		goto error;
	    }
	}
	cursor = XCreateGlyphCursor(nameKey.display, dispPtr->cursorFont,
		dispPtr->cursorFont, namePtr->shape, maskIndex,
		&fg, &bg);
    } else {
	unsigned int width, height, maskWidth, maskHeight;
	int xHot, yHot, dummy1, dummy2;
	XColor fg, bg;

	/*
	 * The cursor is to be created by reading bitmap files.  There
	 * should be either two elements in the list (source, color) or
	 * four (source mask fg bg).
	 */

	if ((argc != 2) && (argc != 4)) {
	    goto badString;
	}
	if (XReadBitmapFile(nameKey.display,
		RootWindowOfScreen(Tk_Screen(tkwin)), &argv[0][1],
		&width, &height, &source, &xHot, &yHot)
		!= BitmapSuccess) {
	    Tcl_AppendResult(interp, "error reading bitmap file \"",
		    &argv[0][1], "\"", (char *) NULL);
	    goto error;
	}
	if ((xHot < 0) || (yHot < 0) || (xHot >= width) || (yHot >= height)) {
	    Tcl_AppendResult(interp, "bad hot spot in bitmap file \"",
		    &argv[0][1], "\"", (char *) NULL);
	    goto error;
	}
	if (argc == 2) {
	    if (XParseColor(nameKey.display,
		    DefaultColormapOfScreen(Tk_Screen(tkwin)),
		    argv[1], &fg) == 0) {
		Tcl_AppendResult(interp, "invalid color name \"",
			argv[1], "\"", (char *) NULL);
		goto error;
	    }
	    cursor = XCreatePixmapCursor(nameKey.display, source, source,
		    &fg, &fg, xHot, yHot);
	} else {
	    if (XReadBitmapFile(nameKey.display,
		    RootWindowOfScreen(Tk_Screen(tkwin)), argv[1],
		    &maskWidth, &maskHeight, &mask, &dummy1,
		    &dummy2) != BitmapSuccess) {
		Tcl_AppendResult(interp, "error reading bitmap file \"",
			argv[1], "\"", (char *) NULL);
		goto error;
	    }
	    if ((maskWidth != width) && (maskHeight != height)) {
		interp->result =
			"source and mask bitmaps have different sizes";
		goto error;
	    }
	    if (XParseColor(nameKey.display,
		    DefaultColormapOfScreen(Tk_Screen(tkwin)),
		    argv[2], &fg) == 0) {
		Tcl_AppendResult(interp, "invalid color name \"", argv[2],
			"\"", (char *) NULL);
		goto error;
	    }
	    if (XParseColor(nameKey.display,
		    DefaultColormapOfScreen(Tk_Screen(tkwin)),
		    argv[3], &bg) == 0) {
		Tcl_AppendResult(interp, "invalid color name \"", argv[3],
			"\"", (char *) NULL);
		goto error;
	    }
	    cursor = XCreatePixmapCursor(nameKey.display, source, mask,
		    &fg, &bg, xHot, yHot);
	}
    }
    ckfree((char *) argv);

    /*
     * Add information about this cursor to our database.
     */

    cursorPtr = (TkCursor *) ckalloc(sizeof(TkCursor));
    cursorPtr->cursor = cursor;
    cursorPtr->display = nameKey.display;
    cursorPtr->refCount = 1;
    cursorPtr->otherTable = &nameTable;
    cursorPtr->hashPtr = nameHashPtr;
    idKey.display = nameKey.display;
    idKey.cursor = cursor;
    idHashPtr = Tcl_CreateHashEntry(&idTable, (char *) &idKey, &new);
    if (!new) {
	panic("cursor already registered in Tk_GetCursor");
    }
    Tcl_SetHashValue(nameHashPtr, cursorPtr);
    Tcl_SetHashValue(idHashPtr, cursorPtr);
    return cursorPtr->cursor;

    error:
    Tcl_DeleteHashEntry(nameHashPtr);
    if (argv != NULL) {
	ckfree((char *) argv);
    }
    if (source != None) {
	XFreePixmap(nameKey.display, source);
    }
    if (mask != None) {
	XFreePixmap(nameKey.display, mask);
    }
    return None;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_GetCursorFromData --
 *
 *	Given a description of the bits and colors for a cursor,
 *	make a cursor that has the given properties.
 *
 * Results:
 *	The return value is the X identifer for the desired cursor,
 *	unless it couldn't be created properly.  In this case, None is
 *	returned and an error message is left in interp->result.  The
 *	caller should never modify the cursor that is returned, and
 *	should eventually call Tk_FreeCursor when the cursor is no
 *	longer needed.
 *
 * Side effects:
 *	The cursor is added to an internal database with a reference count.
 *	For each call to this procedure, there should eventually be a call
 *	to Tk_FreeCursor, so that the database can be cleaned up when cursors
 *	aren't needed anymore.
 *
 *----------------------------------------------------------------------
 */

Cursor
Tk_GetCursorFromData(interp, tkwin, source, mask, width, height,
	xHot, yHot, fg, bg)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    Tk_Window tkwin;		/* Window in which cursor will be used. */
    char *source;		/* Bitmap data for cursor shape. */
    char *mask;			/* Bitmap data for cursor mask. */
    unsigned int width, height;	/* Dimensions of cursor. */
    int xHot, yHot;		/* Location of hot-spot in cursor. */
    Tk_Uid fg;			/* Foreground color for cursor. */
    Tk_Uid bg;			/* Background color for cursor. */
{
    DataKey dataKey;
    IdKey idKey;
    Tcl_HashEntry *dataHashPtr, *idHashPtr;
    register TkCursor *cursorPtr;
    int new;
    XColor fgColor, bgColor;
    Pixmap sourcePixmap, maskPixmap;

    if (!initialized) {
	CursorInit();
    }

    dataKey.source = source;
    dataKey.mask = mask;
    dataKey.width = width;
    dataKey.height = height;
    dataKey.xHot = xHot;
    dataKey.yHot = yHot;
    dataKey.fg = fg;
    dataKey.bg = bg;
    dataKey.display = Tk_Display(tkwin);
    dataHashPtr = Tcl_CreateHashEntry(&dataTable, (char *) &dataKey, &new);
    if (!new) {
	cursorPtr = (TkCursor *) Tcl_GetHashValue(dataHashPtr);
	cursorPtr->refCount++;
	return cursorPtr->cursor;
    }

    /*
     * No suitable cursor exists yet.  Make one using the data
     * available and add it to the database.
     */

    if (XParseColor(dataKey.display, DefaultColormapOfScreen(Tk_Screen(tkwin)),
	    fg, &fgColor) == 0) {
	Tcl_AppendResult(interp, "invalid color name \"", fg, "\"",
		(char *) NULL);
	goto error;
    }
    if (XParseColor(dataKey.display, DefaultColormapOfScreen(Tk_Screen(tkwin)),
	    bg, &bgColor) == 0) {
	Tcl_AppendResult(interp, "invalid color name \"", bg, "\"",
		(char *) NULL);
	goto error;
    }

    cursorPtr = (TkCursor *) ckalloc(sizeof(TkCursor));
    sourcePixmap = XCreateBitmapFromData(dataKey.display,
	    RootWindowOfScreen(Tk_Screen(tkwin)), source, width, height);
    maskPixmap = XCreateBitmapFromData(dataKey.display, 
	    RootWindowOfScreen(Tk_Screen(tkwin)), mask, width, height);
    cursorPtr->cursor = XCreatePixmapCursor(dataKey.display, sourcePixmap,
	    maskPixmap, &fgColor, &bgColor, xHot, yHot);
    XFreePixmap(dataKey.display, sourcePixmap);
    XFreePixmap(dataKey.display, maskPixmap);
    cursorPtr->display = dataKey.display;
    cursorPtr->refCount = 1;
    cursorPtr->otherTable = &dataTable;
    cursorPtr->hashPtr = dataHashPtr;
    idKey.display = dataKey.display;
    idKey.cursor = cursorPtr->cursor;
    idHashPtr = Tcl_CreateHashEntry(&idTable, (char *) &idKey, &new);
    if (!new) {
	panic("cursor already registered in Tk_GetCursorFromData");
    }
    Tcl_SetHashValue(dataHashPtr, cursorPtr);
    Tcl_SetHashValue(idHashPtr, cursorPtr);
    return cursorPtr->cursor;

    error:
    Tcl_DeleteHashEntry(dataHashPtr);
    return None;
}

/*
 *--------------------------------------------------------------
 *
 * Tk_NameOfCursor --
 *
 *	Given a cursor, return a textual string identifying it.
 *
 * Results:
 *	If cursor was created by Tk_GetCursor, then the return
 *	value is the "string" that was used to create it.
 *	Otherwise the return value is a string giving the X
 *	identifier for the cursor.  The storage for the returned
 *	string is only guaranteed to persist up until the next
 *	call to this procedure.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

char *
Tk_NameOfCursor(display, cursor)
    Display *display;		/* Display for which cursor was allocated. */
    Cursor cursor;		/* Identifier for cursor whose name is
				 * wanted. */
{
    IdKey idKey;
    Tcl_HashEntry *idHashPtr;
    TkCursor *cursorPtr;
    static char string[20];

    if (!initialized) {
	printid:
	sprintf(string, "cursor id 0x%x", cursor);
	return string;
    }
    idKey.display = display;
    idKey.cursor = cursor;
    idHashPtr = Tcl_FindHashEntry(&idTable, (char *) &idKey);
    if (idHashPtr == NULL) {
	goto printid;
    }
    cursorPtr = (TkCursor *) Tcl_GetHashValue(idHashPtr);
    if (cursorPtr->otherTable != &nameTable) {
	goto printid;
    }
    return ((NameKey *) cursorPtr->hashPtr->key.words)->name;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_FreeCursor --
 *
 *	This procedure is called to release a cursor allocated by
 *	Tk_GetCursor or TkGetCursorFromData.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The reference count associated with cursor is decremented, and
 *	it is officially deallocated if no-one is using it anymore.
 *
 *----------------------------------------------------------------------
 */

void
Tk_FreeCursor(display, cursor)
    Display *display;		/* Display for which cursor was allocated. */
    Cursor cursor;		/* Identifier for cursor to be released. */
{
    IdKey idKey;
    Tcl_HashEntry *idHashPtr;
    register TkCursor *cursorPtr;

    if (!initialized) {
	panic("Tk_FreeCursor called before Tk_GetCursor");
    }

    idKey.display = display;
    idKey.cursor = cursor;
    idHashPtr = Tcl_FindHashEntry(&idTable, (char *) &idKey);
    if (idHashPtr == NULL) {
	panic("Tk_FreeCursor received unknown cursor argument");
    }
    cursorPtr = (TkCursor *) Tcl_GetHashValue(idHashPtr);
    cursorPtr->refCount--;
    if (cursorPtr->refCount == 0) {
	XFreeCursor(cursorPtr->display, cursorPtr->cursor);
	Tcl_DeleteHashEntry(cursorPtr->hashPtr);
	Tcl_DeleteHashEntry(idHashPtr);
	ckfree((char *) cursorPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * CursorInit --
 *
 *	Initialize the structures used for cursor management.
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
CursorInit()
{
    initialized = 1;
    Tcl_InitHashTable(&nameTable, sizeof(NameKey)/sizeof(int));
    Tcl_InitHashTable(&dataTable, sizeof(DataKey)/sizeof(int));
    Tcl_InitHashTable(&idTable, sizeof(IdKey)/sizeof(int));
}
