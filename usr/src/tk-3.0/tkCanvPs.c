/* 
 * tkCanvPs.c --
 *
 *	This module provides Postscript output support for canvases,
 *	including the "postscript" widget command plus a few utility
 *	procedures used for generating Postscript.
 *
 * Copyright 1991-1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef lint
static char rcsid[] = "$Header: /user6/ouster/wish/RCS/tkCanvPs.c,v 1.5 92/12/14 11:21:37 ouster Exp $ SPRITE (Berkeley)";
#endif

#include <stdio.h>
#include "tkInt.h"
#include "tkCanvas.h"
#include "tkConfig.h"

/*
 * See tkCanvas.h for key data structures used to implement canvases.
 */

/*
 * One of the following structures is created to keep track of Postscript
 * output being generated.  It consists mostly of information provided on
 * the widget command line.
 */

typedef struct PostscriptInfo {
    int x, y, width, height;	/* Area to print, in canvas pixel
				 * coordinates. */
    int x2, y2;			/* x+width and y+height. */
    char *pageXString;		/* String value of "-pagex" option or NULL. */
    char *pageYString;		/* String value of "-pagey" option or NULL. */
    double pageX, pageY;	/* Postscript coordinates (in points)
				 * corresponding to pageXString and
				 * pageYString. Don't forget that y-values
				 * grow upwards for Postscript! */
    char *pageWidthString;	/* Printed width of output. */
    char *pageHeightString;	/* Printed height of output. */
    double scale;		/* Scale factor for conversion: each pixel
				 * maps into this many points. */
    Tk_Anchor pageAnchor;	/* How to anchor bbox on Postscript page. */
    int rotate;			/* Non-zero means output should be rotated
				 * on page (landscape mode). */
    char *fontVar;		/* If non-NULL, gives name of global variable
				 * containing font mapping information.
				 * Malloc'ed. */
    char *colorVar;		/* If non-NULL, give name of global variable
				 * containing color mapping information.
				 * Malloc'ed. */
    char *colorMode;		/* Mode for handling colors:  "monochrome",
				 * "gray", or "color".  Malloc'ed. */
    int colorLevel;		/* Numeric value corresponding to colorMode:
				 * 0 for mono, 1 for gray, 2 for color. */
    char *fileName;		/* Name of file in which to write Postscript;
				 * NULL means return Postscript info as
				 * result. Malloc'ed. */
    FILE *f;			/* Open file corresponding to fileName. */
    Tcl_HashTable fontTable;	/* Hash table containing names of all font
				 * families used in output.  The hash table
				 * values are not used. */
    int prepass;		/* Non-zero means that we're currently in
				 * the pre-pass that collects font information,
				 * so the Postscript generated isn't
				 * relevant. */
} PostscriptInfo;

/*
 * The table below provides a template that's used to process arguments
 * to the canvas "postscript" command and fill in PostscriptInfo
 * structures.
 */

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_STRING, "-colormap", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, colorVar), 0},
    {TK_CONFIG_STRING, "-colormode", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, colorMode), 0},
    {TK_CONFIG_STRING, "-file", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, fileName), 0},
    {TK_CONFIG_STRING, "-fontmap", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, fontVar), 0},
    {TK_CONFIG_PIXELS, "-height", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, height), 0},
    {TK_CONFIG_ANCHOR, "-pageanchor", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, pageAnchor), 0},
    {TK_CONFIG_STRING, "-pageheight", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, pageHeightString), 0},
    {TK_CONFIG_STRING, "-pagewidth", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, pageWidthString), 0},
    {TK_CONFIG_STRING, "-pagex", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, pageXString), 0},
    {TK_CONFIG_STRING, "-pagey", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, pageYString), 0},
    {TK_CONFIG_BOOLEAN, "-rotate", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, rotate), 0},
    {TK_CONFIG_PIXELS, "-width", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, width), 0},
    {TK_CONFIG_PIXELS, "-x", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, x), 0},
    {TK_CONFIG_PIXELS, "-y", (char *) NULL, (char *) NULL,
	"", Tk_Offset(PostscriptInfo, y), 0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0}
};

/*
 * Forward declarations for procedures defined later in this file:
 */

static int		GetPostscriptPoints _ANSI_ARGS_((Tcl_Interp *interp,
			    char *string, double *doublePtr));

/*
 *--------------------------------------------------------------
 *
 * TkCanvPostscriptCmd --
 *
 *	This procedure is invoked to process the "postscript" options
 *	of the widget command for canvas widgets. See the user
 *	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

    /* ARGSUSED */
int
TkCanvPostscriptCmd(canvasPtr, interp, argc, argv)
    register Tk_Canvas *canvasPtr;	/* Information about canvas widget. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings.  Caller has
					 * already parsed this command enough
					 * to know that argv[1] is
					 * "postscript". */
{
    PostscriptInfo psInfo;
    int result = TCL_ERROR;
    register Tk_Item *itemPtr;
#define STRING_LENGTH 400
    char string[STRING_LENGTH+1], *p;
    time_t now;
    struct passwd *pwPtr;
    FILE *f;
    int length;
    int deltaX = 0, deltaY = 0;		/* Offset of lower-left corner of
					 * area to be marked up, measured
					 * in canvas units from the positioning
					 * point on the page (reflects
					 * anchor position).  Initial values
					 * needed only to stop compiler
					 * warnings. */
    Tcl_HashSearch search;
    Tcl_HashEntry *hPtr;
    char *libDir;

    /*
     *----------------------------------------------------------------
     * Initialize the data structure describing Postscript generation,
     * then process all the arguments to fill the data structure in.
     *----------------------------------------------------------------
     */

    psInfo.x = canvasPtr->xOrigin;
    psInfo.y = canvasPtr->yOrigin;
    psInfo.width = -1;
    psInfo.height = -1;
    psInfo.pageXString = NULL;
    psInfo.pageYString = NULL;
    psInfo.pageX = 72*4.25;
    psInfo.pageY = 72*5.5;
    psInfo.pageWidthString = NULL;
    psInfo.pageHeightString = NULL;
    psInfo.pageAnchor = TK_ANCHOR_CENTER;
    psInfo.rotate = 0;
    psInfo.fontVar = NULL;
    psInfo.colorVar = NULL;
    psInfo.colorMode = NULL;
    psInfo.fileName = NULL;
    psInfo.f = NULL;
    psInfo.prepass = 0;
    Tcl_InitHashTable(&psInfo.fontTable, TCL_STRING_KEYS);
    result = Tk_ConfigureWidget(canvasPtr->interp, canvasPtr->tkwin,
	    configSpecs, argc-2, argv+2, (char *) &psInfo,
	    TK_CONFIG_ARGV_ONLY);
    if (result != TCL_OK) {
	goto cleanup;
    }

    if (psInfo.width == -1) {
	psInfo.width = Tk_Width(canvasPtr->tkwin);
    }
    if (psInfo.height == -1) {
	psInfo.height = Tk_Height(canvasPtr->tkwin);
    }
    psInfo.x2 = psInfo.x + psInfo.width;
    psInfo.y2 = psInfo.y + psInfo.height;

    if (psInfo.pageXString != NULL) {
	if (GetPostscriptPoints(canvasPtr->interp, psInfo.pageXString,
		&psInfo.pageX) != TCL_OK) {
	    goto cleanup;
	}
    }
    if (psInfo.pageYString != NULL) {
	if (GetPostscriptPoints(canvasPtr->interp, psInfo.pageYString,
		&psInfo.pageY) != TCL_OK) {
	    goto cleanup;
	}
    }
    if (psInfo.pageWidthString != NULL) {
	if (GetPostscriptPoints(canvasPtr->interp, psInfo.pageWidthString,
		&psInfo.scale) != TCL_OK) {
	    goto cleanup;
	}
	psInfo.scale /= psInfo.width;
    } else if (psInfo.pageHeightString != NULL) {
	if (GetPostscriptPoints(canvasPtr->interp, psInfo.pageHeightString,
		&psInfo.scale) != TCL_OK) {
	    goto cleanup;
	}
	psInfo.scale /= psInfo.height;
    } else {
	psInfo.scale = (72.0/25.4)*WidthMMOfScreen(Tk_Screen(canvasPtr->tkwin));
	psInfo.scale /= WidthOfScreen(Tk_Screen(canvasPtr->tkwin));
    }
    switch (psInfo.pageAnchor) {
	case TK_ANCHOR_NW:
	case TK_ANCHOR_W:
	case TK_ANCHOR_SW:
	    deltaX = 0;
	    break;
	case TK_ANCHOR_N:
	case TK_ANCHOR_CENTER:
	case TK_ANCHOR_S:
	    deltaX = -psInfo.width/2;
	    break;
	case TK_ANCHOR_NE:
	case TK_ANCHOR_E:
	case TK_ANCHOR_SE:
	    deltaX = -psInfo.width;
	    break;
    }
    switch (psInfo.pageAnchor) {
	case TK_ANCHOR_NW:
	case TK_ANCHOR_N:
	case TK_ANCHOR_NE:
	    deltaY = - psInfo.height;
	    break;
	case TK_ANCHOR_W:
	case TK_ANCHOR_CENTER:
	case TK_ANCHOR_E:
	    deltaY = -psInfo.height/2;
	    break;
	case TK_ANCHOR_SW:
	case TK_ANCHOR_S:
	case TK_ANCHOR_SE:
	    deltaY = 0;
	    break;
    }

    if (psInfo.colorMode == NULL) {
	psInfo.colorLevel = 2;
    } else {
	length = strlen(psInfo.colorMode);
	if (strncmp(psInfo.colorMode, "monochrome", length) == 0) {
	    psInfo.colorLevel = 0;
	} else if (strncmp(psInfo.colorMode, "gray", length) == 0) {
	    psInfo.colorLevel = 1;
	} else if (strncmp(psInfo.colorMode, "color", length) == 0) {
	    psInfo.colorLevel = 2;
	} else {
	    Tcl_AppendResult(canvasPtr->interp, "bad color mode \"",
		    psInfo.colorMode, "\": must be monochrome, ",
		    "gray, or color", (char *) NULL);
	    goto cleanup;
	}
    }

    if (psInfo.fileName != NULL) {
	p = Tcl_TildeSubst(canvasPtr->interp, psInfo.fileName);
	if (p == NULL) {
	    goto cleanup;
	}
	psInfo.f = fopen(p, "w");
	if (psInfo.f == NULL) {
	    Tcl_AppendResult(canvasPtr->interp, "couldn't write file \"",
		    psInfo.fileName, "\": ",
		    Tcl_UnixError(canvasPtr->interp), (char *) NULL);
	    goto cleanup;
	}
    }

    /*
     *--------------------------------------------------------
     * Make a pre-pass over all of the items, generating Postscript
     * and then throwing it away.  The purpose of this pass is just
     * to collect information about all the fonts in use, so that
     * we can output font information in the proper form required
     * by the Document Structuring Conventions.
     *--------------------------------------------------------
     */

    psInfo.prepass = 1;
    for (itemPtr = canvasPtr->firstItemPtr; itemPtr != NULL;
	    itemPtr = itemPtr->nextPtr) {
	if ((itemPtr->x1 >= psInfo.x2) || (itemPtr->x2 < psInfo.x)
		|| (itemPtr->y1 >= psInfo.y2) || (itemPtr->y2 < psInfo.y)) {
	    continue;
	}
	if (itemPtr->typePtr->postscriptProc == NULL) {
	    continue;
	}
	result = (*itemPtr->typePtr->postscriptProc)(canvasPtr, itemPtr,
		(Tk_PostscriptInfo *) &psInfo);
	Tcl_ResetResult(canvasPtr->interp);
	if (result != TCL_OK) {
	    /*
	     * An error just occurred.  Just skip out of this loop.
	     * There's no need to report the error now;  it can be
	     * reported later (errors can happen later that don't
	     * happen now, so we still have to check for errors later
	     * anyway).
	     */
	    break;
	}
    }
    psInfo.prepass = 0;

    /*
     *--------------------------------------------------------
     * Generate the header and prolog for the Postscript.
     *--------------------------------------------------------
     */

    Tcl_AppendResult(canvasPtr->interp, "%!PS-Adobe-3.0 EPSF-3.0\n",
	    "%%Creator: Tk Canvas Widget\n", (char *) NULL);
    pwPtr = getpwuid(getuid());
    Tcl_AppendResult(canvasPtr->interp, "%%For: ",
	    (pwPtr != NULL) ? pwPtr->pw_gecos : "Unknown", "\n",
	    (char *) NULL);
    endpwent();
    Tcl_AppendResult(canvasPtr->interp, "%%Title: Window ",
	    Tk_PathName(canvasPtr->tkwin), "\n", (char *) NULL);
    time(&now);
    Tcl_AppendResult(canvasPtr->interp, "%%CreationDate: ",
	    ctime(&now), (char *) NULL);
    if (!psInfo.rotate) {
	sprintf(string, "%d %d %d %d",
		(int) (psInfo.pageX + psInfo.scale*deltaX),
		(int) (psInfo.pageY + psInfo.scale*deltaY),
		(int) (psInfo.pageX + psInfo.scale*(deltaX + psInfo.width)
			+ 1.0),
		(int) (psInfo.pageY + psInfo.scale*(deltaY + psInfo.height)
			+ 1.0));
    } else {
	sprintf(string, "%d %d %d %d",
		(int) (psInfo.pageX - psInfo.scale*(deltaY + psInfo.height)),
		(int) (psInfo.pageY + psInfo.scale*deltaX),
		(int) (psInfo.pageX - psInfo.scale*deltaY + 1.0),
		(int) (psInfo.pageY + psInfo.scale*(deltaX + psInfo.width)
			+ 1.0));
    }
    Tcl_AppendResult(canvasPtr->interp, "%%BoundingBox: ", string,
	    "\n", (char *) NULL);
    Tcl_AppendResult(canvasPtr->interp, "%%Pages: 1\n", 
	    "%%DocumentData: Clean7Bit\n", (char *) NULL);
    Tcl_AppendResult(canvasPtr->interp, "%%Orientation: ",
	    psInfo.rotate ? "Landscape\n" : "Portrait\n", (char *) NULL);
    p = "%%DocumentNeededResources: font ";
    for (hPtr = Tcl_FirstHashEntry(&psInfo.fontTable, &search);
	    hPtr != NULL; hPtr = Tcl_NextHashEntry(&search)) {
	Tcl_AppendResult(canvasPtr->interp, p,
		Tcl_GetHashKey(&psInfo.fontTable, hPtr),
		"\n", (char *) NULL);
	p = "%%+ font ";
    }
    Tcl_AppendResult(canvasPtr->interp, "%%EndComments\n\n", (char *) NULL);

    /*
     * Read a standard prolog file from disk and insert it into
     * the Postscript.
     */

    libDir = Tcl_GetVar(canvasPtr->interp, "tk_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	Tcl_ResetResult(canvasPtr->interp);
	Tcl_AppendResult(canvasPtr->interp, "couldn't find library directory: ",
		"tk_library variable doesn't exist", (char *) NULL);
	goto cleanup;
    }
    sprintf(string, "%.350s/prolog.ps", TK_LIBRARY);
    f = fopen(string, "r");
    if (f == NULL) {
	Tcl_ResetResult(canvasPtr->interp);
	Tcl_AppendResult(canvasPtr->interp, "couldn't open prolog file \"",
		string, "\": ", Tcl_UnixError(canvasPtr->interp),
		(char *) NULL);
	goto cleanup;
    }
    while (fgets(string, STRING_LENGTH, f) != NULL) {
	Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
    }
    if (ferror(f)) {
	fclose(f);
	Tcl_ResetResult(canvasPtr->interp);
	Tcl_AppendResult(canvasPtr->interp, "error reading prolog file \"",
		TK_LIBRARY, "/prolog.ps: ",
		Tcl_UnixError(canvasPtr->interp), (char *) NULL);
	goto cleanup;
    }
    fclose(f);
    if (psInfo.f != NULL) {
	fputs(canvasPtr->interp->result, psInfo.f);
	Tcl_ResetResult(canvasPtr->interp);
    }

    /*
     *-----------------------------------------------------------
     * Document setup:  set the color level and include fonts.
     *-----------------------------------------------------------
     */

    sprintf(string, "/CL %d def\n", psInfo.colorLevel);
    Tcl_AppendResult(canvasPtr->interp, "%%BeginSetup\n", string,
	    (char *) NULL);
    for (hPtr = Tcl_FirstHashEntry(&psInfo.fontTable, &search);
	    hPtr != NULL; hPtr = Tcl_NextHashEntry(&search)) {
	Tcl_AppendResult(canvasPtr->interp, "%%IncludeResource: font ",
		Tcl_GetHashKey(&psInfo.fontTable, hPtr), "\n", (char *) NULL);
    }
    Tcl_AppendResult(canvasPtr->interp, "%%EndSetup\n\n", (char *) NULL);

    /*
     *-----------------------------------------------------------
     * Page setup:  move to page positioning point, rotate if
     * needed, set scale factor, offset for proper anchor position,
     * and set clip region.
     *-----------------------------------------------------------
     */

    Tcl_AppendResult(canvasPtr->interp, "%%Page: 1 1\n", "save\n",
	    (char *) NULL);
    sprintf(string, "%.1f %.1f translate\n", psInfo.pageX, psInfo.pageY);
    Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
    if (psInfo.rotate) {
	Tcl_AppendResult(canvasPtr->interp, "90 rotate\n", (char *) NULL);
    }
    sprintf(string, "%.4g %.4g scale\n", psInfo.scale, psInfo.scale);
    Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
    sprintf(string, "%d %d translate\n", deltaX - psInfo.x, deltaY);
    Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
    sprintf(string, "%d %g moveto %d %g lineto %d %g lineto %d %g",
	    psInfo.x,
	    TkCanvPsY((Tk_PostscriptInfo *) &psInfo, (double) psInfo.y),
	    psInfo.x2,
	    TkCanvPsY((Tk_PostscriptInfo *) &psInfo, (double) psInfo.y),
	    psInfo.x2,
	    TkCanvPsY((Tk_PostscriptInfo *) &psInfo, (double) psInfo.y2),
	    psInfo.x,
	    TkCanvPsY((Tk_PostscriptInfo *) &psInfo, (double) psInfo.y2));
    Tcl_AppendResult(canvasPtr->interp, string,
	" lineto closepath clip newpath\n", (char *) NULL);
    if (psInfo.f != NULL) {
	fputs(canvasPtr->interp->result, psInfo.f);
	Tcl_ResetResult(canvasPtr->interp);
    }

    /*
     *---------------------------------------------------------------------
     * Iterate through all the items, having each relevant one draw itself.
     * Quit if any of the items returns an error.
     *---------------------------------------------------------------------
     */

    result = TCL_OK;
    for (itemPtr = canvasPtr->firstItemPtr; itemPtr != NULL;
	    itemPtr = itemPtr->nextPtr) {
	if ((itemPtr->x1 >= psInfo.x2) || (itemPtr->x2 < psInfo.x)
		|| (itemPtr->y1 >= psInfo.y2) || (itemPtr->y2 < psInfo.y)) {
	    continue;
	}
	if (itemPtr->typePtr->postscriptProc == NULL) {
	    continue;
	}
	Tcl_AppendResult(canvasPtr->interp, "gsave\n", (char *) NULL);
	result = (*itemPtr->typePtr->postscriptProc)(canvasPtr, itemPtr,
		(Tk_PostscriptInfo *) &psInfo);
	if (result != TCL_OK) {
	    char msg[100];

	    sprintf(msg, "\n    (generating Postscript for item %d)",
		    itemPtr->id);
	    Tcl_AddErrorInfo(canvasPtr->interp, msg);
	    goto cleanup;
	}
	Tcl_AppendResult(canvasPtr->interp, "grestore\n", (char *) NULL);
	if (psInfo.f != NULL) {
	    fputs(canvasPtr->interp->result, psInfo.f);
	    Tcl_ResetResult(canvasPtr->interp);
	}
    }

    /*
     *---------------------------------------------------------------------
     * Output page-end information, such as commands to print the page
     * and document trailer stuff.
     *---------------------------------------------------------------------
     */

    Tcl_AppendResult(canvasPtr->interp, "restore showpage\n\n",
	    "%%Trailer\nend\n%%EOF\n", (char *) NULL);
    if (psInfo.f != NULL) {
	fputs(canvasPtr->interp->result, psInfo.f);
	Tcl_ResetResult(canvasPtr->interp);
    }

    /*
     * Clean up psInfo to release malloc'ed stuff.
     */

    cleanup:
    if (psInfo.pageXString != NULL) {
	ckfree(psInfo.pageXString);
    }
    if (psInfo.pageYString != NULL) {
	ckfree(psInfo.pageYString);
    }
    if (psInfo.pageWidthString != NULL) {
	ckfree(psInfo.pageWidthString);
    }
    if (psInfo.pageHeightString != NULL) {
	ckfree(psInfo.pageHeightString);
    }
    if (psInfo.fontVar != NULL) {
	ckfree(psInfo.fontVar);
    }
    if (psInfo.colorVar != NULL) {
	ckfree(psInfo.colorVar);
    }
    if (psInfo.colorMode != NULL) {
	ckfree(psInfo.colorMode);
    }
    if (psInfo.fileName != NULL) {
	ckfree(psInfo.fileName);
    }
    if (psInfo.f != NULL) {
	fclose(psInfo.f);
    }
    Tcl_DeleteHashTable(&psInfo.fontTable);
    return result;
}

/*
 *--------------------------------------------------------------
 *
 * TkCanvPsColor --
 *
 *	This procedure is called by individual canvas items when
 *	they want to set a color value for output.  Given information
 *	about an X color, this procedure will generate Postscript
 *	commands to set up an appropriate color in Postscript.
 *
 * Results:
 *	Returns a standard Tcl return value.  If an error occurs
 *	then an error message will be left in canvasPtr->interp->result.
 *	If no error occurs, then additional Postscript will be
 *	appended to canvasPtr->interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

int
TkCanvPsColor(canvasPtr, handle, colorPtr)
    Tk_Canvas *canvasPtr;		/* Information about canvas. */
    Tk_PostscriptInfo *handle;		/* Information about Postscript being
					 * generated. */
    XColor *colorPtr;			/* Information about color. */
{
    PostscriptInfo *psInfoPtr = (PostscriptInfo *) handle;
    double red, green, blue;
    char string[200];

    if (psInfoPtr->prepass) {
	return TCL_OK;
    }

    /*
     * If there is a color map defined, then look up the color's name
     * in the map and use the Postscript commands found there, if there
     * are any.
     */

    if (psInfoPtr->colorVar != NULL) {
	char *cmdString;

	cmdString = Tcl_GetVar2(canvasPtr->interp, psInfoPtr->colorVar,
		Tk_NameOfColor(colorPtr), 0);
	if (cmdString != NULL) {
	    Tcl_AppendResult(canvasPtr->interp, cmdString, (char *) NULL);
	    return TCL_OK;
	}
    }

    /*
     * No color map entry for this color.  Grab the color's intensities
     * and output Postscript commands for them.
     */

    red = colorPtr->red/65535.0;
    green = colorPtr->green/65535.0;
    blue = colorPtr->blue/65535.0;
    sprintf(string, "%.3f %.3f %.3f setrgbcolor AdjustColor\n",
	    red, green, blue);
    Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * TkCanvPsFont --
 *
 *	This procedure is called by individual canvas items when
 *	they want to output text.  Given information about an X
 *	font, this procedure will generate Postscript commands
 *	to set up an appropriate font in Postscript.
 *
 * Results:
 *	Returns a standard Tcl return value.  If an error occurs
 *	then an error message will be left in canvasPtr->interp->result.
 *	If no error occurs, then additional Postscript will be
 *	appended to the canvasPtr->interp->result.
 *
 * Side effects:
 *	The Postscript font name is entered into psInfoPtr->fontTable
 *	if it wasn't already there.
 *
 *--------------------------------------------------------------
 */

int
TkCanvPsFont(canvasPtr, handle, fontStructPtr)
    register Tk_Canvas *canvasPtr;	/* Information about canvas. */
    Tk_PostscriptInfo *handle;		/* Information about Postscript being
					 * generated. */
    XFontStruct *fontStructPtr;		/* Information about font in which text
					 * is to be printed. */
{
    PostscriptInfo *psInfoPtr = (PostscriptInfo *) handle;
    char *name, *end, *weightString, *slantString;
#define TOTAL_FIELDS	8
#define FAMILY_FIELD	1
#define WEIGHT_FIELD	2
#define SLANT_FIELD	3
#define SIZE_FIELD	7
    char *fieldPtrs[TOTAL_FIELDS];
#define MAX_NAME_SIZE 100
    char fontName[MAX_NAME_SIZE+50], pointString[20];
    int i, c, weightSize, nameSize, points;
    register char *p;

    name = Tk_NameOfFontStruct(fontStructPtr);

    /*
     * First, look up the font's name in the font map, if there is one.
     * If there is an entry for this font, it consists of a list
     * containing font name and size.  Use this information.
     */

    if (psInfoPtr->fontVar != NULL) {
	char *list, **argv;
	int argc;
	double size;

	list = Tcl_GetVar2(canvasPtr->interp, psInfoPtr->fontVar,
		name, 0);
	if (list != NULL) {
	    if (Tcl_SplitList(canvasPtr->interp, list, &argc, &argv)
		    != TCL_OK) {
		badMapEntry:
		Tcl_ResetResult(canvasPtr->interp);
		Tcl_AppendResult(canvasPtr->interp,
			"bad font map entry for \"", name,
			"\": \"", list, "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    if (argc != 2) {
		goto badMapEntry;
	    }
	    size = strtod(argv[1], &end);
	    if ((size <= 0) || (*end != 0)) {
		goto badMapEntry;
	    }
	    sprintf(pointString, "%g", size);
	    Tcl_AppendResult(canvasPtr->interp, "/", argv[0], " findfont ",
		    pointString, " scalefont setfont\n", (char *) NULL);
	    Tcl_CreateHashEntry(&psInfoPtr->fontTable, argv[0], &i);
	    ckfree((char *) argv);
	    return TCL_OK;
	}
    }

    /*
     * Not in the font map.  Try to parse the name to get four fields:
     * family name, weight, slant, and point size.  To do this, split the
     * font name up into fields, storing pointers to the first character
     * of each field in fieldPtrs.
     */

    if (name[0] != '-') {
	goto error;
    }
    for (p =  name+1, i = 0; i < TOTAL_FIELDS; i++) {
	fieldPtrs[i] = p;
	while (*p != '-') {
	    if (*p == 0) {
		goto error;
	    }
	    p++;
	}
	p++;
    }

    /*
     * Use the information from the X font name to make a guess at a
     * Postscript font name of the form "<family>-<weight><slant>" where
     * <weight> and <slant> may be omitted and if both are omitted then
     * the dash is also omitted.  Postscript is very picky about font names,
     * so there are several heuristics in the code below (e.g. don't
     * include a "Roman" slant except for "Times" font, and make sure
     * that the first letter of each field is capitalized but no other
     * letters are in caps).
     */

    nameSize = fieldPtrs[FAMILY_FIELD+1] - 1 - fieldPtrs[FAMILY_FIELD];
    if ((nameSize == 0) || (nameSize > MAX_NAME_SIZE)) {
	goto error;
    }
    strncpy(fontName, fieldPtrs[FAMILY_FIELD], nameSize);
    fontName[0] = toupper(fontName[0]);
    for (p = fontName+1, i = nameSize-1; i > 0; p++, i--) {
	*p = tolower(*p);
    }
    *p = 0;
    weightSize = fieldPtrs[WEIGHT_FIELD+1] - 1 - fieldPtrs[WEIGHT_FIELD];
    if (weightSize == 0) {
	goto error;
    }
    if (strncasecmp(fieldPtrs[WEIGHT_FIELD], "medium", weightSize) == 0) {
	weightString = "";
    } else if (strncasecmp(fieldPtrs[WEIGHT_FIELD], "bold", weightSize) == 0) {
	weightString = "Bold";
    } else {
	goto error;
    }
    if (fieldPtrs[SLANT_FIELD+1] != (fieldPtrs[SLANT_FIELD] + 2)) {
	goto error;
    }
    c = fieldPtrs[SLANT_FIELD][0];
    if ((c == 'r') || (c == 'R')) {
	slantString = "";
	if ((weightString[0] == 0) && (nameSize == 5)
		&& (strncmp(fontName, "Times", 5) == 0)) {
	    slantString = "Roman";
	}
    } else if ((c == 'i') || (c == 'I')) {
	slantString = "Italic";
    } else if ((c == 'o') || (c == 'O')) {
	slantString = "Oblique";
    } else {
	goto error;
    }
    if ((weightString[0] != 0) || (slantString[0] != 0)) {
	sprintf(p, "-%s%s", weightString, slantString);
    }
    points = strtoul(fieldPtrs[SIZE_FIELD], &end, 0);
    if (points == 0) {
	goto error;
    }
    sprintf(pointString, "%g", ((double) points)/10.0);
    Tcl_AppendResult(canvasPtr->interp, "/", fontName, " findfont ",
	    pointString, " scalefont setfont\n", (char *) NULL);
    Tcl_CreateHashEntry(&psInfoPtr->fontTable, fontName, &i);
    return TCL_OK;

    error:
    Tcl_ResetResult(canvasPtr->interp);
    Tcl_AppendResult(canvasPtr->interp, "couldn't translate font name \"",
	    name, "\" to Postscript", (char *) NULL);
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * TkCanvPsBitmap --
 *
 *	This procedure is called to output the contents of a
 *	bitmap in proper image data format for Postscript (i.e.
 *	data between angle brackets, one bit per pixel).
 *
 * Results:
 *	Returns a standard Tcl return value.  If an error occurs
 *	then an error message will be left in canvasPtr->interp->result.
 *	If no error occurs, then additional Postscript will be
 *	appended to canvasPtr->interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

int
TkCanvPsBitmap(canvasPtr, handle, bitmap)
    Tk_Canvas *canvasPtr;		/* Information about canvas. */
    Tk_PostscriptInfo *handle;		/* Information about Postscript being
					 * generated. */
    Pixmap bitmap;			/* Bitmap to use for stippling. */
{
    PostscriptInfo *psInfoPtr = (PostscriptInfo *) handle;
    unsigned int width, height;
    XImage *imagePtr;
    int charsInLine, x, y, value, mask;
    char string[100];

    if (psInfoPtr->prepass) {
	return TCL_OK;
    }

    Tk_SizeOfBitmap(canvasPtr->display, bitmap, &width, &height);
    imagePtr = XGetImage(Tk_Display(canvasPtr->tkwin), bitmap, 0, 0,
	    width, height, 1, XYPixmap);
    Tcl_AppendResult(canvasPtr->interp, "<", (char *) NULL);
    mask = 0x80;
    value = 0;
    charsInLine = 0;
    for (y = 1; y <= height; y++) {
	for (x = 0; x < width; x++) {
	    if (XGetPixel(imagePtr, x, height-y)) {
		value |= mask;
	    }
	    mask >>= 1;
	    if (mask == 0) {
		sprintf(string, "%02x", value);
		Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
		mask = 0x80;
		value = 0;
		charsInLine += 2;
		if (charsInLine >= 60) {
		    Tcl_AppendResult(canvasPtr->interp, "\n", (char *) NULL);
		    charsInLine = 0;
		}
	    }
	}
	if (mask != 0x80) {
	    sprintf(string, "%02x", value);
	    Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
	    mask = 0x80;
	    value = 0;
	    charsInLine += 2;
	}
    }
    Tcl_AppendResult(canvasPtr->interp, ">", (char *) NULL);
    XDestroyImage(imagePtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * TkCanvPsStipple --
 *
 *	This procedure is called by individual canvas items when
 *	they have created a path that they'd like to be filled with
 *	a stipple pattern.  Given information about an X bitmap,
 *	this procedure will generate Postscript commands to fill
 *	the current path using a stipple pattern defined by the
 *	bitmap.
 *
 * Results:
 *	Returns a standard Tcl return value.  If an error occurs
 *	then an error message will be left in canvasPtr->interp->result.
 *	If no error occurs, then additional Postscript will be
 *	appended to canvasPtr->interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

int
TkCanvPsStipple(canvasPtr, handle, bitmap, filled)
    Tk_Canvas *canvasPtr;		/* Information about canvas. */
    Tk_PostscriptInfo *handle;		/* Information about Postscript being
					 * generated. */
    Pixmap bitmap;			/* Bitmap to use for stippling. */
    int filled;				/* Non-zero means the area defined by
					 * the path should be filled with the
					 * stipple pattern;  zero means the
					 * path should be stroked in the
					 * stipple pattern. */
{
    PostscriptInfo *psInfoPtr = (PostscriptInfo *) handle;
    unsigned int width, height;
    char string[100];

    if (psInfoPtr->prepass) {
	return TCL_OK;
    }

    Tk_SizeOfBitmap(canvasPtr->display, bitmap, &width, &height);
    sprintf(string, "%d %d ", width, height);
    Tcl_AppendResult(canvasPtr->interp, string, (char *) NULL);
    if (TkCanvPsBitmap(canvasPtr, handle, bitmap) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_AppendResult(canvasPtr->interp, filled ? " true" : " false",
	    " StippleFill\n", (char *) NULL);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * TkCanvPsY --
 *
 *	Given a y-coordinate in canvas coordinates, this procedure
 *	returns a y-coordinate to use for Postscript output.
 *
 * Results:
 *	Returns the Postscript coordinate that corresponds to
 *	"y".
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

double
TkCanvPsY(handle, y)
    Tk_PostscriptInfo *handle;		/* Information about Postscript being
					 * generated. */
    double y;				/* Y-coordinate in canvas coords. */
{
    PostscriptInfo *psInfoPtr = (PostscriptInfo *) handle;
    return psInfoPtr->y2 - y;
}

/*
 *--------------------------------------------------------------
 *
 * TkCanvPsPath --
 *
 *	Given an array of points for a path, generate Postscript
 *	commands to create the path.
 *
 * Results:
 *	Postscript commands get appended to what's in interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
TkCanvPsPath(interp, coordPtr, numPoints, handle)
    Tcl_Interp *interp;			/* Put generated Postscript in this
					 * interpreter's result field. */
    register double *coordPtr;		/* Pointer to first in array of
					 * 2*numPoints coordinates giving
					 * points for path. */
    int numPoints;			/* Number of points at *coordPtr. */
    Tk_PostscriptInfo *handle;		/* Information about the Postscript;
					 * must be passed back to Postscript
					 * utility procedures. */
{
    PostscriptInfo *psInfoPtr = (PostscriptInfo *) handle;
    char buffer[200];

    if (psInfoPtr->prepass) {
	return;
    }
    sprintf(buffer, "%g %g moveto\n", coordPtr[0],
	    TkCanvPsY(handle, coordPtr[1]));
    Tcl_AppendResult(interp, buffer, (char *) NULL);
    for (numPoints--, coordPtr += 2; numPoints > 0;
	    numPoints--, coordPtr += 2) {
	sprintf(buffer, "%g %g lineto\n", coordPtr[0],
		TkCanvPsY(handle, coordPtr[1]));
	Tcl_AppendResult(interp, buffer, (char *) NULL);
    }
}

/*
 *--------------------------------------------------------------
 *
 * GetPostscriptPoints --
 *
 *	Given a string, returns the number of Postscript points
 *	corresponding to that string.
 *
 * Results:
 *	The return value is a standard Tcl return result.  If
 *	TCL_OK is returned, then everything went well and the
 *	screen distance is stored at *doublePtr;  otherwise
 *	TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int
GetPostscriptPoints(interp, string, doublePtr)
    Tcl_Interp *interp;		/* Use this for error reporting. */
    char *string;		/* String describing a screen distance. */
    double *doublePtr;		/* Place to store converted result. */
{
    char *end;
    double d;

    d = strtod(string, &end);
    if (end == string) {
	error:
	Tcl_AppendResult(interp, "bad distance \"", string,
		"\"", (char *) NULL);
	return TCL_ERROR;
    }
    while ((*end != '\0') && isspace(*end)) {
	end++;
    }
    switch (*end) {
	case 'c':
	    d *= 72.0/2.54;
	    end++;
	    break;
	case 'i':
	    d *= 72.0;
	    end++;
	    break;
	case 'm':
	    d *= 72.0/25.4;
	    end++;
	    break;
	case 0:
	case 'p':
	    end++;
	    break;
	default:
	    goto error;
    }
    while ((*end != '\0') && isspace(*end)) {
	end++;
    }
    if (*end != 0) {
	goto error;
    }
    *doublePtr = d;
    return TCL_OK;
}
