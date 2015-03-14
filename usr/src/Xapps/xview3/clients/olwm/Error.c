/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)Error.c	26.9	91/09/14 SMI"

#include <stdio.h>
#include <X11/Xproto.h>
#include <X11/Xlib.h>

#include "i18n.h"

/* globals */

/* locals */
static char	*hyperSensitive = NULL;

#define NXOPCODES	128	/* # of X opcodes */
typedef struct {
	unsigned int fatal, warning, ignore;
} ErrorEntry;

/* The following defines create bitmasks from the X Error Codes */
#define BReq	(1<<BadRequest)
#define BVal	(1<<BadValue)
#define BWin	(1<<BadWindow)
#define BPix	(1<<BadPixmap)
#define BAtm	(1<<BadAtom)
#define BCur	(1<<BadCursor)
#define BFnt	(1<<BadFont)
#define BMch	(1<<BadMatch)
#define BDrw	(1<<BadDrawable)
#define BAcc	(1<<BadAccess)
#define BAlc	(1<<BadAlloc)
#define BCol	(1<<BadColor)
#define BGC	(1<<BadGC)
#define BIDC	(1<<BadIDChoice)
#define BNam	(1<<BadName)
#define BLen	(1<<BadLength)
#define BImp	(1<<BadImplementation)

#define BAll	(~0)	/* matches all error bitmasks */

/* This error table encodes the severity of an error in relation to
 * an X primitive. For instance if BadWindow was fatal for
 * a certain primitive but BadColor was not, the following entry would be used:
 *	{BWin,	BCol,	0}
 */
static ErrorEntry errorTable[NXOPCODES] = {
	{BAll,	0,	0},	/* 0, Not Used */
	{BAll,	0,	0},	/* 1, X_CreateWindow */
	{0,	~BWin,	BWin},	/* 2, X_ChangeWindowAttributes */
	{0,	~BWin,	BWin},	/* 3, X_GetWindowAttributes */
	{0,	BAll,	0},	/* 4, X_DestroyWindow */
	{0,	BAll,	0},	/* 5, X_DestroySubwindows */
	{0,	~BWin,	BWin},	/* 6, X_ChangeSaveSet */
	{0,	~BWin,	BWin},	/* 7, X_ReparentWindow */
	{0,	~BWin,	BWin},	/* 8, X_MapWindow */
	{0,	BAll,	0},	/* 9, X_MapSubwindows */
	{0,	~BWin,	BWin},	/* 10, X_UnmapWindow */
	{0,	BAll,	0},	/* 11, X_UnmapSubwindows */
	{0,	~BWin,	BWin},	/* 12, X_ConfigureWindow */
	{0,	BAll,	0},	/* 13, X_CirculateWindow */
	{0,	~BDrw,	BDrw},	/* 14, X_GetGeometry */
	{0,	~BWin,	BWin},	/* 15, X_QueryTree */
	{0,	BAll,	0},	/* 16, X_InternAtom */
	{0,	BAll,	0},	/* 17, X_GetAtomName */
	{0,	~BWin,	BWin},	/* 18, X_ChangeProperty */
	{0,	BAll,	0},	/* 19, X_DeleteProperty */
	{0,	~BWin,	BWin},	/* 20, X_GetProperty */
	{0,	BAll,	0},	/* 21, X_ListProperties */
	{0,	BAll,	0},	/* 22, X_SetSelectionOwner */
	{0,	BAll,	0},	/* 23, X_GetSelectionOwner */
	{0,	BAll,	0},	/* 24, X_ConvertSelection */
	{0,	~BWin,	BWin},	/* 25, X_SendEvent */
	{0,	0,	BAll},	/* 26, X_GrabPointer */
	{0,	0,	BAll},	/* 27, X_UngrabPointer */
	{0,	0,	BAll},	/* 28, X_GrabButton */
	{0,	0,	BAll},	/* 29, X_UngrabButton */
	{0,	0,	BAll},	/* 30, X_ChangeActivePointerGrab */
	{0,	BAll,	0},	/* 31, X_GrabKeyboard */
	{0,	BAll,	0},	/* 32, X_UngrabKeyboard */
	{0,	BAll,	0},	/* 33, X_GrabKey */
	{0,	BAll,	0},	/* 34, X_UngrabKey */
	{0,	BAll,	0},	/* 35, X_AllowEvents */
	{BAll,	0,	0},	/* 36, X_GrabServer */
	{0,	BAll,	0},	/* 37, X_UngrabServer */
	{0,	BAll,	0},	/* 38, X_QueryPointer */
	{0,	BAll,	0},	/* 39, X_GetMotionEvents */
	{0,	BAll,	0},	/* 40, X_TranslateCoords */
	{0,	BAll,	0},	/* 41, X_WarpPointer */
	{0,	~(BMch|BWin),	BMch|BWin},	/* 42, X_SetInputFocus */
	{0,	BAll,	0},	/* 43, X_GetInputFocus */
	{0,	BAll,	0},	/* 44, X_QueryKeymap */
	{0,	BAll,	0},	/* 45, X_OpenFont */
	{0,	BAll,	0},	/* 46, X_CloseFont */
	{0,	BAll,	0},	/* 47, X_QueryFont */
	{0,	BAll,	0},	/* 48, X_QueryTextExtents */
	{0,	BAll,	0},	/* 49, X_ListFonts */
	{0,	BAll,	0},	/* 50, X_ListFontsWithInfo */
	{0,	BAll,	0},	/* 51, X_SetFontPath */
	{0,	BAll,	0},	/* 52, X_GetFontPath */
	{0,	BAll,	0},	/* 53, X_CreatePixmap */
	{0,	BAll,	0},	/* 54, X_FreePixmap */
	{0,	BAll,	0},	/* 55, X_CreateGC */
	{0,	BAll,	0},	/* 56, X_ChangeGC */
	{0,	BAll,	0},	/* 57, X_CopyGC */
	{0,	BAll,	0},	/* 58, X_SetDashes */
	{0,	BAll,	0},	/* 59, X_SetClipRectangles */
	{0,	BAll,	0},	/* 60, X_FreeGC */
	{0,	~BMch,	BMch},	/* 61, X_ClearArea */
	{0,	BAll,	0},	/* 62, X_CopyArea */
	{0,	BAll,	0},	/* 63, X_CopyPlane */
	{0,	BAll,	0},	/* 64, X_PolyPoint */
	{0,	BAll,	0},	/* 65, X_PolyLine */
	{0,	BAll,	0},	/* 66, X_PolySegment */
	{0,	BAll,	0},	/* 67, X_PolyRectangle */
	{0,	BAll,	0},	/* 68, X_PolyArc */
	{0,	BAll,	0},	/* 69, X_FillPoly */
	{0,	BAll,	0},	/* 70, X_PolyFillRectangle */
	{0,	BAll,	0},	/* 71, X_PolyFillArc */
	{0,	BAll,	0},	/* 72, X_PutImage */
	{0,	BAll,	0},	/* 73, X_GetImage */
	{0,	BAll,	0},	/* 74, X_PolyText8 */
	{0,	BAll,	0},	/* 75, X_PolyText16 */
	{0,	BAll,	0},	/* 76, X_ImageText8 */
	{0,	BAll,	0},	/* 77, X_ImageText16 */
	{0,	BAll,	0},	/* 78, X_CreateColormap */
	{0,	BAll,	0},	/* 79, X_FreeColormap */
	{0,	BAll,	0},	/* 80, X_CopyColormapAndFree */
	{0,	BAll,	0},	/* 81, X_InstallColormap */
	{0,	BAll,	0},	/* 82, X_UninstallColormap */
	{0,	BAll,	0},	/* 83, X_ListInstalledColormaps */
	{0,	BAll,	0},	/* 84, X_AllocColor */
	{0,	BAll,	0},	/* 85, X_AllocNamedColor */
	{0,	BAll,	0},	/* 86, X_AllocColorCells */
	{0,	BAll,	0},	/* 87, X_AllocColorPlanes */
	{0,	BAll,	0},	/* 88, X_FreeColors */
	{0,	BAll,	0},	/* 89, X_StoreColors */
	{0,	BAll,	0},	/* 90, X_StoreNamedColor */
	{0,	BAll,	0},	/* 91, X_QueryColors */
	{0,	BAll,	0},	/* 92, X_LookupColor */
	{0,	BAll,	0},	/* 93, X_CreateCursor */
	{0,	BAll,	0},	/* 94, X_CreateGlyphCursor */
	{0,	BAll,	0},	/* 95, X_FreeCursor */
	{0,	BAll,	0},	/* 96, X_RecolorCursor */
	{0,	BAll,	0},	/* 97, X_QueryBestSize */
	{0,	BAll,	0},	/* 98, X_QueryExtension */
	{0,	BAll,	0},	/* 99, X_ListExtensions */
	{0,	BAll,	0},	/* 100, X_ChangeKeyboardMapping */
	{0,	BAll,	0},	/* 101, X_GetKeyboardMapping */
	{0,	BAll,	0},	/* 102, X_ChangeKeyboardControl */
	{0,	BAll,	0},	/* 103, X_GetKeyboardControl */
	{0,	BAll,	0},	/* 104, X_Bell */
	{0,	BAll,	0},	/* 105, X_ChangePointerControl */
	{0,	BAll,	0},	/* 106, X_GetPointerControl */
	{0,	BAll,	0},	/* 107, X_SetScreenSaver */
	{0,	BAll,	0},	/* 108, X_GetScreenSaver */
	{0,	BAll,	0},	/* 109, X_ChangeHosts */
	{0,	BAll,	0},	/* 110, X_ListHosts */
	{0,	BAll,	0},	/* 111, X_SetAccessControl */
	{0,	BAll,	0},	/* 112, X_SetCloseDownMode */
	{0,	BAll,	0},	/* 113, X_KillClient */
	{0,	BAll,	0},	/* 114, X_RotateProperties */
	{0,	BAll,	0},	/* 115, X_ForceScreenSaver */
	{0,	BAll,	0},	/* 116, X_SetPointerMapping */
	{0,	BAll,	0},	/* 117, X_GetPointerMapping */
	{0,	BAll,	0},	/* 118, X_SetModifierMapping */
	{0,	BAll,	0},	/* 119, X_GetModifierMapping */
	{BAll,	0,	0},	/* 120, Not Used */
	{BAll,	0,	0},	/* 121, Not Used */
	{BAll,	0,	0},	/* 122, Not Used */
	{BAll,	0,	0},	/* 123, Not Used */
	{BAll,	0,	0},	/* 124, Not Used */
	{BAll,	0,	0},	/* 125, Not Used */
	{BAll,	0,	0},	/* 126, Not Used */
	{0,	BAll,	0},	/* 127, X_NoOperation */
	};
/*
 * ErrorSensitive -- cause an exit on all X errors
 */
ErrorSensitive(s)
char	*s;
{
	hyperSensitive = s;
}

/*
 * ErrorInsensitive -- perform normal X error processing
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
ErrorInsensitive(dpy)
Display	*dpy;
{
	hyperSensitive = NULL;
}

/*
 * ErrorHandler -- this routine is called whenever an error in the X
 *	protocol is detected.
 */
ErrorHandler(dpy, event)
Display	*dpy;
XErrorEvent	*event;
{
	char	buffer[BUFSIZ];
	int	errBitmask;

	XGetErrorText(dpy, event->error_code, buffer, BUFSIZ);

	if (hyperSensitive != NULL)
	{
		/* treat all errors as fatal */
		(void) fprintf(stderr, gettext("olwm: Fatal X Error: %s\n"), buffer);
		(void) fprintf(stderr, "%s\n", hyperSensitive);
		exit(-1);
		/*NOTREACHED*/
	}

	errBitmask = 1 << event->error_code;

	if (errBitmask & errorTable[event->request_code].ignore)
		return 0;
	else if (errBitmask & errorTable[event->request_code].warning)
	{
               (void) fprintf(stderr, gettext("olwm: Warning, X Error: %s\n"),
                              buffer);
               (void) fprintf(stderr, gettext("  Request Major code: %d\n"),
                               event->request_code);
               (void) fprintf(stderr, gettext("  Request Minor code: %d\n"),
                               event->minor_code);
               (void) fprintf(stderr, gettext("  ResourceId 0x%x\n"),
                               event->resourceid);
		return 0;
	}
	else if (errBitmask & errorTable[event->request_code].fatal)
	{
               (void) fprintf(stderr, gettext("olwm: Fatal X Error: %s\n"),
                              buffer);
               (void) fprintf(stderr, gettext("  Request Major code: %d\n"),
                               event->request_code);
               (void) fprintf(stderr, gettext("  Request Minor code: %d\n"),
                               event->minor_code);
               (void) fprintf(stderr, gettext("  ResourceId 0x%x\n"),
                               event->resourceid);
               (void) fprintf(stderr, gettext("  Error Serial #%d\n"),
                              event->serial);
               (void) fprintf(stderr, gettext("  Current Serial #%d\n"),
                              dpy->request);
		/* do not return from fatal errors */
		exit(-1);
		/*NOTREACHED*/
	}

	return 0;
}

/*
 * ErrorGeneral -- this routine is called whenever a general error occurs
 *      within OLWM.  Exits from olwm.
 */
ErrorGeneral(txt)
char *txt;
{
        (void) fprintf(stderr, gettext("olwm: Fatal Error: %s\n"), txt);
#ifdef DEBUG
	abort();
	/*NOTREACHED*/
#else
        exit(-1);
	/*NOTREACHED*/
#endif
}

/*
 * ErrorWarning -- this routine is called whenever a condition requiring
 *      a warning message occurs within OLWM.
 */
void
ErrorWarning(txt)
char *txt;
{
        (void) fprintf(stderr, gettext("olwm: Warning: %s\n"), txt);
}

