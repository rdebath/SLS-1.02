/* $XConsortium: Dvi.c,v 1.18 91/07/26 15:21:52 keith Exp $ */
/*
 * Copyright 1991 Massachusetts Institute of Technology
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
 */

/*
 * Dvi.c - Dvi display widget
 */

#define XtStrlen(s)	((s) ? strlen(s) : 0)

  /* The following are defined for the reader's convenience.  Any
     Xt..Field macro in this code just refers to some field in
     one of the substructures of the WidgetRec.  */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Converters.h>
#include <stdio.h>
#include <ctype.h>
#include "DviP.h"

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */
/* Note: default_font_map was too long a token for some machines...
 *       therefor it has been split in to and assigned to resources
 *       in the ClassInitialize routine.
 */
static char default_font_map_1[] =  "\
R	-*-times-medium-r-normal--*-*-*-*-*-*-iso8859-1\n\
I	-*-times-medium-i-normal--*-*-*-*-*-*-iso8859-1\n\
B	-*-times-bold-r-normal--*-*-*-*-*-*-iso8859-1\n\
F	-*-times-bold-i-normal--*-*-*-*-*-*-iso8859-1\n\
BI	-*-times-bold-i-normal--*-*-*-*-*-*-iso8859-1\n\
C	-*-courier-medium-r-normal--*-*-*-*-*-*-iso8859-1\n\
CO	-*-courier-medium-o-normal--*-*-*-*-*-*-iso8859-1\n\
CB	-*-courier-bold-r-normal--*-*-*-*-*-*-iso8859-1\n\
CF	-*-courier-bold-o-normal--*-*-*-*-*-*-iso8859-1\n\
H	-*-helvetica-medium-r-normal--*-*-*-*-*-*-iso8859-1\n\
HO	-*-helvetica-medium-o-normal--*-*-*-*-*-*-iso8859-1\n\
HB	-*-helvetica-bold-r-normal--*-*-*-*-*-*-iso8859-1\n\
HF	-*-helvetica-bold-o-normal--*-*-*-*-*-*-iso8859-1\n\
";
static char default_font_map_2[] =  "\
N	-*-new century schoolbook-medium-r-normal--*-*-*-*-*-*-iso8859-1\n\
NI	-*-new century schoolbook-medium-i-normal--*-*-*-*-*-*-iso8859-1\n\
NB	-*-new century schoolbook-bold-r-normal--*-*-*-*-*-*-iso8859-1\n\
NF	-*-new century schoolbook-bold-i-normal--*-*-*-*-*-*-iso8859-1\n\
A	-*-charter-medium-r-normal--*-*-*-*-*-*-iso8859-1\n\
AI	-*-charter-medium-i-normal--*-*-*-*-*-*-iso8859-1\n\
AB	-*-charter-bold-r-normal--*-*-*-*-*-*-iso8859-1\n\
AF	-*-charter-bold-i-normal--*-*-*-*-*-*-iso8859-1\n\
S	-*-symbol-medium-r-normal--*-*-*-*-*-*-adobe-fontspecific\n\
S2	-*-symbol-medium-r-normal--*-*-*-*-*-*-adobe-fontspecific\n\
";

#define offset(field) XtOffsetOf(DviRec, field)

static XtResource resources[] = { 
	{XtNfontMap, XtCFontMap, XtRString, sizeof (char *),
	 offset(dvi.font_map_string), XtRString, NULL /* set in code */},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (unsigned long),
	 offset(dvi.foreground), XtRString, XtDefaultForeground},
	{XtNpageNumber, XtCPageNumber, XtRInt, sizeof (int),
	 offset(dvi.requested_page), XtRImmediate, (XtPointer) 1},
	{XtNlastPageNumber, XtCLastPageNumber, XtRInt, sizeof (int),
	 offset (dvi.last_page), XtRImmediate, (XtPointer) 0},
	{XtNfile, XtCFile, XtRFile, sizeof (FILE *),
	 offset (dvi.file), XtRFile, (char *) 0},
	{XtNseek, XtCSeek, XtRBoolean, sizeof (Boolean),
	 offset(dvi.seek), XtRImmediate, (XtPointer) False},
	{XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	 offset(dvi.default_font), XtRString, XtDefaultFont},
	{XtNbackingStore, XtCBackingStore, XtRBackingStore, sizeof (int),
	 offset(dvi.backing_store), XtRString, "default"},
	{XtNnoPolyText, XtCNoPolyText, XtRBoolean, sizeof (Boolean),
	 offset(dvi.noPolyText), XtRImmediate, (XtPointer) False},
	{XtNscreenResolution, XtCScreenResolution, XtRInt, sizeof (int),
	 offset(dvi.screen_resolution), XtRImmediate, (XtPointer) 75},
	{XtNpageWidth, XtCPageWidth, XtRFloat, sizeof (float),
	 offset(dvi.page_width), XtRString, "8.5"},
	{XtNpageHeight, XtCPageHeight, XtRFloat, sizeof (float),
	 offset(dvi.page_height), XtRString, "11"},
};

#undef offset

static void		ClassInitialize ();
static void		Initialize(), Realize (), Destroy (), Redisplay ();
static Boolean		SetValues (), SetValuesHook ();
static XtGeometryResult	QueryGeometry ();
static void		ShowDvi ();
static void		CloseFile (), OpenFile ();

#define SuperClass ((SimpleWidgetClass)&simpleClassRec)

DviClassRec dviClassRec = {
{
	(WidgetClass) SuperClass,	/* superclass		  */	
	"Dvi",				/* class_name		  */
	sizeof(DviRec),			/* size			  */
	ClassInitialize,		/* class_initialize	  */
	NULL,				/* class_part_initialize  */
	FALSE,				/* class_inited		  */
	Initialize,			/* initialize		  */
	NULL,				/* initialize_hook	  */
	Realize,			/* realize		  */
	NULL,				/* actions		  */
	0,				/* num_actions		  */
	resources,			/* resources		  */
	XtNumber(resources),		/* resource_count	  */
	NULLQUARK,			/* xrm_class		  */
	FALSE,				/* compress_motion	  */
	TRUE,				/* compress_exposure	  */
	TRUE,				/* compress_enterleave    */
	FALSE,				/* visible_interest	  */
	Destroy,			/* destroy		  */
	NULL,				/* resize		  */
	Redisplay,			/* expose		  */
	SetValues,			/* set_values		  */
	SetValuesHook,			/* set_values_hook	  */
	XtInheritSetValuesAlmost,	/* set_values_almost	  */
	NULL,				/* get_values_hook	  */
	NULL,				/* accept_focus		  */
	XtVersion,			/* version		  */
	NULL,				/* callback_private	  */
	0,				/* tm_table		  */
	QueryGeometry,			/* query_geometry	  */
	XtInheritDisplayAccelerator,	/* display_accelerator	  */
	NULL,				/* extension		  */
},  /* CoreClass fields initialization */
{
    XtInheritChangeSensitive		/* change_sensitive	*/
},  /* SimpleClass fields initialization */
{
    0,                                     /* field not used    */
},  /* DviClass fields initialization */
};

WidgetClass dviWidgetClass = (WidgetClass) &dviClassRec;

static void ClassInitialize ()
{
  int len1 = strlen(default_font_map_1);
  int len2 = strlen(default_font_map_2);
  char *dfm = XtMalloc(len1 + len2 + 1);
  char *ptr = dfm;
  strcpy(ptr, default_font_map_1); ptr += len1;
  strcpy(ptr, default_font_map_2); 
  resources[0].default_addr = dfm;

  XtAddConverter( XtRString, XtRBackingStore, XmuCvtStringToBackingStore,
		 NULL, 0 );
}

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

/* ARGSUSED */
static void Initialize(request, new)
	Widget request, new;
{
    DviWidget	dw = (DviWidget) new;

    dw->dvi.current_page = 0;
    dw->dvi.font_map = 0;
    dw->dvi.cache.index = 0;
    dw->dvi.file = 0;
    dw->dvi.seek = False;
    dw->dvi.device_resolution = 75;
    dw->dvi.tmpFile = 0;
    dw->dvi.readingTmp = 0;
    dw->dvi.ungot = 0;
    dw->dvi.file_map = 0;
    dw->dvi.fonts = 0;
    dw->dvi.font_map = 0;
    dw->dvi.current_page = 0;
    dw->dvi.font_size = 0;
    dw->dvi.font_number = 0;
    dw->dvi.device_resolution = 0;
    dw->dvi.line_width = 0;
    dw->dvi.backing_store = 0;
    dw->dvi.font = 0;
    dw->dvi.display_enable = 0;
    dw->dvi.state = 0;
    dw->dvi.cache.font = 0; 
    RequestDesiredSize (dw);
}

static void
Realize (w, valueMask, attrs)
	Widget			w;
	XtValueMask		*valueMask;
	XSetWindowAttributes	*attrs;
{
    DviWidget	dw = (DviWidget) w;
    XGCValues	values;

    if (dw->dvi.backing_store != Always + WhenMapped + NotUseful) {
	attrs->backing_store = dw->dvi.backing_store;
	*valueMask |= CWBackingStore;
    }
    XtCreateWindow (w, (unsigned)InputOutput, (Visual *) CopyFromParent,
		    *valueMask, attrs);
    values.foreground = dw->dvi.foreground;
    dw->dvi.normal_GC = XCreateGC (XtDisplay (w), XtWindow (w),
				    GCForeground, &values);
    if (dw->dvi.file)
	OpenFile (dw);
    ParseFontMap (dw);
}

static void
Destroy(w)
	Widget w;
{
	DviWidget	dw = (DviWidget) w;

	XFreeGC (XtDisplay (w), dw->dvi.normal_GC);
	DestroyFontMap (dw->dvi.font_map);
	DestroyFileMap (dw->dvi.file_map);
}

/*
 * Repaint the widget window
 */

/* ARGSUSED */
static void
Redisplay(w, event, region)
	Widget w;
	XEvent *event;
	Region region;
{
	DviWidget	dw = (DviWidget) w;
	XRectangle	extents;
	
	XClipBox (region, &extents);
	dw->dvi.extents.x1 = extents.x;
	dw->dvi.extents.y1 = extents.y;
	dw->dvi.extents.x2 = extents.x + extents.width;
	dw->dvi.extents.y2 = extents.y + extents.height;
	ShowDvi (dw);
}

RequestDesiredSize (dw)
    DviWidget	dw;
{
    XtWidgetGeometry	req, rep;

    dw->dvi.desired_width = dw->dvi.page_width *
				 dw->dvi.screen_resolution;
    dw->dvi.desired_height = dw->dvi.page_height *
				  dw->dvi.screen_resolution;
    req.request_mode = CWWidth|CWHeight;
    req.width = dw->dvi.desired_width;
    req.height = dw->dvi.desired_height;
    XtMakeGeometryRequest ((Widget) dw, &req, &rep);
}

/*
 * Set specified arguments into widget
 */
/* ARGSUSED */
static Boolean
SetValues (current, request, new)
	DviWidget current, request, new;
{
    Boolean		redisplay = FALSE;
    char		*new_map;
    int		cur, req;

    req = request->dvi.requested_page;
    cur = current->dvi.requested_page;
    if (cur != req) {
	    if (req < 1)
		req = 1;
	    if (request->dvi.file)
	    {
		if (current->dvi.last_page != 0 &&
		    req > current->dvi.last_page)
			req = current->dvi.last_page;
	    }
	    if (cur != req)
		redisplay = TRUE;
	    new->dvi.requested_page = req;
    }
    
    if (current->dvi.font_map_string != request->dvi.font_map_string) {
	    new_map = XtMalloc (strlen (request->dvi.font_map_string) + 1);
	    if (new_map) {
		    redisplay = TRUE;
		    strcpy (new_map, request->dvi.font_map_string);
		    new->dvi.font_map_string = new_map;
		    if (current->dvi.font_map_string)
			    XtFree (current->dvi.font_map_string);
		    current->dvi.font_map_string = 0;
		    ParseFontMap (new);
	    }
    }
    if (current->dvi.screen_resolution != request->dvi.screen_resolution)
    {
	ResetFonts (new);
	new->dvi.line_width = -1;
    }
    if (request->dvi.device_resolution)
	new->dvi.scale = ((double) request->dvi.screen_resolution) /
			     ((double) request->dvi.device_resolution);
    if (current->dvi.page_width !=  request->dvi.page_width ||
	current->dvi.page_height != request->dvi.page_height ||
	current->dvi.screen_resolution != request->dvi.screen_resolution)
    {
	RequestDesiredSize (new);
	redisplay = TRUE;
    }
    return redisplay;
}

/*
 * use the set_values_hook entry to check when
 * the file is set
 */

static Boolean
SetValuesHook (dw, args, num_argsp)
	DviWidget	dw;
	ArgList		args;
	Cardinal	*num_argsp;
{
	Cardinal	i;

	for (i = 0; i < *num_argsp; i++) {
		if (!strcmp (args[i].name, XtNfile)) {
			CloseFile (dw);
			OpenFile (dw);
			return TRUE;
		}
	}
	return FALSE;
}

static void CloseFile (dw)
	DviWidget	dw;
{
    if (dw->dvi.tmpFile)
	fclose (dw->dvi.tmpFile);
    ForgetPagePositions (dw);
}

static void OpenFile (dw)
	DviWidget	dw;
{
    char	tmpName[sizeof ("/tmp/dviXXXXXX")];

    dw->dvi.tmpFile = 0;
    if (!dw->dvi.seek) {
	strcpy (tmpName, "/tmp/dviXXXXXX");
	mktemp (tmpName);
	dw->dvi.tmpFile = fopen (tmpName, "w+");
	unlink (tmpName);
    }
    if (dw->dvi.requested_page < 1)
	dw->dvi.requested_page = 1;
    dw->dvi.last_page = 0;
}

static XtGeometryResult
QueryGeometry (w, request, geometry_return)
	Widget			w;
	XtWidgetGeometry	*request, *geometry_return;
{
	XtGeometryResult	ret;
	DviWidget		dw = (DviWidget) w;

	ret = XtGeometryYes;
	if ((int)request->width < dw->dvi.desired_width
	    || (int)request->height < dw->dvi.desired_height)
		ret = XtGeometryAlmost;
	geometry_return->width = dw->dvi.desired_width;
	geometry_return->height = dw->dvi.desired_height;
	geometry_return->request_mode = CWWidth|CWHeight;
	return ret;
}

SetDeviceResolution (dw, resolution)
	DviWidget   dw;
	int	    resolution;
{
    if (resolution != dw->dvi.device_resolution) {
	dw->dvi.device_resolution = resolution;
	dw->dvi.scale = ((double)  dw->dvi.screen_resolution) /
			((double) resolution);
	if (dw->dvi.state)
	    dw->dvi.state->line_width =
		FontSizeInDevice(dw, dw->dvi.state->line_width/10.0);
    }
}

static void
ShowDvi (dw)
	DviWidget	dw;
{
	int	i;
	long	file_position;

	if (!dw->dvi.file) 
	  return;

	if (dw->dvi.requested_page < 1)
		dw->dvi.requested_page = 1;

	if (dw->dvi.last_page != 0 && dw->dvi.requested_page > dw->dvi.last_page)
		dw->dvi.requested_page = dw->dvi.last_page;

	file_position = SearchPagePosition (dw, dw->dvi.requested_page);
	if (file_position != -1) {
		FileSeek(dw, file_position);
		dw->dvi.current_page = dw->dvi.requested_page;
	} else {
		for (i=dw->dvi.requested_page; i > 0; i--) {
			file_position = SearchPagePosition (dw, i);
			if (file_position != -1)
				break;
		}
		if (file_position == -1)
			file_position = 0;
		FileSeek (dw, file_position);

		dw->dvi.current_page = i;
		
		dw->dvi.display_enable = 0;
		while (dw->dvi.current_page != dw->dvi.requested_page) {
			dw->dvi.current_page = ParseInput (dw);
			/*
			 * at EOF, seek back to the begining of this page.
			 */
			if (feof (dw->dvi.file)) {
				file_position = SearchPagePosition (dw,
						dw->dvi.current_page);
				if (file_position != -1)
					FileSeek (dw, file_position);
				break;
			}
		}
	}
	
	dw->dvi.display_enable = 1;
	ParseInput (dw);
	if (dw->dvi.last_page && dw->dvi.requested_page > dw->dvi.last_page)
		dw->dvi.requested_page = dw->dvi.last_page;
}
