/*
 * $XConsortium: DviP.h,v 1.9 91/07/26 15:21:49 keith Exp $
 */

/* 
 * DviP.h - Private definitions for Dvi widget
 */

#ifndef _XtDviP_h
#define _XtDviP_h

#include "Dvi.h"
#include <X11/Xaw/SimpleP.h>
#include "DviChar.h"

/***********************************************************************
 *
 * Dvi Widget Private Data
 *
 ***********************************************************************/

/************************************
 *
 *  Class structure
 *
 ***********************************/

/*
 * New fields for the Dvi widget class record
 */

typedef struct _DviClass {
	int		makes_compiler_happy;  /* not used */
} DviClassPart;

/*
 * Full class record declaration
 */

typedef struct _DviClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    DviClassPart	command_class;
} DviClassRec;

extern DviClassRec dviClassRec;

/***************************************
 *
 *  Instance (widget) structure 
 *
 **************************************/

/*
 * a list of fonts we've used for this widget
 */

typedef struct _dviFontSizeList {
	struct _dviFontSizeList	*next;
	int			size;
	char			*x_name;
	XFontStruct		*font;
	int			doesnt_exist;
} DviFontSizeList;

typedef struct _dviFontList {
	struct _dviFontList	*next;
	char			*dvi_name;
	char			*x_name;
	int			dvi_number;
	Boolean			initialized;
	Boolean			scalable;
	DviFontSizeList		*sizes;
	DviCharNameMap		*char_map;
} DviFontList;

typedef struct _dviFontMap {
	struct _dviFontMap	*next;
	char			*dvi_name;
	char			*x_name;
} DviFontMap;

#define DVI_TEXT_CACHE_SIZE	256
#define DVI_CHAR_CACHE_SIZE	1024

typedef struct _dviCharCache {
	XTextItem	cache[DVI_TEXT_CACHE_SIZE];
	char		char_cache[DVI_CHAR_CACHE_SIZE];
	int		index;
	int		max;
	int		char_index;
	int		font_size;
	int		font_number;
	XFontStruct	*font;
	int		start_x, start_y;
	int		x, y;
} DviCharCache;

typedef struct _dviState {
	struct _dviState	*next;
	int			font_size;
	int			font_bound;
	int			font_number;
	int			line_style;
	int			line_width;
	int			x;
	int			y;
} DviState;

typedef struct _dviFileMap {
	struct _dviFileMap	*next;
	long			position;
	int			page_number;
} DviFileMap;

/*
 * New fields for the Dvi widget record
 */

typedef struct {
	/*
	 * resource specifiable items
	 */
	char		*font_map_string;
	unsigned long	foreground;
	int		requested_page;
	int		last_page;
	XFontStruct	*default_font;
	FILE		*file;
	Boolean		noPolyText;
	Boolean		seek;		/* file is "seekable" */
	int		screen_resolution;
	float		page_width;
	float		page_height;
	/*
 	 * private state
 	 */
	FILE		*tmpFile;	/* used when reading stdin */
	char		readingTmp;	/* reading now from tmp */
	char		ungot;		/* have ungetc'd a char */
	GC		normal_GC;
	DviFileMap	*file_map;
	DviFontList	*fonts;
	DviFontMap	*font_map;
	int		current_page;
	int		font_size;
	int		font_number;
	int		device_resolution;
	int		line_width;
	int		line_style;
	int		desired_width;
	int		desired_height;
	int		backing_store;
	XFontStruct	*font;
	int		display_enable;
	double		scale;		/* device coordinates to pixels */
	struct ExposedExtents {
	    int x1, y1, x2, y2;
	}		extents;
	DviState	*state;
	DviCharCache	cache;
} DviPart;

#define DviGetIn(dw,cp)\
    (dw->dvi.tmpFile ? (\
	DviGetAndPut (dw, cp) \
    ) :\
	(*cp = getc (dw->dvi.file))\
)

#define DviGetC(dw, cp)\
    (dw->dvi.readingTmp ? (\
	((*cp = getc (dw->dvi.tmpFile)) == EOF) ? (\
	    fseek (dw->dvi.tmpFile, 0l, 2),\
	    (dw->dvi.readingTmp = 0),\
	    DviGetIn (dw,cp)\
	) : (\
	    *cp\
	)\
    ) : (\
	DviGetIn(dw,cp)\
    )\
)

#define DviUngetC(dw, c)\
    (dw->dvi.readingTmp ? (\
	ungetc (c, dw->dvi.tmpFile)\
    ) : ( \
	(dw->dvi.ungot = 1),\
	ungetc (c, dw->dvi.file)))

#define ToX(dw,device)		    ((int) ((device) * (dw)->dvi.scale + 0.5))
#define ToDevice(dw,x)		    ((int) ((x) / (dw)->dvi.scale + 0.5))
#define FontSizeInPixels(dw,size)   ((int) ((size) * (dw)->dvi.screen_resolution / 72))
#define FontSizeInDevice(dw,size)   ((int) ((size) * (dw)->dvi.device_resolution / 72))

/*
 * Full widget declaration
 */

typedef struct _DviRec {
    CorePart	core;
    SimplePart	simple;
    DviPart	dvi;
} DviRec;

extern XFontStruct	*QueryFont ();

extern DviCharNameMap	*QueryFontMap ();
#endif /* _XtDviP_h */


