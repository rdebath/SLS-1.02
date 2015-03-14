/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: ScrollByLP.h,v 1.8 91/07/31 22:41:59 keith Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   December 5, 1987
 */

#ifndef _XtScrollByLinePrivate_h
#define _XtScrollByLinePrivate_h

#include <X11/Xaw/SimpleP.h>

#include "ScrollByL.h"

/***********************************************************************
 *
 * ScrollByLine Widget Private Data
 *
 ***********************************************************************/

/* New fields for the ScrollByLine widget class record */
typedef struct {
     int mumble;   /* No new procedures */
} ScrollByLineClassPart;

/* Full class record declaration */
typedef struct _ScrollByLineClassRec {
    CoreClassPart	  core_class;
    SimpleClassPart       simple_class;
    ScrollByLineClassPart scrolled_widget_class;
} ScrollByLineClassRec;

extern ScrollByLineClassRec scrollByLineClassRec;

/* New fields for the ScrollByLine widget record */
typedef struct _ScrollByLinePart {
  Pixel foreground;		/* The color for the forground of the text. */
  Boolean force_vert,		/* Must have scrollbar visable */
    use_right;			/* put scroll bar on right side of window. */
  FILE * file;			/* The file to display. */
  Dimension indent;		/* amount to indent the file. */
  XFontStruct * bold_font,	/* The four fonts. */
    * normal_font,
    * italic_font,
    * symbol_font;
  
/* variables not in resource list. */

  Widget bar;			/* The scrollbar. */
  int font_height;		/* the height of the font. */
  int line_pointer;		/* The line that currently is at the top 
				   of the window being displayed. */
  Dimension offset;		/* Drawing offset because of scrollbar. */
  GC move_gc;			/* GC to use when moving the text. */
  GC bold_gc, normal_gc, italic_gc, symbol_gc; /* gc for drawing. */

  char ** top_line;		/* The top line of the file. */
  int lines;			/* number of line in the file. */
} ScrollByLinePart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _ScrollByLineRec {
    CorePart	      core;
    SimplePart        simple;
    ScrollByLinePart  scroll;
} ScrollByLineRec;

#endif /* _XtScrollByLinePrivate_h --- DON'T ADD STUFF AFTER THIS LINE */
