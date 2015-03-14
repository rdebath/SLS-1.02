/* MsP.h - private header for MandelSpawn popup widget */
/* Copyright (C) 1990, 1991 Andreas Gustafsson */

#ifndef _MsP_h
#define _MsP_h

#include <X11/CompositeP.h>

#include "Ms.h"
#include "Mama.h"
#include "mspawn.h"

/* data type for the rubberband box */
struct box
{ int x0, x1, y0, y1;
};

/* widget instance structure */
typedef struct 
{ ms_state xi;			/* X-independent stuff */
  GC box_gc;			/* GC for the rubberband box */
  XPoint box_origin;
  XPoint box_corner;
  Bool box_exists;		/* the box is supposed to be visible */
  unsigned int box_hidden;	/* count of box hiding levels */
  XImage *rectbuffer;		/* chunk buffer (previously scanline buffer) */
  unsigned int rectbuffer_size; /* size of chunk buffer in bytes */
  MamaWidget mama;	      	/* our creator */
  Bool center_box;		/* make box symmetric around origin */
  Cursor my_cursor;		/* cursor to use within Ms window */
  int type;			/* frame buffer type (for fast drawing) */
#ifdef MENU
  Widget menu;			/* the popup menu */
#endif
#ifdef LABEL
  Widget underflow_label;	/* the underflow label widget */
  int underflow;		/* true if underflow label has been created */
#endif
  Bool sony_bug_workaround;	/* Sony bug compatibility mode */
  int crosshair_size;           /* length of each arm of the crosshair */
} MsPart;

typedef struct _MsRec 
{ CorePart	core;
  CompositePart composite;
  MsPart	ms;
} MsRec;

/* Widget class structure */
typedef struct 
{ int dummy;
} MsClassPart;

typedef struct _MsClassRec 
{ CoreClassPart core_class;
  CompositeClassPart composite_class;
  MsClassPart ms;
} MsClassRec;

extern MsClassRec msClassRec;

#endif /* _MsP_h */
