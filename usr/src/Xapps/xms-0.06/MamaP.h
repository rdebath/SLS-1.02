/* MamaP.h - private header for MandelSpawn master widget */
/* Copyright (C) 1990, 1991 Andreas Gustafsson */

#ifndef _MamaP_h
#define _MamaP_h

#include "Ms.h" /* public header of child widget */
#include "Mama.h"
#include "work.h"

/* widget instance structure */
typedef struct 
{ struct wf_state *workforce;	/* X-independent slave handling stuff */
  unsigned n_hues;		/* max. iterations */
  unsigned n_colours;		/* number of colours */
  char *spectrum;		/* text definitions of our colours */
  XtInputId input_id;		/* unique channel id */
  unsigned long *pixels;	/* pixel values for our colours */
  unsigned n_popups_created;	/* number of popups created (not existing) */
  Bool bw;			/* force black-and-white operation */
  Colormap my_colormap;		/* may be the default colormap */
  Bool wrap;			/* wrap colourmap when too few colours */
  Bool debug;			/* debugging mode */
} MamaPart;

typedef struct _MamaRec 
{ CorePart	core;
  MamaPart	mama;
} MamaRec;

/* Widget class structure */
typedef struct 
{ int dummy;
} MamaClassPart;

typedef struct _MsClassRec 
{ CoreClassPart core_class;
  MamaClassPart mama;
} MamaClassRec;

extern MamaClassRec mamaClassRec;
#endif /* _MamaP_h */
