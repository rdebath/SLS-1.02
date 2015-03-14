/* Ms.c - MandelSpawn popup widget */

/*  This file is part of MandelSpawn, a parallel Mandelbrot program for
    the X window system.

    Copyright (C) 1990, 1991 Andreas Gustafsson

    MandelSpawn is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License, version 1,
    as published by the Free Software Foundation.

    MandelSpawn is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License,
    version 1, along with this program; if not, write to the Free 
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* This widget creates a window showing a part of the Mandelbrot set */

#include <stdio.h>
#include <math.h>

#include <X11/IntrinsicP.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>

/* We need ntohs() and htonl(); on some systems (HP-UX comes to mind) */
/* they are macros defined in <netinet/in.h> and nowhere else, with */
/* no corresponding library function. */
#include <netinet/in.h>	

#include "backward.h" /* X11R2 backward compatibility stuff */

#include "MsP.h" /* includes Ms.h which includes MsJob.h */
#include "Mama.h"

extern XtAppContext thisApp;
extern Display *myDisplay;
extern Screen *myScreen;

/* Misc. functions */
static void
    ClassInitialize(), Initialize(), Resize(), Realize(), Destroy(), 
    InvertBox(), EraseBox(), HideBox(), UnhideBox(),
    DoExpose();

/* These are used by menu choises in R4menu.c */
void ZoomIn(), WindowStats();

/* Action functions */
static void
    BeginBoxAction(), StretchBoxAction(), EndBoxAction(),
    ZoomAction(), CloseAction(), QuitAction(), ApplStatsAction(), 
    WindowStatsAction();

#ifdef MENU
void MsCreateMenu();
#endif
#ifdef LABEL
void MsCreateUnderflowLabel();
#endif

static Boolean SetValues();


/* supported visual type / iteration count length combinations */
/* are obtained by ORing togeter one ITER_x and one DISP_x value */

#define ITER_BYTE		0
#define ITER_WORD		1

#define DISP_unknown		0
#define DISP_1plane32msb 	2  /* like a Sun monochrome display */
#define DISP_1plane32lsb 	4  /* like a DECstation monochrome display */
#define DISP_8plane		6  /* typical 8-plane framebuffer */
#define DISP_generic		8  /* none of the above */


/* Defaults */

static Dimension default_width = 400;	/* window width in pixels */
static Dimension default_height = 250;	/* window height in pixels */
static int default_iteration_limit = 0;	/* 0 means undefined */
static double default_center_x = (-0.5); /* x coordinate of window center */
static double default_center_y = 0.0;	/* y coordinate of window center */
static double default_range = 4.0; 	/* window range in x direction */
static Bool default_center_box = True;	/* do we center the rubberband box? */
static Bool default_julia = False;	/* do we show the Julia set? */
static double default_c_x = 0.0; 	/* c parameter for Julia, real */
static double default_c_y = 0.0;	/* c parameter for Julia, im */
static double default_julia_range = 4.0; /* window range for Julia */
static double default_julia_center_x = 0.0; /* window center x for Julia */
static double default_julia_center_y = 0.0; /* window center y for Julia */
static char default_cursor[] = "top_left_arrow"; 
static unsigned default_chunk_width = 32;
static unsigned default_chunk_height = 32;
static Bool default_sony_bug_workaround = False;
static Bool default_crosshair_size = 3;
static Bool default_show_interior = False;


extern char msDefaultTranslations[];

#ifndef MENU
/* Some of these bindings are less than obvious.  Don't complain, */
/* get X11R4 and Xaw so you can use the popup menu in R4menu.c. */
char msDefaultTranslations[] = 
    "<Btn1Down>:	BeginBox()		\n\
     <Btn1Motion>:	StretchBox()		\n\
     <Btn1Up>:		EndBox()		\n\
     Shift<Btn2Down>:	Zoom(nopop,nojulia,in)	\n\
     <Btn2Down>:	Zoom(popup,nojulia,in)	\n\
     Shift<Btn3Down>:	Quit()			\n\
     <Btn3Down>:	Close()			\n\
     Shift<Key>z:	Zoom(nopop,nojulia,in)	\n\
     <Key>z:		Zoom(popup,nojulia,in)	\n\
     Shift<Key>o:	Zoom(nopop,nojulia,out)	\n\
     <Key>o:		Zoom(popup,nojulia,out)	\n\
     Shift<Key>j:	Zoom(nopop,julia,in)	\n\
     <Key>j:		Zoom(popup,julia,in)	\n\
     <Key>s:		ApplStats()		\n\
     <Key>w:		WindowStats()		\n\
     <Key>c:		Close()			\n\
     <Key>q:		Quit()			\n\
  ";
#endif

static XtActionsRec actionsList[] =
{
    { "BeginBox",	BeginBoxAction		},
    { "EndBox",		EndBoxAction		},
    { "StretchBox",	StretchBoxAction	},
    { "Zoom",		ZoomAction		},
    { "WindowStats",	WindowStatsAction	},
    { "ApplStats",	ApplStatsAction		},
    { "Close",		CloseAction		},
    { "Quit",		QuitAction		}
};

static XtResource resources[] = 
{   /* Core resources */
    { XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	XtOffset(Widget, core.width), XtRDimension,
	(caddr_t) &default_width },
    { XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	XtOffset(Widget, core.height), XtRDimension,
	(caddr_t) &default_height },
    /* Noncore resources */
    { XtNiteration_limit, XtCValue, XtRInt, sizeof(int),
	XtOffset(MsWidget, ms.xi.job.iteration_limit), XtRInt,
	(caddr_t) &default_iteration_limit },
    { XtNCenterX, XtCValue, XtRDouble, sizeof(double),
	XtOffset(MsWidget, ms.xi.center_x), XtRDouble,
	(caddr_t) &default_center_x },
    { XtNCenterY, XtCValue, XtRDouble, sizeof(double),
	XtOffset(MsWidget, ms.xi.center_y), XtRDouble,
	(caddr_t) &default_center_y },
    { XtNRange, XtCValue, XtRDouble, sizeof(double),
	XtOffset(MsWidget, ms.xi.xrange), XtRDouble,
	(caddr_t) &default_range },
    { XtNCenterBox, XtCValue, XtRBool, sizeof(Bool),
	XtOffset(MsWidget, ms.center_box), XtRBool,
	(caddr_t) &default_center_box },
    { XtNCursor, XtCValue, XtRCursor, sizeof(Cursor),
	XtOffset(MsWidget, ms.my_cursor), XtRString,
	(caddr_t) default_cursor },
    { XtNMama, XtCValue, XtRPointer, sizeof(MamaWidget),
	XtOffset(MsWidget, ms.mama), XtRPointer,
	(caddr_t) 0 } ,
    { XtNJulia, XtCValue, XtRBool, sizeof(Bool),
	XtOffset(MsWidget, ms.xi.julia), XtRBool,
	(caddr_t) &default_julia },
    { XtNCX, XtCValue, XtRDouble, sizeof(double),
	XtOffset(MsWidget, ms.xi.c_x), XtRDouble,
	(caddr_t) &default_c_x },
    { XtNCY, XtCValue, XtRDouble, sizeof(double),
	XtOffset(MsWidget, ms.xi.c_y), XtRDouble,
	(caddr_t) &default_c_y },
    { XtNChunkWidth, XtCValue, XtRInt, sizeof(unsigned int),
	XtOffset(MsWidget, ms.xi.chunk_width), XtRInt,
	(caddr_t) &default_chunk_width },
    { XtNChunkHeight, XtCValue, XtRInt, sizeof(unsigned int),
	XtOffset(MsWidget, ms.xi.chunk_height), XtRInt,
	(caddr_t) &default_chunk_height },
    { XtNSony, XtCValue, XtRBool, sizeof(Bool),
        XtOffset(MsWidget, ms.sony_bug_workaround), XtRBool,
        (caddr_t) &default_sony_bug_workaround },
    { XtNCrosshairSize, XtCValue, XtRInt, sizeof(int),
        XtOffset(MsWidget, ms.crosshair_size), XtRInt,
        (caddr_t) &default_crosshair_size },
    { XtNInterior, XtCValue, XtRBool, sizeof(Bool),
        XtOffset(MsWidget, ms.xi.show_interior), XtRBool,
        (caddr_t) &default_show_interior }

};

MsClassRec msClassRec = 
{   /* core fields */
    { 
    /* superclass		*/	(WidgetClass) &compositeClassRec,
    /* class_name		*/	"MandelSpawn",
    /* widget_size		*/	sizeof(MsRec),
    /* class_initialize		*/      NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actionsList,
    /* num_actions		*/	XtNumber(actionsList),
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	NULL,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	FALSE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	DoExpose,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	msDefaultTranslations,
    /* query_geometry		*/	NULL
    },
    /* CompositeClassPart fields */
    { XtInheritGeometryManager,
      XtInheritChangeManaged,
      XtInheritInsertChild,
      XtInheritDeleteChild
    },
    /* msClassPart fields */
    {
    /* dummy			*/	0
    }
};

WidgetClass msWidgetClass = (WidgetClass) &msClassRec;


/* Initialize the widget */

static void
Initialize(request, new)
     MsWidget   request;
     MsWidget   new;
{ XGCValues gcv;
  int depth = DefaultDepthOfScreen(XtScreen(new));
  /* set up a GC for the rubberband box */
  gcv.function=GXxor;
  
  /* "gcv.foreground=~0l" messes up the black-and-white mode of the */
  /* X11R2 server for the Sony NWS-1510 4-plane display quite badly; */
  /* I'd call that a server bug.  The code below works but assumes */
  /* 2's complement. */
  if(depth==32)
    gcv.foreground=0xffffffff; /* 1<<32 is undefined, says K&R */
  else
    gcv.foreground= (1 << depth) - 1;
  new->ms.box_gc=XtGetGC(new, GCFunction|GCForeground, &gcv);
  new->ms.box_exists=False;
  new->ms.box_hidden=0;
  new->ms.rectbuffer=(XImage *) NULL; /* no rectangle buffer so far */
#ifdef LABEL
  new->ms.underflow=0;
  new->ms.underflow_label=NULL;
#endif
  ms_init(&new->ms.xi, MamaWorkforce(new->ms.mama));
}


/* Initialize, or re-initialize widget fields that may need to be */
/* changed when the widget is resized, zoomed or otherwise modified */

static void MsPrecalculate(w) MsWidget w;
{ int underflow;
  w->ms.xi.height = w->core.height;
  w->ms.xi.width = w->core.width;
  
  ms_calculate_job_parameters(&w->ms.xi, &w->ms.xi.job);

#ifdef LABEL
  underflow=(w->ms.xi.job.delta.re == 0 || w->ms.xi.job.delta.im == 0);

  if(underflow && !w->ms.underflow)
  { MsCreateUnderflowLabel(w);
  }
  if(!underflow && w->ms.underflow)
  { XtDestroyWidget(w->ms.underflow_label);
  }
  w->ms.underflow = underflow;
#endif

  /* if it had a rectangle buffer already, destroy it */
  if(w->ms.rectbuffer)
    XDestroyImage(w->ms.rectbuffer);

  /* create the rectangle buffer */
  w->ms.rectbuffer=
    XCreateImage(XtDisplay(w), DefaultVisualOfScreen(XtScreen(w)), 
		 DefaultDepthOfScreen(XtScreen(w)), ZPixmap, 0, (char *) NULL, 
		 w->ms.xi.chunk_width, w->ms.xi.chunk_height, 32,
		 0 /* zero means let XCreateImage determine bytes/line */
		);
  w->ms.rectbuffer_size=(unsigned) w->ms.rectbuffer->bytes_per_line * 
    w->ms.xi.chunk_height;
  w->ms.rectbuffer->data=(char *) XtMalloc(w->ms.rectbuffer_size);

  if(w->ms.xi.job.iteration_limit == 0) /* not set yet? */
  { /* iterate as far as possible by default */
    w->ms.xi.job.iteration_limit = MaxIterations(w->ms.mama);
  }
  else
    if(w->ms.xi.job.iteration_limit > MaxIterations(w->ms.mama))
    { w->ms.xi.job.iteration_limit=MaxIterations(w->ms.mama);
      XtAppWarning(thisApp,
		   "Iteration limit truncated");
    }

  w->ms.xi.bytes_per_count=(w->ms.xi.job.iteration_limit > 256 ? 2 : 1);

  /* determine the kind of draw routine to use depending on */
  /* display characteristics and iteration count size */

  if(w->ms.rectbuffer->bits_per_pixel == 8)
    w->ms.type=DISP_8plane;
  else if(w->ms.rectbuffer->bits_per_pixel == 1  &&
	    w->ms.rectbuffer->bitmap_unit==32 &&
	    w->ms.rectbuffer->bitmap_bit_order != LSBFirst)
      w->ms.type=DISP_1plane32msb;
  else if(w->ms.rectbuffer->bits_per_pixel == 1  &&
            w->ms.rectbuffer->bitmap_unit==32 &&
            w->ms.rectbuffer->bitmap_bit_order == LSBFirst)
      w->ms.type=DISP_1plane32lsb;
  else
  { w->ms.type=DISP_generic;
    XtAppWarning(thisApp,
		 "No special case support for this display type;\n\
drawing a pixel at a time, this will be very slow.");
  }

  w->ms.type |= (w->ms.xi.bytes_per_count==2) ? ITER_WORD : ITER_BYTE;
 
  /* We now have a new configuration; give it a unique number */
  w->ms.xi.configuration++;
}


/* Calculate the coordinates for the rubberband box given the initial */
/* and current mouse position (box_origin, box_corner). Use the box_origin */
/* as either the center or the oppsite corner depending on the center_box */
/* flag */

struct box UnflipBox(w)
     MsWidget w;
{ struct box r;
  if(w->ms.center_box)
  { int halfwidth=ABS(w->ms.box_corner.x - w->ms.box_origin.x);
    int halfheight=ABS(w->ms.box_corner.y - w->ms.box_origin.y);
    r.x0=w->ms.box_origin.x-halfwidth;
    r.x1=w->ms.box_origin.x+halfwidth;
    r.y0=w->ms.box_origin.y-halfheight;
    r.y1=w->ms.box_origin.y+halfheight;
  }
  else
  { r.x0=MIN(w->ms.box_origin.x, w->ms.box_corner.x);
    r.x1=MAX(w->ms.box_origin.x, w->ms.box_corner.x);
    r.y0=MIN(w->ms.box_origin.y, w->ms.box_corner.y);
    r.y1=MAX(w->ms.box_origin.y, w->ms.box_corner.y);
  }
  return(r);
}


/* Zoom into the area selected using the box.  If "pop", pop up a new */
/* window.  If "julia", show the Julia set corresponding to the point */
/* at the center of the box.  If "outwards", zoom out instead of in. */

void ZoomIn(w, pop, julia, outwards)
       MsWidget w;
       int pop;
       int julia;
       int outwards;
{ struct box b;
  double scale;
  double new_xrange, new_center_x, new_center_y;
  Arg arglist[16];
  int num_args;
  Arg shell_arglist[16];
  int num_shell_args;
  int boxwidth;
  double nx, ny;

  if(!w->ms.box_exists)
    return;

  b=UnflipBox(w);  

  boxwidth = b.x1-b.x0;

  /* caculate location of box center normalized so that 0 = window center, */
  /* +-0.5 = window edge */
  nx = ((((double) b.x0+b.x1)/2)-(w->core.width/2)) / (double)(w->core.width);
  ny = ((((double) b.y0+b.y1)/2)-(w->core.height/2)) /	(double)(w->core.height);
	  
  if(outwards) /* zoom out */
  { if(boxwidth==0)
    { /* avoid division by zero */
      new_xrange = default_range;
      new_center_x = w->ms.xi.center_x;
      new_center_y = w->ms.xi.center_y;
    }
    else
    { scale = (double) w->core.width / (double) boxwidth;
      new_xrange = w->ms.xi.xrange * scale;
      new_center_x = w->ms.xi.center_x - nx * scale * w->ms.xi.xrange;
      new_center_y = w->ms.xi.center_y - ny * scale * w->ms.xi.yrange;
    }
  }
  else /* zoom in */
  { scale = (double) boxwidth / (double) (w->core.width);
    new_xrange = w->ms.xi.xrange * scale;
    new_center_x = w->ms.xi.center_x + nx * w->ms.xi.xrange;
    new_center_y = w->ms.xi.center_y + ny * w->ms.xi.yrange;
  }
  
  /* build arguments for the changed resources in the zoomed widget */
  num_args=0;

  /* Transition from Mandelbrot to Julia set */
  if(julia && !(w->ms.xi.julia))
  { XtSetArg(arglist[num_args], XtNJulia, True); num_args++;
    /* Set the C parameter (chooses a Julia set out of infinitely many) */
    XtSetArg(arglist[num_args], XtNCX, &new_center_x); num_args++;
    XtSetArg(arglist[num_args], XtNCY, &new_center_y); num_args++;
    /* This is an initial Julia picture, so show the whole Julia set */
    XtSetArg(arglist[num_args], XtNRange, &default_julia_range); num_args++;
    XtSetArg(arglist[num_args], XtNCenterX,
	     &default_julia_center_x); num_args++;
    XtSetArg(arglist[num_args], XtNCenterY,
	     &default_julia_center_y); num_args++;
  }
  else /* No change in M/J mode */
  { XtSetArg(arglist[num_args], XtNJulia, w->ms.xi.julia); num_args++;
    XtSetArg(arglist[num_args], XtNCenterX, &new_center_x); num_args++;
    XtSetArg(arglist[num_args], XtNCenterY, &new_center_y); num_args++;
    XtSetArg(arglist[num_args], XtNRange, &new_xrange); num_args++;
    /* these two parameters are redundant in the Mandelbrot mode */
    XtSetArg(arglist[num_args], XtNCX, &w->ms.xi.c_x); num_args++;
    XtSetArg(arglist[num_args], XtNCY, &w->ms.xi.c_y); num_args++;
  }

  XtSetArg(arglist[num_args], XtNMama, w->ms.mama); num_args++;

  if(pop)
  { num_shell_args = 0;
    /* make the new window as big as this one initially */
    XtSetArg(shell_arglist[num_shell_args], XtNheight, w->core.height);
    num_shell_args++;
    XtSetArg(shell_arglist[num_shell_args], XtNwidth, w->core.width);
    num_shell_args++;
    /* create a new pop-up window for the zoomed area */
    PopupAnother(w->ms.mama, shell_arglist, num_shell_args, arglist, num_args);
  }
  else 
  {
#if 1
    /* zoom using the old window: just set the changed resources */
    EraseBox(w);
    XtSetValues(w, arglist, num_args);
#else
    /* Code under construction.  Hazardous area, do not enter. */
    /* unmap the shell widget */
    XtUnmapWidget(XtParent(w));
    /* should make new window use same location as the old one, */
    /* without WM intervention.  How? */
    num_shell_args = 0;
    /* make the new window as big as this one initially */
    XtSetArg(shell_arglist[num_shell_args], XtNheight, w->core.height);
    num_shell_args++;
    XtSetArg(shell_arglist[num_shell_args], XtNwidth, w->core.width);
    num_shell_args++;
    /* create a new pop-up window for the zoomed area */
    PopupAnother(w->ms.mama, shell_arglist, num_shell_args, arglist, num_args);
#endif
  }
}


/* Action routine interface to ZoomIn */

static void ZoomAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ 
  if(*nparams != 3)
  { XtAppWarning(thisApp, "ZoomAction: wrong number of arguments");
    return;
  }
  ZoomIn(w, params[0][0] == 'p', params[1][0] == 'j', params[2][0] == 'o');
}


/* Handle widget exposure */
/* Can't call this "Expose" because that is #defined to 12 in X.h! */

/*ARGSUSED*/
static void DoExpose (w, e, r)
     MsWidget    w;
     XEvent *e;
     Region r;
{
#ifdef LABEL
  /* It's no use redrawing if underflow has occured */
  if(w->ms.underflow)
    return;
#endif

  /* remove the box for the duration of the update */
  if(w->ms.xi.chunks_out==0)
    HideBox(w);
  
  ms_dispatch_rect(&w->ms.xi, (char *) w,
		   e->xexpose.x, e->xexpose.y,
		   e->xexpose.width, e->xexpose.height
		  );
}


/* Draw (or undraw) the rubberband box */

static void InvertBox(w)
     MsWidget w;
{ struct box b;
  /* draw only if no update is in progress */
  if(!w->ms.box_hidden)
  { int armx, army;
    armx=army=w->ms.crosshair_size;
    b=UnflipBox(w);
    XDrawRectangle(XtDisplay(w), XtWindow(w), w->ms.box_gc,
		   b.x0, b.y0, b.x1-b.x0, b.y1-b.y0);
    if(armx || army)
    { /* draw a crosshair at the center of the selected area */
      int xc = (b.x0+b.x1)/2;
      int yc = (b.y0+b.y1)/2;
      XDrawLine(XtDisplay(w), XtWindow(w), w->ms.box_gc,
		xc-armx, yc, xc+armx, yc);
      XDrawLine(XtDisplay(w), XtWindow(w), w->ms.box_gc,
		xc, yc-army, xc, yc+army);
    }
    w->ms.box_exists = !(w->ms.box_exists);
  }
}


/* Erase the rubberband box if it exists */

static void EraseBox(w)
     MsWidget w;
{ if(w->ms.box_exists) InvertBox(w);
}



/* Create the rubberband box */

static void BeginBoxAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ EraseBox(w);
  w->ms.box_origin.x=w->ms.box_corner.x=((XButtonEvent *) event)->x;
  w->ms.box_origin.y=w->ms.box_corner.y=((XButtonEvent *) event)->y;
  InvertBox(w);
}


/* Stretch the rubberband box */

static void StretchBoxAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ EraseBox(w);
  w->ms.box_corner.x=((XButtonEvent *) event)->x;
  w->ms.box_corner.y=((XButtonEvent *) event)->y;
  InvertBox(w);
}


/* Stop stretching the rubberband box (currently a no-op) */

static void EndBoxAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{
}


/* Hide the box when beginning an asynchronous update */

static void HideBox(w)
     MsWidget w;
{ if(w->ms.box_hidden==0 && w->ms.box_exists)
    InvertBox(w);
  w->ms.box_hidden++;
}


/* Unhide the box when an asynchronous update is done */

static void UnhideBox(w)
     MsWidget w;
{ w->ms.box_hidden--;
  if(w->ms.box_hidden==0 && w->ms.box_exists) 
    InvertBox(w);
}


/* Realize the widget */

static void
Realize (w, valueMask, attrs)
     MsWidget w;
     XtValueMask *valueMask;
     XSetWindowAttributes *attrs;
{ /* setting the colormap here is probably redundant */
  attrs->backing_store=Always;
  attrs->save_under=False;
  attrs->bit_gravity=ForgetGravity;
  attrs->cursor=w->ms.my_cursor;
  attrs->colormap=MamaColormap(w->ms.mama);
  XtCreateWindow (w, InputOutput, (Visual *) CopyFromParent,
    *valueMask | CWBackingStore | CWSaveUnder| CWBitGravity | CWCursor |
		  CWColormap, attrs);
  MsPrecalculate(w);
#ifdef MENU
  MsCreateMenu(w, !w->ms.xi.julia);
#endif
}


/* Destroy the widget */

static void
Destroy (w)
     MsWidget w;
{ XtReleaseGC(w, w->ms.box_gc);
  XtFree((char *) w->ms.rectbuffer);
}


/* Update widget resources */

static Boolean SetValues(current, request, new)
     MsWidget current, request, new;
{ MsPrecalculate(new); /* do the dirty work */
  return(True); /* widget must be redisplayed */
}
 

/* Resize the widget */

static void
Resize(w)
     MsWidget w;
{ MsPrecalculate(w); /* do the dirty work */
}


/* Print the view coordinates */

void WindowStats(w)
     MsWidget w;
{ (void) printf("current picture area: x = %f .. %f, y = %f .. %f\n",
		w->ms.xi.center_x - w->ms.xi.xrange / 2,
		w->ms.xi.center_x + w->ms.xi.xrange / 2,
		w->ms.xi.center_y - w->ms.xi.yrange / 2,
		w->ms.xi.center_y + w->ms.xi.yrange / 2
	       );
  (void) printf("return here with: xms ");
  if(w->ms.xi.julia)
    (void) printf("-julia -cx %f -cy %f ", w->ms.xi.c_x, w->ms.xi.c_y);
  (void) printf("-x %f -y %f -range %f\n",
	       w->ms.xi.center_x, w->ms.xi.center_y, w->ms.xi.xrange);
}


/* Action routine for the above */

static void WindowStatsAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ WindowStats(w);
}


/* Print slave performance statistics */

static void ApplStatsAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ SlaveStatistics(w->ms.mama);
}


/* auxiliary macro for ms_draw: */

#define DRAW_SINGLEPLANE(iter_type, init_mask, shift_op) \
 { iter_type *datap = (iter_type *) data; \
   for(j=0; j<height; j++) \
   { unsigned char *bufp = (unsigned char *) w->ms.rectbuffer->data + \
       (w->ms.rectbuffer->bytes_per_line * j); \
     for(i=0; i<width; ) /* for each byte */ \
     { unsigned pixbyte = 0; \
       unsigned mask = init_mask; \
       unsigned end=MIN(width, i+8); \
       for(; i<end; i++) /* for each bit in the byte */ \
       { if(pixels[*datap++]) \
	   pixbyte |= mask; \
	 mask shift_op 1; \
       } \
       *bufp++ = pixbyte; \
     } \
   } \
 }


/* This function is called when a chunk has been completed by a slave */
/* to draw it on the screen */

void ms_draw(client, client_data, data)
     char *client;
     char *client_data;
     char *data; 
{ MsWidget w = (MsWidget) client;
  register int i, j;
  unsigned int x, y, width, height;

  ms_client_info *the_info = (ms_client_info *) client_data;

  unsigned long *pixels = MamaPixels(w->ms.mama);

  /* Ignore the reply if the widget has changed shape or something */
  if(the_info->configuration == w->ms.xi.configuration)
  { x=the_info->s.x; 
    y=the_info->s.y; 
    width=the_info->s.width;
    height=the_info->s.height;

    /* 16-bit iteration counts from the slave need to be */
    /* converted from network byte order.  It is faster */
    /* to do it all at once here than to call ntohs() */
    /* in the tight loops below.  Any decent compiler will */
    /* optimize away the whole "if" statement if the CPU */
    /* is big-endian; if it is little-endian then a single */
    /* redundant ntohs() call will remain in the "if" condition. */

    if(ntohs(1) != 1)
    { if(w->ms.xi.bytes_per_count == sizeof(unsigned short))
      {	unsigned short *end;
	register unsigned char *p;
	end = (unsigned short *) data + (width * height);
	for(p = (unsigned char *) data; p < (unsigned char *) end; p += 2)
	{ unsigned char tmp;
	  tmp = *p;
	  *p = *(p+1);
	  *(p+1) = tmp;
	}
      }
    }

    /* sorry about the combinatorial explosion, but this part easily */
    /* becomes a bottleneck if all the tests are done inside the loop */
    switch(w->ms.type)
    {
    case ITER_BYTE|DISP_1plane32msb:
      DRAW_SINGLEPLANE(unsigned char, 0x80, >>=);
      break;
    case ITER_WORD|DISP_1plane32msb:
      DRAW_SINGLEPLANE(unsigned short, 0x80, >>=);
      break;
    case ITER_BYTE|DISP_1plane32lsb:
      DRAW_SINGLEPLANE(unsigned char, 0x01, <<=);
      break;
    case ITER_WORD|DISP_1plane32lsb:
      DRAW_SINGLEPLANE(unsigned short, 0x01, <<=);
      break;
    case ITER_BYTE|DISP_8plane:
      /* Quick-and-dirty drawing of 8-bit values on 8-plane displays */
      { unsigned char *datap = (unsigned char *) data;
	for(j=0; j<height; j++)
	{ unsigned char *bufp= (unsigned char *)
	    w->ms.rectbuffer->data + (w->ms.rectbuffer->bytes_per_line * j);
	  for(i=0; i<width; i++)
	  { *bufp++ = pixels[*datap++];
	  }
	}
      }
      break;
    case ITER_WORD|DISP_8plane:
      /* Quick-and-dirty drawing of 16-bit values on 8-plane displays */
      { unsigned short *datap = (unsigned short *) data;
	for(j=0; j<height; j++)
	{ unsigned char *bufp= (unsigned char *)
	    w->ms.rectbuffer->data + (w->ms.rectbuffer->bytes_per_line * j);
	  for(i=0; i<width; i++)
	  { *bufp++ = pixels[*datap++];
	  }
	}
      }
      break;
    case ITER_BYTE|DISP_generic:
      /* Slow but portable drawing of 8-bit values on 8-plane displays */
      { unsigned char *datap = (unsigned char *) data;
	for(j=0; j<height; j++)
	{ for(i=0; i<width; i++)
	    XPutPixel(w->ms.rectbuffer, i, j, pixels[*datap++]);
	}
      }
      break;
    case ITER_WORD|DISP_generic:
      /* Slow but portable drawing of 16-bit values on 8-plane displays */
      { unsigned short *datap = (unsigned short *) data;
	for(j=0; j<height; j++)
	{ for(i=0; i<width; i++)
	    XPutPixel(w->ms.rectbuffer, i, j, pixels[*datap++]);
	}
      }
      break;
    default:
      XtAppError(thisApp, "no drawing routine defined for this display type");
    }

    if(w->ms.sony_bug_workaround)
    { /* ugly workaround for a bug causing some Sony X servers to */
      /* crash in greyscale mode */
      height=32;
      width=32;
    }

    /* go do it! */
    XPutImage(XtDisplay(w), XtWindow(w), DefaultGCOfScreen(XtScreen(w)),
	      w->ms.rectbuffer, 
	      0, 0, /* position in the buffer */
	      x, y, /* position on the screen */
	      width, height);
  }
  w->ms.xi.chunks_out--; /* one less to go */

  if(w->ms.xi.chunks_out==0)
    UnhideBox(w);
}


/* Destroy this window (only) */

static void
CloseAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ XtDestroyWidget(XtParent(w)); /* destroy the shell widget */
}


/* Ask Mama to shut down */

static void 
QuitAction(w, event, params, nparams)
     MsWidget w;
     XEvent *event;
     String *params;
     Cardinal *nparams;
{ Shutdown(w->ms.mama);
}
