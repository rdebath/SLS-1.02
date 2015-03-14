/* Mama.c - MandelSpawn master widget */

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

/* The Mama widget is never realized.  Its purpose is to manage */
/* resources common to all the Ms windows, such as the colormap and */
/* the computation servers.  Now that the computation server interface */
/* has been modularized, Mama has much less to do than before. */

/* On some systems, <fcntl.h> may need to be included also */
#include <stdio.h>
#include <errno.h>
extern int errno; /* at least Sony's errno.h misses this */
#include <math.h>

#include <X11/IntrinsicP.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include "backward.h" /* X11R2 backward compatibility stuff */

#include "MamaP.h"
#include "Ms.h" /* public header of the child widget */


/* default timeout in milliseconds per iteration, and constant factor */
#define TIMEOUT_PER_ITER 	40	/* this makes 10 s for 250 iters */
#define TIMEOUT_CONST		3000	/* add 3 seconds for network delays */


/* On the Mach/i386 machines I have tested, floor(0.26) returns -0.5.  Not */
/* good.  Here is a nonportable workaround that's good enough for our */
/* purposes. */
#ifdef BROKEN_FLOOR
#define floor(x) ((double)(long)(x))
#endif

extern XtAppContext thisApp;
extern Display *myDisplay;
extern Screen *myScreen;

static void Initialize(), ClassInitialize(), Realize(), Destroy(), ReExpose(),
  Resize(), TimeoutCallback();

static Boolean SetValues();

/* Defaults */

/* the width/height defaults are never really used */
static Dimension default_width = 400;
static Dimension default_height = 250;
static int default_colors = 250;
static int default_hues = 250;
static char default_spectrum[] = 
 "blue-aquamarine-cyan-medium sea green-forest green-lime green-\
yellow green-yellow-coral-pink-black";
static Bool default_bw = False;
static Bool default_wrap = False;


static XtResource resources[] = 
{
  /* Core resources */
  { XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
      XtOffset(Widget, core.width), XtRDimension, 
      (caddr_t) &default_width },
  { XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
      XtOffset(Widget, core.height), XtRDimension, 
      (caddr_t) &default_height },
  /* Noncore resources */
  { XtNSpectrum, XtCString, XtRString, sizeof(String),
      XtOffset(MamaWidget, mama.spectrum), XtRString,
      default_spectrum },
  { XtNColours, XtCValue, XtRInt, sizeof(int),
      XtOffset(MamaWidget, mama.n_colours), XtRInt,
      (caddr_t) &default_colors },
  { XtNHues, XtCValue, XtRInt, sizeof(int),
      XtOffset(MamaWidget, mama.n_hues), XtRInt,
      (caddr_t) &default_hues },
  { XtNBw, XtCValue, XtRBool, sizeof(Bool),
      XtOffset(MamaWidget, mama.bw), XtRBool,
      (caddr_t) &default_bw },
  { XtNWrap, XtCValue, XtRBool, sizeof(Bool),
      XtOffset(MamaWidget, mama.wrap), XtRBool,
      (caddr_t) &default_wrap }
};


MamaClassRec mamaClassRec = 
{   /* core fields */
    { 
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"Mama",
    /* widget_size		*/	sizeof(MamaRec),
    /* class_initialize		*/      NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	NULL,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	NULL,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	NULL,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	NULL
    },
    /* noncore fields */
    {
    /* dummy			*/	0
    }
};

WidgetClass mamaWidgetClass = (WidgetClass) &mamaClassRec;


/* Set up the colour map */

static void 
  MsColourSetup(w)
MamaWidget w;
{ unsigned int i;
  unsigned int ncolours;
  unsigned int nhues;
  unsigned int nstops;
  unsigned long *phpixels;
  unsigned long *pixels;
  XColor *stops;
  XColor *colourset;
  unsigned stops_allocated=4; /* initial allocation of colour stops */
  char *p;
  stops=(XColor *) XtMalloc(stops_allocated * sizeof(XColor));
  /* parse the "spectrum" resource */
  p=w->mama.spectrum;
  i=0;
  while(1)
  { char *end;
    XColor dummy;
    end=index(p, '-');
    if(end) *end = '\0';
    if(i >= stops_allocated)
    { stops_allocated *= 2;
      stops=(XColor *)
	XtRealloc((char *) stops, stops_allocated * sizeof(XColor));
    }
    if(! XLookupColor(myDisplay, DefaultColormapOfScreen(myScreen), p,
		     &stops[i], &dummy))
    { static char *err = "unrecognized colour in spectrum: ";
      char *msg = XtMalloc(sizeof(err)+strlen(p));
      strcpy(msg, err);
      strcat(msg, p);
      XtAppError(thisApp, msg);
      XtFree(msg); /*NOTREACHED*/
    }
    i++;
    if(end) p=end+1; 
    else break;
  }
  nstops=i;
  ncolours=w->mama.n_colours;
  nhues=w->mama.n_hues;

  /* physical pixel mapping (from colour number to pixel value, */
  /* "ncolours" entries) */
  phpixels=
    (unsigned long *) XtMalloc(ncolours * sizeof(unsigned long));
  
  /* logical pixel mapping (from iteration count to pixel value, */
  /* "nhues" entries) */
  pixels=
    (unsigned long *) XtMalloc(nhues * sizeof(unsigned long));

  w->mama.my_colormap = DefaultColormapOfScreen(myScreen);
  
  if(! XAllocColorCells(myDisplay, w->mama.my_colormap,
		       False, NULL, 0,  
		       phpixels, ncolours))
  { /* allocation our of the default colormap failed, so we */
    /* try to allocate one of our own */
    XColor dummy;
    w->mama.my_colormap =
      XCopyColormapAndFree(myDisplay, w->mama.my_colormap);
    if(! w->mama.my_colormap)
      XtAppError(thisApp, "XCopyColormapAndFree failed");
#ifdef MENU
    /* make sure that this colourmap also has black and white pixels */
    /* for use by the popup menu, if any.  Don't care about errors here. */
    (void) XAllocNamedColor(myDisplay, w->mama.my_colormap,
			    "white", &dummy, &dummy);
    (void) XAllocNamedColor(myDisplay, w->mama.my_colormap,
			    "black", &dummy, &dummy);
#endif
    /* retry the allocation */
    if(! XAllocColorCells(myDisplay, w->mama.my_colormap,
			  False, NULL, 0,  
			  phpixels, ncolours))
    { XtAppError(thisApp,
"too many colours for this screen.  Please specify a smaller number\n\
of colours with the -colours option, or use -bw to run in black and\n\
white.");
    }
  }

  colourset=(XColor *) XtMalloc(ncolours * sizeof(XColor));
  for(i=0; i<ncolours; i++)
  { double flstop, stop, fpart;
    int istop, jstop;
    flstop=(double) i / (double) (ncolours-1) * (double) (nstops-1);
    /* calculate the number of the next lower stop */
    /* should be ==nstops-1 only for i==ncolours-1 */
    stop=floor(flstop);
    fpart=flstop-stop;
    /* calculate the indices of the two stops to interpolate between */
    /* If we are at the last stop, interpolate between it and itself */
    /* to avoid accesses past the end of the array (the value will be */
    /* multiplied by zero anyway) */
    istop=(int) stop;
    jstop=(istop+1 >= nstops ? nstops-1 : istop+1);

    colourset[i].pixel=phpixels[i];
    colourset[i].red=
      floor((1-fpart)*stops[istop].red + fpart*stops[jstop].red);
    colourset[i].green=
      floor((1-fpart)*stops[istop].green + fpart*stops[jstop].green);
    colourset[i].blue=
      floor((1-fpart)*stops[istop].blue + fpart*stops[jstop].blue);
    colourset[i].flags=DoRed|DoGreen|DoBlue;
  }

  if(w->mama.wrap)
  { /* Wrap the spectrum around several times if needed to get the */
    /* desired number of "hues" from a limited number of "colours" */
    for(i=0; i<nhues-1; i++)
    { pixels[i] = phpixels[i % ncolours];
    }
  }
  else
  { /* Don't wrap; give the same colour to several consecutive iteration */
    /* values */
    int divfactor = (nhues + ncolours-1) / ncolours;
    for(i=0; i<nhues-1; i++)
    { pixels[i] = phpixels[i / divfactor];
    }
  }
  /* ...but make sure the last colour is what the user specified. */
  pixels[nhues-1] = phpixels[ncolours-1];
  w->mama.pixels=pixels;
  
  XStoreColors(myDisplay, w->mama.my_colormap, 
	       colourset, ncolours);
  XtFree((char *) colourset);
}


/* Set up for black-and-white operation */

static void 
  MsBwSetup(w)
MamaWidget w;
{ unsigned long *pixels;

  unsigned niter = w->mama.n_hues;
  register int i;
  
  pixels=w->mama.pixels=
    (unsigned long *) XtMalloc(niter * sizeof(unsigned long));

  /* Use alternating black/white bands */
  for(i=0; i<niter; i++)
  { pixels[i] = (niter - i) & 0x01 ?
      BlackPixelOfScreen(myScreen) : WhitePixelOfScreen(myScreen);
  }
}


/* Callback function to be called when data arrives from a slave */

void MsSocketInputCallback(client_data, source, id)
     caddr_t client_data; int *source; XtInputId *id;
{ MamaWidget w=(MamaWidget) client_data;
  if(*id != w->mama.input_id || *source != wf_socket(w->mama.workforce)) 
  { XtAppError(thisApp, "unexpected input from slave");
  }
  /* this will receive the message and call GotResult with it */
  wf_handle_socket_input(w->mama.workforce, (char *) w);
}


/* Initialize the widget */
static void
Initialize(request, new)
     MamaWidget request;
     MamaWidget new;
{ /* we haven't made any popups yet */
  new->mama.n_popups_created=0;

  /* initialize the workforce */
  new->mama.workforce =
    wf_init(new->mama.n_hues * TIMEOUT_PER_ITER + TIMEOUT_CONST);

  /* pass a pointer to the master widget as "client data" */
  new->mama.input_id=
    XtAppAddInput(thisApp,
		  wf_socket(new->mama.workforce),
		  (caddr_t) XtInputReadMask,
		  MsSocketInputCallback,
		  (caddr_t) new);

#ifndef OLD_TIMEOUT
  (void) XtAppAddTimeOut(thisApp, 1000,
		  TimeoutCallback, (caddr_t) new->mama.workforce);
#endif

  if(new->mama.n_colours > new->mama.n_hues)
  { /* we have more colours than iterations, throw some away */
    new->mama.n_colours = new->mama.n_hues;
  }
  if(new->mama.bw)
    goto force_bw;

  /* set new->mama.pixels and the colour map, if applicable */
  switch(DefaultVisualOfScreen(myScreen)->class)
  {
  case PseudoColor:
    MsColourSetup(new); break;
  case StaticGray:
  force_bw:
    MsBwSetup(new); break;
  default:
    XtAppError(thisApp, "unsupported visual type");
  }
}


/* Provide memory allocation and error handling services to the workforce */
/* package, in a way compatible with Xt */

char *wf_alloc(size) unsigned size; { return(XtMalloc(size)); }
char *wf_realloc(p, size) char *p; unsigned size;
  { return(XtRealloc(p, size)); }
void wf_free(p) char *p; { XtFree(p); }

void wf_error(msg) char *msg; { XtAppError(thisApp, msg); }
void wf_warn(msg) char *msg; { XtAppWarning(thisApp, msg); }


#ifndef OLD_TIMEOUT
static void 
TimeoutCallback(client_data, id) 
     caddr_t client_data;
     XtIntervalId *id;
{ wf_tick((wf_state *) client_data);
  /* reset the timeout */
  XtAppAddTimeOut(thisApp, 1000,
		  TimeoutCallback, client_data);
}

#else

/* Callback to be called whenever a slave times out */

static void 
TimeoutCallback(client_data, id) 
     caddr_t client_data;
     XtIntervalId *id;
{ wf_timed_out(client_data);
}

char *wf_add_timeout(millisecs, client_data)
  unsigned millisecs; char *client_data;
{ return((char *) XtAppAddTimeOut(thisApp, millisecs,
				  TimeoutCallback, (caddr_t) client_data));
}

void wf_remove_timeout(id) char *id;
{ XtRemoveTimeOut((XtIntervalId *) id);
}
#endif



/* Update widget resources (no changeable resources so far) */

static Boolean SetValues(current, request, new)
     MamaWidget current, request, new;
{ return(False);
}


/* Die */

void Shutdown(w)
     MamaWidget w;
{ XCloseDisplay(XtDisplay(w));
  exit(0);
}


/* This callback is called whenever a popup child dies */

void
PostMortem(w, client_data, call_data)
     MsWidget w;
     caddr_t client_data;
     caddr_t call_data;
{ MamaWidget ma=(MamaWidget) client_data;
  /* inform the slave handler that we don't want any more replies */
  wf_client_died(ma->mama.workforce, (char *) w);
  /* If our last child is dying we have nothing to live for; */
  /* commit suicide */
  if(ma->core.num_popups == 1)
    Shutdown(ma);
}


/* Pop up a new Ms window, making it a child of the Mama. */
/* The caller must make sure there is room in the "shell_args" array */
/* for one more arg. */

void PopupAnother (w, shell_args, num_shell_args, args, num_args)
     MamaWidget w;
     ArgList shell_args;
     Cardinal num_shell_args;
     ArgList args;
     Cardinal num_args;
{ Widget the_widget;
  Widget the_shell;
  char popup_name[32]; /* unique name for each popup widget */

  sprintf(popup_name, "ms_%d", ++w->mama.n_popups_created);

  /* make the popup use our colormap */
  XtSetArg(shell_args[num_shell_args], XtNcolormap, w->mama.my_colormap);
  num_shell_args++;

  /* this is for OpenWindows... */
  XtSetArg(shell_args[num_shell_args], XtNinput, True);
  num_shell_args++;

  the_shell=
    XtCreatePopupShell(popup_name, topLevelShellWidgetClass, w, 
		       shell_args, num_shell_args);
  
  the_widget=XtCreateManagedWidget("view",
				   (WidgetClass) msWidgetClass,
				   (Widget) the_shell, 
				   args, num_args);
  XtAddCallback(the_widget, XtNdestroyCallback, PostMortem, (caddr_t) w);
		
  XtPopup(the_shell, XtGrabNone);
}


/* Destroy the widget */

static void
Destroy (w)
  MamaWidget w;
{
}


/* The Ms widget needs to know the maximums message size, */
/* how may colours there are, etc.  Make that information */
/* available through public functions */

unsigned 
MaxIterations(w)
     MamaWidget w;
{ return(w->mama.n_hues);
}

unsigned MamaHeight(w)
     MamaWidget w;
{ return(w->core.height);
}

unsigned MamaWidth(w)
     MamaWidget w;
{ return(w->core.width);
}

unsigned 
MamaColormap(w)
     MamaWidget w;
{ return(w->mama.my_colormap);
}

wf_state *MamaWorkforce(w)
     MamaWidget w;
{ return(w->mama.workforce);
}

unsigned long *
MamaPixels(w)
     MamaWidget w;
{ return(w->mama.pixels);
}

/* Print some performance statistics about the slaves */

void SlaveStatistics(w)
     MamaWidget w;
{ wf_print_stats(w->mama.workforce, stdout);
}

