/*
 * xload - display system load average in a window
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * $XConsortium: xload.c,v 1.36 91/05/24 16:57:46 converse Exp $
 */

#include <stdio.h> 
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xmu/SysUtil.h>

#include "xload.bit"

char *ProgramName;

extern void exit(), GetLoadPoint();
static void quit();
static void ClearLights();
static void SetLights();

/*
 * Definition of the Application resources structure.
 */

typedef struct _XLoadResources {
  Boolean show_label;
  Boolean use_lights;
} XLoadResources;

/*
 * Command line options table.  Only resources are entered here...there is a
 * pass over the remaining options after XtParseCommand is let loose. 
 */

static XrmOptionDescRec options[] = {
 {"-scale",		"*load.minScale",	XrmoptionSepArg,	NULL},
 {"-update",		"*load.update",		XrmoptionSepArg,	NULL},
 {"-hl",		"*load.highlight",	XrmoptionSepArg,	NULL},
 {"-highlight",		"*load.highlight",	XrmoptionSepArg,	NULL},
 {"-label",		"*label.label",		XrmoptionSepArg,	NULL},
 {"-nolabel",		"*showLabel",	        XrmoptionNoArg,       "False"},
 {"-lights",		"*useLights",		XrmoptionNoArg,	      "True"},
 {"-jumpscroll",	"*load.jumpScroll",	XrmoptionSepArg,	NULL},
};

/*
 * The structure containing the resource information for the
 * Xload application resources.
 */

#define Offset(field) (XtOffsetOf(XLoadResources, field))

static XtResource my_resources[] = {
  {"showLabel", XtCBoolean, XtRBoolean, sizeof(Boolean),
     Offset(show_label), XtRImmediate, (XtPointer) TRUE},
  {"useLights", XtCBoolean, XtRBoolean, sizeof(Boolean),
    Offset(use_lights), XtRImmediate, (XtPointer) FALSE},
};

#undef Offset

static XLoadResources resources;

static XtActionsRec xload_actions[] = {
    { "quit",	quit },
};
static Atom wm_delete_window;
static int light_update = 10 * 1000;

/*
 * Exit with message describing command line format.
 */

void usage()
{
    fprintf (stderr, "usage:  %s [-options ...]\n\n", ProgramName);
    fprintf (stderr, "where options include:\n");
    fprintf (stderr,
      "    -display dpy            X server on which to display\n");
    fprintf (stderr,
      "    -geometry geom          size and location of window\n");
    fprintf (stderr, 
      "    -fn font                font to use in label\n");
    fprintf (stderr, 
      "    -scale number           minimum number of scale lines\n");
    fprintf (stderr, 
      "    -update seconds         interval between updates\n");
    fprintf (stderr,
      "    -label string           annotation text\n");
    fprintf (stderr, 
      "    -bg color               background color\n");
    fprintf (stderr, 
      "    -fg color               graph color\n");
    fprintf (stderr, 
      "    -hl color               scale and text color\n");
    fprintf (stderr, 
      "    -nolabel                removes the label from above the chart.\n");
    fprintf (stderr, 
      "    -jumpscroll value       number of pixels to scroll on overflow\n");
    fprintf (stderr, "\n");
    exit(1);
}

void main(argc, argv)
    int argc;
    char **argv;
{
    XtAppContext app_con;
    Widget toplevel, load, pane, label_wid, load_parent;
    Arg args[1];
    Pixmap icon_pixmap = None;
    char *label, host[256];

    ProgramName = argv[0];

    /* For security reasons, we reset our uid/gid after doing the necessary
       system initialization and before calling any X routines. */
    InitLoadPoint();
    setgid(getgid());		/* reset gid first while still (maybe) root */
    setuid(getuid());

    toplevel = XtAppInitialize(&app_con, "XLoad", options, XtNumber(options),
			       &argc, argv, NULL, NULL, (Cardinal) 0);
    if (argc != 1) usage();

    XtGetApplicationResources( toplevel, (XtPointer) &resources, 
			      my_resources, XtNumber(my_resources),
			      NULL, (Cardinal) 0);
    
    if (resources.use_lights)
    {
	char	    name[1024];
	XrmString   type;
	XrmValue    db_value;
	XrmValue    int_value;
	Bool	    found = False;

	(void) sprintf (name, "%s.paned.load.update", XtName(toplevel));
	found = XrmGetResource (XtScreenDatabase(XtScreen(toplevel)),
				name, "XLoad.Paned.StripChart.Interval",
				&type, &db_value);
	if (found) {
	    int_value.size = sizeof(int);
	    int_value.addr = (XPointer) &light_update;
	    found = XtConvertAndStore(toplevel, type, &db_value, XtRInt,
				      &int_value);
	    if (found) light_update *= 1000;
	}
	ClearLights (XtDisplay (toplevel));
	SetLights ((XtPointer) toplevel, (XtIntervalId *) 0);
    }
    else
    {
    	/*
     	 * This is a hack so that f.delete will do something useful in this
     	 * single-window application.
     	 */
    	XtAppAddActions (app_con, xload_actions, XtNumber(xload_actions));
    	XtOverrideTranslations(toplevel,
		    	XtParseTranslationTable ("<Message>WM_PROTOCOLS: quit()"));
    
    	XtSetArg (args[0], XtNiconPixmap, &icon_pixmap);
    	XtGetValues(toplevel, args, ONE);
    	if (icon_pixmap == None) {
	    XtSetArg(args[0], XtNiconPixmap, 
		     XCreateBitmapFromData(XtDisplay(toplevel),
				       	   XtScreen(toplevel)->root,
				       	   (char *)xload_bits,
				       	   xload_width, xload_height));
	    XtSetValues (toplevel, args, ONE);
    	}
    
    	if (resources.show_label) {
      	  pane = XtCreateManagedWidget ("paned", panedWidgetClass,
				    	toplevel, NULL, ZERO);
    
      	  label_wid = XtCreateManagedWidget ("label", labelWidgetClass,
					     pane, NULL, ZERO);
      	  
      	  XtSetArg (args[0], XtNlabel, &label);
      	  XtGetValues(label_wid, args, ONE);
      	  
      	  if ( strcmp("label", label) == 0 ) {
	    (void) XmuGetHostname (host, 255);
	    XtSetArg (args[0], XtNlabel, host);
	    XtSetValues (label_wid, args, ONE);
      	  }
    
      	  load_parent = pane;
    	}
    	else
      	  load_parent = toplevel;
    
    	load = XtCreateManagedWidget ("load", stripChartWidgetClass,
				      load_parent, NULL, ZERO);    
    
    	XtAddCallback(load, XtNgetValue, GetLoadPoint, NULL);
    
    	XtRealizeWidget (toplevel);
    	wm_delete_window = XInternAtom (XtDisplay(toplevel), "WM_DELETE_WINDOW",
				    	False);
    	(void) XSetWMProtocols (XtDisplay(toplevel), XtWindow(toplevel),
			    	&wm_delete_window, 1);
    }
    XtAppMainLoop(app_con);
}

static unsigned long	current_leds;

static void
ClearLights (dpy)
    Display *dpy;
{
    XKeyboardControl	cntrl;

    cntrl.led_mode = LedModeOff;
    XChangeKeyboardControl (dpy, KBLedMode, &cntrl);
    current_leds = 0;
}

static void
SetLights (data, timer)
    XtPointer	    data;
    XtIntervalId    *timer;
{
    Widget		toplevel;
    Display		*dpy;
    double		value;
    unsigned long	new_leds, change, bit;
    int			i;
    XKeyboardControl	cntrl;

    toplevel = (Widget) data;
    dpy = XtDisplay (toplevel);
    GetLoadPoint (toplevel, (XtPointer) 0, (XtPointer) &value);
    new_leds = (1 << (int) (value + 0.1)) - 1;
    change = new_leds ^ current_leds;
    i = 1;
    bit = 1;
    while (current_leds != new_leds)
    {
	if (change & bit)
	{
	    cntrl.led = i;
	    cntrl.led_mode = new_leds & bit ? LedModeOn : LedModeOff;
	    XChangeKeyboardControl (dpy, KBLed|KBLedMode, &cntrl);
	    current_leds ^= bit;
	}
	i++;
	bit <<= 1;
    }
    XtAppAddTimeOut(XtWidgetToApplicationContext(toplevel), light_update,
		    SetLights, data);
}

static void quit (w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (event->type == ClientMessage &&
        event->xclient.data.l[0] != wm_delete_window) {
        XBell (XtDisplay(w), 0);
        return;
    }
    if (resources.use_lights)
	ClearLights (XtDisplay (w));
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    exit (0);
}
