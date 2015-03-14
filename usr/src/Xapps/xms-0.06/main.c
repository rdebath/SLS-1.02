/* main.c - main program for MandelSpawn */

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

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xos.h>	/* just for rindex() declaration / macro */
#include <X11/StringDefs.h>

#include "backward.h"	/* X11R2 backward compatibility stuff */

#include "Mama.h"
#include "Ms.h"

#include "version.h"

static XrmOptionDescRec options[] = {
    {"-center",		 "*center_box", XrmoptionNoArg, "True" },
    {"-chunk_width",	 "*chunk_width", XrmoptionSepArg, NULL },
    {"-chunk_height",	 "*chunk_height", XrmoptionSepArg, NULL },
    {"-colors",		 "*colours",	XrmoptionSepArg, NULL }, 
    {"-colours",	 "*colours",	XrmoptionSepArg, NULL },
    {"-cursor",		 "*cursor",     XrmoptionSepArg, NULL },
    {"-geometry",	 "*ms_1.geometry", XrmoptionSepArg, NULL },
    {"-iterations",	 "*hues",	XrmoptionSepArg, NULL },
    {"-nocenter",	 "*center_box", XrmoptionNoArg, "False" },
    {"-spectrum",	 "*spectrum",	XrmoptionSepArg, NULL },
    {"-greyscale",	 "*spectrum",	XrmoptionNoArg, "white-black" },
    {"-grayscale",	 "*spectrum",	XrmoptionNoArg, "white-black" },
    {"-x",		 "*ms_1*center_x", XrmoptionSepArg, NULL },
    {"-y",		 "*ms_1*center_y", XrmoptionSepArg, NULL },
    {"-range",		 "*ms_1*range",	XrmoptionSepArg, NULL },
    {"-julia",		 "*ms_1*julia",	XrmoptionNoArg, "True" },
    {"-cx",		 "*ms_1*c_x",	XrmoptionSepArg, NULL },
    {"-cy",		 "*ms_1*c_y",	XrmoptionSepArg, NULL },
    {"-bw",		 "*bw",		XrmoptionNoArg, "True" },
    {"-wrap",		 "*wrap",	XrmoptionNoArg, "True" },
    {"-nowrap",		 "*wrap",	XrmoptionNoArg, "False" },
    {"-interior",	 "*interior",	XrmoptionNoArg, "True" },
    {"-sony",		 "*sony_bug_workaround",
       				        XrmoptionNoArg, "True" },
    {"-crosshair_size",	 "*ms_1*crosshair_size",
       		                        XrmoptionSepArg, NULL }
};


/* Ugly global state */

XtAppContext thisApp;
Display *myDisplay;
Screen *myScreen;


/* Genereate a usage message */

static void
Usage(name)
String name;
{ int i;
  fprintf(stderr, "usage: %s ", name);
  for(i=0; i<XtNumber(options); i++)
  { printf("[%s%s]%s", options[i].option,
	   options[i].argKind==XrmoptionSepArg ? " ..." : "",
	   i==XtNumber(options)-1 ? "\n" :
	     i%4==3 ? "\n\t" : " ");
  }
  exit(1);
}


#ifdef R4
/* Type converter from String to Double - why on Earth isn't */
/* this in Xt?  There is a converter from String to Float that */
/* first converts a string to a double and then goes to the */
/* additional trouble of truncating it into a float... but not */
/* one returning a double. */

#define	done(type, value) \
	{							\
	    if (toVal->addr != NULL) {				\
		if (toVal->size < sizeof(type)) {		\
		    toVal->size = sizeof(type);			\
		    return False;				\
		}						\
		*(type*)(toVal->addr) = (value);		\
	    }							\
	    else {						\
		static type static_val;				\
		static_val = (value);				\
		toVal->addr = (XtPointer)&static_val;		\
	    }							\
	    toVal->size = sizeof(type);				\
	    return True;					\
	}


static Boolean CvtStringToDouble(dpy, args, num_args,
				 fromVal, toVal, closure_ret)
     Display* dpy;
     XrmValuePtr args;
     Cardinal *num_args;
     XrmValuePtr fromVal;
     XrmValuePtr toVal;
     XtPointer	*closure_ret;
{ double d;
  double atof();
  if (*num_args != 0)
    XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		    "wrongParameters", "cvtStringToDouble", "XtToolkitError",
		    "String to Double conversion needs no extra arguments",
		    (String *) NULL, (Cardinal *) NULL);
  
  d = atof(fromVal->addr);
  done(double, d);
}
#endif


main(argc, argv)
  int argc; char *argv[];
{
    Widget toplevel;
    MamaWidget theMama;
    char *name;
    Arg arglist[12];
    int num_args;
    Arg shell_arglist[12];
    int num_shell_args;

    if (name = rindex(argv[0], '/')) name++;
    else name = argv[0];

    toplevel = XtInitialize(NULL, "Ms", 
			    options, XtNumber(options),
			    &argc, argv);
    thisApp=XtWidgetToApplicationContext(toplevel);
    myDisplay=XtDisplay(toplevel);
    myScreen=ScreenOfDisplay(myDisplay, DefaultScreen(myDisplay));

    if (argc == 2 && !strcmp(argv[1], "-version"))
      printf("MandelSpawn version %s\n", ms_version);
    else
      if (argc != 1)
	Usage(name);

#ifdef R4
    XtAppSetTypeConverter(thisApp, XtRString, XtRDouble, CvtStringToDouble,
			  NULL, 0, XtCacheAll, NULL);
#endif

    theMama=(MamaWidget) 
      XtCreateManagedWidget("mama",
			    (WidgetClass) mamaWidgetClass, 
			    (Widget) toplevel, 
			    (ArgList) NULL, 
			    0);

#ifdef MENU
    XawSimpleMenuAddGlobalActions(thisApp);
#endif

    /* we supply no arguments to the shell, but the PopupAnother */
    /* function does, so we need to pass a real buffer (not NULL) */
    num_shell_args=0;

    /* create an initial child with default settings */
    num_args=0;
    XtSetArg(arglist[num_args], XtNMama, theMama); num_args++;
    PopupAnother(theMama, shell_arglist, num_shell_args, arglist, num_args);

    XtMainLoop();
    return(0); /* keep lint happy */
}
