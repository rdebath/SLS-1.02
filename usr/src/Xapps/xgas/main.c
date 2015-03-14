/*
 * Copyright 1989 Massachusetts Institute of Technology
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
 * main.c
 *   xgas: Copyright 1990 Larry Medwin: @(#)xgas.c	1.9 2/9/90
 *   Larry Medwin -- Dec 18, 1989
 *   Dave Sternlicht -- Dec 18 1990, ported from Xw to Xaw widget set.
 *   Dave Sternlicht -- Dec 20 1990, creation of the Gas widget.
 *   Larry Medwin -- April 5, 1991, added help stuff.
 */

#include "xgas.h"
#include "xgas.icon"
#include <X11/Shell.h>

static String FallbackResources[] = {
"*lab.height: 300",
"*lab.width: 300",
"*Scrollbar.height: 300",
"*help.label: Missing app-defaults!",
NULL
};

#define Offset(field) XtOffsetOf(LabData, field)
static XtResource resources[] = {
  { "timestepSize", "TimestepSize", XtRFloat, sizeof(float),
      Offset(timestepSize), XtRString, "3.0" },
  { "delay", "Delay", XtRInt, sizeof(int),
      Offset(delay), XtRImmediate, (XtPointer) 50 },
  { "randomBounce", "RandomBounce", XtRFloat, sizeof(float),
      Offset(randomBounce), XtRString, "0.1" },
  { "equilibrium", "Equilibrium", XtRFloat, sizeof(float),
      Offset(equilibrium), XtRString, "0.5" },
  { "maxMolecules", "MaxMolecules", XtRInt, sizeof(int),
      Offset(maxMolecules), XtRImmediate, (XtPointer) 100 },
  { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
      Offset(foreground), XtRString, XtDefaultForeground },
  { XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
      Offset(background), XtRString, XtDefaultBackground },
};
#undef Offset

/*
 *  Global array describing wall and corner geometry.
 *    wallReflect, toRotate, fromRotate
 *
 * Rotation Matrix is:
 *	[ cos theta	- sin theta ]
 *	[ sin theta	cos theta   ]
 */
WallType WallParam[] =	{
  /* Walls: */
  { 	{1,-1},  {{0,1},{-1,0}},  {{0,-1},{1,0}}},	/* TOP */
  {     {-1,1},  {{-1,0},{0,-1}},  {{-1,0},{0,-1}}},	/* RIGHT */
  { 	{1,-1},  {{0,-1},{1,0}},  {{0,1},{-1,0}}},	/* BOTTOM */
  { 	{-1,1},	 {{1,0},{0,1}},  {{1,0},{0,1}}},	/* LEFT */
  /* Corners: */
  { 	{-1,-1}, {{1,0},{0,1}},  {{1,0},{0,1}}},	/* NW */
  { 	{-1,-1}, {{0,-1},{1,0}},  {{0,1},{-1,0}}},	/* SW */
  { 	{-1,-1}, {{-1,0},{0,-1}},  {{-1,0},{0,-1}}},	/* SE */
  { 	{-1,-1}, {{0,1},{-1,0}},  {{0,-1},{1,0}}},	/* NE */
};

main( argc, argv )
     int argc;
     char *argv[];
{
  XtAppContext app;
  Widget toplevel;
  Widget frame;
  Widget run, pause, step, quit;	/* Push Buttons */
  Widget lab;
  Widget help;
  LabData labData;
  Arg wargs[3];
  int i;
  Pixmap icon;
  
  /* TOPLEVEL */
  toplevel = XtAppInitialize(&app, "XGas", NULL, 0, &argc, argv,
			     FallbackResources, NULL, (Cardinal)0);
  
  /* Get Resources */
  XtGetApplicationResources( toplevel, (XtPointer) &labData, resources,
			    XtNumber( resources), (ArgList)NULL, 0);
  
  /* Allocate dynamic arrays, now that we have maxMolecules */
  labData.molecules
    = (Molecule*) malloc( (unsigned)labData.maxMolecules
			 *sizeof(Molecule));
  labData.allPos
    = (XRectangle*) malloc((unsigned)labData.maxMolecules
			   *2*sizeof(XRectangle));
  
  /* FRAMEWORK */
  frame = XtCreateManagedWidget("frame", formWidgetClass,
				toplevel, NULL, 0);
  
  /* QUIT BUTTON */
  quit = XtVaCreateManagedWidget("quit", commandWidgetClass, frame,
				 NULL);
  XtAddCallback(quit, XtNcallback, quit_callback, (XtPointer)NULL);
  
  /* RUN BUTTON */
  run = XtVaCreateManagedWidget("run", toggleWidgetClass, frame, 
			        XtNfromHoriz, (XtPointer)quit,
			       XtNhorizDistance, 50,
				NULL);
  XtAddCallback(run, XtNcallback, run_callback, (XtPointer)&labData);
  
  /* PAUSE BUTTON */
  pause = XtVaCreateManagedWidget("pause", toggleWidgetClass, frame, 
				  XtNfromHoriz, (XtPointer)run,
				  XtNradioGroup, (XtPointer)run,
				  NULL);
  XtAddCallback(pause, XtNcallback, pause_callback, (XtPointer)&labData);
  
  /* STEP BUTTON */
  step = XtVaCreateManagedWidget("step", commandWidgetClass, frame,
				 XtNfromHoriz, (XtPointer)pause,
				 NULL);
  XtAddCallback(step, XtNcallback, oneTimestep, (XtPointer)&labData);
  
  /* HELP BUTTON */
  help = XtVaCreateManagedWidget("help", commandWidgetClass, frame,
				XtNfromHoriz, (XtPointer)step,
			       XtNhorizDistance, 50,
			       NULL);
  createHelpWidgets( help );
  
  /* TEMP CONTROL and TEMP DISPLAY */
  labData.chamber[0].control = 
    XtVaCreateManagedWidget("tempControl0", scrollbarWidgetClass, frame, 
			    XtNfromVert, (XtPointer)help,
			    NULL);

  /* LAB */
  lab = XtVaCreateManagedWidget("lab", gasWidgetClass, frame,
			   XtNfromHoriz, (XtPointer)labData.chamber[0].control,
			   XtNfromVert, (XtPointer)help,
			   NULL);
  XtAddCallback(lab, XtNresize, labResize, (XtPointer)&labData);
  
  labData.chamber[1].control = 
    XtVaCreateManagedWidget("tempControl1", scrollbarWidgetClass, frame, 
			    XtNfromHoriz, (XtPointer)lab,
			    XtNfromVert, (XtPointer)help,
			    NULL);

  XtAddCallback( labData.chamber[0].control, XtNjumpProc, 
		changeTemp, (XtPointer)&labData.chamber[0]);
  XtAddCallback( labData.chamber[1].control, XtNjumpProc, 
		changeTemp, (XtPointer)&labData.chamber[1]);


  labData.chamber[0].display = 
    XtVaCreateManagedWidget("tempDisplay0", labelWidgetClass, frame,
			    XtNfromVert, (XtPointer)labData.chamber[0].control,
			    XtNlabel, (XtPointer)" 300.0K ",
			    NULL);

  /* CLOCK DISPLAY */
  labData.clock = XtVaCreateManagedWidget("clock", labelWidgetClass, frame,
			   XtNfromHoriz, (XtPointer)labData.chamber[0].display,
			   XtNfromVert, (XtPointer)lab,
			   XtNlabel, (XtPointer)" 0.000 msec ",
			   NULL);
  
  labData.chamber[1].display = 
    XtVaCreateManagedWidget("tempDisplay1", labelWidgetClass, frame, 
			    XtNfromHoriz, (XtPointer)labData.clock,
			    XtNfromVert, (XtPointer)labData.chamber[1].control,
			    XtNlabel, " 300.0 K ",
			    NULL);

  /* Start things up. */
  
  /*   Need to create GC's before adding callbacks */
  labInit( lab, &labData);
  XtAddEventHandler(lab, ExposureMask, False, labExpose, (XtPointer)&labData);


  /* resize is handled through the resize callback in the gas widget. */
  XtCallCallbacks(lab, XtNresize, (XtPointer)&labData);

  XtAddEventHandler( lab, ButtonPressMask, FALSE,
		    addMolecules, (XtPointer)&labData);
  
  XtRealizeWidget( toplevel);

  /* initialize the scrollbar positions */
  for (i=0; i<2; i++)
    XawScrollbarSetThumb(labData.chamber[i].control, 
			 (float)INITTEMP / (float)MAXTEMP, -1.0);  

  /* Tell wm about icon */
  icon = XCreateBitmapFromData( XtDisplay(frame),
	       XtWindow(frame), (char *)xgas_bits, xgas_width, xgas_height);
  XtSetArg( wargs[0], XtNiconPixmap, icon);
  XtSetValues( toplevel, wargs, 1);
	   
  /* Figure out lab dimensions, create walls */

  /* Initialize temperature */
  XtCallCallbacks( labData.chamber[0].control, XtNscrollProc, (XtPointer)300);
  XtCallCallbacks( labData.chamber[1].control, XtNscrollProc, (XtPointer)300);
  
  XtAppMainLoop(app);
}
