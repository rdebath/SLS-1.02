/*
 * Copyright 1991 John L. Cwikla
 * 
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * University of Illinois not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.  John L. Cwikla and University of Illinois make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and University of Illinois disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * University of Illinois be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 * 	John L. Cwikla
 * 	Materials Research Laboratory Center for Computation
 * 	University Of Illinois at Urbana-Champaign
 *	104 S. Goodwin
 * 	Urbana, IL 61801
 * 
 * 	cwikla@uimrl7.mrl.uiuc.edu
*/ 

#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Core.h>

#include <stdio.h>
#include <math.h>

#define APPNAME "XColormap"

#ifdef ULONG_NEEDED
typedef unsigned long ulong;
#endif

static Display *TheDisplay;
static GC TheGC;

static ulong NumColors;
static ulong Width = 10; /* Of the squares */
static ulong Height = 10; /* Of the squares */
static ulong Margin = 2; /* space between squares */
static ulong Xcolor, Ycolor; /* Number of colors per direction in the matrix */
static int TheScreenNo;

static void DrawSquare(Drawable d, int i)
{
  int row, col, x, y;

  row = i / Xcolor;
  col = i % Xcolor;

  x = (Width + Margin) * col + Margin;
  y = (Height + Margin) * row + Margin;

  XSetForeground(TheDisplay, TheGC, i);
  XFillRectangle(TheDisplay, d, TheGC, x, y,
    Width, Height);
}

static void RedrawWindow(Drawable d)
{
  int i;
  for(i=0;i<NumColors;i++)
    DrawSquare(d, i);
}

static void Redraw(Widget w)
{
  RedrawWindow(XtWindow(w));
}

static void Resize(Widget wid)
{
  Width = (wid->core.width - Margin)/Xcolor - Margin; 
  Height = (wid->core.height - Margin)/Xcolor - Margin;
  XClearWindow(TheDisplay, XtWindow(wid));
  Redraw(wid);
}

static void QuitIt()
{
  printf("Have a nice day. --JLC\n");
  exit(1);
}

main(int argc, char *argv[])
{
  Widget colorwidget, toplevel;
  XtAppContext app;
  Arg warg[2];
  int n;
  ulong xWindowWidth, yWindowHeight;

  XtToolkitInitialize();
  app = XtCreateApplicationContext();
	
  TheDisplay = XtOpenDisplay (app, NULL, APPNAME, APPNAME, 
    NULL, 0, &argc, argv);

  if (!TheDisplay)
  {
#ifdef ORIGINAL
	XtWarning ("%s: can't open display, exiting...", APPNAME);
#else
	XtWarning ("xcolormap: can't open display, exiting...");
#endif
	exit (0);
  }

  TheScreenNo = DefaultScreen(TheDisplay);
  NumColors = XDisplayCells(TheDisplay, TheScreenNo);
  Xcolor = (int)floor(sqrt(NumColors)); /* Best try at a square */
  Ycolor = NumColors/Xcolor;


  if ((argc == 2) && (!strncmp(argv[1], "-root", 5)))
  {
      Drawable TheWindow;
      Pixmap ThePixmap;
      xWindowWidth = DisplayWidth(TheDisplay, TheScreenNo);
      yWindowHeight = DisplayHeight(TheDisplay, TheScreenNo);
      Width = (xWindowWidth - Margin)/Xcolor - Margin;
      Height = (yWindowHeight - Margin)/Xcolor - Margin;
      printf("Display is %d x %d.\n", xWindowWidth, yWindowHeight);
      TheWindow = RootWindow(TheDisplay, TheScreenNo);
      ThePixmap = XCreatePixmap(TheDisplay, TheWindow, 
	xWindowWidth, yWindowHeight, DefaultDepth(TheDisplay, TheScreenNo));
      TheGC = XCreateGC(TheDisplay, TheWindow, NULL, 0);
      RedrawWindow(ThePixmap);
      XSetWindowBackgroundPixmap(TheDisplay, TheWindow, 
	ThePixmap);
      XClearWindow(TheDisplay, TheWindow);
      while(XtAppPending(app))
      {
        XEvent event;
        XtAppNextEvent(app, &event);
        XtDispatchEvent(&event);
      }
      XFreeGC(TheDisplay, TheGC);
      XFreePixmap(TheDisplay, ThePixmap);
      exit(0);
  }

  colorwidget = XtAppCreateShell (APPNAME, APPNAME,
		applicationShellWidgetClass, TheDisplay, NULL, 0);
  xWindowWidth = Xcolor * (Width + Margin) + Margin; /* Actual window width */
  yWindowHeight = Ycolor * (Height + Margin) + Margin; /* Actual window height */

  n = 0;
  XtSetArg(warg[n], XtNwidth, xWindowWidth); n++;
  XtSetArg(warg[n], XtNheight, yWindowHeight); n++;
  toplevel = XtCreateManagedWidget("TopLevel", widgetClass,
	colorwidget, warg, n);

  XtRealizeWidget(colorwidget);
  XtAddEventHandler(toplevel, ButtonPressMask, FALSE, QuitIt, NULL);
  XtAddEventHandler(toplevel, ExposureMask, FALSE, Redraw, NULL);
  XtAddEventHandler(toplevel, StructureNotifyMask, FALSE, Resize, NULL);


  TheGC = XCreateGC(TheDisplay, XtWindow(toplevel), NULL, 0);

  XtAppMainLoop(app);
}
