/* menu.c - popup menu using the X11R4 SimpleMenu widget */

/* (This needs Xaw and it won't work with X11R3) */
   
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

#include <X11/IntrinsicP.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>

#include <X11/Xaw/Cardinals.h>

#include "MsP.h"	/* this is really part of the Ms widget */
#include "Mama.h"

extern Screen *myScreen;

/* menu choise callbacks */
void
  ZoomPopChoise(), ZoomNopopChoise(), ZoomOutPopChoise(),
  ZoomOutNopopChoise(), 
  JuliaNopopChoise(), JuliaPopChoise(), WindowStatsChoise(),
  SlaveStatisticsChoise(), CloseChoise(), 
  QuitChoise();

char msDefaultTranslations[] = 
    "<Btn1Down>:	BeginBox()		\n\
     <Btn1Motion>:	StretchBox()		\n\
     <Btn1Up>:		EndBox()		\n\
     Shift<Btn2Down>:	Zoom(nopop,nojulia,in)	\n\
     <Btn2Down>:	Zoom(popup,nojulia,in)	\n\
     <Btn3Down>:	XawPositionSimpleMenu(menu) MenuPopup(menu) \n\
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


void MsCreateMenu(w, julia_enabled)
     MsWidget w;
     int julia_enabled;
{ Arg arglist[10];
  int num_args=0;
 
  XtSetArg(arglist[num_args], XtNlabel, "MandelSpawn menu");
  num_args++;
#if 0  
  XtSetArg(arglist[num_args], XtNcolormap, DefaultColormapOfScreen(myScreen));
  num_args++;
#endif
  
  w->ms.menu = XtCreatePopupShell("menu", simpleMenuWidgetClass, w, 
    arglist, num_args);

  XtAddCallback(
    XtCreateManagedWidget("Zoom",
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, ZoomPopChoise, (caddr_t)w);

#ifndef POPUP_ONLY
  XtAddCallback(
    XtCreateManagedWidget("Zoom (old window)", 
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, ZoomNopopChoise, (caddr_t)w);
#endif

  XtAddCallback(
    XtCreateManagedWidget("Inverse zoom",
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, ZoomOutPopChoise, (caddr_t)w);

#ifndef POPUP_ONLY
  XtAddCallback(
    XtCreateManagedWidget("Inverse zoom (old window)", 
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, ZoomOutNopopChoise, (caddr_t)w);
#endif
  
  if(julia_enabled)
  {
    XtAddCallback(
      XtCreateManagedWidget("Julia set",
        smeBSBObjectClass, w->ms.menu, NULL, ZERO),
      XtNcallback, JuliaPopChoise, (caddr_t)w);
#ifndef POPUP_ONLY
    XtAddCallback(
      XtCreateManagedWidget("Julia set (old window)", 
	smeBSBObjectClass, w->ms.menu, NULL, ZERO),
      XtNcallback, JuliaNopopChoise, (caddr_t)w);
#endif
  }
  XtAddCallback(
    XtCreateManagedWidget("Performance statistics", 
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, SlaveStatisticsChoise, (caddr_t)w);
  XtAddCallback(
    XtCreateManagedWidget("Show view coordinates", 
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, WindowStatsChoise, (caddr_t)w);
  XtAddCallback(
    XtCreateManagedWidget("Close this window", 
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, CloseChoise, (caddr_t)w);
  XtAddCallback(
    XtCreateManagedWidget("Quit MandelSpawn", 
      smeBSBObjectClass, w->ms.menu, NULL, ZERO),
    XtNcallback, QuitChoise, (caddr_t)w);
}

void ZoomNopopChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  ZoomIn(w, 0, 0, 0);
}

void ZoomPopChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  ZoomIn(w, 1, 0, 0);
}

void ZoomOutNopopChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  ZoomIn(w, 0, 0, 1);
}

void ZoomOutPopChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  ZoomIn(w, 1, 0, 1);
}

void JuliaNopopChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  ZoomIn(w, 0, 1, 0);
}

void JuliaPopChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  ZoomIn(w, 1, 1, 0);
}


void 
WindowStatsChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  WindowStats(w);
}

void 
SlaveStatisticsChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  SlaveStatistics(w->ms.mama);
}


/* Destroy this window (only) */
void 
CloseChoise(wx, client_data)
     Widget wx;
     caddr_t client_data;
{ MsWidget w=(MsWidget)client_data;
  XtDestroyWidget(XtParent(w)); /* destroy the shell widget */
}


/* Ask Mama to shut down */
void 
QuitChoise(w, client_data)
     Widget w;
     caddr_t client_data;
{ Shutdown(((MsWidget)client_data)->ms.mama);
}
