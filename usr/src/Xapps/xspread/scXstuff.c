
/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations. Selling this software for profit without
 * the permission of the Board of Regents of the University
 * of Wisconsin is strictly forbidden. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */

/**********************************************************************
/*                                                                    *
/*      Error rountine modified by Mike Frey and Jim Cornelius.       *       
/*                                              Fall, 1991            *
/**********************************************************************
/* scXstuff.c
This file contains the code for initializing and keeping track of basic
X-Windows information such as fonts, the size of the window, etc.  Functions
and macros are explained as they are presented.  */

/* REVISION HISTORY */
/* 7-19-91  B. Backman    Creation */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>  /*define FILE and NULL */
#include "sc.h"

unsigned long white, black;     /* white and black pixels */
unsigned long bw;         	/* border width */
XGCValues gcv;         	  	/* struct for creating GC */
XSizeHints   xsh;		/* size hints for window manager */
XWMHints wmhints;
XSetWindowAttributes xswa;	/* Temporary Set Window Attribute struct */
XWindowAttributes xwa;	        /* Temporary Window Attribute struct */

/* The following variables are declared in scXstuff.h */
Display    *dpy;            /* X server (workstation) connection */
Window     mainwin;         /* resource ID of main window */
GC         maingc,          /* GC for mainwin */
	   maingcreversed;  /* Reverse-field GC for mainwin */
XFontStruct *curfont;       /* Font descriptor struct for current font */
Font       curfontid;       /* resource id of current font */
int        curfontwidth,
	   curfontheight;   /* pixel dimensions of current font */
int        maintextrows,    /* text rows in main window */
	   maintextcols;    /* text cols in main window */

extern int seenerr;	    /* 1 if an error has just been displayed, 0 otherwise */

/* macros textrow() and textcol() compute the y-coordinate of the bottom of row
   r and the x-coordinate of the left-hand side of column c, respectively. 
   This is for use with XDrawImageString.  The coordinates are based on 
   curfontheight and curfontwidth.  NOTE: textcol() will only work for a 
   fixed-width font! Otherwise, it doesn't make sense to calculate column 
   positions anyway because they change */
#define textrow(r)  ((((r)+1)*curfontheight)-1)
#define textcol(c)  ((c)*curfontwidth)


/***
 function error() used to be a macro, but the X call is more complicated than 
 the curses version was 
***/
void error(errstring)
  char *errstring;
{
  clearlines(1,1);
  fprintf(stderr,"\007");
  XDrawImageString(dpy,mainwin,maingc,
		   textcol(0), textrow(1),
		   errstring, strlen(errstring));
  XFlush(dpy);
  seenerr = 1; 
}


/***
the function usefont() takes a font structure as an argument
and sets the global variables curfont, curfontid, curfontheignt, and
Curfontwidth to the values appropriate values for the specified font. 
***/
usefont(fontinfo)
  XFontStruct *fontinfo;
{
  curfont = fontinfo;
  curfontid = fontinfo->fid;
  curfontheight = fontinfo->max_bounds.ascent + fontinfo->max_bounds.descent;
  curfontwidth = fontinfo->max_bounds.width;
} /* end of usefont() */


/***
function sc_Xinit() initializes all of the global variables defined in 
this file.  argc and argv are used to set some of the window parameters,
if any X parameters were given on the command line.
***/
sc_Xinit(argc, argv)
  int    argc;
  char **argv;
{
  /* open the display, using the DISPLAY env. variable as default */
  if ((dpy = XOpenDisplay(NULL)) == NULL)
  {
    fprintf(stderr, "%s: Can't open display %s\n",argv[0], XDisplayName(NULL));
    exit(1);
  }

  /* load the font to use */
  if ((curfont=XLoadQueryFont(dpy,SC_FONT))==NULL)
  {
    fprintf(stderr, "%s: Display %s doesn't know font \"%s\" ",
	    argv[0], DisplayString(dpy), SC_FONT);
    exit(1);
  }
  /* initialize the font-related globals */
  usefont(curfont);

  /* initialize pixel values  */
  black = BlackPixel(dpy, DefaultScreen(dpy)); /* border */
  white = WhitePixel(dpy, DefaultScreen(dpy)); /* background */

  /* border width of 1 */
  bw = 1;

  /* fill in the window manager hints */
  xsh.flags = ( PMinSize | PResizeInc | PPosition );
  xsh.min_width= (MIN_COLS*curfontwidth);
  xsh.width = (MIN_COLS * curfontwidth);
  xsh.min_height = xsh.height = ((MIN_ROWS + 1) * curfontheight);
  xsh.width_inc = curfontwidth;
  xsh.height_inc = curfontheight;
  xsh.x = xsh.y = 0; 
  wmhints.flags = InputHint;
  wmhints.input = True;

  /* create the main window and give the hints to the window manager */
  mainwin = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy),
 				xsh.x, xsh.y, xsh.min_width, xsh.min_height,
				bw, black, white);
  XSetStandardProperties(dpy,mainwin,SC_TITLE, SC_TITLE, None, argv,argc,&xsh);

  XSetWMHints(dpy, mainwin, &wmhints);

  /* Insure that the window's colormap points to the default colormap, and
     set the window's bit gravity to NorthWest, because that is the origin 
     of everything in the window */
  xswa.colormap = DefaultColormap(dpy,DefaultScreen(dpy));
  XChangeWindowAttributes(dpy,mainwin, CWColormap, &xswa);

  /* create the normal Graphics Context */
  maingc = XCreateGC(dpy,mainwin, 0,0); /* create default GC */
  XSetFont(dpy, maingc, curfontid);
  XSetForeground(dpy, maingc, black);
  XSetBackground(dpy, maingc, white);
  /* and the reversed GC */
  maingcreversed = XCreateGC(dpy,mainwin, 0,0);
  XSetFont(dpy, maingcreversed, curfontid);
  XSetForeground(dpy, maingcreversed, white);
  XSetBackground(dpy, maingcreversed, black);

  /* input event selection */
  XSelectInput(dpy, mainwin, 
   StructureNotifyMask | KeyPressMask | ButtonPressMask | 
		         ExposureMask | PointerMotionMask);

  /* map the window to make it visible */
  XMapRaised(dpy, mainwin);

  /*determine the window's dimensions */
  if (XGetWindowAttributes(dpy, mainwin, &xwa) == 0)
  {
    fprintf(stderr,"%s: Error. Cannot get attributes of main window.",argv[0]);
    exit(1);
  }
  maintextcols = xwa.width / curfontwidth;
  maintextrows = xwa.height / curfontheight - 1;
  /* successful completion  */
  return 0;
}

/***
function sc_handleresize() handles ConfigureNotify events, resetting the 
global variables maintextrows and maintextcols 
***/
sc_handleresize(event)
  XEvent *event;
{
  if (event->type != ConfigureNotify) return(0);
  maintextrows =  event->xconfigure.height / curfontheight - 1;
  maintextcols =  event->xconfigure.width / curfontwidth;
  return (0);
}

/*** 
function clearlines() clears the lines start through end, given
in text coordinates
***/
clearlines(start, end)
  int start, end;
{
  int temp; /* swap variable */
  if (start>end)  
  {
    temp=end;
    end=start;
    start=temp;
  }
  XClearArea(dpy,mainwin,
	     0,textrow(start) - curfontheight + 3,          /*top left corner of area */
	     0,                             /* 0 width => full window width*/
	     textrow(end) - textrow(start) + curfontheight  + 1,/* height of area */
	     0);                            /* don't send Expose events */
}

/***
function cleardownfrom() clears the window from row, down to the bottom.
***/
cleardownfrom(row)
  int row;
{
  XClearArea(dpy,mainwin,
	     0, textrow(row-1)+1,
	     0,                        /* 0 width => clear to right side */
             0,                        /* 0 height => remaining window height*/
	     0);                       /* don't generate Expose events */
}

/***
function clearupfrom() clears the row r, and any lines above it 
***/
clearupfrom(r)
  int r;
{
  XClearArea(dpy,mainwin,
	     0,0,            /* top left corner */
	     0,              /* use width of window */
             textrow(r),     /* go through row r */
	     0);             /* don't send Expose events */
}

