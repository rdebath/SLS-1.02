
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


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define STRING  "xlotus"
#define BORDER	1
#define FONT	"fixed"


   Display    *dpy;		/* X server connection */
   Window      win;		/* Window ID */
   GC          gc, gc1;		/* GC to draw with */
   XFontStruct *fontstruct;	/* Font descriptor */
   unsigned long fth, pad;	/* Font size parameters */
   unsigned long fg, bg, bd;	/* Pixel values */
   unsigned long bw;		/* Border width */
   XGCValues   gcv, gcv1;	/* Struct for creating GC */
   XEvent      event;		/* Event received */
   XSizeHints  xsh;		/* Size hints for window manager */
   char       *geomSpec;	/* Window geometry string */
   XSetWindowAttributes xswa;	/* Temporary Set Window Attribute struct */
   XWindowAttributes xwa;	/* Temporary Window Attribute struct */

