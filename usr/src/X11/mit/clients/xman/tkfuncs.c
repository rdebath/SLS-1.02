/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: tkfuncs.c,v 1.5 91/01/09 17:31:46 rws Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   February 6, 1988
 */

#include <X11/X.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>

/* 
 * I am doing the "wrong" thing here by looking in the core field for the
 * widget to get this info, the "right" thing to do is to do a XtGetValues
 * to get this information.
 */

/*	Function Name: Width
 *	Description: finds the width of a widget.
 *	Arguments: w - the widget.
 *	Returns: the width of that widget.
 */

int
Width(w)
Widget w;
{
  return( (int) w->core.width );
}

/*	Function Name: Height
 *	Description: finds the height of a widget.
 *	Arguments: w - the widget.
 *	Returns: the height of that widget.
 */

int
Height(w)
Widget w;
{
  return( (int) w->core.height );
}

/*	Function Name: BorderWidth
 *	Description: finds the BorderWidth of a widget.
 *	Arguments: w - the widget.
 *	Returns: the BorderWidth of that widget.
 */

int
BorderWidth(w)
Widget w;
{
  return( (int) w->core.border_width );
}

/* 
 * These functions have got to be able to get at the widget tree, I don't see
 * any way around this one.
 */

/*	Function Name: Name
 *	Description: This function returns the correct popup child
 *	Arguments: w - widget
 *	Returns: the popup child.
 */

char * 
Name(w)
Widget w;
{
  return( w->core.name);
}
