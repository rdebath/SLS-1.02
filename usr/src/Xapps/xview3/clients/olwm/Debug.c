/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)Debug.c	26.5	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "i18n.h"
#include "olwm.h"
#include "win.h"

char *eventNames[] = {
	"<EventZero>",
	"<EventOne>",
	"KeyPress",
	"KeyRelease",
	"ButtonPress",
	"ButtonRelease",
	"MotionNotify",
	"EnterNotify",
	"LeaveNotify",
	"FocusIn",
	"FocusOut",
	"KeymapNotify",
	"Expose",
	"GraphicsExpose",
	"NoExpose",
	"VisibilityNotify",
	"CreateNotify",
	"DestroyNotify",
	"UnmapNotify",
	"MapNotify",
	"MapRequest",
	"ReparentNotify",
	"ConfigureNotify",
	"ConfigureRequest",
	"GravityNotify",
	"ResizeRequest",
	"CirculateNotify",
	"CirculateRequest",
	"PropertyNotify",
	"SelectionClear",
	"SelectionRequest",
	"SelectionNotify",
	"ColormapNotify",
	"ClientMessage",
	"MappingNotify",
};

DebugEvent(ep, str)
	XEvent	*ep;
	char	*str;
{
	fprintf(stderr, "%s:%s - ", str, eventNames[ep->type]);
	fflush(stderr);
}

static char *typeNames[] = {
	"Frame",
	"Icon",
	"Resize",
	"Pushpin",
	"Button",
	"Pane",
	"IconPane",
	"Colormap",
	"Menu",
	"NoFocus",
	"Root",
	"Busy",
};

DebugWindow(win)
WinGeneric *win;
{
	if (win == NULL)
		fprintf(stderr, "other window - ");
	else
		fprintf(stderr, "win %x (self %d) %s - ",win,win->core.self,
			typeNames[win->core.kind]);
	fflush(stderr);
}
