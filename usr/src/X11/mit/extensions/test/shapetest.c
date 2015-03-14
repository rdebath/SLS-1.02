/************************************************************
Copyright 1989 by the Massachusetts Institute of Technology

Permission  to  use,  copy,  modify,  and  distribute   this
software  and  its documentation for any purpose and without
fee is hereby granted, provided that the above copyright no-
tice  appear  in all copies and that both that copyright no-
tice and this permission notice appear in  supporting  docu-
mentation,  and  that  the name of MIT not be used in adver-
tising  or publicity pertaining to distribution of the soft-
ware without specific prior written permission. M.I.T. makes
no representation about the suitability of this software for
any  purpose.  It is provided "as is" without any express or
implied warranty.

********************************************************/

/* $XConsortium: shapetest.c,v 1.4 91/01/24 15:14:58 gildea Exp $ */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xproto.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>

Display *dpy;

StartConnectionToServer(argc, argv)
int     argc;
char    *argv[];
{
    char *display;

    display = NULL;
    for(--argc, ++argv; argc; --argc, ++argv)
    {
	if ((*argv)[0] == '-') {
	    switch((*argv)[1]) {
	    case 'd':
		display = argv[1];
		++argv; --argc;
		break;
	    }
	}
    }
    if (!(dpy = XOpenDisplay(display)))
    {
       perror("Cannot open display\n");
       exit(0);
   }
}

XRectangle  rects[] = { 0,0, 100, 100, 10, 10, 100, 100 };

main(argc, argv)
    int argc;
    char **argv;

{
	Window  w;
	GC gc;
	char *windowName = "Test of Shape Extension";
	XSetWindowAttributes xswa;
	unsigned long	mask;
	XEvent pe;
	XColor screen_def_blue, exact_def_blue;
	XColor screen_def_red, exact_def_red;

	/*_Xdebug = 1;*/   /* turn on synchronization */

	StartConnectionToServer(argc, argv);

	xswa.event_mask = ExposureMask;
	xswa.background_pixel = BlackPixel (dpy, DefaultScreen (dpy));
	mask = CWEventMask | CWBackPixel;
	w = XCreateWindow(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		100, 100, 340, 340, 0, 
		CopyFromParent, CopyFromParent,	CopyFromParent,
		mask, &xswa);

	XChangeProperty(dpy,
	    w, XA_WM_NAME, XA_STRING, 8, PropModeReplace,
	    (unsigned char *)windowName, strlen(windowName));

	XShapeCombineRectangles (dpy, w, ShapeBounding, 0, 0, 
		          rects, sizeof (rects) / sizeof (rects[0]),
			  ShapeSet, Unsorted);

	XMapWindow(dpy, w);

	gc = XCreateGC(dpy, w, 0, 0);
	XAllocNamedColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), "blue",
	       &screen_def_blue,  &exact_def_blue);
	XAllocNamedColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), "red",
	       &screen_def_red,  &exact_def_red);
	XSetForeground(dpy, gc, screen_def_red.pixel);
	XSetBackground(dpy, gc, screen_def_blue.pixel);

	while (1) {
	    XNextEvent(dpy, &pe);
	}
}
