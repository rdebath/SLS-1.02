/*
 * $XConsortium: oclock.c,v 1.13 91/01/10 21:33:48 gildea Exp $
 *
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
 */

#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include "Clock.h"
#include <stdio.h> 

#include "oclock.bit"
#include "oclmask.bit"

extern void exit();
static void quit();

static XtActionsRec actions[] = {
    {"quit",	quit}
};

static Atom wm_delete_window;

/* Command line options table.  Only resources are entered here...there is a
   pass over the remaining options after XtParseCommand is let loose. */

/* Exit with message describing command line format */

void usage()
{
    fprintf(stderr,
"usage: oclock\n");
    fprintf (stderr, 
"       [-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]] [-display [{host}]:[{vs}]]\n");
    fprintf(stderr,
"       [-fg {color}] [-bg {color}] [-bd {color}] [-bw {pixels}]\n");
    fprintf(stderr,
"       [-minute {color}] [-hour {color}] [-jewel {color}]\n");
    fprintf(stderr,
"       [-backing {backing-store}]\n");
    exit(1);
}

static XrmOptionDescRec options[] = {
{"-fg",		"*Foreground",		XrmoptionSepArg,	NULL},
{"-bg",		"*Background",		XrmoptionSepArg,	NULL},
{"-foreground",	"*Foreground",		XrmoptionSepArg,	NULL},
{"-background",	"*Background",		XrmoptionSepArg,	NULL},
{"-minute",	"*clock.minute",	XrmoptionSepArg,	NULL},
{"-hour",	"*clock.hour",		XrmoptionSepArg,	NULL},
{"-jewel",	"*clock.jewel",		XrmoptionSepArg,	NULL},
{"-backing",	"*clock.backingStore",	XrmoptionSepArg,	NULL},
{"-shape",	"*clock.shapeWindow",	XrmoptionNoArg,		"TRUE"},
{"-noshape",	"*clock.shapeWindow",	XrmoptionNoArg,		"FALSE"},
{"-transparent","*clock.transparent",	XrmoptionNoArg,		"TRUE"},
};

void main(argc, argv)
    int argc;
    char **argv;
{
    XtAppContext xtcontext;
    Widget toplevel;
    Widget clock;
    Arg arg[2];
    int	i;
    
    toplevel = XtAppInitialize(&xtcontext, "Clock", options, XtNumber(options),
			       &argc, argv, NULL, NULL, 0);

    if (argc != 1) usage();

    XtAppAddActions
	(xtcontext, actions, XtNumber(actions));
    XtOverrideTranslations
	(toplevel, XtParseTranslationTable ("<Message>WM_PROTOCOLS: quit()"));

    i = 0;
    XtSetArg (arg[i], XtNiconPixmap, 
	      XCreateBitmapFromData (XtDisplay(toplevel),
				     XtScreen(toplevel)->root,
				     (char *)oclock_bits, oclock_width, oclock_height));
    i++;
    XtSetArg (arg[i], XtNiconMask,
	      XCreateBitmapFromData (XtDisplay(toplevel),
				     XtScreen(toplevel)->root,
				     (char *)oclmask_bits, oclmask_width, oclmask_height));
    i++;
    XtSetValues (toplevel, arg, i);

    clock = XtCreateManagedWidget ("clock", clockWidgetClass, toplevel, NULL, 0);
    XtRealizeWidget (toplevel);

    wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
				   False);
    (void) XSetWMProtocols (XtDisplay(toplevel), XtWindow(toplevel),
                            &wm_delete_window, 1);

    XtAppMainLoop(xtcontext);
}

static void quit(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (event->type == ClientMessage && 
	event->xclient.data.l[0] != wm_delete_window) {
	XBell(XtDisplay(w), 0);
    } else {
	XCloseDisplay(XtDisplay(w));
	exit(0);
    }
}
