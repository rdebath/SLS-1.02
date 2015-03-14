/*
 * $XConsortium: xmbufinfo.c,v 1.2 89/10/09 11:52:17 jim Exp $
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

#include <stdio.h>
#ifndef MULTIBUFFER
main (argc, argv)
    int argc;
    char **argv;
{
    fprintf (stderr, "%s:  multibuffer extension not compiled.\n", argv[0]);
    exit (1);
}
#else
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/extensions/multibuf.h>

char *ProgramName;

static void usage ()
{
    fprintf (stderr, "usage:  %s [-display dpy]\n", ProgramName);
    exit (1);
}

main (argc, argv)
    int argc;
    char **argv;
{
    char *displayname = NULL;
    Display *dpy;
    int i;    
    int event_base, error_base;

    ProgramName = argv[0];
    for (i = 1; i < argc; i++) {
	char *arg = argv[i];

	if (arg[0] == '-') {
	    switch (arg[1]) {
	      case 'd':			/* -display dpy */
		if (++i >= argc) usage ();
		displayname = argv[i];
		continue;
	    }
	}
	usage ();
    }

    dpy = XOpenDisplay (displayname);
    if (!dpy) {
	fprintf (stderr, "%s:  unable to open display \"%s\"\n",
		 ProgramName, XDisplayName(displayname));
	exit (1);
    }

    if (!XmbufQueryExtension (dpy, &event_base, &error_base)) {
	fprintf (stderr, 
	 "%s:  multibuffering extension not supported on server \"%s\"\n",
		 ProgramName, DisplayString(dpy));
	XCloseDisplay(dpy);
	exit (1);
    }

    printf ("Multibuffer information for server \"%s\":\n",
	    DisplayString(dpy));
    printf ("  First event number:  %d\n", event_base);
    printf ("  First error number:  %d\n", error_base);

    for (i = 0; i < ScreenCount(dpy); i++) {
	int j;
	int nmono, nstereo;
	XmbufBufferInfo *mono_info = NULL, *stereo_info = NULL;
	
	if (!XmbufGetScreenInfo (dpy, RootWindow (dpy, i), &nmono, &mono_info,
				 &nstereo, &stereo_info)) {
	    fprintf (stderr,
		     "%s:  unable to get multibuffer info for screen %d\n",
		     ProgramName, i);
	    continue;
	}

	printf ("  Screen %d:\n", i);
	printf ("    Number of mono types:  %d\n", nmono);
	for (j = 0; j < nmono; j++) {
	    printf ("      Visual id, max buffers, depth:  0x%lx, %d, %d\n",
		    mono_info[j].visualid, mono_info[j].max_buffers,
		    mono_info[j].depth);
	}
	printf ("    Number of stereo types:  %d\n", nstereo);
	for (j = 0; j < nstereo; j++) {
	    printf ("      Visual id, max buffers, depth:  0x%lx, %d, %d\n",
		   stereo_info[j].visualid, stereo_info[j].max_buffers,
		   stereo_info[j].depth);
	}
	if (mono_info) XFree ((char *) mono_info);
	if (stereo_info) XFree ((char *) stereo_info);
    }
    XCloseDisplay (dpy);
    exit (0);
}
#endif /* MULTIBUFFER */
