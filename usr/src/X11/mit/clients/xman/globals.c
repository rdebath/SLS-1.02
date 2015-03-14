/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: globals.c,v 1.8 91/06/08 18:15:23 rws Exp $
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
 * Created:   October 22, 1987
 */

#include "man.h"

Xman_Resources resources;	/* Resource manager sets these. */

/* bookkeeping global variables. */

Widget help_widget;		/* The help widget. */

int default_height,default_width; /* Approximately the default with and
					    height, of the manpage when shown,
					    the the top level manual page 
					    window */

Manual * manual;		/* The manual structure. */
int sections;			/* The number of manual sections. */

int man_pages_shown;		/* The current number of manual
				   pages being shown, if 0 we exit. */

Widget initial_widget;		/* The initial widget, never realized. */

XContext manglobals_context;	/* The context for man_globals. */
