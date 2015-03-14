/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: globals.h,v 1.8 91/09/03 17:42:51 dave Exp $
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

extern Xman_Resources resources;	/* Resource manager sets these. */

/* bookkeeping global variables. */

extern Widget help_widget;		/* The help widget. */

extern int default_height,default_width; /* Approximately the default with and
					    height, of the manpage when shown,
					    the the top level manual page 
					    window */
extern int man_pages_shown;		/* The current number of manual
					   pages being shown, if 0 we exit. */

extern Manual * manual;		        /* The manual structure. */
extern int sections;			/* The number of manual sections. */

extern XContext manglobals_context;	/* The context for man_globals. */

extern Widget initial_widget;	      /* The initial widget, never realized. */

extern char * option_names[];

extern char **saved_argv;
extern int saved_argc;
