/*
 * xmodmap - program for loading keymap definitions into server
 *
 * $XConsortium: xmodmap.c,v 1.21 91/07/18 10:25:49 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
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
 * Author:  Jim Fulton, MIT X Consortium
 */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <ctype.h>
#include "xmodmap.h"

char *ProgramName;
Display *dpy = NULL;
int min_keycode, max_keycode;
Bool verbose = False;
Bool dontExecute = False;

void Exit (status)
    int status;
{
    if (dpy) {
	XCloseDisplay (dpy);
	dpy = NULL;
    }
    exit (status);
}


static char *help_message[] = {
"\nwhere options include:",
"    -display host:dpy            X server to use",
"    -verbose, -quiet             turn logging on or off",
"    -n                           don't execute changes, just show like make",
"    -e expression                execute string",
"    -pm                          print modifier map",
"    -pk                          print keymap table",
"    -pke                         print keymap table as expressions",
"    -pp                          print pointer map",
"    -grammar                     print out short help on allowable input",
"    -                            read standard input",
"",
NULL};


void usage ()
{
    char **cpp;

    fprintf (stderr, "usage:  %s [-options ...] [filename]\n", ProgramName);
    for (cpp = help_message; *cpp; cpp++) {
	fprintf (stderr, "%s\n", *cpp);
    }
    Exit (1);
}

static char *grammar_message[] = {
"    pointer = default              reset pointer buttons to default",
"    pointer = NUMBER ...           set pointer button codes",
"    keycode NUMBER = [KEYSYM ...]  map keycode to given keysyms",
"    keysym KEYSYM = [KEYSYM ...]   look up keysym and do a keycode operation",
"    clear MODIFIER                 remove all keys for this modifier",
"    add MODIFIER = KEYSYM ...      add the keysyms to the modifier",
"    remove MODIFIER = KEYSYM ...   remove the keysyms from the modifier",
"",
"where NUMBER is a decimal, octal, or hex constant; KEYSYM is a valid",
"Key Symbol name; and MODIFIER is one of the eight modifier names:  Shift,",
"Lock, Control, Mod1, Mod2, Mod3, Mod4, or Mod5.  Lines beginning with",
"an exclamation mark (!) are taken as comments.  Case is significant except",
"for MODIFIER names.",
"",
"Keysyms on the left hand side of the = sign are looked up before any changes",
"are made; keysyms on the right are looked up after all of those on the left",
"have been resolved.  This makes it possible to swap modifier keys.",
"",
NULL };


void grammar_usage ()
{
    char **cpp;

    fprintf (stderr, "%s accepts the following input expressions:\n\n",
	     ProgramName);
    for (cpp = grammar_message; *cpp; cpp++) {
	fprintf (stderr, "%s\n", *cpp);
    }
    Exit (0);
}

int parse_errors = 0;

main (argc, argv)
    int argc;
    char **argv;
{
    int i;
    char *displayname = NULL;
    int status;
    Bool printMap = False;
    Bool printKeyTable = False;
    Bool printKeyTableExprs = False;
    Bool printPointerMap = False;
    Bool didAnything = False;

    ProgramName = argv[0];

    /*
     * scan the arg list once to find out which display to use
     */

    for (i = 1; i < argc; i++) {
	if (strncmp (argv[i], "-d", 2) == 0) {
	    if (++i >= argc) usage ();
	    displayname = argv[i];
	}
    }

    dpy = XOpenDisplay (displayname);
    if (!dpy) {
	fprintf (stderr, "%s:  unable to open display '%s'\n",
		 ProgramName, XDisplayName (displayname));
	Exit (1);
    }

    XDisplayKeycodes (dpy, &min_keycode, &max_keycode);

    initialize_map ();

    /*
     * scan the arg list again to do the actual work (since it requires
     * the display being open.
     */

    status = 0;
    for (i = 1; i < argc; i++) {
	char *arg = argv[i];

	if (arg[0] == '-') {
	    switch (arg[1]) {
	      case 'd':			/* -display host:dpy */
		++i;			/* handled above */
		continue;
	      case 'v':			/* -verbose */
		verbose = True;
		continue;
	      case 'q':			/* -quiet */
		verbose = False;
		continue;
	      case 'n':			/* -n (like make) */
		dontExecute = True;
		continue;
	      case 'e':			/* -e expression */
		didAnything = True;
		if (++i >= argc) usage ();
		process_line (argv[i]);
		continue;
	      case 'p':			/* -p... */
		switch (arg[2]) {
		  case '\0':
		  case 'm':		/* -pm */
		    printMap = True;
		    break;
		  case 'k':		/* -pk, -pke */
		    switch (arg[3]) {
		    case '\0':
			printKeyTable = True;
			break;
		    case 'e':
			printKeyTableExprs = True;
			break;
		    default:
			usage ();
		    }
		    break;
		  case 'p':		/* -pp */
		    printPointerMap = True;
		    break;
		  default:
		    usage ();
		    /* NOTREACHED */
		}
		didAnything = True;
		continue;
	      case 'g':			/* -grammar */
		grammar_usage ();
		/*NOTREACHED*/
	      case '\0':		/* - (use standard input) */
		didAnything = True;
		process_file (NULL);
		continue;

	      /*
	       * provide old xmodmap args
	       */
	      case 'S':
		didAnything = True;
		process_line ("clear shift");
		continue;
	      case 'L':
		didAnything = True;
		process_line ("clear lock");
		continue;
	      case 'C':
		didAnything = True;
		process_line ("clear control");
		continue;
	      case '1':
	      case '2':
	      case '3':
	      case '4':
	      case '5': {
		  char *cmd = "clear modX";
		  cmd[9] = arg[1];
		  process_line (cmd);
		  continue;
	      }
	      case 's':
	      case 'l':
	      case 'c': {
		  char cmd[80];		/* big enough to hold line */
		  didAnything = True;
		  if (++i >= argc) usage ();
		  (void) sprintf (cmd, "remove %s = %s",
				  ((arg[1] == 's') ? "shift" :
				   ((arg[1] == 'l') ? "lock" :
				    "control")), argv[i]);
		  process_line (cmd);
		  continue;
	      }
	      default:
		usage ();
		/*NOTREACHED*/
	    }
	} else if (arg[0] == '+') {	/* old xmodmap args */
	    switch (arg[1]) {
	      case '1':
	      case '2':
	      case '3':
	      case '4':
	      case '5': {
		  char cmd[80];		/* big enough to hold line */
		  didAnything = True;
		  if (++i >= argc) usage ();

		  (void) sprintf (cmd, "add mod%c = %s", arg[1], argv[i]);
		  process_line (cmd);
		  continue;
	      }
	      case 'S':
	      case 'L':
	      case 'C':
		arg[1] = tolower (arg[1]);
		/* fall through to handler below */
	      case 's':
	      case 'l':
	      case 'c': {
		  char cmd[80];		/* big enough to hold line */
		  didAnything = True;
		  if (++i >= argc) usage ();
		  (void) sprintf (cmd, "add %s = %s",
				  ((arg[1] == 's') ? "shift" :
				   ((arg[1] == 'l') ? "lock" :
				    "control")), argv[i]);
		  process_line (cmd);
		  continue;
	      }
	      default:
		usage ();
	    }
	} else {
	    didAnything = True;
	    process_file (arg);
	    continue;
	}
    }					/* end for loop */

    /* for compatibility */
    if (!didAnything) printMap = True;

    /*
     * at this point, the work list has been built and we can view it or
     * execute it
     */

    if (dontExecute) {
	print_work_queue ();
	Exit (0);
    }

    if (parse_errors != 0) {
	fprintf (stderr, "%s:  %d errors encountered, aborting.\n",
		 ProgramName, parse_errors);
    } else {
	status = execute_work_queue ();
    }

    if (printMap) {
	print_modifier_map ();
    }

    if (printKeyTable) {
	print_key_table (False);
    }

    if (printKeyTableExprs) {
	print_key_table (True);
    }

    if (printPointerMap) {
	print_pointer_map ();
    }

    Exit (status < 0 ? 1 : 0);
}

