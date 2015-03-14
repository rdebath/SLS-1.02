/*
 * xmessage - utility for querying users
 *
 * Copyright 1988,1991 Massachusetts Institute of Technology
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
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <stdio.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>

extern char *malloc();
extern char *ProgramName;
extern int default_exitstatus;

typedef struct _ButtonRecord {
    char *name;
    int exitstatus;
    Boolean print_value;
    Widget widget;
} ButtonRecord;

static void unquote_pairs (br, n)
    register ButtonRecord *br;
    int n;
{
    int i;

    for (i = 0; i < n; i++) {
	register char *dst, *src;
	register int quoted = 0;

	for (src = dst = br->name; *src; src++) {
	    if (quoted) {
		*dst++ = *src;
		quoted = 0;
	    } else if (src[0] == '\\') {
		quoted = 1;
	    } else {
		*dst++ = *src;
	    }
	}
	*dst = '\0';
    }
    return;
}

/*
 * parses string of form "yes:1,no:2, ..."
 * sets brptr to point to parsed table
 * returns 0 if successful, -1 if not
 */
static int parse_name_and_exit_code_list (buttonlist, brptr)
    char *buttonlist;
    ButtonRecord **brptr;
{
    register char *cp;
    int shouldfind = 0, npairs = 0;
    int default_exitcode = 100;
    int quoted = 0;
    ButtonRecord *br;
    int len;
    char *copy;

    if (!buttonlist) return 0;

    /*
     * Figure out how many matches we will find so that we can preallocate
     * space for button structures.  If you add stripping of white space,
     * make sure that you update this as well as the walking algorithm below.
     */
    if (buttonlist[0]) shouldfind++;
    for (cp = buttonlist; *cp; cp++) {
	if (quoted == 1) quoted = 0;
	else if (*cp == '\\') quoted = 1;
	else if (*cp == ',') shouldfind++;
    }
    len = (cp - buttonlist);

    /*
     * allocate space for button record
     */
    br = (ButtonRecord *) malloc (sizeof(ButtonRecord) * shouldfind);
    if (!br) return -1;

    cp = malloc (len + 1);
    if (!cp) {
	(void) free ((char *) br);
	return -1;
    }
    copy = cp;
    strcpy (copy, buttonlist);


    /*
     * walk down list separating into name:exitcode pairs
     */
    while (*cp) {
	char *start, *colon, *comma;
	int exitcode;

	start = cp;
	colon = comma = NULL;
	exitcode = ++default_exitcode;
	quoted = 0;

	/* find the next name and exit code */
	for (; *cp; cp++) {
	    if (quoted) quoted = 0;
	    else if (*cp == '\\') quoted = 1;
	    else if (*cp == ':') colon = cp;
	    else if (*cp == ',') {
		comma = cp;
		break;
	    }
	}

	/*
	 * If comma is NULL then we are at the end of the string.  If colon
	 * is NULL, then there was no exit code given, so default to zero.
	 */

	if (comma) *comma = '\0';

	if (colon) {
	    exitcode = atoi (colon+1);
	    *colon = '\0';
	}

	/*
	 * make sure that we aren't about to stomp on memory
	 */
	if (npairs >= shouldfind) {
	    fprintf (stderr,
		     "%s:  internal error, found extra pairs (should be %d)\n",
		     ProgramName, shouldfind);
	    (void) free ((char *) br);
	    (void) free (copy);
	    return -1;
	}

	/*
	 * got it!  start and exitcode contain the right values
	 */
	br[npairs].name = start;
	br[npairs].exitstatus = exitcode;
	npairs++;

	if (comma) cp++;
    }


    if (npairs != shouldfind) {
	fprintf (stderr, "%s:  internal error found %d instead of %d pairs\n",
		 ProgramName, npairs, shouldfind);
	(void) free ((char *) br);
	(void) free (copy);
	return -1;
    }

    /*
     * now, strip any quoted characters
     */
    unquote_pairs (br, npairs);
    *brptr = br;
    return npairs;
}

/* ARGSUSED */
static void handle_button (w, closure, client_data)
    Widget w;
    XtPointer closure;
    XtPointer client_data;
{
    ButtonRecord *br = (ButtonRecord *) closure;

    if (br->print_value)
	puts (br->name);
    exit (br->exitstatus);
}

Widget make_queryform(parent, msgstr, button_list, print_value, default_button)
    Widget parent;			/* into whom widget should be placed */
    char *msgstr;			/* message string */
    char *button_list;			/* list of button title:status */
    Boolean print_value;		/* print button string on stdout? */
    char *default_button;		/* button activated by Return */
{
    ButtonRecord *br;
    int npairs, i;
    Widget form, text, prev;
    Arg args[10];
    Cardinal n, thisn;

    npairs = parse_name_and_exit_code_list (button_list, &br);

    form = XtCreateManagedWidget ("form", formWidgetClass, parent, NULL, 0);
    
    n = 0;
    XtSetArg (args[n], XtNleft, XtChainLeft); n++;
    XtSetArg (args[n], XtNright, XtChainLeft); n++;
    XtSetArg (args[n], XtNtop, XtChainTop); n++;
    XtSetArg (args[n], XtNbottom, XtChainBottom); n++;

    XtSetArg (args[n], XtNlabel, msgstr); n++;

    text = XtCreateManagedWidget ("message", labelWidgetClass, form, args, n);

    /*
     * Create the buttons
     */
    n = 0;
    XtSetArg (args[n], XtNleft, XtChainLeft); n++;
    XtSetArg (args[n], XtNright, XtChainLeft); n++;
    XtSetArg (args[n], XtNtop, XtChainBottom); n++;
    XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
    XtSetArg (args[n], XtNfromVert, text); n++;
    XtSetArg (args[n], XtNvertDistance, 5); n++;

    prev = NULL;
    for (i = 0; i < npairs; i++) {
	thisn = n;
	XtSetArg (args[thisn], XtNfromHoriz, prev); thisn++;
	prev = XtCreateManagedWidget (br[i].name, commandWidgetClass,
				      form, args, thisn);
	br[i].widget = prev;
	br[i].print_value = print_value;
	XtAddCallback (prev, XtNcallback, handle_button, (XtPointer) &br[i]);
	if (default_button && !strcmp(default_button, br[i].name)) {
	    Dimension border;

	    default_exitstatus = br[i].exitstatus;
	    XtVaGetValues(br[i].widget, XtNborderWidth, &border, NULL);
	    border *= 2;
	    XtVaSetValues(br[i].widget, XtNborderWidth, border, NULL);
	}
    }
    return form;
}
