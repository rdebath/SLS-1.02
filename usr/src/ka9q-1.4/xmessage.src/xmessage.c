/*
 * xmessage - utility for querying users
 *
 * Copyright 1988,1991 Massachusetts Institute of Technology
 * Time-stamp: <91/08/27 18:33:26 gildea>
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
 * Originally written by Chris D. Peterson, MIT Project Athena, 1988.
 * Updated by Jim Fulton, MIT X Consortium.
 * Current version by Stephen Gildea <gildea@expo.lcs.mit.edu>, August 1991.
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <stdio.h>

extern char *malloc();

extern char *read_file();
extern Widget make_queryform();

/*
 * data used by xmessage
 */

char *ProgramName;

static Atom wm_delete_window;

static struct _QueryResources {
    char *file;
    char *button_list;
    char *default_button;
    Boolean print_value;
    char *message;
} qres;				/* initialized by resources below */

#define offset(field) XtOffsetOf(struct _QueryResources, field)
static XtResource resources[] = {
    { "file", "File", XtRString, sizeof (char *), 
      offset(file), XtRString, (XtPointer) NULL },
    { "buttons", "Buttons", XtRString, sizeof (char *),
      offset(button_list), XtRString, (XtPointer) "okay:0" },
    { "defaultButton", "DefaultButton", XtRString, sizeof (char *),
      offset(default_button), XtRString, (XtPointer) NULL },
    { "printValue", "PrintValue", XtRBoolean, sizeof (Boolean),
      offset(print_value), XtRString, "false" },
};
#undef offset

static XrmOptionDescRec optionList[] =  {
    { "-file",    ".file", XrmoptionSepArg, (XPointer) NULL },
    { "-buttons", ".buttons", XrmoptionSepArg, (XPointer) NULL },
    { "-default", ".defaultButton", XrmoptionSepArg, (XPointer) NULL },
    { "-print",   ".printValue", XrmoptionNoArg, (XPointer) "on" },
};

static String fallback_resources[] = {
    "*baseTranslations: #override :<Key>Return: default-exit()",
    NULL};


/*
 * usage
 */

static void usage ()
{
    static char *options[] = {
"    -file filename              file to read message from, - means stdin",
"    -buttons string             comma-separated list of label:exitcode",
"    -default button             button to activate if Return is pressed",
"    -print                      print the button label when selected",
"",
NULL};
    char **cpp;

    fprintf (stderr, "usage: %s [-options] [message ...]\n\n",
	     ProgramName);
    fprintf (stderr, "where options include:\n");
    for (cpp = options; *cpp; cpp++) {
	fprintf (stderr, "%s\n", *cpp);
    }
    exit (1);
    /* NOTREACHED */
}

/*
 * Action to implement ICCCM delete_window and other translations.
 * Takes one argument, the exit status.
 */
/* ARGSUSED */
static void
exit_action(w, event, params, num_params)
    Widget w;			/* unused */
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int exit_status = 0;

    if(event->type == ClientMessage
       && event->xclient.data.l[0] != wm_delete_window)
	return;
    
    if (*num_params == 1)
	exit_status = atoi(params[0]);
    exit(exit_status);
}

int default_exitstatus = -1;		/* value of button named by -default */

/* ARGSUSED */
static void
default_exit_action(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (default_exitstatus >= 0)
	exit(default_exitstatus);
}

XtActionsRec actions_list[] = {
    "exit", exit_action,
    "default-exit", default_exit_action,
};

static String top_trans =
    "<ClientMessage>WM_PROTOCOLS: exit(1)\n";

/*
 * xmessage main program - make sure that there is a message,
 * then create the query box and go.  Callbacks take care of exiting.
 */
main (argc, argv)
    int argc;
    char **argv;
{
    Widget top, queryform;
    XtAppContext app_con;

    ProgramName = argv[0];

    top = XtAppInitialize (&app_con, "Xmessage",
			   optionList, XtNumber(optionList), &argc, argv,
			   fallback_resources, NULL, 0);

    XtGetApplicationResources (top, (XtPointer) &qres, resources,
			       XtNumber(resources), NULL, 0);

    if (argc > 1 && !strcmp(argv[1], "-help")) {
	usage();
    }
    if (argc == 1 && qres.file != NULL) {
	qres.message = read_file (qres.file);
	if (qres.message == NULL) {
	    fprintf (stderr, "%s: problems reading message file\n",
		     ProgramName);
	    exit (1);
	}
    } else if (argc > 1 && qres.file == NULL) {
	int i, len;
	char *cp;

	len = argc - 1;		/* spaces between words and final NULL */
	for (i=1; i<argc; i++)
	    len += strlen(argv[i]);
	qres.message = malloc(len);
	if (!qres.message)
	    perror("string malloc");
	cp = qres.message;
	for (i=1; i<argc; i++) {
	    strcpy(cp, argv[i]);
	    cp += strlen(argv[i]);
	    if (i != argc-1)
		*cp++ = ' ';
	    else
		*cp = '\0';
	}
    } else {
	usage ();
    }

    if (qres.message == NULL) {
	fprintf (stderr, "%s: no message\n", ProgramName);
	exit (1);
    }

    XtAppAddActions(app_con, actions_list, XtNumber(actions_list));
    XtOverrideTranslations(top, XtParseTranslationTable(top_trans));

    /*
     * create the query form; this is where most of the real work is done
     */
    queryform = make_queryform (top, qres.message, qres.button_list,
				qres.print_value, qres.default_button);
    if (!queryform) {
	fprintf (stderr,
		 "%s: unable to create query form with buttons: %s\n",
		 ProgramName, qres.button_list);
	exit (1);
    }

    XtSetMappedWhenManaged(top, FALSE);
    XtRealizeWidget(top);

    /* do WM_DELETE_WINDOW before map */
    wm_delete_window = XInternAtom(XtDisplay(top), "WM_DELETE_WINDOW", False);
    XSetWMProtocols(XtDisplay(top), XtWindow(top), &wm_delete_window, 1);

    XtMapWidget(top);
    XtAppMainLoop(app_con);

    exit (0);
}
