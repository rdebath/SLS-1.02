/*
 * $XConsortium: appres.c,v 1.11 91/07/13 21:49:16 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
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
 *
 * Author:  Jim Fulton, MIT X Consortium
 */

#include <X11/Intrinsic.h>
#include <stdio.h>

#define NONAME "-AppResTest-"

char *ProgramName;

XrmQuark XrmQString;

static void usage ()
{
    fprintf (stderr,
	     "usage:  %s  [class [instance]] [-1] [toolkitoptions]\n",
	     ProgramName);
    fprintf (stderr,
	     "-1      list resources only at the specified level\n");
    fprintf (stderr,
	     "The number of class and instance elements must be equal.\n");
    exit (1);
}

/* stolen from Xlib Xrm.c */
static void PrintBindingQuarkList(bindings, quarks, stream)
    XrmBindingList      bindings;
    XrmQuarkList	quarks;
    FILE		*stream;
{
    Bool	firstNameSeen;

    for (firstNameSeen = False; *quarks; bindings++, quarks++) {
	if (*bindings == XrmBindLoosely) {
	    (void) fprintf(stream, "*");
	} else if (firstNameSeen) {
	    (void) fprintf(stream, ".");
	}
	firstNameSeen = True;
	(void) fputs(XrmQuarkToString(*quarks), stream);
    }
}

/* stolen from Xlib Xrm.c */
/* output out the entry in correct file syntax */
/*ARGSUSED*/
static Bool DumpEntry(db, bindings, quarks, type, value, data)
    XrmDatabase		*db;
    XrmBindingList      bindings;
    XrmQuarkList	quarks;
    XrmRepresentation   *type;
    XrmValuePtr		value;
    XPointer		data;
{
    FILE			*stream = (FILE *)data;
    register unsigned int	i;
    register char		*s;
    register char		c;

    if (*type != XrmQString)
	(void) putc('!', stream);
    PrintBindingQuarkList(bindings, quarks, stream);
    s = value->addr;
    i = value->size;
    if (*type == XrmQString) {
	(void) fputs(":\t", stream);
	if (i)
	    i--;
    }
    else
	fprintf(stream, "=%s:\t", XrmRepresentationToString(*type));
    if (i && (*s == ' ' || *s == '\t'))
	(void) putc('\\', stream); /* preserve leading whitespace */
    while (i--) {
	c = *s++;
	if (c == '\n') {
	    if (i)
		(void) fputs("\\n\\\n", stream);
	    else
		(void) fputs("\\n", stream);
	} else if (c == '\\')
	    (void) fputs("\\\\", stream);
	else if ((c < ' ' && c != '\t') ||
		 ((unsigned char)c >= 0x7f && (unsigned char)c < 0xa0))
	    (void) fprintf(stream, "\\%03o", (unsigned char)c);
	else
	    (void) putc(c, stream);
    }
    (void) putc('\n', stream);
    return False;
}

main (argc, argv)
    int argc;
    char **argv;
{
    Widget toplevel;
    char *iname = NONAME, *cname = NONAME;
    XtAppContext xtcontext;
    XrmName names[101];
    XrmClass classes[101];
    int i;
    int mode = XrmEnumAllLevels;

    ProgramName = argv[0];
    if (argc > 1 && argv[1][0] != '-') {
	cname = argv[1];
	if (argc > 2 && argv[2][0] != '-')
	    iname = argv[2];
    }

    XrmStringToClassList(cname, classes);
    XrmStringToNameList(iname, names);
    for (i = 0; names[i]; i++)
	;
    if (!i || classes[i] || !classes[i-1]) usage ();
    argv[0] = XrmNameToString(names[0]);

    toplevel = XtAppInitialize(&xtcontext, XrmClassToString(classes[0]),
			       NULL, 0, &argc, argv, NULL, NULL, 0);

    iname = NULL;
    cname = NULL;
    for (i = 1; i < argc; i++) {
	if (!strcmp(argv[i], "-1"))
	    mode = XrmEnumOneLevel;
	else if (argv[i][0] == '-')
	    usage();
	else if (!cname)
	    cname = argv[i];
	else if (!iname)
	    iname = argv[i];
	else
	    usage();
    }

    if (!iname) {
	XtGetApplicationNameAndClass(XtDisplay(toplevel), &iname, &cname);
	names[0] = XrmStringToName(iname);
    }

    XrmQString = XrmPermStringToQuark("String");

    XrmEnumerateDatabase(XtDatabase(XtDisplay(toplevel)),
			 names, classes, mode,
			 DumpEntry, (XPointer)stdout);

    exit (0);
}
