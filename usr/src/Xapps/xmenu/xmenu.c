#ifndef lint
static char rcsid[] = "$Header: xmenu.c,v 1.1 90/06/19 16:59:36 rlh2 Rel $";
#endif !lint

/* 
 * Copyright 1990 Richard Hesketh / rlh2@ukc.ac.uk
 *                Computing Lab. University of Kent at Canterbury, UK
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Richard Hesketh and The University of
 * Kent at Canterbury not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 * Richard Hesketh and The University of Kent at Canterbury make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * Richard Hesketh AND THE UNIVERSITY OF KENT AT CANTERBURY DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL Richard Hesketh OR THE
 * UNIVERSITY OF KENT AT CANTERBURY BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 *
 * Author:  Richard Hesketh / rlh2@ukc.ac.uk, 
 *                Computing Lab. University of Kent at Canterbury, UK
 */

/* XMenu - provide a popup menu from a shell script etc.
 *
 * This little toolkit program produces a popup menu on the screen taking
 * the command line arguments as menu entries.  If an argument contains
 * a non-escaped '=' (equals sign) the string to the left is the name
 * displayed in the menu and the string to the right is the text output
 * when this menu button is pressed.
 *
 * A special argument "line" can be used to draw a dividing line between
 * successive menu entries.
 *
 * A menu title can be given as an argument after a "-heading" flag.
 *
 * The menu is only popped down when a menu button is pressed.
 * By default the menu is popped up below and to the right of the current
 * pointer position, use "-geometry" to position the menu at a particular
 * point (and in a particular size). 
 *
 * Examples:
 *
 *	xmenu -heading "Choose Files by Suffix" Compressed="*.Z" \
 *			"C sources"="*.c" "Headers"="*.h" \
 *			line line "        Cancel"
 *
 *	xmenu Hello="Hello World" line " " " " line Goodbye="Goodbye World"
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>

#define LINE (String)1
#define LINE_TOK "line"

static String MTrans = "<EnterWindow>: highlight()\n\
			<LeaveWindow>: unhighlight()\n\
			<MotionNotify>: highlight()\n\
			<BtnUp>: notify() unhighlight()";
static XtTranslations MPTrans = NULL;

static XrmOptionDescRec options[] = {
	{"-heading", ".heading", XrmoptionSepArg, (XtPointer)NULL},
};


static void
button_pressed(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
	if (client_data != NULL)
		printf("%s\n", client_data);
	exit(0);
}

String
get_prompt(str, out_val)
String str;
String *out_val;
{
	int s, t;
	char name[1000];
	Boolean found_escape = FALSE;

	*out_val = NULL;

	if (strncmp(str, LINE_TOK, strlen(LINE_TOK)) == 0)
		return (LINE);

	for (t = s = 0; str[s] != '\0'; s++) {
		if (str[s] == '\\') {
			if (found_escape) {
				found_escape = FALSE;
				name[t++] = '\\';
			} else
				found_escape = TRUE;
		} else if (str[s] == '=') {
			if (found_escape) {
				found_escape = FALSE;
				name[t++] = '=';
			} else {
				*out_val = str + s + 1;
				break;
			}
		} else
			name[t++] = str[s];
	}
	name[t] = '\0';
	return (XtNewString(name));
}


main(argc, argv)
int argc;
char *argv[];
{
	Widget shell, button;
	Cardinal i;
	Position x, y;
	int int_x, int_y;
	Window junk1;
	int junk2;
	unsigned int junk3;
	Display *dpy;
	String out_val;
	String name, heading;
	char *blank;
	XrmValue value;

	x = y = 400; /* just in case XQueryPointer() fails 8-) */

	XtToolkitInitialize();
	dpy = XtOpenDisplay(XtCreateApplicationContext(), NULL,
					"xmenu", "XMenu",
					options, XtNumber(options),
					&argc, argv);

	if (XQueryPointer(dpy, DefaultRootWindow(dpy), &junk1, &junk1,
	    &int_x, &int_y, &junk2, &junk2, &junk3)) {
		x = int_x;
		y = int_y;
	}

	if (XrmGetResource(dpy->db, "xmenu.heading", "", &blank, &value))
		heading = (char *)value.addr;
	else
		heading = NULL;

	MPTrans = XtParseTranslationTable(MTrans);
	shell = XtVaAppCreateShell("xmenu", "XMenu", simpleMenuWidgetClass,
					dpy, XtNx, x, XtNy, y,
					XtNlabel, heading,
					XtNtranslations, MPTrans,
					NULL);
	if (heading != NULL)
		button = XtVaCreateManagedWidget("menuLine", 
					smeLineObjectClass, shell,
					XtNlineWidth, 3,
					NULL);

	for (i = 1; i < argc; i++) {
		name = get_prompt(argv[i], &out_val);

		if (name == LINE)
			button = XtVaCreateManagedWidget("menuLine",
					smeLineObjectClass, shell,
					NULL);
		else {
			button = XtVaCreateManagedWidget("menuButton",
					smeBSBObjectClass, shell,
					XtNlabel, name,
					NULL);
			XtAddCallback(button, XtNcallback, button_pressed,
					(XtPointer)out_val);
		}
	}

	XtRealizeWidget(shell);
	XtAppMainLoop(XtWidgetToApplicationContext(shell));
}
