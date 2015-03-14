/*
 * $XConsortium: menu.c,v 1.5 89/12/14 21:10:50 converse Exp $
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
 *
 */

#include "xmh.h"
#include "bboxint.h"


void AttachMenuToButton(button, menu, menu_name)
    Button	button;
    Widget	menu;
    char *	menu_name;
{
    Arg		args[3];

    if (button == NULL) return;
    button->menu = menu;
    XtSetArg(args[0], XtNmenuName, XtNewString(menu_name));
    XtSetValues(button->widget, args, (Cardinal) 1);
}


/*ARGSUSED*/
void DoRememberMenuSelection(widget, client_data, call_data)
    Widget	widget;		/* menu entry object */
    XtPointer	client_data;
    XtPointer	call_data;
{
    static Arg	args[] = {
	{ XtNpopupOnEntry,	(XtArgVal) NULL },
    };
    args[0].value = (XtArgVal) widget;
    XtSetValues(XtParent(widget), args, XtNumber(args));
}


void SendMenuEntryEnableMsg(button, entry_name, value)
    Button	button;
    char *	entry_name;
    int		value;
{
    Widget	entry;
    static Arg	args[] = { XtNsensitive, (XtArgVal) NULL };

    if ((entry = XtNameToWidget(button->menu, entry_name)) != NULL) {
	args[0].value = (XtArgVal) ((value == 0) ? False : True);
	XtSetValues(entry, args, (Cardinal) 1);
    }
}


void ToggleMenuItem(entry, state)
    Widget	entry;
    Boolean	state;
{
    Arg		args[1];

    XtSetArg(args[0], XtNleftBitmap, (state ? MenuItemBitmap : None));
    XtSetValues(entry, args, (Cardinal) 1);
}
