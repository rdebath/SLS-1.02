/*
 * $XConsortium: Dialog.c,v 1.10 91/07/24 15:46:20 converse Exp $
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
 * Author:  Davor Matic, MIT X Consortium
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xmu/CharSet.h>
    
#include "Dialog.h"

#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))

static void SetDialogButton();

static XtActionsRec actions_table[] = {
  {"set-dialog-button", SetDialogButton},
};

static DialogButton dialog_buttons[] = {
    {"yes", Yes},
    {"no", No},
    {"maybe", Maybe},
    {"okay", Okay},
    {"abort", Abort},
    {"cancel", Cancel},
    {"retry", Retry},
};

static unsigned long selected;

/* ARGSUSED */
static void SetSelected(w, clientData, callData) /* ARGSUSED */
     Widget w;
     XtPointer clientData, callData;
{
    String name = (String)clientData;
    int i;
    
    for (i = 0; i < XtNumber(dialog_buttons); i++) 
	if (!strcmp(dialog_buttons[i].name, name)) 
	    selected |= dialog_buttons[i].flag;
}

/* ARGSUSED */
static void SetDialogButton(w, event, argv, argc)
     Widget w;         /* not used */
     XEvent *event;    /* not used */
     String *argv;
     Cardinal *argc;  
{
  char button_name[80];
  XtPointer dummy = NULL;
  int i;

  for (i = 0; i < *argc; i++) {
    XmuCopyISOLatin1Lowered (button_name, argv[i]);
    SetSelected(w, button_name, dummy);
  }
}

static Boolean firstTime = True;

Dialog CreateDialog(top_widget, name, options)
     Widget top_widget;
     String name;
     unsigned long options;
{
    int i;
    Dialog popup;

    popup = (Dialog) XtMalloc(sizeof(_Dialog));

    if (popup) {
        if (firstTime) {
	  XtAddActions(actions_table, XtNumber(actions_table));
	  firstTime = False;
	}
	popup->top_widget = top_widget;
	popup->shell_widget = XtCreatePopupShell(name, 
						 transientShellWidgetClass, 
						 top_widget, NULL, 0);
	popup->dialog_widget = XtCreateManagedWidget("dialog", 
						     dialogWidgetClass,
						     popup->shell_widget, 
						     NULL, 0);
	for (i = 0; i < XtNumber(dialog_buttons); i++)
	    if (options & dialog_buttons[i].flag)
		XawDialogAddButton(popup->dialog_widget, 
				   dialog_buttons[i].name, 
				   SetSelected, dialog_buttons[i].name);
	popup->options = options;
	return popup;
    }
    else
	return NULL;
}

void PopdownDialog(popup, answer)
    Dialog popup;
    String *answer;
{
    if (answer)
	*answer = XawDialogGetValueString(popup->dialog_widget);
    
    XtPopdown(popup->shell_widget);
}

unsigned long PopupDialog(popup, message, suggestion, answer, grab)
    Dialog popup;
    String message, suggestion, *answer;
    XtGrabKind grab;
{
  Position popup_x, popup_y, top_x, top_y;
  Dimension popup_width, popup_height, top_width, top_height, border_width;
  int n;
  Arg wargs[4];

  n = 0;
  XtSetArg(wargs[n], XtNlabel, message); n++;
  XtSetArg(wargs[n], XtNvalue, suggestion); n++;
  XtSetValues(popup->dialog_widget, wargs, n);

  XtRealizeWidget(popup->shell_widget);

  n = 0;
  XtSetArg(wargs[n], XtNx, &top_x); n++;
  XtSetArg(wargs[n], XtNy, &top_y); n++;
  XtSetArg(wargs[n], XtNwidth, &top_width); n++;
  XtSetArg(wargs[n], XtNheight, &top_height); n++;
  XtGetValues(popup->top_widget, wargs, n);

  n = 0;
  XtSetArg(wargs[n], XtNwidth, &popup_width); n++;
  XtSetArg(wargs[n], XtNheight, &popup_height); n++;
  XtSetArg(wargs[n], XtNborderWidth, &border_width); n++;
  XtGetValues(popup->shell_widget, wargs, n);

  popup_x = max(0, 
	min(top_x + ((Position)top_width - (Position)popup_width) / 2, 
	    (Position)DisplayWidth(XtDisplay(popup->shell_widget), 
		   DefaultScreen(XtDisplay(popup->shell_widget))) -
	    (Position)popup_width - 2 * (Position)border_width));
  popup_y = max(0, 
	min(top_y + ((Position)top_height - (Position)popup_height) / 2,
	    (Position)DisplayHeight(XtDisplay(popup->shell_widget), 
		    DefaultScreen(XtDisplay(popup->shell_widget))) -
	    (Position)popup_height - 2 * (Position)border_width));
  n = 0;
  XtSetArg(wargs[n], XtNx, popup_x); n++;
  XtSetArg(wargs[n], XtNy, popup_y); n++;
  XtSetValues(popup->shell_widget, wargs, n);

  selected = None;

  XtPopup(popup->shell_widget, grab);
  XWarpPointer(XtDisplay(popup->shell_widget), 
	       XtWindow(popup->top_widget),
	       XtWindow(popup->shell_widget), 
	       0, 0, top_width, top_height,
	       popup_width / 2, popup_height / 2);

  while ((selected & popup->options) == None) {
      XEvent event;
      XtNextEvent(&event);
      XtDispatchEvent(&event);
  }

  PopdownDialog(popup, answer);

  return (selected & popup->options);
}
