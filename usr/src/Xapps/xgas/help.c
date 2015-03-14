/*
 * help.c
 *   xgas: Copyright 1991 Larry Medwin: @(#)gas.c	1.9 2/9/90
 *   Larry Medwin -- April 11, 1991, added help stuff.
 */

#include "xgas.h"
#include <X11/Shell.h>
extern char quick[];
extern char *man[];
extern char *doc[];

/* static to allow show_text callback to set its args. */
static Widget helpText;

/* Temporarily put the help callbacs here*/
void help_callback(w, widget_to_popup, call_data) /* ARGSUSED */
     Widget     w;
     Widget     widget_to_popup;
     caddr_t    call_data;
{
     XtPopup( widget_to_popup, XtGrabNone);
}

void show_text(w, text, call_data) /* ARGSUSED */
    Widget      w;
    char    	*text;
    caddr_t     call_data;
{
    Arg wargs[1];

    XtSetArg( wargs[0], XtNstring, text);
    XtSetValues( helpText, wargs, 1);
}

void pop_down(w, widget_to_popdown, call_data) /* ARGSUSED */
     Widget     w;
     Widget     widget_to_popdown;
     caddr_t    call_data;
{
     XtPopdown( widget_to_popdown);
}


createHelpWidgets( parent )
     Widget parent;
{
  Widget helpShell, helpFrame, helpQuick, helpMan;
  Widget helpDoc, helpQuit;
  int sizedoc = 0, sizeman;
  char **docp, *doct, *mant;

  /* HELP POPUP */
  helpShell = XtVaCreatePopupShell("helpShell",
				topLevelShellWidgetClass, parent,
				 NULL);
  /* Now add callback for help button */
  XtAddCallback(parent, XtNcallback, (XtCallbackProc)help_callback,
		(XtPointer)helpShell);
  
  /* HELP FRAME */
  helpFrame = XtVaCreateManagedWidget("helpFrame",
				formWidgetClass, helpShell,
				 NULL);

  /* HELP COMMAND BUTTON: QUIT */
  helpQuit = XtVaCreateManagedWidget("helpQuit", commandWidgetClass,
			helpFrame,
			NULL);
  XtAddCallback(helpQuit, XtNcallback, (XtCallbackProc)pop_down, 
		(XtPointer)helpShell);

  /* HELP COMMAND BUTTON: QUICK */
  helpQuick = XtVaCreateManagedWidget("helpQuick", commandWidgetClass,
			helpFrame,
			XtNfromHoriz, (XtPointer)helpQuit,
			XtNhorizDistance, 100,
			NULL);
  XtAddCallback(helpQuick, XtNcallback, (XtCallbackProc)show_text, 
		(char*)quick);

  sizeman = strlen(man[0]) + strlen(man[1]) + 1;
  mant = XtMalloc(sizeman);
  strcpy(mant, man[0]);
  strcat(mant, man[1]);
  
  /* HELP COMMAND BUTTON: MAN */
  helpMan = XtVaCreateManagedWidget("helpMan", commandWidgetClass, helpFrame,
			XtNfromHoriz, (XtPointer)helpQuick,
			NULL);
  XtAddCallback(helpMan, XtNcallback, (XtCallbackProc)show_text, 
		(XtPointer)mant);
  
  /* HELP COMMAND BUTTON: DOC */
  helpDoc = XtVaCreateManagedWidget("helpDoc", commandWidgetClass, helpFrame,
			XtNfromHoriz, (XtPointer)helpMan,
			NULL);
  
  docp = doc;
  sizedoc = strlen(*docp) + 1;
  doct = XtMalloc(sizedoc);
  strcpy(doct, *docp);
  docp++;
  while (*docp[0]) {
    sizedoc += strlen(*docp) + 1;
    doct = XtRealloc(doct, sizedoc);
    strcat(doct, *docp);
    docp++;
  }
  
  XtAddCallback(helpDoc, XtNcallback, (XtCallbackProc)show_text, doct);
  
  /* HELP TEXT */
  helpText = XtVaCreateManagedWidget("helpText",
				     asciiTextWidgetClass, helpFrame,
				     XtNfromVert, (XtPointer)helpQuit,
				     /*XtNtype, XawAsciiString,*/
				     /*XtNuseStringInPlace, True,*/
				     NULL);
}

