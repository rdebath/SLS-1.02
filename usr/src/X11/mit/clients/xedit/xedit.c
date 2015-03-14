/* $XConsortium: xedit.c,v 1.27 91/02/17 15:48:25 dave Exp $ */
 
/*
 *			  COPYRIGHT 1987
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Digital Equipment Corporation not be 
 * used in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 */

#include "xedit.h"

static XtActionsRec actions[] = {
{"quit", DoQuit}
};

static Atom wm_delete_window;

Widget textwindow, messwidget, labelwindow, filenamewindow;

void ResetSourceChanged();

static void makeButtonsAndBoxes();

Display *CurDpy;

struct _app_resources app_resources;

#define Offset(field) XtOffsetOf(struct _app_resources, field)

static XtResource resources[] = {
   {"enableBackups", "EnableBackups", XtRBoolean, sizeof(Boolean),
         Offset(enableBackups), XtRImmediate, FALSE},
   {"backupNamePrefix", "BackupNamePrefix", XtRString, sizeof(char *),
         Offset(backupNamePrefix),XtRString, ""},
   {"backupNameSuffix", "BackupNameSuffix", XtRString, sizeof(char *),
         Offset(backupNameSuffix),XtRString, ".BAK"}
};

#undef Offset

void
main(argc, argv)
int argc;
char **argv;
{
  XtAppContext appcon;
  Widget top;
  String filename = NULL;

  top = XtAppInitialize(&appcon, "Xedit", NULL, 0, &argc, argv, NULL, NULL, 0);

  XtAppAddActions(appcon, actions, XtNumber(actions));
  XtOverrideTranslations
      (top, XtParseTranslationTable ("<Message>WM_PROTOCOLS: quit()"));
  
  XtGetApplicationResources(top, (XtPointer) &app_resources, resources,
			    XtNumber(resources), NULL, 0);

  CurDpy = XtDisplay(top);

  if (argc > 1) {
    Boolean exists;
    filename = argv[1];

    switch ( CheckFilePermissions(filename, &exists)) {
    case NO_READ:
	if (exists)
	    fprintf(stderr, 
		    "File %s exists, and could not opened for reading.\n", 
		    filename);
	else
	    fprintf(stderr, "File %s %s %s",  filename, "does not exist,",
		    "and the directory could not be opened for writing.\n");
	exit(1);
    case READ_OK:
    case WRITE_OK:
	makeButtonsAndBoxes(top, filename);
	break;
    default:
	fprintf(stderr, "%s %s", "Internal function MaybeCreateFile()",
		"returned unexpected value.\n");
	exit(1);
    }
  }  
  else
      makeButtonsAndBoxes(top, NULL);

  XtRealizeWidget(top);
  XDefineCursor(XtDisplay(top),XtWindow(top),
		XCreateFontCursor( XtDisplay(top), XC_left_ptr));
  
  wm_delete_window = XInternAtom(XtDisplay(top), "WM_DELETE_WINDOW",
				 False);
  (void) XSetWMProtocols (XtDisplay(top), XtWindow(top),
			  &wm_delete_window, 1);
  
  XtAppMainLoop(appcon);
}

static void
makeButtonsAndBoxes(parent, filename)
Widget parent;
char * filename;
{
  Widget outer, b_row;
  Arg arglist[10];
  Cardinal num_args;

  outer = XtCreateManagedWidget( "paned", panedWidgetClass, parent,
				NULL, ZERO);
 
  b_row= XtCreateManagedWidget("buttons", panedWidgetClass, outer, NULL, ZERO);
  {
    MakeCommandButton(b_row, "quit", DoQuit);
    MakeCommandButton(b_row, "save", DoSave);
    MakeCommandButton(b_row, "load", DoLoad);
    filenamewindow = MakeStringBox(b_row, "filename", filename); 
  }
  XtCreateManagedWidget("bc_label", labelWidgetClass, outer, NULL, ZERO);

  num_args = 0;
  XtSetArg(arglist[num_args], XtNeditType, XawtextEdit); num_args++;
  messwidget = XtCreateManagedWidget("messageWindow", asciiTextWidgetClass,
				      outer, arglist, num_args);

  num_args = 0;
  if (filename != NULL) 
    XtSetArg(arglist[num_args], XtNlabel, filename); num_args++;

  labelwindow = XtCreateManagedWidget("labelWindow",labelWidgetClass, 
				      outer, arglist, num_args);

  num_args = 0;
  XtSetArg(arglist[num_args], XtNtype, XawAsciiFile); num_args++;
  XtSetArg(arglist[num_args], XtNeditType, XawtextEdit); num_args++;
  textwindow =  XtCreateManagedWidget("editWindow", asciiTextWidgetClass, 
				      outer, arglist, num_args);

  if (filename != NULL)
      DoLoad();
  else
      ResetSourceChanged(textwindow);
}

/*	Function Name: Feep
 *	Description: feeps the bell.
 *	Arguments: none.
 *	Returns: none.
 */

void
Feep()
{
  XBell(CurDpy, 0);
}
