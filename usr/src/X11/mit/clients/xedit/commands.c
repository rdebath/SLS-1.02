/* $XConsortium: commands.c,v 1.32 91/07/09 15:50:59 rws Exp $ */

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
#ifdef CRAY
#include <unistd.h>
#endif

extern Widget textwindow, labelwindow, filenamewindow;

void ResetSourceChanged();

static void ResetDC();

static Boolean double_click = FALSE, source_changed = FALSE;

/*	Function Name: AddDoubleClickCallback(w)
 *	Description: Adds a callback that will reset the double_click flag
 *                   to false when the text is changed.
 *	Arguments: w - widget to set callback upon.
 *                 state - If true add the callback, else remove it.
 *	Returns: none.
 */

AddDoubleClickCallback(w, state)
Widget w;
Boolean state;
{
  Arg args[1];
  static XtCallbackRec cb[] = { {NULL, NULL}, {NULL, NULL} };
 
  if (state) 
    cb[0].callback = ResetDC;
  else
    cb[0].callback = NULL;

  XtSetArg(args[0], XtNcallback, cb);
  XtSetValues(w, args, ONE);
}
  
/*	Function Name: ResetDC
 *	Description: Resets the double click flag.
 *	Arguments: w - the text widget.
 *                 junk, garbage - *** NOT USED ***
 *	Returns: none.
 */

/* ARGSUSED */
static void
ResetDC(w, junk, garbage)
Widget w;
caddr_t junk, garbage;
{
  double_click = FALSE;

  AddDoubleClickCallback(w, FALSE);
}

void
DoQuit()
{
  if( double_click || !source_changed ) 
    exit(0); 

  XeditPrintf("Unsaved changes. Save them, or Quit again.\n");
  Feep();
  double_click = TRUE;
  AddDoubleClickCallback(textwindow, TRUE);
}

char *
makeBackupName(buf, filename)
String buf, filename;
{
  sprintf(buf, "%s%s%s", app_resources.backupNamePrefix,
	  filename, app_resources.backupNameSuffix);
  return (buf);
}
  
#if defined(USG) && !defined(CRAY)
int rename (from, to)
    char *from, *to;
{
    (void) unlink (to);
    if (link (from, to) == 0) {
        unlink (from);
        return 0;
    } else {
        return -1;
    }
}
#endif

void
DoSave()
{
  String filename = GetString(filenamewindow);
  char buf[BUFSIZ];

  if( (filename == NULL) || (strlen(filename) == 0) ){
    XeditPrintf("Save:  no filename specified -- nothing saved\n");
    Feep();
    return;
  }
  
  if (app_resources.enableBackups) {
    char backup_file[BUFSIZ];
    makeBackupName(backup_file, filename);

    if (rename(filename, backup_file) != 0) {
      sprintf(buf, "error backing up file:  %s\n",  backup_file); 
      XeditPrintf(buf);
    }
  }
  
  switch( MaybeCreateFile(filename)) {
  case NO_READ:
  case READ_OK:
      sprintf(buf, "File %s could not be opened for writing.\n", filename);
      break;
  case WRITE_OK:
      if ( XawAsciiSaveAsFile(XawTextGetSource(textwindow), filename) ) {
	  sprintf(buf, "Saved file:  %s\n", filename);
	  ResetSourceChanged(textwindow);
      }
      else 
	  sprintf(buf, "Error saving file:  %s\n",  filename);
      break;
  default:
      sprintf(buf, "%s %s", "Internal function MaybeCreateFile()",
	      "returned unexpected value.\n");
  }

  XeditPrintf(buf);
}

void
DoLoad()
{
    Arg args[5];
    Cardinal num_args = 0;
    String filename = GetString(filenamewindow);
    char buf[BUFSIZ], label_buf[BUFSIZ];

    if ( source_changed && !double_click) {
	XeditPrintf("Unsaved changes. Save them, or press Load again.\n");
	Feep();
	double_click = TRUE;
	AddDoubleClickCallback(textwindow, TRUE);
	return;
    }
    double_click = FALSE;
    
    if ( (filename != NULL)  &&  ((int) strlen(filename) > 0) ) {
	Boolean exists;

	switch( CheckFilePermissions(filename, &exists) ) {
	case NO_READ:
	    if (exists)
		sprintf(buf, "File %s, %s", filename,
			"exists, and could not be opened for reading.\n");
	    else
		sprintf(buf, "File %s %s %s",  filename, "does not exist, and",
			"the directory could not be opened for writing.\n");

	    XeditPrintf(buf);
	    Feep();
	    return;
	case READ_OK:
	    XtSetArg(args[num_args], XtNeditType, XawtextRead); num_args++;
	    sprintf(label_buf, "%s       READ ONLY", filename);
	    sprintf(buf, "File %s opened READ ONLY.\n", filename);
	    break;
	case WRITE_OK:
	    XtSetArg(args[num_args], XtNeditType, XawtextEdit); num_args++;
	    sprintf(label_buf, "%s       Read - Write", filename);
	    sprintf(buf, "File %s opened read - write.\n", filename);
	    break;
	default:
	    sprintf(buf, "%s %s", "Internal function MaybeCreateFile()",
		    "returned unexpected value.\n");
	    XeditPrintf(buf);
	    return;
	}

	XeditPrintf(buf);
	
	if (exists) {
	    XtSetArg(args[num_args], XtNstring, filename); num_args++;
	}
	else {
	    XtSetArg(args[num_args], XtNstring, NULL); num_args++;
	}

	XtSetValues( textwindow, args, num_args);
	
	num_args = 0;
	XtSetArg(args[num_args], XtNlabel, label_buf); num_args++;
	XtSetValues( labelwindow, args, num_args);
	ResetSourceChanged(textwindow);
	return;
    }

    XeditPrintf("Load: No file specified.\n");
    Feep();
}

/*	Function Name: SourceChanged
 *	Description: A callback routine called when the source has changed.
 *	Arguments: w - the text source that has changed.
 *                 junk, garbage - *** UNUSED ***.
 *	Returns: none.
 */

static void
SourceChanged(w, junk, garbage)
Widget w;
XtPointer junk, garbage;
{
    XtRemoveCallback(w, XtNcallback, SourceChanged, NULL);
    source_changed = TRUE;
}

/*	Function Name: ResetSourceChanged.
 *	Description: Sets the source changed to FALSE, and
 *                   registers a callback to set it to TRUE when
 *                   the source has changed.
 *	Arguments: widget - widget to register the callback on.
 *	Returns: none.
 */

void
ResetSourceChanged(widget)
Widget widget;
{
    XtAddCallback(XawTextGetSource(widget), XtNcallback, SourceChanged, NULL);
    source_changed = FALSE;
}
