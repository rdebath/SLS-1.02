/****************************************************************************/
/* Module FmErrors                                                          */
/*                                                                          */
/* Error handling routines                                                  */
/****************************************************************************/

#include <errno.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>

#include "Am.h"
#include "Fm.h"

#define LABEL_WIDTH 300

/* Uggh ...... */
#ifndef linux
extern char *sys_errlist[];
#endif

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

typedef struct {
  Widget shell, label1, label2;
} ErrorPopupRec;

static ErrorPopupRec errors;

static Boolean error_flag;

/****************************************************************************/
/* Widget Argument lists                                                    */
/****************************************************************************/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) "Error" },
};

static Arg *form_args = NULL;

static Arg label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) LABEL_WIDTH },
  { XtNfont, (XtArgVal) NULL },
  { XtNresize, False },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight }
};

static Arg bitmap_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg button_box_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static void errorOkCb(Widget w, XtPointer client_data, XtPointer call_data)
{
  XtPopdown(errors.shell);
  error_flag = True;
}

/****************************************************************************/
/* Button data                                                              */
/****************************************************************************/

static ButtonRec error_buttons[] = {
  { "ok", "Ok", (FmCallbackProc *) errorOkCb }
};

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void createErrorPopup()
{
  Widget form, bitmap, button_box;

  /* create shell */
  errors.shell = XtCreatePopupShell("error", transientShellWidgetClass,
				   aw.shell, shell_args, XtNumber(shell_args));

  /* create outer form */
  form = XtCreateManagedWidget("form", formWidgetClass, errors.shell,
				      form_args, XtNumber(form_args) );

  /* create  bitmap */
  bitmap_args[2].value = (XtArgVal) bm[EXCLAM_BM];
  bitmap = XtCreateManagedWidget("bitmap", labelWidgetClass, form, bitmap_args,
					XtNumber(bitmap_args) );

  /* create label 1 */
  label_args[0].value = (XtArgVal) bitmap;
  label_args[4].value = (XtArgVal) resources.label_font;
  errors.label1 = XtCreateManagedWidget("label1",labelWidgetClass, form,
				       label_args, XtNumber(label_args) );

  /* create label 2 */
  label_args[1].value = (XtArgVal) errors.label1;
  errors.label2 = XtCreateManagedWidget("label2",labelWidgetClass, form,
				       label_args, XtNumber(label_args) );

  /* create button box */
  button_box_args[1].value = (XtArgVal) bitmap;
  button_box = XtCreateManagedWidget("button box", boxWidgetClass, form,
			     button_box_args, XtNumber(button_box_args) );
  createButtons(error_buttons, XtNumber(error_buttons), button_box,
		NULL);
}

/****************************************************************************/

void untilOk()
{
  XEvent e;

  error_flag = False;

  do {
    XtAppNextEvent(app_context, &e);
    XtDispatchEvent(&e);
  } while (!error_flag);
}

/****************************************************************************/

void sysError(String string1)
{
  XtVaSetValues(errors.label1, XtNlabel, string1, NULL);
  XtVaSetValues(errors.label2, XtNlabel, sys_errlist[errno], NULL);
  popupByCursor(errors.shell, XtGrabExclusive);
  untilOk();
}

/****************************************************************************/

void error(String string1, String string2)
{
  XtVaSetValues(errors.label1, XtNlabel, string1, NULL);
  XtVaSetValues(errors.label2, XtNlabel, string2, NULL);
  popupByCursor(errors.shell, XtGrabExclusive);
  untilOk();
}



