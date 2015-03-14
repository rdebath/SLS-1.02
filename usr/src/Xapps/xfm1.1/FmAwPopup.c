/****************************************************************************/
/* Module FmAwPopup.c                                                       */
/*                                                                          */
/* Functions & data for creating the popup 'install application' window     */
/****************************************************************************/

#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>

#include "Am.h"

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

static Widget install_popup;

static int app_number;

static char app_name[MAXAPPSTRINGLEN], app_cmd[MAXAPPSTRINGLEN],
  app_icon[MAXAPPSTRINGLEN];

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static FmCallbackProc installOkCb, installCancelCb;

static void installOkCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int result;

  result = installApplication(app_name, app_cmd, app_icon);

  XtPopdown(install_popup);

  if (app_number != -1 && result == 0)
    removeApplication(app_number);

  updateApplicationDisplay();
}

/*****************************************************************************/

static void installCancelCb(Widget w, FileWindowRec *fw, 
			    XtPointer call_data)
{
  XtPopdown(install_popup);
}

/*****************************************************************************/
/* Question and button data                                                  */
/*****************************************************************************/

static QuestionRec install_questions[] = {
  { "Name", app_name, MAXAPPSTRINGLEN, NULL },
  { "Command", app_cmd, MAXAPPSTRINGLEN, NULL },
  { "Icon", app_icon, MAXAPPSTRINGLEN, NULL }
};

static ButtonRec install_buttons[] = {
  { "install", "Install", installOkCb },
  { "cancel", "Cancel", installCancelCb }
};

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void createInstallPopup()
{
  install_popup = createPopupQuestions("install", "Install Application", None,
		       install_questions, XtNumber(install_questions),
		       install_buttons, XtNumber(install_buttons));
}

/*----------------------------------------------------------------------------*/

void installNewPopup()
{
  register int i;

  for (i=0; i < XtNumber(install_questions); i++) {
    install_questions[i].value[0] = '\0';
    XtVaSetValues(install_questions[i].widget, XtNstring, 
		  install_questions[i].value, NULL);
  }

  app_number = -1;

  popupByCursor(install_popup, XtGrabExclusive);
}

/*----------------------------------------------------------------------------*/

void installExistingPopup()
{
  register int i;

  for (i=0; i<aw.n_apps; i++)
    if (aw.apps[i].selected) {
      app_number = i;
      strcpy(install_questions[0].value, aw.apps[i].name);
      strcpy(install_questions[1].value, aw.apps[i].cmd);
      strcpy(install_questions[2].value, aw.apps[i].icon);
      break;
    }

  for (i=0; i < XtNumber(install_questions); i++) {
    XtVaSetValues(install_questions[i].widget, XtNstring, 
		  install_questions[i].value, NULL);
  }

  popupByCursor(install_popup, XtGrabExclusive);
}
