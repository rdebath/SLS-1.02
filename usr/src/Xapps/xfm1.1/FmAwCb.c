/****************************************************************************/
/* Module FmAwCb                                                            */
/*                                                                          */
/* Callback routines for widgets in the application window                  */
/****************************************************************************/

#include <stdio.h>
#include <memory.h>

#include <X11/Intrinsic.h>

#include "Am.h"

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void appInstallCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  if (aw.n_selections)
    installExistingPopup();
  else
    installNewPopup();
}

/**************************************************************************/

void appRemoveCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;

  for (i=0; i<aw.n_apps; i++)
    if (aw.apps[i].selected) {
      removeApplication(i);
      i--;
    }
  updateApplicationDisplay();
}

/**************************************************************************/

void appToggleCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;
  
  for (i=0; i < aw.n_apps; i++)
    if (aw.apps[i].toggle == w)
      if (aw.apps[i].selected) {
	aw.apps[i].selected = False;
	aw.n_selections--;
	break;
      }
      else {
	aw.apps[i].selected = True;
	aw.n_selections++;
	break;
      }
  updateAppMenu();
}

/**************************************************************************/

void appSaveCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  writeApplicationData(resources.app_file);
}

/**************************************************************************/

void appLoadCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;

  for(i=0; i<aw.n_apps; i++)
    freeApplicationResources(&aw.apps[i]);
  XTFREE(aw.apps);

  readApplicationData(resources.app_file);
  updateApplicationDisplay();
}

/**************************************************************************/

void appCloseCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;
  
  for (i=0; i<aw.n_apps; i++)
    freeApplicationResources(&aw.apps[i]);

  quit();
}
