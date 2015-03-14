/****************************************************************************/
/* Module FmFwCb                                                            */
/*                                                                          */
/* Callback routines for widgets in a file window                           */
/****************************************************************************/

#include <X11/Intrinsic.h>
#include <X11/Xaw/Toggle.h>

#include <string.h>
#include <stdio.h>

#include "Fm.h"

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void fileOpenButtonCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;
  char pathname[MAXPATHLEN];
  FileWindowRec *new_fw;

  for (i=0; i<fw->n_files; i++) {
    if (fw->files[i]->selected && S_ISDIR(fw->files[i]->stats.st_mode)) {
      strcpy(pathname, fw->directory);
      if (pathname[strlen(pathname)-1] != '/')
	strcat(pathname, "/");
      strcat(pathname, fw->files[i]->name);
      
      new_fw = createFileWindow(pathname, "File Manager",
				resources.default_display_type);
      createFileDisplay(new_fw);
      XtRealizeWidget(new_fw->shell);
      popupByCursor(new_fw->shell, XtGrabNone);
    }
  }
}

/*****************************************************************************/

void fileTreeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->display_type = Tree;
  updateFileDisplay(fw);
}

/*****************************************************************************/

void fileIconsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  DisplayType t = fw->display_type;

  fw->display_type = Icons;
  if (t == Text)
    reDisplayFileWindow(fw);
  else
    updateFileDisplay(fw);
}

/*****************************************************************************/

void fileTextCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  DisplayType t = fw->display_type;

  fw->display_type = Text;
  if (t == Icons)
    reDisplayFileWindow(fw);
  else
    updateFileDisplay(fw);
}

/*****************************************************************************/

void mainHomeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  strcpy(fw->directory, user.home);
  updateFileDisplay(fw);
}

/*****************************************************************************/

void fileSelectAllCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;
  
  for (i=0; i < fw->n_files; i++)
    if (fw->files[i]->selected == False && fw->files[i]->icon.toggle) {
      fw->files[i]->selected = True;
      XtVaSetValues(fw->files[i]->icon.toggle, XtNstate,
		    (XtArgVal) True, NULL);
    }
  fw->n_selections = fw->n_files;
  updateMenus(fw);
}

/*****************************************************************************/

void fileDeselectCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;
  
  for (i=0; i < fw->n_files; i++)
    if (fw->files[i]->selected && fw->files[i]->icon.toggle) {
      fw->files[i]->selected = False;
      XtVaSetValues(fw->files[i]->icon.toggle, XtNstate,
		    (XtArgVal) False, NULL);
    }
  fw->n_selections = 0;
  updateMenus(fw);
}

/*****************************************************************************/

void fileSortNameCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->sort_type = SortByName;
  reSortFileDisplay(fw);
}

/*****************************************************************************/

void fileSortSizeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->sort_type = SortBySize;
  reSortFileDisplay(fw);
}

/*****************************************************************************/

void fileSortMTimeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->sort_type = SortByMTime;
  reSortFileDisplay(fw);
}

/*****************************************************************************/

void fileCloseCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  FileWindowRec *p;

  if (fw == file_windows && fw->next == NULL)
    quit();

  /**** BUG IN X TOOLKIT:  THIS DOES NOT WORK
  XtDestroyWidget(fw->shell)
  *****/

  XtPopdown(fw->shell);

  if (fw == file_windows)
    file_windows = fw->next;
  else {
    for (p = file_windows; p->next != fw; p = p->next);
    p->next = fw->next;
  }

  XTFREE(fw->files);
  XTFREE(fw->operations_items);
  XTFREE(fw->select_items);
  XTFREE(fw->move_items);
  XTFREE(fw->options_items);
  XTFREE(fw->buttons);
  XTFREE(fw);

  updateCloseButtons();
}

/****************************************************************************/

void fileUpCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  chdir(fw->directory);
  chdir("..");
  getwd(fw->directory);
  updateFileDisplay(fw);
}

/****************************************************************************/

void mainArrowCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;

  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->icon.arrow == w) {
      chdir(fw->directory);
      chdir(fw->files[i]->name);
      getwd(fw->directory);
      updateFileDisplay(fw);
      return;
    }
}


/*****************************************************************************/

void fileShowDirsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->show_dirs = !fw->show_dirs;
  updateFileDisplay(fw);
}

/*****************************************************************************/

void fileDirsFirstCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->dirs_first = !fw->dirs_first;
  reSortFileDisplay(fw);
}
