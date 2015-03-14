/****************************************************************************/
/* Module FmFwCb                                                            */
/*                                                                          */
/* Routines for creating and managing popup forms and dialog boxes          */
/****************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Toggle.h>

#include "Fm.h"

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

typedef struct {
  FileWindowRec *fw;
  
  Widget mkdir;
  char mkdir_s[FILENAME_MAX];

  Widget move;
  char move_s[FILENAME_MAX];

  Widget rename;
  char rename_file[FILENAME_MAX];
  char rename_s[FILENAME_MAX];

  Widget select;
  char select_s[FILENAME_MAX];
} PopupsRec;

static PopupsRec popups;

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static FmCallbackProc 
  mkdirOkCb, mkdirCancelCb, moveOkCb, moveCancelCb, renameOkCb, renameCancelCb,
  selectAddCb, selectCancelCb, selectReplaceCb;

static void mkdirOkCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  XtPopdown(popups.mkdir);
  if (chdir(popups.fw->directory))
    sysError("Can't change directory:");
  else if (mkdir(popups.mkdir_s, S_IRUSR | S_IWUSR | S_IXUSR))
    sysError("Can't make directory");
  else {
    clearUpdateMarks();
    markForUpdate(popups.fw->directory);
    intUpdate();
  }
}

/*****************************************************************************/

static void mkdirCancelCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  XtPopdown(popups.mkdir);
}

/*****************************************************************************/

static void moveOkCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  char path[MAXPATHLEN];

  XtPopdown(popups.move);
  fnSub(popups.move_s);

  if (chdir(popups.fw->directory) || chdir(popups.move_s))
    sysError("Can't change directory:");
  else if (!getwd(path))
    error("System error:", path);
  else {
    strcpy(popups.fw->directory, path);
    updateFileDisplay(popups.fw);
  }
}

/*****************************************************************************/

static void moveCancelCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  XtPopdown(popups.move);
}

/*****************************************************************************/

static void renameOkCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  XtPopdown(popups.rename);
  if (chdir(popups.fw->directory))
    sysError("Can't change directory:");
  else if (rename(popups.rename_file, popups.rename_s))
    sysError("Can't rename file:");
  else {
    clearUpdateMarks();
    markForUpdate(popups.fw->directory);
    intUpdate();
  }
}

/*****************************************************************************/

static void renameCancelCb(Widget w, FileWindowRec *fw, 
			   XtPointer call_data)
{
  XtPopdown(popups.rename);
}

/*****************************************************************************/

/* match a pattern with a filename, returning True if the match was correct */
static int fncmp(char *pattern, char *fn)
{
  
  for (;; fn++, pattern++) {
    
    switch (*pattern) {
      
    case '?':
      if (!*fn)
	return False;
      break;
      
    case '*':
      pattern++;
      do
	if (fncmp(pattern,fn))
	  return True;
      while (*fn++);
      return False;
      
    case '[':
      do {
	pattern++;
	if (*pattern == ']')
	  return False;
      } while (*fn != *pattern);
      while (*pattern != ']')
	if (!*pattern++)
	  return False;
      break;
      
    default:
      if (*fn != *pattern)
	return False;
    }
    
    if (!*fn)
      return True;
  };
}

/*****************************************************************************/

static void selectReplaceCb(Widget w, FileWindowRec *fw, 
			    XtPointer call_data)
{
  register int i;

  XtPopdown(popups.select);
  popups.fw->n_selections = 0;
  for (i=0; i<popups.fw->n_files; i++) {
    if (popups.fw->files[i]->icon.toggle) {
      if (fncmp(popups.select_s, popups.fw->files[i]->name)) {
	popups.fw->files[i]->selected = True;
	popups.fw->n_selections++;
      }
      else
	popups.fw->files[i]->selected = False;
      XtVaSetValues(popups.fw->files[i]->icon.toggle, XtNstate,
		    (XtArgVal) popups.fw->files[i]->selected, NULL);
    }
  }
  updateMenus(popups.fw);
}

/*****************************************************************************/

static void selectAddCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;
  
  XtPopdown(popups.select);
  for(i=0; i<popups.fw->n_files; i++)
    if (popups.fw->files[i]->icon.toggle) {
      if (!popups.fw->files[i]->selected && 
	  (fncmp(popups.select_s, popups.fw->files[i]->name))) {
	popups.fw->files[i]->selected = True;
	popups.fw->n_selections++;
	XtVaSetValues(popups.fw->files[i]->icon.toggle, XtNstate, True, NULL);
      }
    }
  updateMenus(popups.fw);
}

/*****************************************************************************/

static void selectCancelCb(Widget w, FileWindowRec *fw, 
			   XtPointer call_data)
{
  XtPopdown(popups.select);
}

/*****************************************************************************/
/* Button information for popups                                             */
/*****************************************************************************/

static ButtonRec mkdir_buttons[] = {
  { "ok", "Ok", mkdirOkCb },
  { "cancel", "Cancel", mkdirCancelCb }
};

static ButtonRec move_buttons[] = {
  { "ok", "Ok", moveOkCb },
  { "cancel", "Cancel", moveCancelCb }
};

static ButtonRec rename_buttons[] = {
  { "ok", "Ok", renameOkCb },
  { "cancel", "Cancel", renameCancelCb }
};

static ButtonRec select_buttons[] = {
  { "replace", "Replace", selectReplaceCb },
  { "add", "Add", selectAddCb },
  { "cancel", "Cancel", selectCancelCb }
};

/*****************************************************************************/
/* Question information for popups                                           */
/*****************************************************************************/

static QuestionRec make_questions[] = {
  { "New Directory", popups.mkdir_s, FILENAME_MAX, NULL }
};

static QuestionRec move_questions[] = {
  { "Path", popups.move_s, MAXPATHLEN, NULL }
};

static QuestionRec rename_questions[] = {
  { "New name", popups.rename_s, MAXPATHLEN, NULL }
};

static QuestionRec select_questions[] = {
  { "Enter pattern", popups.select_s, FILENAME_MAX, NULL }
};

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void createMainPopups()
{
  /* Make Directory */
  popups.mkdir = createPopupQuestions("mkdir", "Make Directory", bm[DIR_BM], 
				      make_questions, XtNumber(make_questions),
				      mkdir_buttons, XtNumber(mkdir_buttons) );

  /* Change current directory */
  popups.move = createPopupQuestions("move", "Go To New Directory", 
				     bm[RARROW_BM], move_questions, 
				     XtNumber(move_questions), move_buttons,
				     XtNumber(move_buttons) );

  /* rename file */
  popups.rename = createPopupQuestions("rename", "Rename File", bm[FILE_BM], 
			       rename_questions, XtNumber(rename_questions),
			       rename_buttons, XtNumber(rename_buttons) );

  /* Select Some */
  popups.select = createPopupQuestions("select", "Select Some Files", None,
				select_questions, XtNumber(select_questions),
				select_buttons, XtNumber(select_buttons) );

  /* Change Access Mode */
  createChmodPopup();

  /* Info */
  createInfoPopup();

  /* Errors */
  createErrorPopup();

  /* Deletions */
  createConfirmPopup();
}

/*****************************************************************************/
 
void selectPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  popups.fw = fw;
  popups.select_s[0] = '\0';
  XtVaSetValues(select_questions[0].widget, XtNstring, 
		(XtArgVal) popups.select_s, NULL);
  popupByCursor(popups.select, XtGrabExclusive);
}

/*****************************************************************************/

void mkdirPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  popups.fw = fw;
  popups.mkdir_s[0] = '\0';
  XtVaSetValues(make_questions[0].widget, XtNstring, 
		(XtArgVal) popups.mkdir_s, NULL);
  popupByCursor(popups.mkdir, XtGrabExclusive);
}

/*****************************************************************************/

void movePopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  popups.fw = fw;
  popups.move_s[0] = '\0';
  XtVaSetValues(move_questions[0].widget, XtNstring, 
		(XtArgVal) popups.move_s, NULL);
  popupByCursor(popups.move, XtGrabExclusive);
}

/*****************************************************************************/

void renameCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  register int i;

  for (i=0;; i++)
    if (fw->files[i]->selected) {
      renamePopup(fw,i);
      return;
    }
}

void renamePopup(FileWindowRec *fw, int i)
{
  popups.fw = fw;
  popups.rename_s[0] = '\0';
  XtVaSetValues(rename_questions[0].widget, XtNstring, 
		(XtArgVal) popups.rename_s, NULL);

  strcpy(popups.rename_file, fw->files[i]->name);
  popupByCursor(popups.rename, XtGrabExclusive);
}  

/*****************************************************************************/

