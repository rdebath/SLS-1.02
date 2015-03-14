/****************************************************************************/
/* Module FmChmod                                                           */
/*                                                                          */
/* Functions & data for handling the chmod feature                          */
/****************************************************************************/

#include <sys/stat.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>

#include "Am.h"
#include "Fm.h"

#define FORM_WIDTH 96

#define OWNER 0
#define GROUP 1
#define OTHERS 2

#define READ 0
#define WRITE 1
#define EXECUTE 2

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

/****************************************************************************/
/* Data for the chmod window                                                */
/****************************************************************************/

typedef struct {
  Widget w;
  int value;
} ChmodItem;

typedef struct {
  Widget shell;
  Widget label;
  FileWindowRec *fw;
  int file;
  ChmodItem items[3][3];
} ChmodData;

static ChmodData chmode;

/****************************************************************************/
/* Widget Argument lists                                                    */
/****************************************************************************/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) "Change Access Mode" }
};

static Arg label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNfont, (XtArgVal) NULL },
  { XtNjustify, XtJustifyLeft },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight }
};

static Arg tickbox_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) None },
  { XtNwidth, (XtArgVal) 0 },
  { XtNresize, (XtArgVal) False },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight }
};

static Arg *form_args = NULL;

static Arg form2_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) FORM_WIDTH },
  { XtNdefaultDistance, (XtArgVal) 0 },
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
/* Strings to display in labels                                             */
/****************************************************************************/

static String big_labels[] = { "Owner", "Group", "Others" };

static String small_labels[] = { "r", "w", "x" };

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static FmCallbackProc chmodRestoreCb, chmodOkCb, chmodCancelCb;

static void setupTicks()
{
  register int i,j;
  struct stat *stats;

  stats = &chmode.fw->files[chmode.file]->stats;

  chmode.items[OWNER][READ].value     = (stats->st_mode) & S_IRUSR;
  chmode.items[OWNER][WRITE].value    = (stats->st_mode) & S_IWUSR;
  chmode.items[OWNER][EXECUTE].value  = (stats->st_mode) & S_IXUSR;

  chmode.items[GROUP][READ].value     = (stats->st_mode) & S_IRGRP;
  chmode.items[GROUP][WRITE].value    = (stats->st_mode) & S_IWGRP;
  chmode.items[GROUP][EXECUTE].value  = (stats->st_mode) & S_IXGRP;

  chmode.items[OTHERS][READ].value    = (stats->st_mode) & S_IROTH;
  chmode.items[OTHERS][WRITE].value   = (stats->st_mode) & S_IWOTH;
  chmode.items[OTHERS][EXECUTE].value = (stats->st_mode) & S_IXOTH;

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      XtVaSetValues(chmode.items[i][j].w, XtNbitmap, 
		    chmode.items[i][j].value ? bm[TICK_BM] : bm[NOTICK_BM],
		    NULL);
    }
  }
}

/****************************************************************************/
  
static void chmodRestoreCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  setupTicks();
}

/****************************************************************************/

static void tickBoxCb(Widget w, XtPointer client_data, XtPointer call_data)
{
  register int i,j;

  i = (int) client_data;

  for (j=0; j<3; j++)
    if (w == chmode.items[i][j].w)
      break;

  if (chmode.items[i][j].value)
    chmode.items[i][j].value = False;
  else
    chmode.items[i][j].value = True;

  XtVaSetValues(chmode.items[i][j].w, XtNbitmap, 
	       chmode.items[i][j].value ? bm[TICK_BM] : bm[NOTICK_BM], NULL);
}

/****************************************************************************/

static void chmodOkCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  mode_t mode;

  mode = chmode.fw->files[chmode.file]->stats.st_mode;
  mode &= ~(S_IRUSR | S_IWUSR | S_IXUSR |
            S_IRGRP | S_IWGRP | S_IXGRP |
            S_IROTH | S_IWOTH | S_IXOTH);

  mode |= chmode.items[OWNER][READ].value     ? S_IRUSR : 0;
  mode |= chmode.items[OWNER][WRITE].value    ? S_IWUSR : 0;
  mode |= chmode.items[OWNER][EXECUTE].value  ? S_IXUSR : 0;

  mode |= chmode.items[GROUP][READ].value     ? S_IRGRP : 0;
  mode |= chmode.items[GROUP][WRITE].value    ? S_IWGRP : 0;
  mode |= chmode.items[GROUP][EXECUTE].value  ? S_IXGRP : 0;

  mode |= chmode.items[OTHERS][READ].value    ? S_IROTH : 0;
  mode |= chmode.items[OTHERS][WRITE].value   ? S_IWOTH : 0;
  mode |= chmode.items[OTHERS][EXECUTE].value ? S_IXOTH : 0;

  if (chdir(chmode.fw->directory)) {
    sysError("Can't change directory:");
    return;
  }

  if (chmod(chmode.fw->files[chmode.file]->name, mode)) {
    char s[0xff];
    sprintf(s, "Can't change modes for %s:", 
	    chmode.fw->files[chmode.file]->name);
    sysError(s);
  }
  else
    chmode.fw->files[chmode.file]->stats.st_mode = mode;

  XtPopdown(chmode.shell);
}

/****************************************************************************/

static void chmodCancelCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  XtPopdown(chmode.shell);
}		   

/****************************************************************************/
/* Button Information                                                       */
/****************************************************************************/

static ButtonRec chmod_buttons[] = {
  { "ok", "Ok", chmodOkCb },
  { "restore", "Restore", chmodRestoreCb },
  { "cancel", "Cancel", chmodCancelCb }
};

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void createChmodPopup()
{
  Widget form, form2, blabel, w;
  register int i,j;

  /* create shell */
  chmode.shell = XtCreatePopupShell("chmod", transientShellWidgetClass,
				   aw.shell, shell_args, XtNumber(shell_args));
  /* create outer form */
  form = XtCreateManagedWidget("form", formWidgetClass, chmode.shell,
			       form_args, XtNumber(form_args) );

  /* create two labels for message */
  label_args[0].value = (XtArgVal) NULL;
  label_args[1].value = (XtArgVal) NULL;
  label_args[3].value = (XtArgVal) FORM_WIDTH*3 + 30;
  label_args[4].value = (XtArgVal) resources.label_font;
  chmode.label = XtCreateManagedWidget("label1", labelWidgetClass, form, 
				       label_args, XtNumber(label_args) );

  form2_args[1].value = (XtArgVal) chmode.label;
  label_args[5].value = (XtArgVal) XtJustifyCenter;

  form2 = NULL;
  /* create smaller forms */
  for (i=0; i<3; i++) {
    form2_args[0].value = (XtArgVal) form2;
    form2 = XtCreateManagedWidget(big_labels[i], formWidgetClass, form,
				  form2_args, XtNumber(form2_args) );

    label_args[0].value = label_args[1].value = (XtArgVal) NULL;
    label_args[2].value = (XtArgVal) big_labels[i];
    label_args[3].value = (XtArgVal) FORM_WIDTH;
    blabel = XtCreateManagedWidget("label", labelWidgetClass, form2,
				   label_args, XtNumber(label_args) );


    w = NULL;
    for (j=0; j<3; j++) {
      label_args[0].value = tickbox_args[0].value = (XtArgVal) w;
      label_args[1].value = (XtArgVal) blabel;
      label_args[2].value = (XtArgVal) small_labels[j];
      label_args[3].value = (XtArgVal) FORM_WIDTH/3;
      w = XtCreateManagedWidget(small_labels[j], labelWidgetClass, form2,
				     label_args, XtNumber(label_args) );

      tickbox_args[1].value = (XtArgVal) w;
      tickbox_args[2].value = (XtArgVal) NULL;
      tickbox_args[3].value = (XtArgVal) FORM_WIDTH/3;
      w = XtCreateManagedWidget(small_labels[j], commandWidgetClass,
				form2, tickbox_args, XtNumber(tickbox_args) );
      XtAddCallback(w, XtNcallback, (XtCallbackProc) tickBoxCb, (XtPointer) i);
      chmode.items[i][j].w = w;
    }
  }
  
  /* create button box & buttons */
  button_box_args[1].value = (XtArgVal) form2;
  w = XtCreateManagedWidget("button box", boxWidgetClass, form, 
			    button_box_args, XtNumber(button_box_args) );
  createButtons(chmod_buttons, XtNumber(chmod_buttons), w, NULL);

  XtRealizeWidget(chmode.shell);
}

/****************************************************************************/

void chmodPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  char message[MAXPATHLEN];
  register int i;

  chmode.fw = fw;

  for (i=0;; i++)
    if (fw->files[i]->selected) {
      chmode.file = i;
      break;
    }
  
  strcpy(message, "Changing access permissions for ");
  strcat(message, chmode.fw->files[chmode.file]->name);
  XtVaSetValues(chmode.label, XtNlabel, (XtArgVal) message, NULL);

  setupTicks();
  
  popupByCursor(chmode.shell, XtGrabExclusive);
}







