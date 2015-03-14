/*****************************************************************************/
/* Module : FmInfo.c                                                         */
/*                                                                           */
/* Routines for creating and initialising the info window.                   */
/*****************************************************************************/

#include <pwd.h>
#include <time.h>
#include <grp.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>

#include "Am.h"
#include "Fm.h"

#define LABEL_WIDTH 200
#define PADDING 30

#define I_NAME 0
#define I_LENGTH 1
#define I_OWNER 2
#define I_GROUP 3
#define I_ACCESS 4
#define I_TYPE 5
#define I_LINK 6
#define I_MODIFY_T 7
#define I_STATUS_T 8
#define N_ITEMS 9

/*****************************************************************************/
/*                          STATIC DATA                                      */
/*****************************************************************************/

typedef struct {
  FileRec *file;
  Widget shell;
  Widget items[N_ITEMS];
} InfoData;

static InfoData info;

/*****************************************************************************/
/* Strings to go in labels                                                   */
/*****************************************************************************/

static String labels[] = {
  "Name",
  "Length",
  "Owner",
  "Group",
  "Access Permissions",
  "Type",
  "Symbolic Link To",
  "Last Modification",
  "Last Status Change"
  };

/*****************************************************************************/
/* Widget argument lists                                                     */
/*****************************************************************************/

static Arg *form_args = NULL;

static Arg left_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNjustify, XtJustifyRight },
  { XtNfont, (XtArgVal) NULL },
  { XtNresize, False },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNhorizDistance, (XtArgVal) PADDING },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg right_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) LABEL_WIDTH },
  { XtNjustify, XtJustifyLeft },
  { XtNfont, (XtArgVal) NULL },
  { XtNresize, False },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNhorizDistance, (XtArgVal) PADDING },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight }
};

static Arg button_box_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) "File Info" }
};

/*****************************************************************************/
/*                         PRIVATE FUNCTIONS                                 */
/*****************************************************************************/

FmCallbackProc infoOkCb;

void infoOkCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  XtPopdown(info.shell);
}

/*****************************************************************************/
/* Button data                                                               */
/*****************************************************************************/

static ButtonRec info_buttons[] = {
  { "ok", "Ok", infoOkCb }
};

/*****************************************************************************/
/*                          PUBLIC FUNCTIONS                                 */
/*****************************************************************************/

void createInfoPopup()
{
  register int i;
  Widget form, w;
  Dimension left_width = 0;

  /* create shell */
  info.shell = XtCreatePopupShell("info", transientShellWidgetClass,
				   aw.shell, shell_args, XtNumber(shell_args));

  /* create outer form */
  form = XtCreateManagedWidget("form", formWidgetClass, info.shell,
			       form_args, XtNumber(form_args) );

  left_args[5].value = (XtArgVal) resources.bold_font;
  right_args[5].value = (XtArgVal) resources.label_font;
  
  for (i=0; i<N_ITEMS; i++) {
    Dimension l;
    l = XTextWidth(resources.bold_font, labels[i], strlen(labels[i]));
    if (l > left_width)
      left_width = l;
  }

  left_args[3].value = (XtArgVal) left_width;

  /* create all the smaller labels */
  w = NULL;
  for (i=0; i<N_ITEMS; i++) {
    left_args[1].value = (XtArgVal) w;
    right_args[1].value = (XtArgVal) w;
    left_args[2].value = (XtArgVal) labels[i];
    w = XtCreateManagedWidget("leftlabel", labelWidgetClass, form,
			      left_args, XtNumber(left_args));

    right_args[0].value = (XtArgVal) w;
    info.items[i] = XtCreateManagedWidget("rightlabel", labelWidgetClass, form,
					  right_args, XtNumber(right_args));
  }

  button_box_args[1].value = (XtArgVal) w;
  w = XtCreateManagedWidget("button box", boxWidgetClass, form,
			    button_box_args, XtNumber(button_box_args));
  createButtons(info_buttons, XtNumber(info_buttons), w, NULL);
}

/*****************************************************************************/

void infoPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  char s[10], link[MAXPATHLEN];
  char *type;
  struct passwd *pw;
  struct group *gp;
  register int i;

  for (i=0;; i++)
    if (fw->files[i]->selected) {
      info.file = fw->files[i];
      break;
    }

  XtVaSetValues(info.items[I_NAME], XtNlabel, (XtArgVal) info.file->name, 
		NULL);

  sprintf(s, "%ld", (long) info.file->stats.st_size);
  XtVaSetValues(info.items[I_LENGTH], XtNlabel, (XtArgVal) s, NULL);

  pw = getpwuid(info.file->stats.st_uid);
  XtVaSetValues(info.items[I_OWNER], XtNlabel, (XtArgVal) pw->pw_name, NULL);

  gp = getgrgid(info.file->stats.st_gid);
  XtVaSetValues(info.items[I_GROUP], XtNlabel, (XtArgVal) gp->gr_name, NULL);

  makePermissionsString(s, info.file->stats.st_mode);
  XtVaSetValues(info.items[I_ACCESS], XtNlabel, (XtArgVal) s, NULL);

  if(info.file->sym_link) {
    type = "Symbolic link";
    if (chdir(fw->directory)) {
      sysError("System error:");
      return;
    }
    
    i = readlink(info.file->name, link, MAXPATHLEN);
    if (i == -1) {
      sysError("Error reading link:");
      return;
    }
    link[i] = '\0';
    
    XtVaSetValues(info.items[I_LINK], XtNlabel, (XtArgVal) link, NULL);
  }    
  else {
    mode_t mode = info.file->stats.st_mode;

    if (S_ISDIR(mode))
      type = "Directory";
    else if (S_ISCHR(mode))
      type = "Character special file";
    else if(S_ISBLK(mode))
      type = "Block special file";
    else if(S_ISREG(mode))
      type = "Ordinary file";
    else if(S_ISSOCK(mode))
      type = "Socket";
    else if(S_ISFIFO(mode))
      type = "Pipe or FIFO special file";

    XtVaSetValues(info.items[I_LINK], XtNlabel, (XtArgVal) "-", NULL);
  }

  XtVaSetValues(info.items[I_TYPE], XtNlabel, (XtArgVal) type, NULL);

  XtVaSetValues(info.items[I_MODIFY_T], XtNlabel, 
		(XtArgVal) ctime(&info.file->stats.st_mtime), NULL);

  XtVaSetValues(info.items[I_STATUS_T], XtNlabel, 
		(XtArgVal) ctime(&info.file->stats.st_ctime), NULL);

  popupByCursor(info.shell, XtGrabExclusive);
}

