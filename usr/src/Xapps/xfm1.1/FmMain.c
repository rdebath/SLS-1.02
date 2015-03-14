/*****************************************************************************/
/* FmMain.c - main module for file manager     (c) S.Marlow 1991             */
/*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Shell.h>

#include "Am.h"
#include "Fm.h"

#define XtRDisplayType "DisplayType"
#define XtRSortType "SortType"

/*****************************************************************************/
/* Public variables                                                          */
/*****************************************************************************/

/* information about the user */
UserInfo user;

/* application resource values */
Resources resources;

/* application context */
XtAppContext app_context;

/*****************************************************************************/
/* Command line options                                                      */
/*****************************************************************************/

static XrmOptionDescRec options[] = {
  { "-appmgr", ".appmgr", XrmoptionNoArg, "True" },
  { "-filemgr", ".filemgr", XrmoptionNoArg, "True" }
};

/*****************************************************************************/
/* Application Resources                                                     */
/*****************************************************************************/

static XtResource resource_list[] = {
  { "appmgr", "Appmgr", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, appmgr), XtRImmediate, (XtPointer) False },
  { "filemgr", "Filemgr", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, filemgr), XtRImmediate, (XtPointer) False },
  { "iconFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, icon_font), XtRString, XtDefaultFont },
  { "buttonFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, button_font), XtRString, XtDefaultFont },
  { "menuFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, menu_font), XtRString, XtDefaultFont },
  { "labelFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, label_font), XtRString, XtDefaultFont },
  { "boldFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, bold_font), XtRString, XtDefaultFont },
  { "cellFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, cell_font), XtRString, XtDefaultFont },
  { "applicationDataFile", "ApplicationDataFile",  XtRString, sizeof(String),
      XtOffsetOf(Resources, app_file_r), XtRString, "~/.xfmrc" },
  { "confirmDeletes", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_deletes), XtRImmediate, (XtPointer) True },
  { "confirmMoves", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_moves), XtRImmediate, (XtPointer) True },
  { "confirmCopies", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_copies), XtRImmediate, (XtPointer) True },
  { "showOwner", "ShowOwner", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_owner), XtRImmediate, (XtPointer) True },
  { "showDate", "ShowDate", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_date), XtRImmediate, (XtPointer) True },
  { "showPermissions", "ShowPermissions", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_perms), XtRImmediate, (XtPointer) True },
  { "showLength", "ShowLength", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_length), XtRImmediate, (XtPointer) True },
  { "defaultDisplayType", "DefaultDisplayType", XtRDisplayType, 
      sizeof(DisplayType), XtOffsetOf(Resources, default_display_type),
      XtRImmediate, (XtPointer) Icons },
  { "defaultSortType", "DefaultSortType", XtRSortType, 
      sizeof(SortType), XtOffsetOf(Resources, default_sort_type),
      XtRImmediate, (XtPointer) SortByName },
  { "doubleClickTime", "DoubleClickTime", XtRInt, sizeof(int), 
      XtOffsetOf(Resources, double_click_time), XtRImmediate, (XtPointer) 500 }
};

/*****************************************************************************/
/* Fallback resources                                                        */
/*****************************************************************************/

static String fallback_resources[] = {
  "*Command.cursor : hand2",
  "*Command.shapeStyle : roundedRectangle",
  "*MenuButton.cursor : hand2",
  "*MenuButton.shapeStyle : roundedRectangle",
  "*popup form*bitmap.borderWidth : 0",
  "*popup form*label.borderWidth : 0",
  "*button box.orientation : horizontal",
  "*button box.borderWidth : 0",
  "*viewport.borderWidth : 0",
  "*viewport.forceBars : True",
  "*viewport.icon box*Label.borderWidth : 0",
  "*viewport.icon box.Command.borderWidth : 0",
  "*viewport.icon box.Form.borderWidth : 0",
  "*viewport.icon box*Toggle.borderWidth : 0",
  "*chmod*Label.borderWidth : 0",
  "*info*Label.borderWidth : 0",
  "*error*Label.borderWidth : 0",
  "*confirm*Label.borderWidth : 0",
  "*Text*translations : #override \\n\
    <Key>Return: no-op() \\n\
    <Key>Linefeed : no-op() \\n\
    Ctrl<Key>J : no-op() \\n",
NULL,
};

/*****************************************************************************/
/* Widget argument lists                                                     */
/*****************************************************************************/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) "Applications Manager" },
};

/*****************************************************************************/
/* Resource converter functions                                              */
/*****************************************************************************/

static void CvtStringToDisplayType(XrmValue *args, Cardinal *n_args,
				   XrmValue *fromVal, XrmValue *toVal)
{
  static DisplayType d;

  if (!strcmp(fromVal->addr, "Tree"))
    d = Tree;
  else if (!strcmp(fromVal->addr, "Icons"))
    d = Icons;
  else if (!strcmp(fromVal->addr, "Text"))
    d = Text;
  else {
    XtStringConversionWarning(fromVal->addr, XtRDisplayType);
    return;
  }
  
  toVal->addr = (caddr_t) &d;
  toVal->size = sizeof(DisplayType);
}

/*****************************************************************************/
 
static void CvtStringToSortType(XrmValue *args, Cardinal *n_args,
				XrmValue *fromVal, XrmValue *toVal)
{
  static SortType d;

  if (!strcmp(fromVal->addr, "SortByName"))
    d = SortByName;
  else if (!strcmp(fromVal->addr, "SortBySize"))
    d = SortBySize;
  else if (!strcmp(fromVal->addr, "SortByDate"))
    d = SortByMTime;
  else {
    XtStringConversionWarning(fromVal->addr, XtRSortType);
    return;
  }
  
  toVal->addr = (caddr_t) &d;
  toVal->size = sizeof(SortType);
}

/*****************************************************************************/
/* Main function                                                             */
/*****************************************************************************/

void main(int argc, char *argv[])
{
  FileWindowRec *fw;
  register char *s;

  /* get some information about the user */
  user.uid = getuid();
  user.gid = getgid();

  if (s = getenv("HOME"))
    strcpy(user.home,s);
  else
    getwd(user.home);

  if (s = getenv("SHELL"))
    strcpy(user.shell,s);
  else
    strcpy(user.shell,"/bin/sh");

  /* initialise the application and create the application shell */
  aw.shell = XtAppInitialize(&app_context, "Xfm", options, XtNumber(options),
			     &argc, argv, fallback_resources, shell_args,
			     XtNumber(shell_args) );

  /* register resource converters */
  XtAppAddConverter(app_context, XtRString, XtRDisplayType, 
		    CvtStringToDisplayType, NULL, ZERO);
  XtAppAddConverter(app_context, XtRString, XtRSortType, 
		    CvtStringToSortType, NULL, ZERO);

  /* get the application resources */
  XtGetApplicationResources(aw.shell, &resources, resource_list,
			    XtNumber(resource_list), NULL, ZERO);

  /* set the multi-click time */
  XtSetMultiClickTime(XtDisplay(aw.shell), resources.double_click_time);

  /* initialise the utilities module */
  initUtils();

  /* create all the bitmaps & cursors needed */
  readBitmaps();
  
  /* Set the icon for the application manager */
  XtVaSetValues(aw.shell, XtNiconPixmap, bm[APPMGR_BM], NULL);

  /* create the main popup shells */
  createMainPopups();

  /* initialise the applications module & create the window*/
  if (!resources.filemgr) {
    strcpy(resources.app_file, resources.app_file_r);
    fnSub(resources.app_file);
    readApplicationData(resources.app_file);
    createApplicationWindow();
    createApplicationDisplay();
    XtRealizeWidget(aw.shell);
  }

  /* initialise the file windows module & create a file window*/
  if (!resources.appmgr) {
    initFileWindows();
    fw = createFileWindow(user.home, "File Manager", Tree);
    createFileDisplay(fw);
    XtRealizeWidget(fw->shell);
    XtPopup(fw->shell, XtGrabNone);
  }

  /* collect & process events */
  XtAppMainLoop(app_context);
}

/*****************************************************************************/

void quit()
{
  Display *dpy = XtDisplay(aw.shell);

  freeBitmaps();
  
  /* X386 dumps core if I try to free the fonts, although Xsun is OK.
     it might have something to do with trying to free the same font twice.
     Anyway, I'll comment this out for now. */
  /*
  XFreeFont(dpy, resources.icon_font);
  XFreeFont(dpy, resources.menu_font);
  XFreeFont(dpy, resources.label_font);
  XFreeFont(dpy, resources.bold_font);
  XFreeFont(dpy, resources.button_font);
  XFreeFont(dpy, resources.cell_font);
  */
  
  XtDestroyApplicationContext(app_context);

  exit(0);
}
