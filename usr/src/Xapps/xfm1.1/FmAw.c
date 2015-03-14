/****************************************************************************/
/* Module fmaw.c                                                            */
/*                                                                          */
/* Functions & data for creating & maintaining  the application window      */
/****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <memory.h>

#include <X11/Intrinsic.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>

#include "Am.h"

#define APPWIDTH 300
#define MAXAPPSTRINGLEN 0xff
#define DEF_ICON_WIDTH 64

/****************************************************************************/
/*                           PUBLIC DATA                                    */
/****************************************************************************/

AppWindowRec aw;

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

/****************************************************************************/
/* Menu data                                                                */
/****************************************************************************/

static MenuItemRec app_menu[] = {
  { "install", "Install Application ...", appInstallCb },
  { "remove", "Remove Application(s)", appRemoveCb },
  { "save", "Save setup", appSaveCb },
  { "load", "Load setup", appLoadCb }
};

static ButtonRec app_buttons[] = {
  { "close", "Quit", appCloseCb }
};

/****************************************************************************/
/* Widget Argument lists                                                    */
/****************************************************************************/

static Arg form_args[] = {
  { XtNdefaultDistance, 0 }
};

static Arg button_box_args[] = {
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft },
};

static Arg viewport_args[] = {
  { XtNfromVert, (XtArgVal) NULL },
  { XtNwidth, APPWIDTH },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainBottom },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight },
  { XtNallowVert, (XtArgVal) True }
};

static Arg icon_box_args[] = {
  { XtNwidth, 0 },
  { XtNtranslations, (XtArgVal) NULL }
};

static Arg icon_form_args[] = {
  { XtNdefaultDistance, 0 },
  { XtNwidth, 0 }
};

static Arg icon_toggle_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNwidth, 0 }
};

static Arg icon_label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, 0 },
  { XtNinternalWidth, 0 },
  { XtNinternalHeight, 0 }
};

/****************************************************************************/
/* Translation tables                                                       */
/****************************************************************************/

static char app_translations[] = "\
    <Enter> : appMaybeHighlight()\n\
    <Leave> : unhighlight()\n\
    <Btn1Up>(2) : runApp()\n\
    <Btn1Down>,<Btn1Up> : toggle() notify()\n";

/* This is a hack to get the icon box to recognise button events */
static char iconbox_translations[] = "\
    <Btn2Up> : dummy()\n\
    <Btn3Up> : dummy()\n";
  
/****************************************************************************/
/* Action Tables                                                            */
/****************************************************************************/

static void dummy(Widget w, XEvent *event, String *params, 
		       Cardinal *num_params) {}

static XtActionsRec app_actions[] = {
  { "appMaybeHighlight", appMaybeHighlight },
  { "runApp", runApp },
  { "dummy", dummy }
};

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static int longestName()
{
  register int i,l;
  int longest = 0;

  for (i=0; i<aw.n_apps; i++)
    if ((l = XTextWidth(resources.icon_font, aw.apps[i].name, 
			strlen(aw.apps[i].name))) > longest)
      longest = l;
  return longest;
}

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void createApplicationWindow()
{
  XtTranslations t;

  /* Add new actions and parse the translation tables */
  XtAppAddActions(app_context, app_actions, XtNumber(app_actions));

  t = XtParseTranslationTable(app_translations);
  icon_toggle_args[3].value = (XtArgVal) t;

  t = XtParseTranslationTable(iconbox_translations);
  icon_box_args[1].value = (XtArgVal) t;

  icon_label_args[3].value = (XtArgVal) resources.icon_font;

  /* create the install application popup */
  createInstallPopup();

  /* create the form */
  aw.form = XtCreateManagedWidget("form", formWidgetClass, aw.shell,
				  form_args, XtNumber(form_args) );
  
  /* create the button box */
  aw.button_box = XtCreateManagedWidget("button box", boxWidgetClass,
    aw.form, button_box_args, XtNumber(button_box_args) );
  
  /* create the buttons */
  aw.app_items = createMenu("options", "Options", app_menu, 
			    XtNumber(app_menu), 4, aw.button_box, NULL);
  createButtons(app_buttons, XtNumber(app_buttons), aw.button_box, NULL);

  /* create the viewport */
  viewport_args[0].value = (XtArgVal) aw.button_box;
  aw.viewport = XtCreateManagedWidget("viewport", viewportWidgetClass,
    aw.form, viewport_args, XtNumber(viewport_args) );

  aw.n_selections = 0;
  updateAppMenu();
}

/****************************************************************************/

void createApplicationDisplay()
{
  register int i;
  Dimension width;

  for (i=0; i<aw.n_apps; i++)
    aw.apps[i].selected = False;
  aw.n_selections = 0;
  
  updateAppMenu();

  XtVaGetValues(aw.viewport, XtNwidth, &width, NULL);
  icon_box_args[0].value = (XtArgVal) width;

  aw.icon_box = XtCreateWidget("icon box", boxWidgetClass,
    aw.viewport, icon_box_args, XtNumber(icon_box_args) );

  if (aw.n_apps == 0)
    XtVaCreateManagedWidget("label", labelWidgetClass, aw.icon_box,
			    XtNlabel, "No configured applications",
			    XtNfont, resources.label_font, NULL);
  else {
    width = longestName();
    if (width < DEF_ICON_WIDTH)
      width = DEF_ICON_WIDTH;
    icon_form_args[1].value = (XtArgVal) width;
    icon_label_args[4].value = (XtArgVal) width;
    icon_toggle_args[4].value = (XtArgVal) width;
    
    for (i=0; i < aw.n_apps; i++) {
      aw.apps[i].form = XtCreateManagedWidget(aw.apps[i].name,
					      formWidgetClass, aw.icon_box,
					      icon_form_args,
					      XtNumber(icon_form_args) );
      icon_toggle_args[2].value = aw.apps[i].icon_bm;
      aw.apps[i].toggle = XtCreateManagedWidget("icon", toggleWidgetClass,
						aw.apps[i].form,
						icon_toggle_args,
						XtNumber(icon_toggle_args) );
      XtAddCallback(aw.apps[i].toggle, XtNcallback,
		    (XtCallbackProc)appToggleCb, NULL);
      icon_label_args[1].value = (XtArgVal) aw.apps[i].toggle;
      icon_label_args[2].value = (XtArgVal) aw.apps[i].name;
      aw.apps[i].label = XtCreateManagedWidget("label", labelWidgetClass,
					       aw.apps[i].form,
					       icon_label_args,
					       XtNumber(icon_label_args) );
    };
  }

  XtManageChild(aw.icon_box);
}

/****************************************************************************/

void updateApplicationDisplay()
{
  XtDestroyWidget(aw.icon_box);
  createApplicationDisplay();
}

/****************************************************************************/

void updateAppMenu()
{
  if (aw.n_selections == 0) {
    grayOut(aw.app_items[1]);
    fillIn(aw.app_items[0]);
  }
  else {
    fillIn(aw.app_items[1]);
    if (aw.n_selections > 1)
      grayOut(aw.app_items[0]);
    else
      fillIn(aw.app_items[0]);
  }
}

/****************************************************************************/

void readApplicationBitmaps()
{
  register int i;
  Screen *screen;
  unsigned int width, height;
  int x_hot, y_hot;

  screen = XtScreen(aw.shell);

  for (i=0; i<aw.n_apps; i++)
    if (!aw.apps[i].icon[0])
      aw.apps[i].icon_bm = bm[APP_DEF_BM];
    else 
      if ((aw.apps[i].icon_bm = XmuLocateBitmapFile(screen, aw.apps[i].icon, 
						    NULL, 0, &width, &height, 
						    &x_hot, &y_hot)) == None) {
	error("Cannot read bitmap for", aw.apps[i].name);
	aw.apps[i].icon_bm = bm[APP_DEF_BM];
      }
}
    
/****************************************************************************/

static Boolean nextLine(FILE *fp, char *line)
{
  register int i = 0;
  int c;

  if (feof(fp))
    return False;

  c = fgetc(fp);
  for (; c != '\n' && c != EOF; c = fgetc(fp))
    line[i++] = (char) c;

  if (c == EOF && i == 0)
    return False;

  line[i] = '\0';
  return True;
}

/****************************************************************************/

void readApplicationData(String path)
{
  FILE *fp;
  char s[MAXAPPSTRINGLEN];
  register int i;
  
  aw.n_apps = 0;
  aw.apps = NULL;
  
  if (!(fp = fopen(path, "r"))) {
    error("No applications file", "");
    return;
  }
  
  for (i=0; nextLine(fp, s); i++) {
    aw.apps = (AppList) XTREALLOC(aw.apps, (i+1)*sizeof(AppRec) );
    strcpy(aw.apps[i].name, s);
    if (!nextLine(fp, aw.apps[i].icon) || !nextLine(fp, aw.apps[i].cmd)) {
      error("Error in applications data file", "");
      XTFREE(aw.apps);
      aw.apps = NULL;
      aw.n_apps = 0;
      return;
    }
  }

  aw.n_apps = i;
  
  if (fclose(fp)) {
    sysError("Error reading applications:");
    XTFREE(aw.apps);
    aw.apps = NULL;
    aw.n_apps = 0;
    return;
  }

  readApplicationBitmaps();
}

/****************************************************************************/

int writeApplicationData(String path)
{
  FILE *fp;
  register int i;
  
  if (! (fp = fopen(path, "w") )) {
    sysError("Error saving applications:");
    return -1;
  }
  
  for (i=0; i < aw.n_apps; i++)
    fprintf(fp, "%s\n%s\n%s\n", aw.apps[i].name, aw.apps[i].icon, 
	    aw.apps[i].cmd);
  
  if (fclose(fp)) {
    sysError("Error saving applications:");
    return -1;
  }

  return 0;
}

/****************************************************************************/

void freeApplicationResources(AppRec *app)
{
  if (app->icon_bm != bm[APP_DEF_BM])
    XFreePixmap(XtDisplay(aw.shell), app->icon_bm);
}

/****************************************************************************/

void removeApplication(int i)
{
  int size;

  freeApplicationResources(&aw.apps[i]);
  if (size = (aw.n_apps - i - 1) * sizeof(AppRec))
    memcpy(&aw.apps[i], &aw.apps[i+1], size);
  aw.n_apps--;
  aw.apps = (AppRec *) XTREALLOC(aw.apps, aw.n_apps * sizeof(AppRec));
}  

/****************************************************************************/

int installApplication(char *name, char *cmd, char *icon)
{
  register int i;
  Pixmap icon_bm;
  Screen *screen;
  unsigned int width, height;
  int x_hot, y_hot;

  if (name[0] == '\0' || cmd[0] == '\0') {
    error("Invalid application definition", "");
    return -1;
  }

  screen = XtScreen(aw.shell);

  if (icon[0] == '\0')
    icon_bm = bm[APP_DEF_BM];
  else if ((icon_bm = XmuLocateBitmapFile(screen, icon, NULL, 0, &width,
					  &height, &x_hot, &y_hot)) == None) {
    error("I can't find the icon", "you have specified.");
    return -1;
  }

  i = aw.n_apps++;
  aw.apps = (AppList) XTREALLOC(aw.apps, aw.n_apps * sizeof(AppRec));

  strcpy(aw.apps[i].name, name);
  strcpy(aw.apps[i].cmd, cmd);
  strcpy(aw.apps[i].icon, icon);
  aw.apps[i].icon_bm = icon_bm;
  aw.apps[i].form = aw.apps[i].toggle = aw.apps[i].label = NULL;
 
  return 0;
}



