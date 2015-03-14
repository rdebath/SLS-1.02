/*****************************************************************************/
/* Module FmFw.c                                                             */
/*                                                                           */
/* functions & data for creating a file window, and various functions        */
/* related to file windows                                                   */
/*****************************************************************************/

#include <pwd.h>
#include <time.h>
#include <sys/stat.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>

#include "Am.h"
#include "Fm.h"

#define FW_WIDTH 400
#define FW_HEIGHT 300
#define DEF_ICON_WIDTH 75
#define DEF_TREE_ICON_WIDTH 48
#define TEXT_PADDING 10

/*****************************************************************************/
/*                         PUBLIC DATA                                       */
/*****************************************************************************/

FileWindowList file_windows = NULL;

/*****************************************************************************/
/*                         STATIC DATA                                       */
/*****************************************************************************/

static MenuItemRec operations_list[] = {
  { "info", "Info",  infoPopup },
  { "delete", "Delete",  deleteItems },
  { "rename", "Rename ...",  renameCb },
  { "access", "Change Permissions ...",  chmodPopup },
  { "make", "Make Directory ...",  mkdirPopup }
};

static MenuItemRec move_list[] = {
  { "up", "Up",  fileUpCb },
  { "specify", "Specify ...",  movePopup },
  { "home", "Home",  mainHomeCb },
};

static MenuItemRec select_list[] = {
  { "select all", "Select All",  fileSelectAllCb },
  { "select some", "Select Some ...",  selectPopup },
  { "deselect all", "Deselect All",  fileDeselectCb }
};

static MenuItemRec options_list[] = {
  { "tree", "Tree",  fileTreeCb },
  { "icons", "Icons",  fileIconsCb },
  { "textop", "Text",  fileTextCb },
  { "line1", NULL,  NULL },
  { "sortname", "Sort By Name",  fileSortNameCb },
  { "sortsize", "Sort By Size",  fileSortSizeCb },
  { "sortmtime", "Sort By Date",  fileSortMTimeCb },
  { "line2", NULL,  NULL },
  { "hide directories", "Hide Directories",  fileShowDirsCb },
  { "mix directories/files", "Mix Directories/Files",
       fileDirsFirstCb },
};


static ButtonRec file_buttons[] = {
  { "open", "Open",  fileOpenButtonCb },
  { "close", "Close",  fileCloseCb }
};

/*****************************************************************************/
/* Widget argument lists                                                     */
/*****************************************************************************/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) NULL },
  { XtNiconPixmap, (XtArgVal) NULL }
};

static Arg form_args[] = {
  { XtNdefaultDistance, (XtArgVal) 0 }
};

static Arg button_box_args[] = {
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft },
};

static Arg label_args[] = {
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) FW_WIDTH },
  { XtNfont, (XtArgVal) NULL },
  { XtNresize, (XtArgVal) False },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight }
};

static Arg viewport_args[] = {
  { XtNfromVert, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) FW_WIDTH },
  { XtNheight, (XtArgVal) FW_HEIGHT },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainBottom },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight },
  { XtNallowVert, (XtArgVal) True }
};

static Arg icon_box_args[] = {
  { XtNwidth, (XtArgVal) 0 },
  { XtNtranslations, (XtArgVal) NULL }
};

static Arg tree_box_args[] = {
  { XtNwidth, (XtArgVal) 1 },
  { XtNdefaultDistance, (XtArgVal) 0 },
  { XtNheight, (XtArgVal) 1 }
};

static Arg icon_form_args[] = {
  { XtNdefaultDistance, (XtArgVal) 0 },
  { XtNwidth, (XtArgVal) 0 }
};

static Arg tree_form_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNdefaultDistance, (XtArgVal) 0 },
  { XtNwidth, (XtArgVal) 0 },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg icon_toggle_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 }
};

static Arg icon_label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 },
};

static Arg text_label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNjustify, XtJustifyLeft },
  { XtNhorizDistance, (XtArgVal) TEXT_PADDING },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

static Arg text_toggle_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNjustify, XtJustifyLeft },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

static Arg arrow_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNsensitive, (XtArgVal) True },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

static Arg line_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

/*****************************************************************************/
/* Translation tables                                                        */
/*****************************************************************************/

static char tree_translations_s[] = "\
  <Enter> : maybeHighlight()\n\
  <Leave> : unhighlight()\n\
  <Btn1Down>,<Btn1Up> : maybeToggle()\n\
  <Btn2Down> : fileBeginDrag()\n\
  <Btn3Down> : fileBeginCopy()\n";

static char dir_translations_s[] = "\
  <Enter> : maybeHighlight()\n\
  <Leave> : unhighlight()\n\
  <Btn1Up>(2) : fileOpenDir()\n\
  <Btn1Down>,<Btn1Up> : maybeToggle()\n\
  <Btn2Down> : fileBeginDrag()\n\
  <Btn3Down> : fileBeginCopy()\n";

static char file_translations_s[] = "\
  <Btn1Down>,<Btn1Up> : maybeToggle()\n\
  <Btn2Down> : fileBeginDrag()\n\
  <Btn3Down> : fileBeginCopy()\n";

static char label_translations_s[] = "\
  <Btn1Up>(2) : renameObject()\n";

static char exec_translations_s[] = "\
  <Enter> : maybeHighlight()\n\
  <Leave> : unhighlight()\n\
  <Btn1Up>(2) : fileOpenFile()\n\
  <Btn1Down>,<Btn1Up> : maybeToggle()\n\
  <Btn2Down> : fileBeginDrag()\n\
  <Btn3Down> : fileBeginCopy()\n";

static char iconbox_translations_s[] = "\
  <Btn2Down> : fileBeginDrag()\n\
  <Btn3Down> : fileBeginCopy()\n";

/*****************************************************************************/
/* Action Tables                                                             */
/*****************************************************************************/

static XtActionsRec file_actions[] = {
  { "maybeToggle", maybeToggle },
  { "maybeHighlight", maybeHighlight },
  { "fileOpenDir", fileOpenDir },
  { "fileBeginDrag", fileBeginDrag },
  { "fileBeginCopy", fileBeginCopy },
  { "fileOpenFile", fileOpenFile },
  { "renameObject", renameObject }
};

/*****************************************************************************/
/* Parsed translations                                                       */
/*****************************************************************************/

static XtTranslations dir_translations, file_translations, 
  iconbox_translations, tree_translations, exec_translations, 
  label_translations;

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static int longestName(FileWindowRec *fw)
{
  register int i,l;
  int longest = 0;

  for (i=0; i<fw->n_files; i++)
    if ((l = XTextWidth(resources.icon_font, fw->files[i]->name, 
			strlen(fw->files[i]->name))) > longest)
      longest = l;
  return longest;
}

/*----------------------------------------------------------------------------*/

static void createFileIcons(FileWindowRec *fw)
{
  register int i;
  Dimension width;
  FileRec *file;

  XtVaGetValues(fw->viewport, XtNwidth, &width, NULL);
  icon_box_args[0].value = (XtArgVal) width;

  width = longestName(fw);
  if (width < DEF_ICON_WIDTH)
    width = DEF_ICON_WIDTH;
  icon_form_args[1].value = (XtArgVal) width;
  icon_toggle_args[4].value = (XtArgVal) width;
  icon_label_args[4].value = (XtArgVal) width;

  fw->icon_box = XtCreateWidget("icon box",  boxWidgetClass,
    fw->viewport, icon_box_args, XtNumber(icon_box_args) );

  for (i=0; i < fw->n_files; i++) {
    file = fw->files[i];
    file->icon.form = XtCreateManagedWidget(file->name,
      formWidgetClass, fw->icon_box, icon_form_args,
      XtNumber(icon_form_args) );

    if (S_ISDIR(file->stats.st_mode)) {
      icon_toggle_args[3].value = (XtArgVal) dir_translations;
      if (file->sym_link)
	icon_toggle_args[2].value = (XtArgVal) bm[DIRLINK_BM];
      else
	icon_toggle_args[2].value = (XtArgVal) bm[DIR_BM];
    }
    else if (file->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) {
      icon_toggle_args[3].value = (XtArgVal) exec_translations;
      if (file->sym_link)
	icon_toggle_args[2].value = (XtArgVal) bm[EXECLINK_BM];
      else
	icon_toggle_args[2].value = (XtArgVal) bm[COMMAND_BM];
    }
    else {
      icon_toggle_args[3].value = (XtArgVal) file_translations;
      if (file->sym_link)
	icon_toggle_args[2].value = (XtArgVal) bm[SYMLNK_BM];
      else
	icon_toggle_args[2].value = (XtArgVal) bm[FILE_BM];
    }

    file->icon.toggle = XtCreateManagedWidget("icon",
      toggleWidgetClass, file->icon.form, icon_toggle_args,
      XtNumber(icon_toggle_args) );

    icon_label_args[0].value = (XtArgVal) NULL;
    icon_label_args[1].value = (XtArgVal) file->icon.toggle;
    icon_label_args[2].value = (XtArgVal) file->name;
    file->icon.label = XtCreateManagedWidget("label",
      labelWidgetClass, file->icon.form, icon_label_args,
      XtNumber(icon_label_args) );
  }
}

/*----------------------------------------------------------------------------*/

static void createTextDisplay(FileWindowRec *fw)
{
  register int i, l;
  Widget w;
  Dimension width, m_width, name_w, size_w, perm_w, own_w, date_w;
  char s[10], name[FILENAME_MAX];
  struct passwd *pw;
  char **owners;
  FileRec *file;

  XtVaGetValues(fw->viewport, XtNwidth, &width, NULL);
  icon_box_args[0].value = (XtArgVal) width;

  m_width = XTextWidth(resources.icon_font, "m", 1);
  name_w = longestName(fw) + 2*m_width;
  size_w = m_width * 7;
  perm_w = m_width * 9;
  date_w = m_width * 20;
  
  if (resources.show_owner) {
    owners = (char **) XtMalloc(fw->n_files * sizeof(char *));
    own_w = 0;
    for (i=0; i<fw->n_files; i++) {
      pw = getpwuid(fw->files[i]->stats.st_uid);
      owners[i] = XtNewString(pw->pw_name);
      l = XTextWidth(resources.icon_font, owners[i], strlen(owners[i]));
      if (l > own_w)
	own_w = l;
    }
  }

  fw->icon_box = XtCreateWidget("icon box",  boxWidgetClass,
				fw->viewport, icon_box_args,
				XtNumber(icon_box_args) );

  for (i=0; i<fw->n_files; i++) {
    file = fw->files[i];
    if (S_ISDIR(file->stats.st_mode)) {
      sprintf(name, "[%s]", file->name);
      text_toggle_args[2].value = (XtArgVal) name;
      text_toggle_args[5].value = (XtArgVal) dir_translations;
    }
    else  {
      text_toggle_args[2].value = (XtArgVal) file->name;
      if (file->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))
	text_toggle_args[5].value = (XtArgVal) exec_translations;
      else
	text_toggle_args[5].value = (XtArgVal) file_translations;
    }

    file->icon.form = XtCreateManagedWidget(file->name,
      formWidgetClass, fw->icon_box, icon_form_args,
      XtNumber(icon_form_args) );

    text_toggle_args[0].value = (XtArgVal) NULL;
    text_toggle_args[4].value = (XtArgVal) name_w;
    w = file->icon.toggle = XtCreateManagedWidget("name", 
      toggleWidgetClass, file->icon.form, text_toggle_args,
      XtNumber(text_toggle_args) );

    if (resources.show_length) {
      sprintf(s, "%ld", (long) file->stats.st_size);
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal) s;
      text_label_args[4].value = (XtArgVal) size_w;
      text_label_args[5].value = (XtArgVal) XtJustifyRight;
      w = XtCreateManagedWidget("size", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }

    if (resources.show_owner) {
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal) owners[i];
      text_label_args[4].value = (XtArgVal) own_w;
      text_label_args[5].value = (XtArgVal) XtJustifyLeft;
      w = XtCreateManagedWidget("owner", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }

    if (resources.show_perms) {
      makePermissionsString(s, file->stats.st_mode);
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal) s;
      text_label_args[4].value = (XtArgVal) perm_w;
      text_label_args[5].value = (XtArgVal) XtJustifyLeft;
      w = XtCreateManagedWidget("permissions", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }

    if (resources.show_date) {
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal)ctime(&file->stats.st_mtime);
      text_label_args[4].value = (XtArgVal) date_w;
      text_label_args[5].value = (XtArgVal) XtJustifyLeft;
      w = XtCreateManagedWidget("date", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }
  }

  if (resources.show_owner) {
    for(i=0; i<fw->n_files; i++)
      XTFREE(owners[i]);
    XTFREE(owners);
  }
}


/*----------------------------------------------------------------------------*/

/* create a directory icon in position specified by horiz & vert */
static Widget createDirIcon(FileWindowRec *fw, int i, Widget horiz,Widget vert)
{
  FileRec *file = fw->files[i];

  /* create form */
  tree_form_args[0].value = (XtArgVal) horiz;
  tree_form_args[1].value = (XtArgVal) vert;
  file->icon.form = XtCreateManagedWidget(file->name,
    formWidgetClass, fw->icon_box, tree_form_args, XtNumber(tree_form_args) );

  /* create icon */
  icon_toggle_args[0].value = (XtArgVal) NULL;
  icon_toggle_args[1].value = (XtArgVal) NULL;
  icon_toggle_args[2].value = (XtArgVal) bm[DIR_BM];
  icon_toggle_args[3].value = (XtArgVal) tree_translations;
  file->icon.toggle = XtCreateManagedWidget("icon",
    toggleWidgetClass, file->icon.form, icon_toggle_args,
    XtNumber(icon_toggle_args) );

  /* create label */
  icon_label_args[0].value = (XtArgVal) NULL;
  icon_label_args[1].value = (XtArgVal) file->icon.toggle;
  if (i == 0)
    icon_label_args[2].value = (XtArgVal) strrchr(fw->directory, '/') + 1;
  else
    icon_label_args[2].value = (XtArgVal) file->name;
  file->icon.label = XtCreateManagedWidget("label",
    labelWidgetClass, file->icon.form, icon_label_args,
    XtNumber(icon_label_args) );

  return file->icon.form;
}

/*----------------------------------------------------------------------------*/

/* create the icons for the directory display */
static void createTreeDisplay(FileWindowRec *fw)
{
  register int i;
  Widget vert, horiz;
  Pixmap line_bm;
  Dimension width;
  FileList files = fw->files;

  /* find width of icons */
  width = longestName(fw);
  if (width < DEF_TREE_ICON_WIDTH)
    width = DEF_TREE_ICON_WIDTH;
  tree_form_args[3].value = (XtArgVal) width;
  icon_toggle_args[4].value = (XtArgVal) width;
  icon_label_args[4].value = (XtArgVal) width;

  /* create icon box in viewport */
  XtVaGetValues(fw->viewport, XtNwidth, &width, NULL);
  tree_box_args[0].value = (XtArgVal) width;
  fw->icon_box = XtCreateWidget("icon box", formWidgetClass,
    fw->viewport, tree_box_args, XtNumber(tree_box_args) );

  /* The '..' directory is not displayed, and no arrow for '.'  */
  files[1]->icon.form = files[1]->icon.toggle = 
    files[1]->icon.label = NULL;
  files[0]->icon.arrow = NULL;
    
  /* create left arrow */
  arrow_args[0].value = (XtArgVal) NULL;
  arrow_args[1].value = (XtArgVal) NULL;
  if (!permission(files[1], P_EXECUTE)) {
    arrow_args[2].value = bm[NOENTRY_BM];
    arrow_args[3].value = False;
  }
  else {
    arrow_args[2].value = bm[LARROW_BM];
    arrow_args[3].value = True;
  }
  horiz = files[1]->icon.arrow = XtCreateManagedWidget("left arrow",
	commandWidgetClass, fw->icon_box, arrow_args, XtNumber(arrow_args) );
  XtAddCallback(horiz, XtNcallback, (XtCallbackProc) mainArrowCb, fw);

  /* create current directory icon */
  horiz = createDirIcon(fw, 0,  horiz, NULL);

  vert = NULL;
 
  for(i = 2; i < fw->n_files; i++, horiz = files[0]->icon.form) {
    
    /* create line */
    if (i == 2)
      if (fw->n_files == 3)
	line_bm = bm[LLINE_BM];
      else
	line_bm = bm[TLINE_BM];
    else
      if (i == fw->n_files - 1)
	line_bm = bm[CLINE_BM];
      else
	line_bm = bm[FLINE_BM];
    line_args[0].value = (XtArgVal) horiz;
    line_args[1].value = (XtArgVal) vert;
    line_args[2].value = (XtArgVal) line_bm;
    horiz  = XtCreateManagedWidget("line", labelWidgetClass, 
      fw->icon_box, line_args, XtNumber(line_args) );
    
    /* create icon */
    horiz = createDirIcon(fw, i, horiz, vert);
    
    /* create right arrow */
    arrow_args[0].value = (XtArgVal) horiz;
    arrow_args[1].value = (XtArgVal) vert;
    if (!permission(files[i], P_EXECUTE)) {
      arrow_args[2].value = bm[NOENTRY_BM];
      arrow_args[3].value = False;
    }
    else if (files[i]->sym_link) {
      arrow_args[2].value = bm[WAVY_BM];
      arrow_args[3].value = True;
    }
    else {
      arrow_args[2].value = bm[RARROW_BM];
      arrow_args[3].value = True;
    }
    vert = files[i]->icon.arrow = XtCreateManagedWidget("right arrow",
      commandWidgetClass, fw->icon_box, arrow_args, XtNumber(arrow_args) );
    XtAddCallback(vert, XtNcallback, (XtCallbackProc) mainArrowCb, fw);
  }
}

/*****************************************************************************/
/*                        PUBLIC FUNCTIONS                                   */
/*****************************************************************************/

/* initialise the file Windows module */
void initFileWindows()
{
  XtAppAddActions(app_context, file_actions, XtNumber(file_actions));
  dir_translations = XtParseTranslationTable(dir_translations_s);
  file_translations = XtParseTranslationTable(file_translations_s);
  iconbox_translations = XtParseTranslationTable(iconbox_translations_s);
  tree_translations = XtParseTranslationTable(tree_translations_s);
  exec_translations = XtParseTranslationTable(exec_translations_s);
  label_translations = XtParseTranslationTable(label_translations_s);

  icon_box_args[1].value = (XtArgVal) iconbox_translations;
  icon_label_args[5].value = (XtArgVal) label_translations;

  label_args[3].value = (XtArgVal) resources.label_font;
  icon_label_args[3].value = (XtArgVal) resources.icon_font;
  text_toggle_args[3].value = (XtArgVal) resources.icon_font;
  text_label_args[3].value = (XtArgVal) resources.icon_font;
  shell_args[1].value = (XtArgVal) bm[ICON_BM];
}

/*----------------------------------------------------------------------------*/

/* Create a file Window at the specified path, in the specified format */
FileWindowRec *createFileWindow(String path, String title, DisplayType format)
{
  FileWindowRec *fw;
  char *shell_name;
  
  /* put at front of linked list */
  fw = (FileWindowRec *) XtMalloc(sizeof(FileWindowRec));
  fw->next = file_windows;
  file_windows = fw;
  
  /* set up defaults */
  fw->display_type = format;
  fw->sort_type = resources.default_sort_type;
  fw->show_dirs = True;
  fw->dirs_first = True;
  fw->n_selections = 0;
  fw->unreadable = NULL;

  chdir(path);
  getwd(fw->directory);
  
  shell_name = "file window";
  shell_args[0].value = (XtArgVal) title;
  fw->shell = XtCreatePopupShell(shell_name, topLevelShellWidgetClass,
				 aw.shell, shell_args, XtNumber(shell_args) );
  
  /* create form */
  fw->form = XtCreateManagedWidget("form", formWidgetClass, fw->shell,
				   form_args, XtNumber(form_args) );
  
  /* create button box */
  fw->button_box = XtCreateManagedWidget("button box", boxWidgetClass,
					 fw->form, button_box_args, 
					 XtNumber(button_box_args) );
  
  /* create menus & buttons */
  fw->operations_items = createMenu("operations", "Operations", 
				    operations_list, XtNumber(operations_list),
				    4, fw->button_box, (XtPointer) fw);
  fw->select_items = createMenu("select", "Select", select_list, 
				XtNumber(select_list), 4, fw->button_box,
				(XtPointer) fw);
  fw->move_items = createMenu("move", "Move", move_list, XtNumber(move_list),
			      4, fw->button_box, (XtPointer) fw);
  fw->options_items = createMenu("options", "Options", options_list,
				 XtNumber(options_list), 16, fw->button_box,
				 (XtPointer) fw);

  fw->buttons = createButtons(file_buttons, XtNumber(file_buttons),
			      fw->button_box, (XtPointer) fw);
  
  /* create label */
  label_args[0].value = (XtArgVal) fw->button_box;
  label_args[1].value = (XtArgVal) fw->directory;
  fw->label = XtCreateManagedWidget("label", labelWidgetClass, fw->form,
				    label_args, XtNumber(label_args) );
  
  /* create viewport */
  viewport_args[0].value = (XtArgVal) fw->label;
  fw->viewport = XtCreateManagedWidget("viewport", viewportWidgetClass,
				       fw->form, viewport_args, 
				       XtNumber(viewport_args) );

  updateMenus(fw);
  updateCloseButtons();
  return fw;
}

/*----------------------------------------------------------------------------*/

/* Main procedure to create the display in the viewport */
void createFileDisplay(FileWindowRec *fw)
{
  register int i;

  XtVaSetValues(fw->label, XtNlabel, (XtArgVal) fw->directory, NULL);

  fw->icon_box = NULL;

  if (fw->unreadable) {
    XtDestroyWidget(fw->unreadable);
    fw->unreadable = NULL;
  }

  if (readDirectory(&fw->files, &fw->n_files, fw->directory)) {
    sysError("Error reading directory:");
    fw->unreadable = 
      XtVaCreateManagedWidget("label", labelWidgetClass, fw->viewport,
			      XtNlabel, "Directory is unreadable",
			      XtNfont, resources.label_font, NULL);
    return;
  }

  for (i=0; i<fw->n_files; i++)
    fw->files[i]->selected = False;
  fw->n_selections = 0;

  updateMenus(fw);
  
  switch (fw->display_type) {
  case Tree:
    filterDirectory(&fw->files, &fw->n_files, Directories);
    sortDirectory(fw->files+2, fw->n_files-2, fw->sort_type, False);
    createTreeDisplay(fw);
    break;
  case Icons:
    filterDirectory(&fw->files, &fw->n_files, fw->show_dirs ? All : Files);
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createFileIcons(fw);
    break;
  case Text:
    filterDirectory(&fw->files, &fw->n_files, fw->show_dirs ? All : Files);
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createTextDisplay(fw);
    break;
  }

  XtManageChild(fw->icon_box);
}

/*----------------------------------------------------------------------------*/

/* Update the display in the viewport */
void updateFileDisplay(FileWindowRec *fw)
{
  int i;

  zzz();

  if (fw->icon_box)
    XtDestroyWidget(fw->icon_box);
    
  if (fw->files) {
    for (i = 0; i < fw->n_files; i++)
      XTFREE(fw->files[i]);
    XTFREE(fw->files);
  }

  createFileDisplay(fw);

  wakeUp();
}

/*----------------------------------------------------------------------------*/

/* resort the icons in the display */
void reSortFileDisplay(FileWindowRec *fw)
{
  if (fw->unreadable)
    return;

  zzz();

  XtDestroyWidget(fw->icon_box);
  fw->n_selections = 0;
  updateMenus(fw);

  switch (fw->display_type) {
  case Tree:
    sortDirectory(fw->files+2, fw->n_files-2, fw->sort_type, False);
    createTreeDisplay(fw);
    break;
  case Icons:
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createFileIcons(fw);
    break;
  case Text:
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createTextDisplay(fw);
    break;
  }

  XtManageChild(fw->icon_box);

  wakeUp();
}

/*----------------------------------------------------------------------------*/

void reDisplayFileWindow(FileWindowRec *fw)
{
  if (fw->unreadable)
    return;

  zzz();

  XtDestroyWidget(fw->icon_box);
  fw->n_selections = 0;
  updateMenus(fw);

  switch (fw->display_type) {
  case Tree:
    createTreeDisplay(fw);
    break;
  case Icons:
    createFileIcons(fw);
    break;
  case Text:
    createTextDisplay(fw);
    break;
  }

  XtManageChild(fw->icon_box);

  wakeUp();
}

/*----------------------------------------------------------------------------*/

/* Intelligent update - only update the windows needed */
void clearUpdateMarks()
{
  register FileWindowRec *fw;

  for (fw = file_windows; fw; fw = fw->next)
    fw->update = False;
}

void markForUpdate(String path)
{
  register FileWindowRec *fw;

  for (fw = file_windows; fw; fw = fw->next)
    if (!strcmp(path, fw->directory))
      fw->update = True;
}

void intUpdate()
{
  register FileWindowRec *fw;

  for (fw = file_windows; fw; fw = fw->next)
    if (fw->update)
      updateFileDisplay(fw);
}

/*----------------------------------------------------------------------------*/

void updateMenus(FileWindowRec *fw)
{
  if (fw->n_selections > 0) {
    register int i;
    FileList files = fw->files;
    for (i=0; i<fw->n_files; i++)
      if (files[i]->selected && S_ISDIR(files[i]->stats.st_mode)) {
	fillIn(fw->buttons[0]);
	goto out; /* aaargh !! */
      }
  }
  grayOut(fw->buttons[0]);

 out:

  if (fw->n_selections == 1) {
    fillIn(fw->operations_items[0]);
    fillIn(fw->operations_items[1]);
    fillIn(fw->operations_items[2]);
    fillIn(fw->operations_items[3]);
    fillIn(fw->select_items[2]);
  }
  else if (fw->n_selections > 1) {
    grayOut(fw->operations_items[0]);
    fillIn(fw->operations_items[1]);
    grayOut(fw->operations_items[2]);
    grayOut(fw->operations_items[3]);
    fillIn(fw->select_items[2]);
  }
  else {
    grayOut(fw->operations_items[0]);
    grayOut(fw->operations_items[1]);
    grayOut(fw->operations_items[2]);
    grayOut(fw->operations_items[3]);
    grayOut(fw->select_items[2]);
    grayOut(fw->buttons[0]);
  }

  if (fw->display_type == Tree) {
    grayOut(fw->options_items[8]);
    grayOut(fw->options_items[9]);
    noTick(fw->options_items[8]);
    noTick(fw->options_items[9]);
  }
  else {
    fillIn(fw->options_items[8]);
    if (fw->show_dirs) {
      fillIn(fw->options_items[9]);
      noTick(fw->options_items[8]);
      if (fw->dirs_first)
	noTick(fw->options_items[9]);
      else
	tick(fw->options_items[9]);
    }
    else {
      grayOut(fw->options_items[9]);
      tick(fw->options_items[8]);
      noTick(fw->options_items[9]);
    }
  }

  noTick(fw->options_items[0]);
  noTick(fw->options_items[1]);
  noTick(fw->options_items[2]);
  noTick(fw->options_items[4]);
  noTick(fw->options_items[5]);
  noTick(fw->options_items[6]);

  switch (fw->display_type) {
  case Tree:
    tick(fw->options_items[0]);
    break;
  case Icons:
    tick(fw->options_items[1]);
    break;
  case Text:
    tick(fw->options_items[2]);
    break;
  }

  switch (fw->sort_type) {
  case SortByName:
    tick(fw->options_items[4]);
    break;
  case SortBySize:
    tick(fw->options_items[5]);
    break;
  case SortByMTime:
    tick(fw->options_items[6]);
    break;
  }
}

/*----------------------------------------------------------------------------*/

void updateCloseButtons()
{
  register FileWindowRec *fw = file_windows;

  if (fw->next == NULL && !resources.filemgr)
    grayOut(fw->buttons[1]);
  else
    for (; fw; fw = fw->next)
      fillIn(fw->buttons[1]);
}
