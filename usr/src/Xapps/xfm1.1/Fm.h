/* This will be an include file for the whole file manager package,
   conatining mainly type definitions and stuff like that */

#ifndef FM_H
#define FM_H

#include <stdio.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/param.h>

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024
#endif

/*---FmDirs-------------------------------------------------------------------*/

/* structure containing the widget ids of a icon */
typedef struct {
	Widget form, toggle, label, arrow;
} IconRec;

/* structure into which the directory information is read */
typedef struct {
	char name[FILENAME_MAX];
	Boolean sym_link;
	struct stat stats;
	IconRec icon;
	Boolean selected;
} FileRec, **FileList;

/* enumerated arguments passed to functions */
typedef enum { Files, Directories, All } FilterType;
typedef enum { SortByName, SortBySize, SortByMTime } SortType;

#define P_READ 0x1
#define P_WRITE 0x2
#define P_EXECUTE 0x4

/* public functions */
int readDirectory(FileList *fl_return, int *n_return, String path);
void filterDirectory(FileList *fl_return, int *n_return, FilterType type);
void sortDirectory(FileList fl, int n, SortType type, Boolean dirs_first);
int permission(FileRec *file, int perms);
void makePermissionsString(char *s, int perms);
void fnSub(char *fn);
char *fnSubA(char *fn);

/*---FmFw----------------------------------------------------------------------*/

typedef enum { Tree, Icons, Text } DisplayType;

typedef struct _FileWindowRec {
  struct _FileWindowRec *next;
  DisplayType display_type;
  Boolean show_dirs, dirs_first;
  Boolean update;
  SortType sort_type;
  Widget shell, form, button_box, label, viewport, icon_box;
  char directory[MAXPATHLEN];
  FileList files;
  int n_files;
  int n_selections;
  Widget *operations_items, *select_items, *move_items, *options_items;
  Widget *buttons;
  Widget unreadable;
} FileWindowRec, *FileWindowList;

extern FileWindowList file_windows;

void initFileWindows();
void createFileDisplay(FileWindowRec *fw);
FileWindowRec *createFileWindow(String path, String title, DisplayType format);
void updateFileDisplay(FileWindowRec *fw);
void reSortFileDisplay(FileWindowRec *fw);
void reDisplayFileWindow(FileWindowRec *fw);

void clearUpdateMarks();
void markForUpdate(String path);
void intUpdate();

void updateMenus(FileWindowRec *fw);
void updateCloseButtons();

/* Braindamaged Intrinsic.h gives no way to declare a callback properly */
typedef void FmCallbackProc(Widget w, FileWindowRec *fw,
			    XtPointer call_data);

/*---FmFwCb--------------------------------------------------------------------*/

FmCallbackProc
  fileOpenButtonCb, fileSelectAllCb, fileDeselectCb, fileTreeCb, fileIconsCb,
  fileTextCb, fileSortNameCb, fileSortSizeCb, fileSortMTimeCb, fileShowDirsCb,
  fileDirsFirstCb, fileCloseCb, mainHomeCb, mainArrowCb, fileUpCb;

/*---FmFwActions---------------------------------------------------------------*/

typedef enum { SingleFile, MultipleFiles, Executable, Directory } FileType;

typedef struct {
  Widget dragged_from;
  FileWindowRec *fw;
  FileType type;
} MoveInfo;

extern MoveInfo move_info;
extern Boolean dragging;

void maybeHighlight(Widget w, XEvent *event, String *params, 
		    Cardinal *num_params);
void maybeToggle(Widget w, XEvent *event, String *params, 
		 Cardinal *num_params);
void fileOpenDir(Widget w, XEvent *event, String *params,
		 Cardinal *num_params);
void fileBeginDrag(Widget w, XEvent *event, String *params, 
		   Cardinal *num_params);
void fileBeginCopy(Widget w, XEvent *event, String *params, 
		   Cardinal *num_params);
void treeOpenDir(Widget w, XEvent *event, String *params, 
		 Cardinal *num_params);
void fileOpenFile(Widget w, XEvent *event, String *params, 
		  Cardinal *num_params);
void renameObject(Widget w, XEvent *event, String *params, 
		     Cardinal *num_params);

/*---FmBitmaps----------------------------------------------------------------*/

#define LLINE_BM 0
#define TLINE_BM 1
#define FLINE_BM 2
#define CLINE_BM 3
#define LARROW_BM 4
#define RARROW_BM 5
#define DIR_BM 6
#define FILE_BM 7
#define NOENTRY_BM 8
#define ICON_BM 9
#define TICK_BM 10
#define NOTICK_BM 11
#define COMMAND_BM 12
#define FILES_BM 13
#define EXCLAM_BM 14
#define FILEMSK_BM 15
#define DIRMSK_BM 16
#define COMMANDMSK_BM 17
#define WAVY_BM 18
#define SYMLNK_BM 19
#define WATCH_BM 20
#define WATCHMSK_BM 21
#define DIRLINK_BM 22
#define EXECLINK_BM 23
#define APP_DEF_BM 24
#define APPMGR_BM 25

/* Cursors */
#define FILE_CUR 0
#define FILES_CUR 1
#define NOENTRY_CUR 2
#define DIR_CUR 3
#define COMMAND_CUR 4
#define WATCH_CUR 5

extern Cursor *curs;
extern Pixmap *bm;

void readBitmaps();
void freeBitmaps();

/*---FmChmod------------------------------------------------------------------*/

void createChmodPopup();
FmCallbackProc chmodPopup;

/*---FmConfirm----------------------------------------------------------------*/

void createConfirmPopup();
int confirm(String s1, String s2, String s3);

/*---FmDelete-----------------------------------------------------------------*/

FmCallbackProc deleteItems;

/*---FmErrors-----------------------------------------------------------------*/

void createErrorPopup();
void error(String label1, String label2);
void sysError(String label);

/*---FmExec-------------------------------------------------------------------*/

char **makeArgv(int i);
char **expandArgv(char **argv);
void freeArgv(char **argv);
void executeApplication(char *path, char **argv);

/*---FmInfo-------------------------------------------------------------------*/

void createInfoPopup();
FmCallbackProc infoPopup;

/*---FmMain-------------------------------------------------------------------*/

/* Structure containing information about the user */
typedef struct {
  int uid, gid;
  char home[MAXPATHLEN];
  char shell[MAXPATHLEN];
} UserInfo;

typedef struct {
  Boolean appmgr, filemgr;
  XFontStruct *icon_font, *button_font, *menu_font, *label_font, *bold_font,
    *cell_font;
  String app_file_r;
  Boolean confirm_deletes, confirm_moves, confirm_copies;
  SortType default_sort_type;
  DisplayType default_display_type;
  Boolean show_owner, show_perms, show_date, show_length;
  char app_file[MAXPATHLEN];
  int double_click_time;
} Resources;

extern Resources resources;
extern XtAppContext app_context;
extern UserInfo user;

void quit();

/*---FmPopup------------------------------------------------------------------*/

void createMainPopups();
void renamePopup(FileWindowRec *fw, int i);

FmCallbackProc selectPopup, mkdirPopup, movePopup, renameCb;

/*---FmUtils------------------------------------------------------------------*/

/* structures containing information required to set up a menu */
typedef struct {
  String item_name;
  String item_label;
  FmCallbackProc *callback;
} MenuItemRec, *MenuItemList;


/* structures containing information required to set up a button */
typedef struct {
  String button_name;
  String button_label;
  FmCallbackProc *callback;
} ButtonRec, *ButtonList;


/* structure for creating a popup questionaire */
typedef struct {
  String label;
  String value;
  Cardinal length;
  Widget widget;
} QuestionRec, *QuestionList;

/* functions */

void initUtils();

Widget *createMenu(String menu_name, String menu_label, MenuItemList items,
		   Cardinal n_items, Dimension left_margin, Widget parent,
		   XtPointer client_data);
Widget *createButtons(ButtonList buttons, Cardinal n_buttons, Widget parent,
		      XtPointer client_data);
Widget createPopupQuestions(String name, String title, Pixmap bitmap, 
			    QuestionList questions, Cardinal n_questions,
			    ButtonList buttons, Cardinal n_buttons);
void fillIn(Widget w);
void grayOut(Widget w);

void tick(Widget w);
void noTick(Widget w);

void popupByCursor(Widget shell, XtGrabKind grab_kind);

void zzz(void), wakeUp(void);

/*----------------------------------------------------------------------------*/

/* Horrible kludge to avoid warnings, as XtFree is designed to take a (char *) */
#define XTFREE(p) XtFree((void *)(p))
#define XTREALLOC(p,n) XtRealloc((void *)(p),(n))

#endif
