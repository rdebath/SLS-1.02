#ifndef AM_H
#define AM_H

#include "Fm.h"
#include <sys/param.h>

/*---FmAw----------------------------------------------------------------------*/

#define MAXAPPSTRINGLEN 0xff

typedef struct {
	char name[MAXAPPSTRINGLEN];
	char cmd[MAXPATHLEN];
	char icon[MAXPATHLEN];
	Pixmap icon_bm;
	Widget form, toggle, label;
	Boolean selected;
} AppRec, *AppList;

typedef struct {
	Widget shell, form, button_box, viewport, icon_box;
	Widget *app_items;
	AppList apps;
	int n_apps;
	int n_selections;
} AppWindowRec;
 
extern AppWindowRec aw;

void createApplicationWindow();
void createApplicationDisplay();
void updateApplicationDisplay();
void readApplicationData(String path);
int writeApplicationData(String path);
void updateAppMenu();
int installApplication(char *name, char *cmd, char *icon);
void removeApplication(int i);
void freeApplicationResources(AppRec *app);

/*---FmAwCb-------------------------------------------------------------------*/

FmCallbackProc 
  appInstallCb, appRemoveCb, appToggleCb, appSaveCb, appLoadCb, appCloseCb;

/*---FmAwActions--------------------------------------------------------------*/

int  findAppWidget(Widget w);
void appMaybeHighlight(Widget w, XEvent *event, String *params, 
		       Cardinal *num_params);
void runApp(Widget w, XEvent *event, String *params, Cardinal *num_params);
void appEndMove(int i);
void appEndMoveInBox(void);

/*---FmAwPopup----------------------------------------------------------------*/

void installNewPopup();
void installExistingPopup();
void createInstallPopup();

#endif
