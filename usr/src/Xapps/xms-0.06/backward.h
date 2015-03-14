/* backward.h - fake some X11R3 Xt features for those who have X11R2 */
/* Copyright (C) 1990 Andreas Gustafsson */

/* This is ugly, but it will go away as soon as people stop using X11R2. */
/* (i.e., not soon...) */

/* Test if we have the X11R2 or X11R3 toolkit.  Seems like XtVersion */
/* is 2002 (!) with X11R2 (at least on our Sony NWS-1510 machines), */
/* 11003 (a lot more logical) with X11R3 and (confused again) 11003 */
/* with X11R4 also... */

#include <X11/IntrinsicP.h> /* needed to define XtVersion */
#if XtVersion < 11003
/* Make it legal to declare XtAppContext variables; the macros below */
/* should ensure that those variables are never used */
typedef struct undefined *XtAppContext;
#define XtAppAddInput(app,source,cond,proc,data) \
  XtAddInput(source,cond,proc,data)
#define XtAppAddTimeOut(app,interval,proc,data) \
  XtAddTimeOut(interval,proc,data)
#define XtAppWarning(app,text) XtWarning(text)
#define XtAppError(app,text) XtError(text)
#define XtRDimension "Dimension"
#define XtRBool "Bool"
#define XtWidgetToApplicationContext(widget) (0)
#define XtReleaseGC XtDestroyGC

/* define topLevelShellWidgetClass */
globalref WidgetClass topLevelShellWidgetClass;

#endif
