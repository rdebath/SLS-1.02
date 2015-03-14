/* $XConsortium: opaque.h,v 1.9 89/07/16 14:37:10 rws Exp $ */

#ifndef OPAQUE_H
#define OPAQUE_H

extern char *defaultFontPath;
extern char *defaultTextFont;
extern char *defaultCursorFont;
extern char *rgbPath;
extern long MaxClients;
extern char isItTimeToYield;
extern char dispatchException;

/* bit values for dispatchException */
#define DE_RESET     1
#define DE_TERMINATE 2

extern int CloseFont();
extern void FreeResource();
extern long TimeOutValue;
extern long ScreenSaverTime;
extern long ScreenSaverInterval;
extern int  ScreenSaverBlanking;
extern int  ScreenSaverAllowExposures;
extern int argcGlobal;
extern char **argvGlobal;

#endif /* OPAQUE_H */
