/*
 * Globally accessible information from xgraph
 */

#ifndef _XGRAPH_H_
#define _XGRAPH_H_

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#define VERSION_STRING	"11.3.2 December 1989"

#define MAXKEYS		50
#define MAXATTR 	8
#define MAXSETS		64
#define MAXBUFSIZE 	120
#define MAXLS		50

#define STRDUP(xx)	(strcpy(malloc((unsigned) (strlen(xx)+1)), (xx)))

typedef unsigned long Pixel;

/* Globally accessible values */
extern Display *disp;			/* Open display            */
extern Visual *vis;			/* Standard visual         */
extern Colormap cmap;			/* Standard colormap       */
extern int screen;			/* Screen number           */
extern int depth;			/* Depth of screen         */

extern void do_hardcopy();	/* Carries out hardcopy    */
extern void ho_dialog();	/* Hardcopy dialog         */
extern void set_X();		/* Initializes X device    */

/* To make lint happy */
extern char *malloc();
extern char *realloc();
extern char *sprintf();
extern char *strcpy();
extern char *strcat();
extern char *rindex();
extern char *index();
extern void exit();
extern void free();
extern double atof();
extern void abort();

#endif /* _XGRAPH_H_ */
