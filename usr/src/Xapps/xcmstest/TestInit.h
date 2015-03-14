#ifndef TESTINIT
#ifndef LINT
#ifdef RCS_ID
#endif 
#endif 

#ifndef LIBTEST
#include "LibTest.h"
#ifndef LIBTEST
#define LIBTEST
#endif
#endif
#ifndef _X11_XLIB
#include <X11/Xlib.h>
#ifndef _X11_XLIB
#define _X11_XLIB
#endif
#endif


#ifdef AUTOHEADER
#else
#endif

#ifndef	GLOBAL
#  define	GLOBAL
#endif

extern Display	*pDpy;
extern GC	gc;


extern void
TestInit();
#ifdef XDEBUG
#endif 
#ifdef NEED_COLORMAP
#endif 
#ifdef NEED_WINDOW
#endif 
#ifdef NEED_GC
#endif 
#ifdef NEED_FONTS
#endif 
extern void
TestCleanup();
#ifdef NEED_GC
#endif 
#ifdef NEED_WINDOW
#endif 
#ifdef NEED_FONTS
#endif 
#ifndef TESTINIT
#define TESTINIT
#endif
#endif
