/*****************************************************************************
Copyright 1988, 1989 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************************/

#ifndef VMS
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#else
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#endif
#include <X11/Xfuncs.h>
#ifndef NULL
#define NULL 0
#endif

#define POLY	     1000       /* # (small) items in poly calls	*/
#define MAXROWS	       40       /* Max rows of items in poly calls      */
#define MAXCOLS	       25       /* Max columns of items			*/

#define WIDTH         600	/* Size of large window to work within  */
#define HEIGHT        600

#define CHILDSIZE       8       /* Size of children on windowing tests  */
#define CHILDSPACE      4       /* Space between children		*/

typedef Bool (*InitProc)    (/* XParms xp; Parms p */);
typedef void (*Proc)	    (/* XParms xp; Parms p */);

extern void NullProc	    (/* XParms xp; Parms p */);
extern Bool NullInitProc    (/* XParms xp; Parms p */);

#define VERSION1_2  (1 << 0)
#define VERSION1_3  (1 << 1)
#define VALL	    (VERSION1_2 | VERSION1_3)

typedef unsigned char Version;
    
typedef struct _Parms {
    /* Required fields */
    int  objects;       /* Number of objects to process in one X call	    */

    /* Optional fields.  (Wouldn't object-oriented programming be nice ?)   */
    int  special;       /* Usually size of objects to paint		    */
    char *font, *bfont;
    int  fillStyle;     /* Solid, transparent stipple, opaque stipple, tile */
} ParmRec, *Parms;

typedef struct _XParms {
    Display	    *d;
    Window	    w;
    GC		    fggc;
    GC		    bggc;
    unsigned long   foreground;
    unsigned long   background;
    XVisualInfo     vinfo;
    Bool	    pack;
    Version	    version;
} XParmRec, *XParms;

typedef enum {
    WINDOW,     /* Windowing test, rop has no affect		    */
    ROP,	/* Graphics test, rop has some affect		    */
    NONROP      /* Graphics or overhead test, top has no affect     */
} TestType;

typedef struct _Test {
    char	*option;    /* Name to use in prompt line		    */
    char	*label;     /* Fuller description of test		    */
    InitProc    init;       /* Initialization procedure			    */
    Proc	proc;       /* Timed benchmark procedure		    */
    Proc	passCleanup;/* Cleanup between repetitions of same test     */
    Proc	cleanup;    /* Cleanup after test			    */
    Version     versions;   /* Test in 1.2 only, 1.3 only, or both	    */
    TestType    testType;   /* Windowing, graphics rop, graphics non-rop    */
    int		clips;      /* Number of obscuring windows to force clipping*/
    ParmRec     parms;      /* Parameters passed to test procedures	    */
} Test;

extern Test test[];

#define ForEachTest(x) for (x = 0; test[x].option != NULL; x++)


/*****************************************************************************

About x11perf:

Many graphics benchmarks assume that the graphics device is used to display the
output of a single fancy graphics application, and that the user gets his work
done on some other device, like a terminal.  Such benchmarks usually measure
drawing speed for lines, polygons, text, etc.

Since workstations are not used as standalone graphics engines, but as
super-terminals, x11perf measures window management performance as well as
traditional graphics performace.  x11perf includes benchmarks for the time it
takes to create and map windows (as when you start up an application); to map a
pre-existing set of windows onto the screen (as when you deiconify an
application or pop up a menu); and to rearrange windows (as when you slosh
windows to and fro trying to find the one you want).

x11perf also measures graphics performance for operations not normally used in
standalone graphics displays, but are nonetheless used frequently by X
applications.  Such operations include CopyPlane (used by the PostScript
previewer), scrolling (used in text windows), and various stipples and tiles
(used for CAD and half-toning, respectively).

x11perf DOES NOT attempt to whittle down measurements to a single ``HeXStones''
number.  We consider such numbers to be uninformative at best and misleading at
worst.  x11perf should be used to analyze particular strengths and weaknesses
of servers, and is most useful when used by a server writer who wants to
analyze and improve a server.

For repeatable results, x11perf should be run using a local connection on a
freshly-started server.  The default configuration runs each test 5 times, in
order to see if each trial takes approximately the same amount of time.
Strange glitches should be examined; if non-repeatable I chalk them up to
daemons and network traffic.  Each trial is run for 5 seconds, in order to
reduce random time differences.  The number of objects processed per second is
displayed to 3 significant digits, but you'll be lucky on most UNIX system if
the numbers are actually consistent to 2 digits.

The current program is mostly the responsibility of Joel McCormack.  It is
based upon the x11perf developed by Phil Karlton, Susan Angebranndt, and Chris
Kent, who wanted to assess performance differences between various servers.
Mary Walker, Todd Newman, and I added several tests in order to write and tune
the pmax (DECStation 3100) servers.  For a general release to the world, I've
basically rewritten x11perf to ease making comparisons between widely varying
machines, to cover most important X functionality (the notable exception being
wide lines), and to exercise graphics operations in as many different
orientations and alignments as possible.

******************************************************************************/
