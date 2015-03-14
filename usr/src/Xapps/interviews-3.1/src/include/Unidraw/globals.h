/*
 * Copyright (c) 1990, 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Global constants, objects, and classes.
 */

#ifndef unidraw_globals_h
#define unidraw_globals_h

#include <IV-2_6/InterViews/defs.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

class CSolver;
class Graphic;
class PSBrush;
class PSColor;
class PSFont;
class PSPattern;
class Resource;
class UList;
class Unidraw;

/*
 * global types
 */

typedef unsigned long ClassId;

enum DragConstraint {
    None = 0x0,
    XFixed = 0x1,
    YFixed = 0x2,
    XYEqual = 0x4,
    HorizOrVert = 0x8,
    Gravity = 0x10
};

enum Mobility {
    Fixed = 0,
    Floating = 1,
    Undef = 2
};

typedef unsigned Orientation;
enum {
    Normal = 0,
    Rotated = 1
};
enum {
    Portrait = 0,
    Landscape = 1
};
enum {
    Vertical = 0,
    Horizontal = 1
};

enum TransMethod {
    In = 0,
    Out = 1,
    InOut = 2
};

/*
 * global data
 */

extern CSolver* csolver;
extern Unidraw* unidraw;

extern PSColor* psblack;
extern PSColor* pswhite;
extern PSPattern* pssolid;
extern PSPattern* psclear;
extern PSPattern* psnonepat;
extern PSBrush* pssingle;
extern PSBrush* psnonebr;
extern PSFont* psstdfont;
extern Graphic* stdgraphic;	    /* like default painter */

static const int CHARBUFSIZE = 256; /* size of char buffers used internally */
static const int HANDLE_SIZE = 4;   /* length of selection handle edge */
static const char MARK[] = "%I";    /* marks beginning of input */
static const int PIN_RAD = 5;	    /* radius of pin connector view */
static const int SLOP = 2;	    /* hit detection tolerance */

/*
 * global functions
 */

extern void NormalRect(Coord& left, Coord& bottom, Coord& right, Coord& top);
                                    /* assures left < right, bottom < top */
extern void GetLine(const char*, int, int beg,int& end,int& size,int& nextBeg);
                                    /* extract lines from a string */
extern void GetAlignmentPoint(Graphic*, Alignment, float&, float&);
                                    /* return alignment point on graphic */
extern void Ref(Resource*);         /* calls Reference if resource is nonnil */

extern char* strnew(const char*);   /* return a copy of the given string */

#include <IV-2_6/_leave.h>

#endif
