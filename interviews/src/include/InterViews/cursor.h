/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * An input cursor is defined by two 16x16 bitmaps, one that
 * specifies which pixels are to be drawn and one that specifies
 * which pixels are in foreground color and which in background color.
 * If a device does not support a mask the background pixels are not drawn.
 */

#ifndef iv_cursor_h
#define iv_cursor_h

#include <InterViews/enter-scope.h>

static const int cursorHeight = 16;
static const int cursorWidth = 16;

typedef int CursorPattern[cursorHeight];

class Color;
class CursorRep;
class Bitmap;
class Font;

class Cursor {
public:
    Cursor(
	short xoff, short yoff,			/* hot spot */
	const int* pat, const int* mask,
	const Color* fg = nil, const Color* bg = nil
    );
    Cursor(
	const Bitmap* pat, const Bitmap* mask,
	const Color* fg = nil, const Color* bg = nil
    );
    Cursor(
	const Font*, int pat, int mask,
	const Color* fg = nil, const Color* bg = nil
    );
    Cursor(int, const Color* fg = nil, const Color* bg = nil);
    ~Cursor();

    static void init();

    CursorRep* rep();
private:
    CursorRep* rep_;
};

inline CursorRep* Cursor::rep() { return rep_; }

/*
 * Predefined cursors.
 */

extern Cursor* defaultCursor;
extern Cursor* arrow;
extern Cursor* crosshairs;
extern Cursor* ltextCursor;
extern Cursor* rtextCursor;
extern Cursor* hourglass;
extern Cursor* upperleft;
extern Cursor* upperright;
extern Cursor* lowerleft;
extern Cursor* lowerright;
extern Cursor* noCursor;

#endif
