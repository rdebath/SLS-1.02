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
 * TextDisplay - basic text displaying
 */

#ifndef iv2_6_textdisplay_h
#define iv2_6_textdisplay_h

#include <InterViews/enter-scope.h>
#include <InterViews/boolean.h>
#include <InterViews/coord.h>
#include <IV-2_6/InterViews/textstyle.h>

#include <IV-2_6/_enter.h>

enum CaretStyleOptions {
    NoCaret, DefaultCaret, BarCaret, UnderscoreCaret, OutlineCaret
};

class Painter;
class Canvas;

class TextDisplay {
public:
    TextDisplay(boolean autosized = false);
    ~TextDisplay();

    void Draw(Painter*, Canvas*);
    void LineHeight(IntCoord height);
    void TabWidth(IntCoord width);

    void Scroll(int line, IntCoord x, IntCoord y);

    void Resize(IntCoord xmin, IntCoord ymin, IntCoord xmax, IntCoord ymax);
    void Bounds(
	IntCoord& xmin, IntCoord& ymin, IntCoord& xmax, IntCoord& ymax
    );
    void Redraw(IntCoord l, IntCoord b, IntCoord r, IntCoord t);

    void InsertLinesAfter(int line, int count = 1);
    void InsertLinesBefore(int line, int count = 1);
    void DeleteLinesAfter(int line, int count = 1);
    void DeleteLinesBefore(int line, int count = 1);

    void InsertText(int line, int index, const char*, int count);
    void DeleteText(int line, int index, int count);
    void ReplaceText(int line, const char*, int count);

    void Style(int line1, int index1, int line2, int index2, int style);
    void AddStyle(int line1, int index1, int line2, int index2, int style);
    void RemoveStyle(int line1, int index1, int line2, int index2, int style);

    void Caret(int line, int index);
    void CaretStyle(int);

    int LineNumber(IntCoord y);
    int LineIndex(int line, IntCoord x, boolean between = true);

    IntCoord Width();
    IntCoord Height();

    IntCoord Base(int line);
    IntCoord Top(int line);
    IntCoord Left(int line, int index);
    IntCoord Right(int line, int index);
private:
friend class TextLine;

    void Size(int, int);
    TextLine* Line(int, boolean);
    int Index(int);
    void HideCaret();
    void ShowCaret();

    Painter* painter;
    Canvas* canvas;
    boolean autosized;
    IntCoord xmin, xmax;
    IntCoord ymin, ymax;
    IntCoord x0, y0;
    IntCoord width;
    int lineheight;
    int tabwidth;
    TextLine** lines;
    int maxlines;
    int firstline;
    int lastline;
    int topline;
    int bottomline;
    int widestline;
    int caretline;
    int caretindex;
    int caretstyle;
};

#include <IV-2_6/_leave.h>

#endif
