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
 * StringBrowser - a Mac minifinder-like object for perusing and choosing 
 * from list of strings.
 */

#ifndef ivlook2_6_strbrowser_h
#define ivlook2_6_strbrowser_h

#include <IV-2_6/InterViews/interactor.h>
#include <IV-2_6/InterViews/textstyle.h>

#include <IV-2_6/_enter.h>

static const char* SBDone = "\r\t\007\033";

static const char SBFirstString = 'g';
static const char SBLastString = 'G';
static const char SBSelectAll = 'a';
static const char SBUnselectAll = '\177';
static const char SBUnselectAllAlt = '\010';
static const char SBSelectPreviousString = 'p';
static const char SBSelectNextString = 'n';
static const char SBSelectTopString = '<';
static const char SBSelectBottomString = '>';
static const char SBScrollDown = 'j';
static const char SBScrollUp = 'k';
static const char SBPageDown = ' ';
static const char SBPageUp = 'b';
static const char SBHalfPageDown = 'd';
static const char SBHalfPageUp = 'u';

class ButtonState;
class TextDisplay;

class StringBrowser : public Interactor {
public:
    StringBrowser(
        ButtonState*, int rows, int cols, 
        boolean uniqueSel = true, int highlight = Reversed,
        const char* done = SBDone
    );
    StringBrowser(
        const char* name, ButtonState*, int, int,
        boolean = true, int = Reversed, const char* = SBDone
    );
    virtual ~StringBrowser();

    void Browse();
    void Insert(const char*, int index);
    void Replace(const char*, int index);
    void Append(const char*);
    void Remove(int index);

    int Index(const char*);
    char* String(int);
    int Count();
    void Clear();

    void Select(int index);
    void SelectAll();
    void Unselect(int index);
    void UnselectAll();
    int Selection(int selindex = 0);
    int SelectionIndex(int index);
    int Selections();
    boolean Selected(int index);

    virtual void Handle(Event&);
    virtual void Adjust(Perspective&);
protected:
    void Select(int dot, int mark);
    void Unselect(int dot, int mark);
    void ScrollBy(int, int);
    void ScrollBy(int lines);
    void ScrollTo(int, int);
    void ScrollTo(int index);
    void ScrollToView(IntCoord, IntCoord);
    void GrabScroll(Event&);
    void RateScroll(Event&);

    int Locate(IntCoord, IntCoord);
    boolean DoubleClicked(Event&);

    virtual boolean HandleChar(char);
    virtual boolean LeftButtonDown(Event&);

    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
    virtual void Resize();
protected:
    int rows;
    int columns;
    boolean uniqueSel;
    boolean singleClick;
    int clickDelay;
    int highlight;
    ButtonState* subject;
    const char* done;
private:
    void Init(ButtonState*, int, int, boolean, int, const char*);
    void InitTextDisplay();
    void InitPerspective(boolean scroll_to_top);

    void UpdateSelection(int dot, int mark, int style);
    void UpdateWidth();
    void Note(Event&);

    boolean HandleDownEvent(Event&);
    boolean HandleKeyEvent(Event&);
private:
    char** strbuf;
    int strbufsize;
    int strcount;
    char** selbuf;
    int selbufsize;
    int selcount;

    TextDisplay* display;
    int lineheight;
    unsigned long lasttime;
    IntCoord lastx, lasty;
    int lastdot, lastmark;
};

inline void StringBrowser::Append (const char* s) { Insert(s, strcount); }
inline int StringBrowser::Count () { return strcount; }
inline int StringBrowser::Selections () { return selcount; }
inline boolean StringBrowser::Selected (int i) {return SelectionIndex(i) >= 0;}

#include <IV-2_6/_leave.h>

#endif
