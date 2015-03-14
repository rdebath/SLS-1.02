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
 * TextEditor - basic interactive editor for mulit-line text
 */

#ifndef ivlook2_6_texteditor_h
#define ivlook2_6_texteditor_h

#include <IV-2_6/InterViews/interactor.h>

#include <IV-2_6/_enter.h>

class TextDisplay;
class TextBuffer;

class TextEditor : public Interactor {
public:
    TextEditor(int rows, int cols, int tabsize, int highlight);
    TextEditor(const char* name, int r, int c, int t, int h);
    virtual ~TextEditor();

    void Edit(TextBuffer*, int index = 0);

    int Dot();
    int Mark();

    void InsertText(const char*, int);
    void DeleteText(int);
    void DeleteSelection();

    void BackwardCharacter(int = 1),    ForwardCharacter(int = 1);
    void BackwardLine(int = 1),         ForwardLine(int = 1);
    void BackwardWord(int = 1),         ForwardWord(int = 1);
    void BackwardPage(int = 1),         ForwardPage(int = 1);

    void BeginningOfLine(),             EndOfLine();
    void BeginningOfWord(),             EndOfWord();
    void BeginningOfSelection(),        EndOfSelection();
    void BeginningOfText(),             EndOfText();

    void SetScrollAlignment(Alignment);
    Alignment GetScrollAlignment();

    void ScrollToSelection(boolean always = false);
    void ScrollToView(IntCoord x, IntCoord y);
    void ScrollBy(IntCoord dx, IntCoord dy);
    void GrabScroll(Event&);
    void RateScroll(Event&);
    virtual void Adjust(Perspective&);

    void Select(int dot);
    void SelectMore(int mark);
    void SelectAll();
    void Select(int dot, int mark);

    int Locate(IntCoord x, IntCoord y);
protected:
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
    virtual void Resize();

    TextBuffer* text;
    TextDisplay* display;
private:
    void ScrollTo(int x, int y);
    void Init(int r, int c, int t, int h);

    int dot, mark;
    int tabsize;
    int lineheight;
    int highlight;
    int shaperows;
    int shapecolumns;
    Alignment scrollalign;
};

inline int TextEditor::Dot () { return dot; }
inline int TextEditor::Mark () { return mark; }
inline void TextEditor::SetScrollAlignment (Alignment a) { scrollalign = a; }
inline Alignment TextEditor::GetScrollAlignment() { return scrollalign; }

#include <IV-2_6/_leave.h>

#endif
