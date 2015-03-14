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

#include <InterViews/bitmap.h>
#include <InterViews/cursor.h>
#include <InterViews/event.h>
#include <InterViews/font.h>
#include <InterViews/Bitmaps/hand.bm>
#include <InterViews/Bitmaps/handMask.bm>
#include <InterViews/Bitmaps/dfast.bm>
#include <InterViews/Bitmaps/dfastMask.bm>
#include <InterViews/Bitmaps/ufast.bm>
#include <InterViews/Bitmaps/ufastMask.bm>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/textbuffer.h>
#include <IV-2_6/InterViews/texteditor.h>
#include <IV-2_6/InterViews/textdisplay.h>
#include <OS/math.h>
#include <ctype.h>
#include <memory.h>
#include <string.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

static void ScrollAlign (Alignment a, IntCoord& w, IntCoord& h) {
    switch (a) {
    case TopLeft:
    case CenterLeft:
    case BottomLeft:
    case Left:
    case Top:
    case Bottom:
    case VertCenter:
	w = 0;
	break;
    case TopCenter:
    case Center:
    case BottomCenter:
    case HorizCenter:
	w = w/2;
	break;
    }

    switch (a) {
    case CenterLeft:
    case Center:
    case CenterRight:
    case Left:
    case Right:
    case HorizCenter:
    case VertCenter:
	h = h/2;
	break;
    case BottomLeft:
    case BottomCenter:
    case BottomRight:
    case Bottom:
	h = 0;
	break;
    }
}

/*****************************************************************************/

TextEditor::TextEditor (int r, int c, int t, int h) {
    Init(r, c, t, h);
}

TextEditor::TextEditor (const char* name, int r, int c, int t, int h) {
    SetInstance(name);
    Init(r, c, t, h);
}

void TextEditor::Init (int r, int c, int t, int h) {
    SetClassName("TextEditor");
    text = nil;
    dot = 0;
    mark = 0;
    shaperows = r;
    shapecolumns = c;
    scrollalign = Center;
    lineheight = 1;
    tabsize = t;
    highlight = h;
    display = nil;
    perspective = new Perspective();
}

TextEditor::~TextEditor () {
    delete display;
    Unref(perspective);
}

static Cursor* handCursor;
static Cursor* upCursor;
static Cursor* dnCursor;

void TextEditor::Reconfig () {
    if (handCursor == nil) {
        handCursor = new Cursor(
	    new Bitmap(
		hand_bits, hand_width, hand_height, hand_x_hot, hand_y_hot
	    ),
	    new Bitmap(hand_mask_bits, hand_mask_width, hand_mask_height),
            output->GetFgColor(), output->GetBgColor()
        );

        upCursor = new Cursor(
	    new Bitmap(
		ufast_bits, ufast_width, ufast_height, ufast_x_hot, ufast_y_hot
	    ),
	    new Bitmap(ufast_mask_bits, ufast_mask_width, ufast_mask_height),
            output->GetFgColor(), output->GetBgColor()
        );

        dnCursor = new Cursor(
	    new Bitmap(
		dfast_bits, dfast_width, dfast_height, dfast_x_hot, dfast_y_hot
	    ),
	    new Bitmap(dfast_mask_bits, dfast_mask_width, dfast_mask_height),
            output->GetFgColor(), output->GetBgColor()
        );
    }

    const Font* f = output->GetFont();
    shape->hunits = f->Width("n");
    shape->vunits = f->Height();
    shape->Rect(shape->hunits*shapecolumns, shape->vunits*shaperows);
    shape->Rigid(hfil, hfil, vfil, vfil);
    lineheight = shape->vunits;
    display->LineHeight(lineheight);
    display->TabWidth(tabsize * shape->hunits);
}

void TextEditor::Resize () {
    if (canvas != nil) {
        display->Draw(output, canvas);
        display->Resize(0, 0, xmax, ymax);
        int topmargin = (
            perspective->height - perspective->curheight - perspective->cury
        );
        IntCoord height = ymax + 1;
        IntCoord width = xmax + 1;
        perspective->sy = shape->vunits;
        perspective->ly = height - shape->vunits;
        perspective->sx = shape->hunits;
        perspective->lx = width - shape->hunits;
        perspective->height = display->Height();
        perspective->width = display->Width();
        perspective->cury = perspective->height - topmargin - height;
        perspective->curheight = height;
        perspective->curwidth = width;
        perspective->Update();
    }
}

void TextEditor::Redraw (IntCoord l, IntCoord b, IntCoord r, IntCoord t) {
    if (canvas != nil) {
        display->Draw(output, canvas);
        display->Redraw(l, b, r, t);
    }
}

void TextEditor::Adjust (Perspective& np) {
    float scale = float(perspective->height) / float(np.height);
    ScrollTo(
        perspective->x0 + int((np.curx - np.x0)*scale),
        perspective->y0 + int((np.cury - np.y0)*scale)
    );
    np = *perspective;
}

void TextEditor::Edit (TextBuffer* t, int index) {
    delete display;
    display = new TextDisplay();
    display->Draw(output, canvas);
    display->LineHeight(lineheight);
    display->TabWidth(tabsize * shape->hunits);
    text = t;
    int lines = text->Height();
    for (int i = 0; i < lines; ++i) {
        int bol = text->LineIndex(i);
        int eol = text->EndOfLine(bol);
        display->ReplaceText(i, text->Text(bol, eol), eol - bol);
    }
    perspective->height = display->Height();
    perspective->width = display->Width();
    perspective->curheight = ymax + 1;
    perspective->curwidth = xmax + 1;
    perspective->cury = perspective->height - perspective->curheight;
    perspective->curx = 0;
    perspective->Update();

    Canvas* c = canvas;
    canvas = nil;
    display->Resize(0, 0, xmax, ymax);
    Select(index);
    ScrollToSelection(true);
    canvas = c;
    display->Draw(output, canvas);
    display->Redraw(0, 0, xmax, ymax);
}

void TextEditor::InsertText (const char* s, int count) {
    count = text->Insert(dot, s, count);
    int sline = text->LineNumber(dot);
    int fline = text->LineNumber(dot + count);
    display->Draw(output, canvas);
    if (sline == fline) {
        int offset = text->LineOffset(dot);
        display->InsertText(sline, offset, text->Text(dot), count);
    } else {
        display->InsertLinesAfter(sline, fline-sline);
        for (int i = sline; i <= fline; ++i) {
            int bol = text->BeginningOfLine(text->LineIndex(i));
            int eol = text->EndOfLine(bol);
            display->ReplaceText(i, text->Text(bol, eol), eol-bol);
        }
    }
    if (canvas != nil) {
        int width = display->Width();
        int height = display->Height();
        if (width != perspective->width || height != perspective->height) {
            perspective->cury += height - perspective->height;
            perspective->height = height;
            perspective->width = width;
            perspective->Update();
        }
    }
    Select(dot + count);
}

void TextEditor::DeleteText (int count) {
    int start = dot;
    int finish = dot;
    int c = count;
    while (c > 0) {
        finish = text->NextCharacter(finish);
        --c;
    }
    while (c < 0) {
        start = text->PreviousCharacter(start);
        ++c;
    }
    count = finish - start;
    int sline = text->LineNumber(start);
    int fline = text->LineNumber(finish);
    text->Delete(start, count);
    display->Draw(output, canvas);
    if (sline == fline) {
        int offset = text->LineOffset(start);
        display->DeleteText(sline, offset, count);
    } else {
        int bol = text->BeginningOfLine(start);
        int eol = text->EndOfLine(start);
        display->DeleteLinesAfter(sline, fline-sline);
        display->ReplaceText(sline, text->Text(bol, eol), eol-bol);
    }
    if (canvas != nil) {
        int width = display->Width();
        int height = display->Height();
        if (width != perspective->width || height != perspective->height) {
            perspective->cury += height - perspective->height;
            perspective->height = height;
            perspective->width = width;
            perspective->Update();
        }
    }
    Select(start);
}

void TextEditor::DeleteSelection () {
    if (mark != dot) {
        DeleteText(mark - dot);
    }
}

void TextEditor::BeginningOfSelection () {
    Select(Math::min(mark, dot));
}

void TextEditor::EndOfSelection () {
    Select(Math::max(mark, dot));
}

void TextEditor::BeginningOfWord () {
    if (dot != mark) {
        Select(Math::min(mark, dot));
    } else {
        Select(text->BeginningOfWord(dot));
    }
}

void TextEditor::EndOfWord () {
    if (dot != mark) {
        Select(Math::max(mark, dot));
    } else {
        Select(text->EndOfWord(dot));
    }
}

void TextEditor::BeginningOfLine () {
    if (dot != mark) {
        Select(Math::min(mark, dot));
    } else {
        Select(text->BeginningOfLine(dot));
    }
}

void TextEditor::EndOfLine () {
    if (dot != mark) {
        Select(Math::max(mark, dot));
    } else {
        Select(text->EndOfLine(dot));
    }
}

void TextEditor::BeginningOfText() {
    Select(text->BeginningOfText());
}

void TextEditor::EndOfText() {
    Select(text->EndOfText());
}

void TextEditor::ForwardCharacter (int count) {
    if (mark != dot) {
        Select(Math::max(mark, dot));
    } else {
        int d = dot;
        while (count > 0) {
            d = text->NextCharacter(d);
            --count;
        }
        Select(d);
    }
}

void TextEditor::BackwardCharacter (int count) {
    if (dot != mark) {
        Select(Math::min(mark, dot));
    } else {
        int d = dot;
        while (count > 0) {
            d = text->PreviousCharacter(d);
            --count;
        }
        Select(d);
    }
}

void TextEditor::ForwardLine (int count) {
    if (dot != mark) {
        Select(Math::max(mark, dot));
    } else {
        int d = dot;
        while (count > 0) {
            d = text->BeginningOfNextLine(d);
            --count;
        }
        Select(d);
    }
}

void TextEditor::BackwardLine (int count) {
    if (dot != mark) {
        Select(Math::min(mark, dot));
    } else {
        int d = dot;
        while (count > 0) {
            d = text->BeginningOfLine(text->EndOfPreviousLine(d));
            --count;
        }
        Select(d);
    }
}

void TextEditor::ForwardWord (int count) {
    if (dot != mark) {
        Select(Math::max(mark, dot));
    } else {
        int d = dot;
        while (count > 0) {
            d = text->BeginningOfNextWord(d);
            --count;
        }
        Select(d);
    }
}

void TextEditor::BackwardWord (int count) {
    if (dot != mark) {
        Select(Math::min(mark, dot));
    } else {
        int d = dot;
        while (count > 0) {
            d = text->BeginningOfWord(text->EndOfPreviousWord(d));
            --count;
        }
        Select(d);
    }
}

void TextEditor::ForwardPage (int count) {
    int pagesize = perspective->curheight / perspective->sy;
    ForwardLine(pagesize * count);
}

void TextEditor::BackwardPage (int count) {
    int pagesize = perspective->curheight / perspective->sy;
    BackwardLine(pagesize * count);
}

void TextEditor::ScrollToSelection (boolean always) {
    display->Draw(output, canvas);
    int line = text->LineNumber(dot);
    int offset = text->LineOffset(dot);
    IntCoord l = display->Left(line, offset);
    IntCoord r = display->Right(line, offset);
    IntCoord b = display->Base(line);
    IntCoord t = display->Top(line);

    IntCoord tx = xmax - (r - l);
    IntCoord ty = ymax - (t - b);
    ScrollAlign(scrollalign, tx, ty);

    IntCoord dx = (always || l < 0 || r > xmax) ? l - tx : 0;
    IntCoord dy = (always || b < 0 || t > ymax) ? b - ty : 0;

    if (dx != 0 || dy != 0) {
        ScrollTo(perspective->curx + dx, perspective->cury + dy);
    }
}

void TextEditor::ScrollToView (IntCoord x, IntCoord y) {
    IntCoord dx = x < 0 ? x : x > xmax ? x - xmax : 0;
    IntCoord dy = y < 0 ? y : y > ymax ? y - ymax : 0;
    if (dx != 0 || dy != 0) {
        ScrollTo(perspective->curx + dx, perspective->cury + dy);
    }
}

void TextEditor::ScrollBy (int dx, int dy) {
    if (dx != 0 || dy != 0) {
        ScrollTo(perspective->curx + dx, perspective->cury + dy);
    }
}

void TextEditor::ScrollTo (int x, int y) {
    int maxy = perspective->height - perspective->curheight;
    int miny = Math::min(maxy, -perspective->curheight/2);
    perspective->cury = Math::max(miny, Math::min(y, maxy));
    int minx = 0;
    int maxx = Math::max(minx, perspective->width - perspective->curwidth/2);
    perspective->curx = Math::max(minx, Math::min(x, maxx));
    perspective->Update();
    display->Scroll(0,
        - perspective->curx,
        ymax + perspective->height - (perspective->cury+perspective->curheight)
    );
}

void TextEditor::Select (int d) {
    Select(d, d);
}

void TextEditor::SelectMore (int m) {
    Select(dot, m);
}

void TextEditor::SelectAll () {
    Select(text->EndOfText(), text->BeginningOfText());
}

void TextEditor::Select (int d, int m) {
    int oldl = Math::min(dot, mark);
    int oldr = Math::max(dot, mark);
    int newl = Math::min(d, m);
    int newr = Math::max(d, m);
    display->Draw(output, canvas);
    if (oldl == oldr && newl != newr) {
        display->CaretStyle(NoCaret);
    }
    if (newr < oldl || newl > oldr) {
        if (oldr > oldl) {
            display->RemoveStyle(
                text->LineNumber(oldl), text->LineOffset(oldl),
                text->LineNumber(oldr-1), text->LineOffset(oldr-1),
                highlight
            );
        }
        if (newr > newl) {
            display->AddStyle(
                text->LineNumber(newl), text->LineOffset(newl),
                text->LineNumber(newr-1), text->LineOffset(newr-1),
                highlight
            );
        }
    } else {
        if (newl < oldl) {
            display->AddStyle(
                text->LineNumber(newl), text->LineOffset(newl),
                text->LineNumber(oldl-1), text->LineOffset(oldl-1),
                highlight
            );
        } else if (newl > oldl) {
            display->RemoveStyle(
                text->LineNumber(oldl), text->LineOffset(oldl),
                text->LineNumber(newl-1), text->LineOffset(newl-1),
                highlight
            );
        }
        if (newr > oldr) {
            display->AddStyle(
                text->LineNumber(oldr), text->LineOffset(oldr),
                text->LineNumber(newr-1), text->LineOffset(newr-1),
                highlight
            );
        } else if (newr < oldr) {
            display->RemoveStyle(
                text->LineNumber(newr), text->LineOffset(newr),
                text->LineNumber(oldr-1), text->LineOffset(oldr-1),
                highlight
            );
        }
    }
    if (oldl != oldr && newl == newr) {
        display->CaretStyle(BarCaret);
    }
    if (newl == newr) {
        display->Caret(text->LineNumber(newl), text->LineOffset(newl));
    }
    dot = d;
    mark = m;
}

int TextEditor::Locate (IntCoord x, IntCoord y) {
    display->Draw(output, canvas);
    int line = display->LineNumber(y);
    int index = display->LineIndex(line, x);
    int l = text->LineIndex(line);
    int i = 0;
    while (i < index) {
        l = text->NextCharacter(l);
        i += 1;
    }
    return l;
}

void TextEditor::GrabScroll (Event& e) {
    e.target->GetRelative(e.x, e.y, this);
    int y = e.y;
    int x = e.x;
    Cursor* origCursor = GetCursor();
    SetCursor(handCursor);

    do {
        ScrollBy(0, y - e.y);
        y = e.y;
        x = e.x;
        Poll(e);
    } while (e.middlemouse);

    SetCursor(origCursor);
}

void TextEditor::RateScroll (Event& e) {
    Cursor* origCursor = GetCursor();
    e.target->GetRelative(e.x, e.y, this);
    int y = e.y;
    int x = e.x;

    do {
        ScrollBy(0, e.y - y);
        if (e.y - y < 0) {
            SetCursor(dnCursor);
        } else {
            SetCursor(upCursor);
        }
        Poll(e);
    } while (e.rightmouse);

    SetCursor(origCursor);
}
