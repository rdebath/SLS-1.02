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
 * StringBrowser implementation.
 */

#include <InterViews/bitmap.h>
#include <InterViews/cursor.h>
#include <InterViews/font.h>
#include <InterViews/Bitmaps/hand.bm>
#include <InterViews/Bitmaps/handMask.bm>
#include <InterViews/Bitmaps/dfast.bm>
#include <InterViews/Bitmaps/dfastMask.bm>
#include <InterViews/Bitmaps/ufast.bm>
#include <InterViews/Bitmaps/ufastMask.bm>
#include <IV-2_6/InterViews/button.h>
#include <IV-2_6/InterViews/strbrowser.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/textdisplay.h>
#include <OS/math.h>
#include <OS/memory.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

StringBrowser::StringBrowser (
    ButtonState* bs, int r, int c, boolean u, int h, const char* d
) {
    Init(bs, r, c, u, h, d);
}

StringBrowser::StringBrowser (
    const char* name,
    ButtonState* bs, int r, int c, boolean u, int h, const char* d
) {
    SetInstance(name);
    Init(bs, r, c, u, h, d);
}

void StringBrowser::Init (
    ButtonState* bs, int r, int c, boolean u, int h, const char* d
) {
    const int defaultSize = 256;

    SetClassName("StringBrowser");
    input = new Sensor;
    input->Catch(DownEvent);
    input->Catch(KeyEvent);

    strbufsize = selbufsize = defaultSize;
    strbuf = new char*[strbufsize];
    selbuf = new char*[selbufsize];
    strcount = selcount = 0;

    display = nil;
    lineheight = 0;
    rows = r;
    columns = c;
    uniqueSel = u;
    singleClick = false;
    highlight = h;
    lastx = lasty = -1;
    subject = bs;
    Resource::ref(subject);
    done = d;
    perspective = new Perspective;
    InitTextDisplay();
}

void StringBrowser::InitTextDisplay () {
    delete display;
    display = new TextDisplay;
    display->Draw(output, canvas);
    display->CaretStyle(NoCaret);

    for (int i = 0; i < strcount; ++i) {
        display->ReplaceText(i, strbuf[i], strlen(strbuf[i]));
    }

    if (canvas != nil) {
        output->ClearRect(canvas, 0, 0, xmax, ymax);
    }
}

StringBrowser::~StringBrowser () {
    Clear();
    delete strbuf;
    delete display;
    Resource::unref(subject);
    Resource::unref(perspective);
}

static void BufCheck (char**& buf, int& bufsize, int count, int index) {
    char** newbuf;

    if (index >= bufsize) {
        bufsize = (index+1) * 2;
        newbuf = new char*[bufsize];
        Memory::copy(buf, newbuf, count*sizeof(char*));
        delete buf;
        buf = newbuf;
    }
}

static void BufInsert (
    const char* s, int index, char**& buf, int& bufsize, int& count
) {
    char** spot;
    index = (index < 0) ? count : index;

    if (index < count) {
        BufCheck(buf, bufsize, count, count+1);
        spot = &buf[index];
        Memory::copy(spot, spot+1, (count - index)*sizeof(char*));

    } else {
        BufCheck(buf, bufsize, count, index);
        spot = &buf[index];
    }
    *spot = (char*)s;
    ++count;
}

static void BufRemove (int index, char** buf, int& count) {
    if (index < --count) {
        char** spot = &buf[index];
        Memory::copy(spot+1, spot, (count - index)*sizeof(char*));
    }
}

static int BufFind (
    int index, 
    char** srcbuf, int srccount, 
    char** dstbuf, int dstcount
) {
    if (0 <= index && index < srccount) {
        const char* s = srcbuf[index];

        if (s != nil) {
            for (int i = 0; i < dstcount; ++i) {
                if (dstbuf[i] == s) {
                    return i;
                }
            }
        }
    }
    return -1;
}

void StringBrowser::Insert (const char* s, int index) {
    display->Draw(output, canvas);
    register Perspective* p = perspective;

    char* copy = new char[strlen(s)+1];
    strcpy(copy, s);
    BufInsert(copy, index, strbuf, strbufsize, strcount);

    if (output != nil) {
	p->width = Math::max(p->width, output->GetFont()->Width(s));
    }
    p->height += lineheight;
    p->cury += lineheight;
    p->Update();

    if (index < strcount-1) {
        display->InsertLinesAfter(index-1, 1);
    }
    display->ReplaceText(index, s, strlen(s));
}

void StringBrowser::Replace (const char* s, int index) {
    if (index < strcount) {
	display->Draw(output, canvas);
	register Perspective* p = perspective;

	char* old_string = String(index);
	delete old_string;
	char* copy = new char[strlen(s)+1];
	strcpy(copy, s);
	strbuf[index] = copy;

	if (output != nil) {
	    p->width = Math::max(p->width, output->GetFont()->Width(s));
	}
	p->Update();

	display->ReplaceText(index, s, strlen(s));
    }
}

void StringBrowser::Remove (int index) {
    if (0 <= index && index < strcount) {
        display->Draw(output, canvas);
        register Perspective* p = perspective;
	char* string = String(index);

	if (
	    output != nil && p->width > columns * shape->hunits &&
	    p->width == output->GetFont()->Width(string)
	) {
	    UpdateWidth();
	}
        Unselect(index);
        delete string;
        BufRemove(index, strbuf, strcount);
    
        p->height -= lineheight;
        p->cury -= lineheight;
        p->Update();
        display->DeleteLinesAfter(index-1, 1);
    }
}

int StringBrowser::Index (const char* s) {
    for (int i = 0; i < strcount; ++i) {
        if (strcmp(s, strbuf[i]) == 0) {
            return i;
        }
    }
    return -1;
}

char* StringBrowser::String (int index) { 
    return (0 <= index && index < strcount) ? strbuf[index] : nil;
}

void StringBrowser::Clear () {
    for (int i = 0; i < strcount; ++i) {
        delete strbuf[i];
    }
    strcount = selcount = 0;
    InitTextDisplay();

    Perspective np;
    *perspective = np;
    Resize();
}

void StringBrowser::Select (int index) {
    if (index < strcount && !Selected(index)) {
        BufInsert(String(index), selcount, selbuf, selbufsize, selcount);
        display->Draw(output, canvas);
        display->Style(index, 0, index+1, -1, highlight);
    }
}

void StringBrowser::Unselect (int index) {
    int selindex;

    if (index < strcount && (selindex = SelectionIndex(index)) >= 0) {
        BufRemove(selindex, selbuf, selcount);
        display->Draw(output, canvas);
        display->Style(index, 0, index+1, -1, Plain);
    }
}

int StringBrowser::Selection (int selindex) {
    return BufFind(selindex, selbuf, selcount, strbuf, strcount);
}

int StringBrowser::SelectionIndex (int index) {
    return BufFind(index, strbuf, strcount, selbuf, selcount);
}

void StringBrowser::Browse () {
    Event e;
    e.target = nil;
    e.eventType = EnterEvent;
    Handle(e);
}

boolean StringBrowser::HandleDownEvent (Event& e) {
    boolean done = true;

    if (e.target == this) {
        if (e.button == LEFTMOUSE) {
            done = LeftButtonDown(e);
        } else if (e.button == MIDDLEMOUSE) {
            GrabScroll(e);
        } else if (e.button == RIGHTMOUSE) {
            RateScroll(e);
        }
    } else {
        UnRead(e);
    }
    return done;
}

boolean StringBrowser::HandleKeyEvent (Event& e) {
    boolean done = false;

    if (e.len != 0) {
        done = HandleChar(e.keystring[0]);
    }
    return done;
}    

void StringBrowser::Handle (Event& e) {
    if (e.eventType == KeyEvent) {
        HandleKeyEvent(e);

    } else {
        boolean done = false;

        do {
            switch (e.eventType) {
            case DownEvent:
                done = HandleDownEvent(e);
                break;

            case KeyEvent:
                done = HandleKeyEvent(e);
                break;
            }
            if (!done) {
                Read(e);
            }
        } while (!done);
    }
}

boolean StringBrowser::HandleChar (char c) {
    int index = Selection();
    int i;

    switch (c) {
    case SBFirstString:
        ScrollTo(0);
        break;
    case SBLastString:
        ScrollTo(strcount-1);
        break;
    case SBScrollDown:
        ScrollBy(1);
        break;
    case SBScrollUp:
        ScrollBy(-1);
        break;
    case SBSelectAll:
        if (!uniqueSel) {
            SelectAll();
        }
        break;
    case SBUnselectAll:
    case SBUnselectAllAlt:
        UnselectAll();
        break;
    case SBSelectPreviousString:
        UnselectAll();
        index = Math::max(0, Math::min(--index, strcount-1));
        Select(index);
        ScrollTo(index);
        break;
    case SBSelectNextString:
        UnselectAll();
        index = Math::max(0, Math::min(++index, strcount-1));
        Select(index);
        ScrollTo(index);
        break;
    case SBSelectTopString:
        UnselectAll();
        index = Math::max(0, Locate(0, ymax));
        Select(index);
        break;
    case SBSelectBottomString:
        UnselectAll();
        index = Math::min(Locate(0, 0), strcount-1);
        Select(index);
        break;
    case SBPageDown:
        ScrollBy((ymax+1) / lineheight);
        break;
    case SBPageUp:
        ScrollBy(-(ymax+1) / lineheight);
        break;
    case SBHalfPageDown:
        ScrollBy((ymax+1) / lineheight / 2);
        break;
    case SBHalfPageUp:
        ScrollBy(-(ymax+1) / lineheight / 2);
        break;
    default:
        for (i = 0; done[i] != '\0'; i++) {
            if (c == done[i]) {
                subject->SetValue(c);
                return true;
            }
        }
        break;
    }
    return false;
}

void StringBrowser::Adjust (Perspective& np) {
    register Perspective* p = perspective;
    float scale = (np.height == 0) ? 1 : float(p->height) / float(np.height);
    int x = p->x0 + int((np.curx - np.x0) * scale);
    int y = p->y0 + int((np.cury - np.y0) * scale);
    ScrollTo(x, y);
}

static Cursor* handCursor;
static Cursor* upCursor;
static Cursor* dnCursor;

void StringBrowser::Reconfig () {
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
    lineheight = shape->vunits;
    shape->Rect(shape->hunits*columns, shape->vunits*rows);
    shape->Rigid(hfil, hfil, shape->height - lineheight, vfil);
    singleClick = AttributeIsSet("singleClick");

    const char* attrib = GetAttribute("clickDelay");
    clickDelay = (attrib == nil) ? 250 : atoi(attrib);
}

void StringBrowser::UpdateWidth () {
    if (output != nil) {
	Perspective* p = perspective;
	const Font* f = output->GetFont();
	p->width = columns * shape->hunits;

	for (int i = 0; i < Count(); ++i) {
	    const char* s = String(i);
	    p->width = Math::max(p->width, f->Width(s));
	}
    }
}

void StringBrowser::InitPerspective (boolean scroll_to_top) {
    register Perspective* p = perspective;
    int old_top = p->height - p->cury - p->curheight;

    p->lx = p->curwidth = xmax+1;
    p->ly = p->curheight = ymax+1;
    p->sx = shape->hunits;
    p->sy = lineheight;
    p->height = Count() * lineheight;
    UpdateWidth();

    if (scroll_to_top) {
	p->curx = 0;
	p->cury = p->height - p->curheight;

    } else {
	p->cury = p->height - p->curheight - old_top;
    }

    p->Update();
}

void StringBrowser::Resize () {
    InitPerspective(perspective->curwidth == 0);

    if (lineheight != 0) {
	display->Draw(output, canvas);
	display->LineHeight(lineheight);
	display->Resize(0, -lineheight, xmax, ymax);
    }
}

void StringBrowser::Redraw (IntCoord l, IntCoord b, IntCoord r, IntCoord t) {
    display->Draw(output, canvas);
    display->Redraw(l, b, r, t);
}

void StringBrowser::Select (int dot, int mark) {
    for (int i = Math::min(dot, mark); i <= Math::max(dot, mark); ++i) {
        Select(i);
    }
}

void StringBrowser::SelectAll () {
    selcount = 0;
    for (int i = 0; i < strcount; ++i) {
        BufInsert(strbuf[i], i, selbuf, selbufsize, selcount);
    }
    display->Draw(output, canvas);
    display->Style(0, 0, strcount, -1, highlight);
}

void StringBrowser::Unselect (int dot, int mark) {
    for (int i = Math::min(dot, mark); i <= Math::max(dot, mark); ++i) {
        Unselect(i);
    }
}

void StringBrowser::UnselectAll () {
    selcount = 0;
    display->Draw(output, canvas);
    display->Style(0, 0, strcount, 0, Plain);
}

void StringBrowser::ScrollBy (int dx, int dy) {
    register Perspective* p = perspective;
    ScrollTo(p->curx + dx, p->cury + dy);
}

void StringBrowser::ScrollBy (int lines) {
    ScrollBy(0, -lines*lineheight);
}

void StringBrowser::ScrollTo (int x, int y) {
    register Perspective* p = perspective;
    int minx = 0;
    int maxx = Math::max(minx, p->width - p->curwidth/2);
    int maxy = p->height - p->curheight;
    int miny = Math::min(maxy, 1-lineheight);

    p->curx = Math::max(minx, Math::min(x, maxx));
    p->cury = Math::max(miny, Math::min(y, maxy));
    p->Update();

    int topmargin = p->height - p->curheight - p->cury;
    int line = (lineheight == 0) ? 0 : topmargin / lineheight;

    display->Draw(output, canvas);
    display->Scroll(line, -p->curx, ymax);
}

void StringBrowser::ScrollTo (int index) {
    register Perspective* p = perspective;
    IntCoord y0 = p->y0 + p->cury;
    IntCoord y = p->height - (index+1)*lineheight - y0;

    if (y > ymax) {
        ScrollTo(0, y0 - (ymax-y));
    } else if (y < 0) {
        y -= (p->curheight % lineheight == 0) ? 0 : lineheight;
        ScrollTo(0, y0 - (-y));
    }
}

void StringBrowser::ScrollToView (IntCoord x, IntCoord y) {
    IntCoord dx = x < 0 ? x : x > xmax ? x - xmax : 0;
    IntCoord dy = y < 0 ? y : y > ymax ? y - ymax : 0;
    if (dx != 0 || dy != 0) {
        ScrollTo(perspective->curx + dx, perspective->cury + dy);
    }
}

int StringBrowser::Locate (IntCoord, IntCoord y) {
    register Perspective* p = perspective;

    y = Math::max(p->curheight % lineheight, Math::min(y, p->curheight-1));
    return display->LineNumber(y);
}

void StringBrowser::Note (Event& e) {
    lasttime = e.timestamp;
    lastx = e.x;
    lasty = e.y;
}

boolean StringBrowser::DoubleClicked (Event& e) {
    if (e.eventType != DownEvent) {
	return false;
    }
    const int distThresh = 4;
    int time = abs(int(e.timestamp - lasttime));
    int dist = abs(e.x - lastx) + abs(e.y - lasty);

    return time < clickDelay && dist < distThresh;
}

void StringBrowser::UpdateSelection (int d, int m, int style) {
    int oldl = Math::min(lastdot, lastmark);
    int oldr = Math::max(lastdot, lastmark);
    int newl = Math::min(d, m);
    int newr = Math::max(d, m);

    if (newr < oldl || newl > oldr) {           // no overlap
        if (style == highlight) {
            Unselect(oldl, oldr);
        }
        if (style == highlight) {
            Select(newl, newr);
        } else {
            Unselect(newl, newr);
        }
    } else {                                    // overlap
        if (newl < oldl) {
            if (style == highlight) {
                Select(newl, oldl);
            } else {
                Unselect(newl, oldl);
            }
        } else if (newl > oldl) {
            if (style == highlight) {
                Unselect(oldl, newl-1);
            }
        }
        if (newr > oldr) {
            if (style == highlight) {
                Select(oldr, newr);
            } else {
                Unselect(oldr, newr);
            }
        } else if (newr < oldr) {
            if (style == highlight) {
                Unselect(newr+1, oldr);
            }
        }
    }
    lastdot = d;
    lastmark = m;
}

boolean StringBrowser::LeftButtonDown (Event& e) {
    boolean status = false;

    if (DoubleClicked(e)) {
        subject->SetValue(done[0]);
        status = true;

    } else if (uniqueSel) {
        if (Selections() == 0) {
            Select(Locate(e.x, e.y));
        } else {
            Unselect(Selection());
            if (!e.shift) {
                Select(Locate(e.x, e.y));
            }
        }

    } else {
        lastdot = lastmark = Locate(e.x, e.y);

        if (Selected(lastdot) && e.shift) {
            Unselect(lastdot);
            do {
                ScrollToView(e.x, e.y);
                UpdateSelection(lastdot, Locate(e.x, e.y), Plain);
                Poll(e);
            } while (e.leftmouse);

        } else {
            if (!e.shift) {
                UnselectAll();
            }
            Select(lastdot);
            do {
                ScrollToView(e.x, e.y);
                UpdateSelection(lastdot, Locate(e.x, e.y), highlight);
                Poll(e);
            } while (e.leftmouse);
        }
    }
    Note(e);
    if (singleClick) {
        subject->SetValue(done[0]);
        status = true;
    }
    return status;
}

void StringBrowser::GrabScroll (Event& e) {
    int y = e.y;
    Cursor* origCursor = GetCursor();
    SetCursor(handCursor);

    do {
        ScrollBy(0, y - e.y);
        y = e.y;
        Poll(e);
    } while (e.middlemouse);

    SetCursor(origCursor);
}

void StringBrowser::RateScroll (Event& e) {
    Cursor* origCursor = GetCursor();
    int y = e.y;

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
