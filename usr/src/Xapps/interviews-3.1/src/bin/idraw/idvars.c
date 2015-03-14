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
 * Implementation of idraw-specific state variables
 */

#include "idvars.h"
#include "idclasses.h"

#include <Unidraw/globals.h>
#include <Unidraw/statevars.h>
#include <Unidraw/stateviews.h>
#include <Unidraw/Graphic/pspaint.h>

#include <InterViews/canvas.h>
#include <InterViews/font.h>
#include <InterViews/painter.h>
#include <InterViews/shape.h>

#include <stream.h>

/*****************************************************************************/

static const int VIEW_WIDTH = 30;
static const int VIEW_HEIGHT = 15;
static const int HPAD = 2;
static const int VPAD = 2;
static const int ARROWX = HPAD + 5;
static const int ARROWY = HPAD + 3;

/*****************************************************************************/

static void CenterText (
    const char* text, Painter* output, Coord xmax, Coord ymax
) {
    const Font* f = output->GetFont();
    int width = f->Width(text);
    int height = f->Height();

    output->MoveTo((xmax - width + 1)/2, (ymax - height + 1)/2);
}

/*****************************************************************************/

ArrowVar::ArrowVar(boolean h, boolean t) {
    _head = h;
    _tail = t;
}

void ArrowVar::SetArrows(boolean h, boolean t) {
    if (h != _head || t != _tail) {
	_head = h;
	_tail = t;
	Notify();
    }
}

StateVar& ArrowVar::operator = (StateVar& var) {
    if (var.IsA(ARROW_VAR)) {
        ArrowVar* arrowVar = (ArrowVar*) &var;
        SetArrows(arrowVar->Head(),arrowVar->Tail());
    }
    return *this;
}

StateVar* ArrowVar::Copy () { return new ArrowVar(_head, _tail); }
ClassId ArrowVar::GetClassId () { return ARROW_VAR; }
boolean ArrowVar::IsA (ClassId id) {return ARROW_VAR==id || StateVar::IsA(id);}

void ArrowVar::Read (istream& in) {
    StateVar::Read(in);
    char h, t;
    in >> h >> t;
    _head = (boolean) h;
    _tail = (boolean) t;
}

void ArrowVar::Write (ostream& out) {
    StateVar::Write(out);
    out << _head << " " << _tail << " ";
}

/*****************************************************************************/

class ArrowInteractor : public Interactor {
public:
    ArrowInteractor(
        boolean, boolean, PSBrush*, PSColor* fg = nil, PSColor* bg = nil
    );
    virtual ~ArrowInteractor();

    void SetBrush(PSBrush*);
    void SetColors(PSColor*, PSColor*);
    void SetArrows(boolean, boolean);

    PSBrush* GetBrush();
    PSColor* GetFgColor();
    PSColor* GetBgColor();
protected:
    virtual void Reconfig();
    virtual void Redraw(Coord, Coord, Coord, Coord);
protected:
    boolean _head, _tail;
    PSBrush* _brush;
    PSColor* _fg, *_bg;
};

ArrowInteractor::ArrowInteractor (
    boolean h, boolean t, PSBrush* b, PSColor* fg, PSColor* bg
) {
    _head = h;
    _tail = t;
    _brush = b;
    _fg = fg;
    _bg = bg;

    Ref(_brush);
    Ref(_fg);
    Ref(_bg);

    shape->Rect(VIEW_WIDTH, VIEW_HEIGHT);
    shape->Rigid(VIEW_WIDTH, VIEW_WIDTH, VIEW_HEIGHT, VIEW_HEIGHT);
}

ArrowInteractor::~ArrowInteractor () {
    Unref(_brush);
    Unref(_fg);
    Unref(_bg);
}

void ArrowInteractor::SetBrush (PSBrush* b) {
    Ref(b);
    Unref(_brush);
    _brush = b; 
}

PSBrush* ArrowInteractor::GetBrush () { return _brush; }

void ArrowInteractor::SetArrows (boolean h, boolean t) {
    _head = h;
    _tail = t;
}    

void ArrowInteractor::SetColors (PSColor* fg, PSColor* bg) {
    Ref(fg);
    Ref(bg);
    Unref(_fg);
    Unref(_bg);
    _fg = fg;
    _bg = bg;
}

PSColor* ArrowInteractor::GetFgColor () { return _fg; }
PSColor* ArrowInteractor::GetBgColor () { return _bg; }

void ArrowInteractor::Reconfig () { 
    Painter* p = new Painter(output);
    Ref(p);
    Unref(output);
    output = p;
}

void ArrowInteractor::Redraw (Coord, Coord, Coord, Coord) {
    const char* none = "None";
    Coord x[3], y[3];

    if (canvas != nil) {
	output->ClearRect(canvas, 0, 0, xmax, ymax);

	if (_brush->None()) {
	    const Font* f = output->GetFont();
	    int width = f->Width(none);
	    int height = f->Height();

	    output->MoveTo((xmax - width + 1)/2, (ymax - height + 1)/2);
	    output->Text(canvas, none);

	} else {
            const Color* origfg = output->GetFgColor();
            const Color* origbg = output->GetBgColor();
            Resource::ref(origfg);
            Resource::ref(origbg);

	    output->SetBrush(_brush);
            output->SetColors(_fg, _bg);
	    output->Line(canvas, HPAD, ymax/2, xmax-HPAD, ymax/2);

	    if (_head) {
		x[2] = x[0] = xmax-ARROWX;
		y[0] = ymax/2 - ARROWY;
		x[1] = xmax-HPAD;
		y[1] = ymax/2;
		y[2] = ymax/2 + ARROWY;
		output->MultiLine(canvas, x, y, 3);
	    }
	    if (_tail) {
		x[2] = x[0] = ARROWX;
		y[0] = ymax/2 - ARROWY;
		x[1] = HPAD;
		y[1] = ymax/2;
		y[2] = ymax/2 + ARROWY;
		output->MultiLine(canvas, x, y, 3);
	    }

            if (_brush->Width() == 0) {
                CenterText("0", output, xmax, ymax);
                output->Text(canvas, "0");
            }

            output->SetColors(origfg, origbg);
            Resource::unref(origfg);
            Resource::unref(origbg);
	}
    }
}

/*****************************************************************************/

ArrowVarView::ArrowVarView (
    ArrowVar* av, BrushVar* bv, ColorVar* cv
) : StateVarView(bv) {
    PSColor* fg = nil, *bg = nil;
    _colorSubj = cv;
    _arrowSubj = av;

    _arrowSubj->Attach(this);
    _subject = bv;

    if (_colorSubj != nil) {
        _colorSubj->Attach(this);
        _subject = bv;                          // Attach changes _subject
        fg = _colorSubj->GetFgColor();
        bg = _colorSubj->GetBgColor();
    }
    ArrowInteractor* ab = new ArrowInteractor(
	av->Head(), av->Tail(), bv->GetBrush(), fg, bg
    );
    Insert(ab);
}

ArrowVarView::~ArrowVarView () {
    StateVar* subject = _subject;           // Detach changes _subject
    _arrowSubj->Detach(this);
    _subject = subject;

    if (_colorSubj != nil) {
        _colorSubj->Detach(this);
        _subject = subject;
    }
}

boolean ArrowVarView::Stale () {
    boolean arrowsChanged = (
	(_arrowSubj->Head() != _prevHead || _arrowSubj->Tail() != _prevTail)
    );
	    
    boolean colorsChanged = (
        (_colorSubj != nil) && (
            _colorSubj->GetFgColor() != _prevFg ||
            _colorSubj->GetBgColor() != _prevBg
        )
    );

    return ((BrushVar*) _subject)->GetBrush() != _prevVal ||
	colorsChanged ||
	arrowsChanged;    
}

void ArrowVarView::Init () {
    ArrowInteractor* b = (ArrowInteractor*) interior();

    _prevVal = ((BrushVar*) _subject)->GetBrush();
    b->SetBrush(_prevVal);

    _prevHead = _arrowSubj->Head();
    _prevTail = _arrowSubj->Tail();
    b->SetArrows(_prevHead, _prevTail);

    if (_colorSubj != nil) {
        _prevFg = _colorSubj->GetFgColor();
        _prevBg = _colorSubj->GetBgColor();
        b->SetColors(_prevFg, _prevBg);
    }
}

