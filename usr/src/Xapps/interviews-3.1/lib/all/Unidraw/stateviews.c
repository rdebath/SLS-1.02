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
 * Implementation of StateVarView subclasses.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/globals.h>
#include <Unidraw/statevars.h>
#include <Unidraw/stateviews.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Graphic/pspaint.h>

#include <InterViews/canvas.h>
#include <InterViews/font.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/shape.h>

#include <IV-2_6/_enter.h>

#include <stdio.h>
#include <string.h>

/*****************************************************************************/

static const int VIEW_WIDTH = 30;
static const int VIEW_HEIGHT = 15;
static const int HPAD = 2;
static const int VPAD = 2;

static const char* GRAVITY_ON = "gravity on";
static const char* MODIFIED = "*";
static const char* WPROTECTED = "%";
static const char* UNMODIFIED = " ";
static const char* NONE = "None";
static const char* UNNAMED = "[unnamed]";

/*****************************************************************************/

class TextInteractor : public Interactor {
public:
    TextInteractor(const char*, Alignment = Center);
    virtual ~TextInteractor();

    void SetText(const char*);
    const char* GetText();
protected:
    void Align();
    virtual void Reconfig();
    virtual void Redraw(Coord, Coord, Coord, Coord);
protected:
    char* _text;
    Alignment _alignment;
};

TextInteractor::TextInteractor (const char* t, Alignment a) {
    _text = strnew(t);
    _alignment = a;
}

TextInteractor::~TextInteractor () { delete _text; }

void TextInteractor::SetText (const char* t) {
    delete _text;
    _text = strnew(t);
}

const char* TextInteractor::GetText () { return _text; }

void TextInteractor::Align () {
    const Font* f = output->GetFont();
    int width = f->Width(_text);
    int height = f->Height();
    Coord x;

    switch (_alignment) {
    case Left:
        x = HPAD;
        break;
    case Center:
        x = (xmax - width + 1)/2;
        break;
    case Right:
        x = xmax - width - HPAD;
        break;
    }
    output->MoveTo(x, (ymax - height+ 1)/2);
}

void TextInteractor::Reconfig () {
    const Font* f = output->GetFont();
    int width = f->Width(_text);

    shape->Rect(width + 2*HPAD, f->Height() + 2*VPAD);
    shape->Rigid(width, hfil, 0, 0);
}

void TextInteractor::Redraw (Coord, Coord, Coord, Coord) {
    if (canvas != nil) {
	Align();
        output->ClearRect(canvas, 0, 0, xmax, ymax);
	output->Text(canvas, _text);
    }
}

/*****************************************************************************/

NameVarView::NameVarView (
    NameVar* n, Alignment a, const char* samp
) : StateVarView(n) { 
    const char* sample = (samp == nil) ? "nnnnnnnnnnnnnnnnnnnnnn" : samp;
    TextInteractor* t = new TextInteractor(sample, a);
    Insert(t);
}

boolean NameVarView::Stale () {
    const char* name = ((NameVar*) _subject)->GetName();
    const char* text = ((TextInteractor*) interior())->GetText();

    return 
        (name == nil && strcmp(text, UNNAMED) != 0) ||
        (name != nil && strcmp(text, name) != 0);
}

void NameVarView::Init() {
    const char* name = ((NameVar*) _subject)->GetName();
    TextInteractor* t = (TextInteractor*) interior();
    
    if (name == nil) {
        t->SetText(UNNAMED);
    } else {
        t->SetText(name);
    }
}

/*****************************************************************************/

FileNameVarView::FileNameVarView (
    NameVar* n, Alignment a, boolean relative, const char* samp
) : StateVarView(n) { 
    const char* sample = (samp == nil) ? "nnnnnnnnnnnnnnnnnnnnnn" : samp;
    TextInteractor* t = new TextInteractor(sample, a);
    _relative = relative;
    Insert(t);
}

void FileNameVarView::Init() {
    const char* name = ((NameVar*) _subject)->GetName();
    TextInteractor* t = (TextInteractor*) interior();
    
    if (name == nil) {
        t->SetText(UNNAMED);

    } else if (_relative) {
        const char* relname = strrchr(name, '/');

        if (relname == nil) {
            t->SetText(name);
        } else {
            t->SetText(++relname);
        }
    } else {
        t->SetText(name);
    }
}

/*****************************************************************************/

CompNameVarView::CompNameVarView (
    CompNameVar* n, Alignment a, boolean relative, const char* samp
) : StateVarView(n) { 
    const char* sample = (samp == nil) ? "nnnnnnnnnnnnnnnnnnn" : samp;
    TextInteractor* t = new TextInteractor(sample, a);
    _relative = relative;
    Insert(t);
}

void CompNameVarView::Init() {
    CompNameVar* subj = (CompNameVar*) _subject;
    const char* name = subj->GetName();
    TextInteractor* t = (TextInteractor*) interior();
    
    if (name == nil) {
        t->SetText(UNNAMED);

    } else {
        char buf[CHARBUFSIZE];
        buf[0] = '\0';
        
        if (subj->PartOf() != nil) {
            strcat(buf, "[part of] ");
        }
        if (_relative) {
            const char* relname = strrchr(name, '/');

            if (relname == nil) {
                strcat(buf, name);
            } else {
                strcat(buf, ++relname);
            }

        } else {
            strcat(buf, name);
        }
        t->SetText(buf);
    }
}

/*****************************************************************************/

ModifStatusVarView::ModifStatusVarView (
    ModifStatusVar* m, Alignment a
) : StateVarView(m) {
    TextInteractor* t = new TextInteractor(" ", a);
    Insert(t);
}

boolean ModifStatusVarView::Stale () {
    ModifStatusVar* msv = (ModifStatusVar*) _subject;
    return msv->GetModifStatus() != _prevVal || WriteProtected() != _prevProt;
}

boolean ModifStatusVarView::WriteProtected () {
    ModifStatusVar* msv = (ModifStatusVar*) _subject;
    Catalog* catalog = unidraw->GetCatalog();
    const char* name = catalog->GetName(msv->GetComponent());
    return name != nil &&  catalog->Exists(name) && !catalog->Writable(name);
}

void ModifStatusVarView::Init () {
    ModifStatusVar* msv = (ModifStatusVar*) _subject;
    TextInteractor* t = (TextInteractor*) interior();

    _prevVal = msv->GetModifStatus();
    _prevProt = WriteProtected();

    if (_prevProt) {
        t->SetText(WPROTECTED);

    } else if (_prevVal) {
        t->SetText(MODIFIED);

    } else {
        t->SetText(UNMODIFIED);
    }
}

void ModifStatusVarView::Reconfig () {
    StateVarView::Reconfig();
    shape->Rigid();
}

/*****************************************************************************/

MagnifVarView::MagnifVarView (MagnifVar* m, Alignment a) : StateVarView(m) {
    TextInteractor* t = new TextInteractor("nnnnnnnnnn", a);
    Insert(t);
}

boolean MagnifVarView::Stale () {
    return ((MagnifVar*) _subject)->GetMagnif() != _prevVal;
}

void MagnifVarView::Init() {
    float mag = ((MagnifVar*) _subject)->GetMagnif();
    TextInteractor* t = (TextInteractor*) interior();
    char buf[32];

    _prevVal = mag;
    sprintf(buf, "mag %gx", mag);
    t->SetText(buf);
}

/*****************************************************************************/

GravityVarView::GravityVarView (GravityVar* m, Alignment a) : StateVarView(m) {
    TextInteractor* t = new TextInteractor(GRAVITY_ON, a);
    Insert(t);
}

boolean GravityVarView::Stale () {
    return ((GravityVar*) _subject)->IsActive() != _prevVal;
}

void GravityVarView::Init() {
    boolean active = ((GravityVar*) _subject)->IsActive();
    TextInteractor* t = (TextInteractor*) interior();
    const char* string = active ? GRAVITY_ON : "";

    _prevVal = active;
    t->SetText(string);
}

/*****************************************************************************/

FontVarView::FontVarView (
    FontVar* f, Alignment a, const char* samp
) : StateVarView(f) {
    const char* sample =
	(samp == nil) ? "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn" : samp;
    TextInteractor* t = new TextInteractor(sample, a);
    Insert(t);
}

boolean FontVarView::Stale () {
    return ((FontVar*) _subject)->GetFont() != _prevVal;
}

void FontVarView::Init () {
    PSFont* f = ((FontVar*) _subject)->GetFont();
    const char* name = f->GetPrintFontAndSize();
    TextInteractor* t = (TextInteractor*) interior();

    _prevVal = f;
    t->SetText(name);
}

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

class BrushInteractor : public Interactor {
public:
    BrushInteractor(PSBrush*, PSColor* fg = nil, PSColor* bg = nil);
    virtual ~BrushInteractor();

    void SetBrush(PSBrush*);
    void SetColors(PSColor*, PSColor*);

    PSBrush* GetBrush();
    PSColor* GetFgColor();
    PSColor* GetBgColor();
protected:
    virtual void Reconfig();
    virtual void Redraw(Coord, Coord, Coord, Coord);
protected:
    PSBrush* _brush;
    PSColor* _fg, *_bg;
};

BrushInteractor::BrushInteractor (PSBrush* b, PSColor* fg, PSColor* bg) {
    _brush = b;
    _fg = fg;
    _bg = bg;

    Ref(_brush);
    Ref(_fg);
    Ref(_bg);
}

BrushInteractor::~BrushInteractor () {
    Unref(_brush);
    Unref(_fg);
    Unref(_bg);
}

void BrushInteractor::SetBrush (PSBrush* b) {
    Ref(b);
    Unref(_brush);
    _brush = b; 
}

PSBrush* BrushInteractor::GetBrush () { return _brush; }

void BrushInteractor::SetColors (PSColor* fg, PSColor* bg) {
    Ref(fg);
    Ref(bg);
    Unref(_fg);
    Unref(_bg);
    _fg = fg;
    _bg = bg;
}

PSColor* BrushInteractor::GetFgColor () { return _fg; }
PSColor* BrushInteractor::GetBgColor () { return _bg; }

void BrushInteractor::Reconfig () { 
    Painter* tmp = output;
    output = new Painter(tmp);
    Ref(output);
    Unref(tmp);

    const Font* f = output->GetFont();
    shape->width = max(f->Width(NONE) + 2*HPAD, VIEW_WIDTH);
    shape->height = max(f->Height() + 2*VPAD, VIEW_HEIGHT);

    shape->Rigid(shape->width/2, shape->width, shape->height/2, shape->height);
}

void BrushInteractor::Redraw (Coord, Coord, Coord, Coord) {
    if (canvas != nil) {
	output->ClearRect(canvas, 0, 0, xmax, ymax);

	if (_brush->None()) {
	    CenterText(NONE, output, xmax, ymax);
	    output->Text(canvas, NONE);

	} else {
            const Color* origfg = output->GetFgColor();
            const Color* origbg = output->GetBgColor();
            Resource::ref(origfg);
            Resource::ref(origbg);

	    output->SetBrush(_brush);
            output->SetColors(_fg, _bg);
	    output->Line(canvas, HPAD, ymax/2, xmax-HPAD, ymax/2);

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

BrushVarView::BrushVarView (BrushVar* bv, ColorVar* cv) : StateVarView(bv) {
    PSColor* fg = nil, *bg = nil;
    _colorSubj = cv;

    if (_colorSubj != nil) {
        _colorSubj->Attach(this);
        _subject = bv;                          // Attach changes _subject
        fg = _colorSubj->GetFgColor();
        bg = _colorSubj->GetBgColor();
    }
    BrushInteractor* b = new BrushInteractor(bv->GetBrush(), fg, bg);
    Insert(b);
}

BrushVarView::~BrushVarView () {
    if (_colorSubj != nil) {
        StateVar* subject = _subject;           // Detach changes _subject
        _colorSubj->Detach(this);
        _subject = subject;
    }
}

boolean BrushVarView::Stale () {
    boolean colorsChanged = (
        (_colorSubj != nil) && (
            _colorSubj->GetFgColor() != _prevFg ||
            _colorSubj->GetBgColor() != _prevBg
        )
    );
    return ((BrushVar*) _subject)->GetBrush() != _prevVal || colorsChanged;
}

void BrushVarView::Init () {
    BrushInteractor* b = (BrushInteractor*) interior();

    _prevVal = ((BrushVar*) _subject)->GetBrush();
    b->SetBrush(_prevVal);

    if (_colorSubj != nil) {
        _prevFg = _colorSubj->GetFgColor();
        _prevBg = _colorSubj->GetBgColor();
        b->SetColors(_prevFg, _prevBg);
    }
}

/*****************************************************************************/

class PatternInteractor : public Interactor {
public:
    PatternInteractor(PSPattern*, PSColor* fg = nil, PSColor* bg = nil);
    virtual ~PatternInteractor();

    void SetPattern(PSPattern*);
    void SetColors(PSColor*, PSColor*);

    PSPattern* GetPattern();
    PSColor* GetFgColor();
    PSColor* GetBgColor();
protected:
    virtual void Reconfig();
    virtual void Redraw(Coord, Coord, Coord, Coord);
protected:
    PSPattern* _pattern;
    PSColor* _fg, *_bg;
};

PatternInteractor::PatternInteractor (PSPattern* p, PSColor* fg, PSColor* bg) {
    _pattern = p;
    _fg = fg;
    _bg = bg;

    Ref(_pattern);
    Ref(_fg);
    Ref(_bg);
}

PatternInteractor::~PatternInteractor () {
    Unref(_pattern);
    Unref(_fg);
    Unref(_bg);
}

void PatternInteractor::SetPattern (PSPattern* p) {
    Ref(p);
    Unref(_pattern);
    _pattern = p;
}

PSPattern* PatternInteractor::GetPattern () { return _pattern; }

void PatternInteractor::SetColors (PSColor* fg, PSColor* bg) {
    Ref(fg);
    Ref(bg);
    Unref(_fg);
    Unref(_bg);
    _fg = fg;
    _bg = bg;
}

PSColor* PatternInteractor::GetFgColor () { return _fg; }
PSColor* PatternInteractor::GetBgColor () { return _bg; }

void PatternInteractor::Reconfig () {
    Painter* tmp = output;
    output = new Painter(tmp);
    Ref(output);
    Unref(tmp);

    const Font* f = output->GetFont();
    shape->width = max(f->Width(NONE) + 2*HPAD, VIEW_WIDTH);
    shape->height = max(f->Height() + 2*VPAD, VIEW_HEIGHT);

    shape->Rigid(shape->width/2, shape->width, shape->height/2, shape->height);
}

void PatternInteractor::Redraw (Coord, Coord, Coord, Coord) {
    if (canvas != nil) {
	output->ClearRect(canvas, 0, 0, xmax, ymax);

	if (_pattern->None()) {
	    CenterText(NONE, output, xmax, ymax);
	    output->Text(canvas, NONE);

	} else {
            const Color* origfg = output->GetFgColor();
            const Color* origbg = output->GetBgColor();
            Resource::ref(origfg);
            Resource::ref(origbg);

	    output->SetPattern(_pattern);
            output->SetColors(_fg, _bg);
	    output->FillRect(canvas, HPAD, VPAD, xmax-HPAD, ymax-VPAD);
	    output->Rect(canvas, HPAD, VPAD, xmax-HPAD, ymax-VPAD);

            output->SetColors(origfg, origbg);
            Resource::unref(origfg);
            Resource::unref(origbg);
	}
    }
}

/*****************************************************************************/

PatternVarView::PatternVarView (
    PatternVar* pv, ColorVar* cv
) : StateVarView(pv) {
    PSColor* fg = nil, *bg = nil;
    _colorSubj = cv;

    if (_colorSubj != nil) {
        _colorSubj->Attach(this);
        _subject = pv;                          // Attach changes _subject
        fg = _colorSubj->GetFgColor();
        bg = _colorSubj->GetBgColor();
    }
    PatternInteractor* p = new PatternInteractor(pv->GetPattern(), fg, bg);
    Insert(p);
}

PatternVarView::~PatternVarView () {
    if (_colorSubj != nil) {
        StateVar* subject = _subject;           // Detach changes _subject
        _colorSubj->Detach(this);
        _subject = subject;
    }
}

boolean PatternVarView::Stale () {
    boolean colorsChanged = (
        (_colorSubj != nil) && (
            _colorSubj->GetFgColor() != _prevFg ||
            _colorSubj->GetBgColor() != _prevBg
        )
    );
    return ((PatternVar*) _subject)->GetPattern() != _prevVal || colorsChanged;
}

void PatternVarView::Init() {
    PatternInteractor* p = (PatternInteractor*) interior();

    _prevVal = ((PatternVar*) _subject)->GetPattern();
    p->SetPattern(_prevVal);

    if (_colorSubj != nil) {
        _prevFg = _colorSubj->GetFgColor();
        _prevBg = _colorSubj->GetBgColor();
        p->SetColors(_prevFg, _prevBg);
    }
}
