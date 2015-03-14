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
 * Implementation of Manipulator subclasses.
 */

#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/rubverts.h>
#include <IV-2_6/InterViews/textbuffer.h>
#include <IV-2_6/InterViews/textdisplay.h>
#include <InterViews/transformer.h>
#include <IV-2_6/InterViews/world.h>

#include <IV-2_6/_enter.h>

#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

/****************************************************************************/

class ManipList : public UList {
public:
    ManipList(Manipulator* = nil);

    boolean manipulating();
    void manipulating(boolean);
private:
    boolean _manipulating;
};

ManipList::ManipList (Manipulator* m) : UList(m) { _manipulating = true; }
inline boolean ManipList::manipulating () { return _manipulating; }
inline void ManipList::manipulating (boolean m) { _manipulating = m; }

/****************************************************************************/

ManipGroup::ManipGroup (Viewer* v, Tool* t) {
    _kids = new ManipList;
    _viewer = v;
    _tool = t;
}

ManipGroup::~ManipGroup () { 
    while (!_kids->IsEmpty()) {
	UList* cur = _kids->First();
	_kids->Remove(cur);
        Manipulator* m = Manip(cur);
	delete m;
	delete cur;
    }
    delete _kids;
}

void ManipGroup::Grasp (Event& e) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
	GetManip(i)->Grasp(e);
        ManipList* ml = (ManipList*) Elem(i);
        ml->manipulating(true);
    }
}

boolean ManipGroup::Manipulating (Event& e) {
    Iterator i;
    boolean finished = true;

    for (First(i); !Done(i); Next(i)) {
        ManipList* ml = (ManipList*) Elem(i);

        if (ml->manipulating()) {
            ml->manipulating(GetManip(i)->Manipulating(e));
            finished = finished && !ml->manipulating();
        }
    }
    return !finished;
}

void ManipGroup::Effect (Event& e) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
	GetManip(i)->Effect(e);
    }
}

void ManipGroup::SetViewer (Viewer* v) {
    Iterator i;
    _viewer = v;

    for (First(i); !Done(i); Next(i)) {
	GetManip(i)->SetViewer(v);
    }
}

void ManipGroup::SetTool (Tool* t) {
    Iterator i;
    _tool = t;

    for (First(i); !Done(i); Next(i)) {
	GetManip(i)->SetTool(t);
    }
}

Viewer* ManipGroup::GetViewer () { return _viewer; }
Tool* ManipGroup::GetTool () { return _tool; }

void ManipGroup::First (Iterator& i) { i.SetValue(_kids->First()); }
void ManipGroup::Last (Iterator& i) { i.SetValue(_kids->Last()); }
void ManipGroup::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void ManipGroup::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean ManipGroup::Done (Iterator i) { return Elem(i) == _kids->End(); }
Manipulator* ManipGroup::GetManip (Iterator i) { return Manip(Elem(i)); }

void ManipGroup::SetManip (Manipulator* m, Iterator& i) {
    i.SetValue(_kids->Find(m));
}

void ManipGroup::Append (
    Manipulator* g0, Manipulator* g1, Manipulator* g2, Manipulator* g3
) {
    _kids->Append(new ManipList(g0));

    if (g1 != nil) _kids->Append(new ManipList(g1));
    if (g2 != nil) _kids->Append(new ManipList(g2));
    if (g3 != nil) _kids->Append(new ManipList(g3));
}

void ManipGroup::Prepend (
    Manipulator* g0, Manipulator* g1, Manipulator* g2, Manipulator* g3
) {
    if (g3 != nil) _kids->Prepend(new ManipList(g3));
    if (g2 != nil) _kids->Prepend(new ManipList(g2));
    if (g1 != nil) _kids->Prepend(new ManipList(g1));

    _kids->Prepend(new ManipList(g0));
}

void ManipGroup::InsertBefore (Iterator i, Manipulator* g) {
    Elem(i)->Append(new ManipList(g));
}

void ManipGroup::InsertAfter (Iterator i, Manipulator* g) {
    Elem(i)->Prepend(new ManipList(g));
}

void ManipGroup::Remove (Manipulator* g) {
    _kids->Delete(g);
}

void ManipGroup::Remove (Iterator& i) {
    UList* doomed = Elem(i);
    Manipulator* g = Manip(doomed);

    Next(i);
    _kids->Remove(doomed);
    delete doomed;
}

Manipulator* ManipGroup::Manip (UList* r) { return (Manipulator*) (*r)(); }
UList* ManipGroup::Elem (Iterator i) { return (UList*) i.GetValue(); }

/****************************************************************************/

DragManip::DragManip (
    Viewer* v, Rubberband* newr, Transformer* rel, Tool* t, DragConstraint c
) {
    Init(v, newr, rel, t, c);
    _origPreset = false;
}

DragManip::DragManip (
    Viewer* v, Rubberband* newr, Transformer* rel, Tool* t, DragConstraint c,
    Coord x, Coord y
) {
    Init(v, newr, rel, t, c);
    _origx = x;
    _origy = y;
    _origPreset = true;
}

void DragManip::Init (
    Viewer* v, Rubberband* newr, Transformer* rel, Tool* t, DragConstraint c
) {
    _r = newr;
    Ref(_r);
    
    if (_r != nil) v->InitRubberband(_r);

    _relative = rel;
    Ref(_relative);

    _viewer = v;
    _tool = t;
    _constraint = c;
}

DragManip::~DragManip () { 
    if (_r != nil) Unref(_r);
    if (_relative != nil) Unref(_relative);
}

void DragManip::Grasp (Event& e) {
    _grasp_e = e;

    if (!_origPreset) {
        _origx = e.x;
        _origy = e.y;
    }
    Constrain(e);

    if (_r != nil) _r->Track(e.x, e.y);
}

boolean DragManip::Manipulating (Event& e) {
    if (_r == nil) {
        return false;
    }

    if (e.eventType == MotionEvent) {
        Constrain(e);
	_r->Track(e.x, e.y);

    } else if (e.eventType == UpEvent) {
	return false;
    }
    return true;
}

void DragManip::Effect (Event&) { 
    if (_r != nil) _r->Erase();
}

void DragManip::SetConstraint (DragConstraint c) { _constraint = c; }
DragConstraint DragManip::GetConstraint () { return _constraint; }

void DragManip::Constrain (Event& e) {
    if (e.shift) {
        if (_constraint & XFixed) {
            e.x = _origx;
        }
        if (_constraint & YFixed) {
            e.y = _origy; 
        }
        if (_constraint & XYEqual) {
            Coord w = abs(e.x - _origx);
            Coord h = abs(e.y - _origy);
            if (w > h) {
                e.y = _origy + ((e.y > _origy) ? w : -w);
            } else {
                e.x = _origx + ((e.x > _origx) ? h : -h);
            }
        }
        if (_constraint & HorizOrVert) {
            if (abs(e.x - _origx) < abs(e.y - _origy)) {
                e.x = _origx;
            } else {
                e.y = _origy;
            }
        }
    }
    if (_constraint & Gravity) {
        GetViewer()->Constrain(e.x, e.y);
    }
}

void DragManip::SetViewer (Viewer* v) {
    if (_viewer != v) {
	_viewer = v;
	if (_r != nil) _viewer->InitRubberband(_r);
    }
}    

Viewer* DragManip::GetViewer () { return _viewer; }

void DragManip::SetRubberband (Rubberband* newr) {
    if (_r != newr) {
        Ref(newr);
        Unref(_r);
	_r = newr;

	if (_r != nil) _viewer->InitRubberband(_r);
    }
}

Rubberband* DragManip::GetRubberband () { return _r; }

void DragManip::SetTransformer (Transformer* t) {
    if (_relative != t) {
        Ref(t);
        Unref(_relative);
	_relative = t;
    }
}

Transformer* DragManip::GetTransformer () { return _relative; }
void DragManip::SetTool (Tool* t) { _tool = t; }
Tool* DragManip::GetTool () { return _tool; }

/*****************************************************************************/

VertexManip::VertexManip (
    Viewer* v, GrowingVertices* r, Transformer* rel, Tool* t, DragConstraint c
) : DragManip(v, r, rel, t, c) { }

boolean VertexManip::Manipulating (Event& e) {
    Rubberband* r = GetRubberband();

    if (r == nil) {
        return false;
    }

    if (e.eventType == MotionEvent) {
        Constrain(e);
	r->Track(e.x, e.y);

    } else if (e.eventType == DownEvent) {
        Constrain(e);

        if (e.button == LEFTMOUSE) {
            GetGrowingVertices()->AddVertex(e.x, e.y);

        } else if (e.button == MIDDLEMOUSE) {
            GetGrowingVertices()->AddVertex(e.x, e.y);
            return false;

        } else if (e.button == RIGHTMOUSE) {
            GetGrowingVertices()->RemoveVertex();
        }
    }
    return true;
}

GrowingVertices* VertexManip::GetGrowingVertices () { 
    return (GrowingVertices*) GetRubberband();
}

/****************************************************************************/

ConnectManip::ConnectManip (
     Viewer* newv, Rubberband* newr, Transformer* rel, Tool* t
) : DragManip(newv, newr, rel, t) {
    _target = nil;
}

boolean ConnectManip::Manipulating (Event& e) {
    GraphicView* views = GetViewer()->GetGraphicView();
    Rubberband* r = GetRubberband();
    float cx, cy;

    if (r == nil) {
        return false;
    }

    if (e.eventType == MotionEvent) {
        _target = views->ConnectorIntersecting(
            e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP
        );

        if (_target == nil) {
            r->Track(e.x, e.y);

        } else {
            _target->GetGraphic()->GetCenter(cx, cy);
            r->Track(round(cx), round(cy));
        }

    } else if (e.eventType == UpEvent) {
	r->Erase();
	return false;
    }
    return true;
}

ConnectorView* ConnectManip::GetTarget () { return _target; }

/*****************************************************************************/

TextManip::TextManip (Viewer* v, Painter* p, Coord tab, Tool* t) {
    Init(v, p, p->GetFont()->Height(), tab, t, false);
    _prepositioned = false;
}

TextManip::TextManip (Viewer* v, Painter* p, Coord h, Coord tab, Tool* t) {
    Init(v, p, h, tab, t, true);
    _prepositioned = false;
}

TextManip::TextManip (
    Viewer* v, const char* sample, int samplen, Coord x, Coord y,
    Painter* p, Coord tab, Tool* t
) {
    Init(v, p, p->GetFont()->Height(), tab, t, false, sample, samplen);
    _xpos = x;
    _ypos = y;
    _prepositioned = true;
}

TextManip::TextManip (
    Viewer* v, const char* sample, int samplen, Coord x, Coord y,
    Painter* p, Coord h, Coord tab, Tool* t
) {
    Init(v, p, h, tab, t, true, sample, samplen);
    _xpos = x;
    _ypos = y;
    _prepositioned = true;
}

void TextManip::Init (
    Viewer* v, Painter* p, Coord h, Coord tab, Tool* t, boolean multiline,
    const char* sample, int samplen
) {
    _bufsize = (CHARBUFSIZE > samplen) ? CHARBUFSIZE : samplen*2;
    _buf = new char[_bufsize];
    if (samplen > 0) {
        strncpy(_buf, sample, samplen);
    }
    _text = new TextBuffer(_buf, samplen, _bufsize);

    _viewer = v;
    _painter = p;
    Ref(_painter);
    _lineHt = h;
    _tabWidth = tab;
    _multiline = multiline;
    _tool = t;
    _dot = _mark = 0;
    InitTextDisplay(sample, samplen);
}

void TextManip::InitTextDisplay (const char* sample, int samplen) {
    _display = new TextDisplay(true);
    _display->LineHeight(_lineHt);
    _display->TabWidth(_tabWidth);

    if (samplen > 0) {
        int beg, end, lineSize, nextBeg, line = 0;

        for (beg = 0; beg < samplen; beg = nextBeg) {
            GetLine(sample, samplen, beg, end, lineSize, nextBeg);
            _display->ReplaceText(line, &sample[beg], lineSize);
            ++line;
        }
    }
}

TextManip::~TextManip () { 
    delete _text;
    delete _display;
    Unref(_painter);
}

void TextManip::SetViewer (Viewer* v) { _viewer = v; }
Viewer* TextManip::GetViewer () { return _viewer; }
void TextManip::SetTool (Tool* t) { _tool = t; }
Tool* TextManip::GetTool () { return _tool; }
Painter* TextManip::GetPainter () { return _painter; }
Coord TextManip::GetLineHeight () { return _lineHt; }
Coord TextManip::GetTabWidth () { return _tabWidth; }

void TextManip::GetPosition (Coord& x, Coord& y) {
    x = _xpos;
    y = _ypos;
}

const char* TextManip::GetText (int& size) { 
    size = _text->Length();
    return _buf;
}

void TextManip::CheckBuf (int more) {
    int _textlen = _text->Length();
    char* new_buf;

    if (_textlen + more >= _bufsize) {
        _bufsize = (_textlen + more) * 2;
        new_buf = new char[_bufsize];
        strncpy(new_buf, _buf, _textlen);
        delete _text;
        delete _buf;
        _buf = new_buf;
        _text = new TextBuffer(_buf, _textlen, _bufsize);
    }
}

void TextManip::PlaceTextDisplay (Coord xpos, Coord ypos) {
    GetViewer()->InitTextDisplay(_display, _painter);

    Transformer* rel = _painter->GetTransformer();
    if (rel != nil) rel->InvTransform(xpos, ypos);

    int l = xpos;
    int r = l + _display->Width();
    int t = ypos + _lineHt-1;
    int b = t - _display->Height();
    _display->Resize(l, b, r, t);
}

void TextManip::Grasp (Event& e) {
    _grasp_e = e;

    Viewer* v = GetViewer();
    Selection* s = v->GetSelection();
    v->Constrain(e.x, e.y);

    _selecting = true;
    if (!_prepositioned) {
        _xpos = e.x;
        _ypos = e.y;
    }

    PlaceTextDisplay(_xpos, _ypos);
    Coord l, b, r, t;
    _display->CaretStyle(BarCaret);
    _display->Bounds(l, b, r, t);
    _display->Redraw(l, b, r, t);

    _selection = new Selection(s);
    s->Clear();

    if (_prepositioned) {
        Select(Locate(e.x, e.y));
    }
}

boolean TextManip::Manipulating (Event& e) {
    boolean manipulating = true;

    if (e.eventType == KeyEvent) {
        manipulating = HandleKey(e);

    } else if (e.eventType == MotionEvent && _selecting) {
        SelectMore(Locate(e.x, e.y));

    } else if (e.eventType == DownEvent) {
        if (e.shift) {
            SelectMore(Locate(e.x, e.y));
            _selecting = true;

        } else if (Contains(e.x, e.y)) {
            Select(Locate(e.x, e.y));
            _selecting = true;

        } else {
            manipulating = false;
        }

    } else if (e.eventType == UpEvent) {
        _selecting = false;
    }
    return manipulating;
}

void TextManip::Effect (Event& e) {
    Viewer* v = GetViewer();

    _display->CaretStyle(NoCaret);
    Select(0);
    v->GetSelection()->Merge(_selection);
    v->UnRead(e);
    v->IncurTextDisplayDamage(_display, _painter);
    delete _selection;
}

boolean TextManip::HandleKey (Event& e) {
    World* world = GetViewer()->GetWorld();
    char c = e.keystring[0];
    boolean manipulating = true;

    switch (c) {
        case '\007':  world->RingBell(1); break;
        case '\001':  BeginningOfLine(); break;
        case '\005':  EndOfLine(); break;
        case '\006':  ForwardCharacter(1); break;
        case '\002':  BackwardCharacter(1); break;
        case '\016':  ForwardLine(1); break;
        case '\020':  BackwardLine(1); break;
        case '\013':  DeleteLine(); break;
        case '\004':  DeleteCharacter(1); break;
        case '\010':  DeleteCharacter(-1); break;
        case '\177':  DeleteCharacter(-1); break;
        case '\011':  InsertCharacter('\t'); break;
        case '\015':  if (_multiline) InsertCharacter('\n'); break;
        case '\033':  manipulating = false; break;
        default:
            if (!iscntrl(c & 0x7f)) {
                InsertCharacter(c);
            }
            break;
    }
    return manipulating;
}

void TextManip::InsertCharacter (char c) {
    DeleteSelection();
    InsertText(&c, 1);
}

void TextManip::DeleteCharacter (int count) {
    if (_dot != _mark) {
        DeleteSelection();
    } else {
        DeleteText(count);
    }
}

void TextManip::InsertText (const char* s, int count) {
    CheckBuf(count);
    count = _text->Insert(_dot, s, count);
    int sline = _text->LineNumber(_dot);
    int fline = _text->LineNumber(_dot + count);
    if (sline == fline) {
        int offset = _text->LineOffset(_dot);
        _display->InsertText(sline, offset, _text->Text(_dot), count);
    } else {
        _display->InsertLinesAfter(sline, fline-sline);
        for (int i = sline; i <= fline; ++i) {
            int bol = _text->BeginningOfLine(_text->LineIndex(i));
            int eol = _text->EndOfLine(bol);
            _display->ReplaceText(i, _text->Text(bol, eol), eol-bol);
        }
    }
    Select(_dot + count);
}

void TextManip::DeleteText (int count) {
    int d = _dot;
    int c = count;
    while (c > 0) {
        d = _text->NextCharacter(d);
        --c;
    }
    while (c < 0) {
        _dot = _text->PreviousCharacter(_dot);
        ++c;
    }
    count = d - _dot;
    int sline = _text->LineNumber(_dot);
    int fline = _text->LineNumber(d);
    _text->Delete(_dot, count);
    if (sline == fline) {
        int offset = _text->LineOffset(_dot);
        _display->DeleteText(sline, offset, count);
    } else {
        int bol = _text->BeginningOfLine(_dot);
        int eol = _text->EndOfLine(_dot);
        _display->DeleteLinesAfter(sline, fline-sline);
        _display->ReplaceText(sline, _text->Text(bol, eol), eol-bol);
    }
    Select(_dot);
}

void TextManip::DeleteLine () {
    Select(_text->BeginningOfLine(_mark), _text->BeginningOfNextLine(_mark));
    DeleteSelection();
}

void TextManip::DeleteSelection () {
    if (_mark != _dot) {
        DeleteText(_mark - _dot);
    }
}

void TextManip::BeginningOfSelection () {
    Select(min(_mark, _dot));
}

void TextManip::EndOfSelection () {
    Select(max(_mark, _dot));
}

void TextManip::BeginningOfWord () {
    if (_dot != _mark) {
        Select(min(_mark, _dot));
    } else {
        Select(_text->BeginningOfWord(_dot));
    }
}

void TextManip::EndOfWord () {
    if (_dot != _mark) {
        Select(max(_mark, _dot));
    } else {
        Select(_text->EndOfWord(_dot));
    }
}

void TextManip::BeginningOfLine () {
    if (_dot != _mark) {
        Select(min(_mark, _dot));
    } else {
        Select(_text->BeginningOfLine(_dot));
    }
}

void TextManip::EndOfLine () {
    if (_dot != _mark) {
        Select(max(_mark, _dot));
    } else {
        Select(_text->EndOfLine(_dot));
    }
}

void TextManip::BeginningOfText () {
    Select(_text->BeginningOfText());
}

void TextManip::EndOfText () {
    Select(_text->EndOfText());
}

void TextManip::ForwardCharacter (int count) {
    if (_mark != _dot) {
        Select(max(_mark, _dot));
    } else {
        int d = _dot;
        while (count > 0) {
            d = _text->NextCharacter(d);
            --count;
        }
        Select(d);
    }
}

void TextManip::BackwardCharacter (int count) {
    if (_dot != _mark) {
        Select(min(_mark, _dot));
    } else {
        int d = _dot;
        while (count > 0) {
            d = _text->PreviousCharacter(d);
            --count;
        }
        Select(d);
    }
}

void TextManip::ForwardLine (int count) {
    if (_dot != _mark) {
        Select(max(_mark, _dot));
    } else {
        int d = _dot;
        while (count > 0) {
            d = _text->BeginningOfNextLine(d);
            --count;
        }
        Select(d);
    }
}

void TextManip::BackwardLine (int count) {
    if (_dot != _mark) {
        Select(min(_mark, _dot));
    } else {
        int d = _dot;
        while (count > 0) {
            d = _text->BeginningOfLine(_text->EndOfPreviousLine(d));
            --count;
        }
        Select(d);
    }
}

void TextManip::ForwardWord (int count) {
    if (_dot != _mark) {
        Select(max(_mark, _dot));
    } else {
        int d = _dot;
        while (count > 0) {
            d = _text->BeginningOfNextWord(d);
            --count;
        }
        Select(d);
    }
}

void TextManip::BackwardWord (int count) {
    if (_dot != _mark) {
        Select(min(_mark, _dot));
    } else {
        int d = _dot;
        while (count > 0) {
            d = _text->BeginningOfWord(_text->EndOfPreviousWord(d));
            --count;
        }
        Select(d);
    }
}

void TextManip::Select (int d) {
    Select(d, d);
}

void TextManip::SelectMore (int m) {
    Select(_dot, m);
}

void TextManip::SelectAll () {
    Select(_text->EndOfText(), _text->BeginningOfText());
}

void TextManip::Select (int d, int m) {
    int oldl = min(_dot, _mark);
    int oldr = max(_dot, _mark);
    int newl = min(d, m);
    int newr = max(d, m);
    if (oldl == oldr && newl != newr) {
        _display->CaretStyle(NoCaret);
    }
    if (newr < oldl || newl > oldr) {
        if (oldr > oldl) {
            _display->RemoveStyle(
                _text->LineNumber(oldl), _text->LineOffset(oldl),
                _text->LineNumber(oldr-1), _text->LineOffset(oldr-1),
                Reversed
            );
        }
        if (newr > newl) {
            _display->AddStyle(
                _text->LineNumber(newl), _text->LineOffset(newl),
                _text->LineNumber(newr-1), _text->LineOffset(newr-1),
                Reversed
            );
        }
    } else {
        if (newl < oldl) {
            _display->AddStyle(
                _text->LineNumber(newl), _text->LineOffset(newl),
                _text->LineNumber(oldl-1), _text->LineOffset(oldl-1),
                Reversed
            );
        } else if (newl > oldl) {
            _display->RemoveStyle(
                _text->LineNumber(oldl), _text->LineOffset(oldl),
                _text->LineNumber(newl-1), _text->LineOffset(newl-1),
                Reversed
            );
        }
        if (newr > oldr) {
            _display->AddStyle(
                _text->LineNumber(oldr), _text->LineOffset(oldr),
                _text->LineNumber(newr-1), _text->LineOffset(newr-1),
                Reversed
            );
        } else if (newr < oldr) {
            _display->RemoveStyle(
                _text->LineNumber(newr), _text->LineOffset(newr),
                _text->LineNumber(oldr-1), _text->LineOffset(oldr-1),
                Reversed
            );
        }
    }
    if (oldl != oldr && newl == newr) {
        _display->CaretStyle(BarCaret);
    }
    _dot = d;
    _mark = m;
    if (_dot == _mark) {
        _display->Caret(_text->LineNumber(_dot), _text->LineOffset(_dot));
    }
}

boolean TextManip::Contains (Coord x, Coord y) {
    Transformer* rel = _painter->GetTransformer();

    if (rel != nil) rel->InvTransform(x, y);

    int line = _display->LineNumber(y);
    int index = _display->LineIndex(line, x);

    return
        x >= _display->Left(line, _text->BeginningOfLine(index)) &&
        x <= _display->Right(line, _text->EndOfLine(index)) &&
        y >= _display->Base(line) &&
        y <= _display->Top(line);
}

int TextManip::Locate (Coord x, Coord y) {
    Transformer* rel = _painter->GetTransformer();

    if (rel != nil) rel->InvTransform(x, y);

    int line = _display->LineNumber(y);
    int index = _display->LineIndex(line, x);
    int l = _text->LineIndex(line);
    int i = 0;
    while (i < index) {
        l = _text->NextCharacter(l);
        i += 1;
    }
    return l;
}
