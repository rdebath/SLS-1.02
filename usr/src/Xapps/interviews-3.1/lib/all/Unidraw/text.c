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
 * Text component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/font.h>

#include <Unidraw/Components/text.h>

#include <Unidraw/Graphic/picture.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/textbuffer.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <ctype.h>
#include <math.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

static const int SBUFSIZE = 10000;
static char sbuf[SBUFSIZE];

/*****************************************************************************/

ClassId TextComp::GetClassId () { return TEXT_COMP; }

boolean TextComp::IsA (ClassId id) {
    return TEXT_COMP == id || GraphicComp::IsA(id);
}

Component* TextComp::Copy () {
    return new TextComp((TextGraphic*) GetGraphic()->Copy());
}

TextComp::TextComp (TextGraphic* graphic) : GraphicComp(graphic) { }

void TextComp::Interpret (Command* cmd) {
    TextGraphic* gr = (TextGraphic*) GetGraphic();

    if (cmd->IsA(BRUSH_CMD) || cmd->IsA(PATTERN_CMD)) {
        // do nothing

    } else if (cmd->IsA(FONT_CMD)) {
        PSFont* font = ((FontCmd*) cmd)->GetFont();
        cmd->Store(this, new VoidData(gr->GetFont()));
        gr->SetFont(font);
        gr->SetLineHeight(font->Height());      // hack; should be state var
        Notify();

    } else {
        GraphicComp::Interpret(cmd);
    }
}

void TextComp::Uninterpret (Command* cmd) {
    Graphic* gr = GetGraphic();

    if (cmd->IsA(BRUSH_CMD) || cmd->IsA(PATTERN_CMD)) {
        // do nothing

    } else if (cmd->IsA(FONT_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
        PSFont* font = (PSFont*) vd->_void;
        gr->SetFont(font);
        Notify();

    } else {
        GraphicComp::Uninterpret(cmd);
    }
}

TextGraphic* TextComp::GetText () { return (TextGraphic*) GetGraphic(); }

void TextComp::Read (istream& in) {
    GraphicComp::Read(in);
    int lineHt;

    in >> lineHt;
    char* string = ReadString(in);
    TextGraphic* text = new TextGraphic(string, lineHt);
    delete string;

    text->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    text->SetColors(fg, bg);
    text->SetFont(ReadFont(in));

    Transformer* t = ReadTransformer(in);
    text->SetTransformer(t);
    Unref(t);

    SetGraphic(text);
}

void TextComp::Write (ostream& out) {
    GraphicComp::Write(out);
    TextGraphic* text = GetText();

    out << text->GetLineHeight() << "\n";
    WriteString(text->GetOriginal(), out);

    WriteBgFilled(text->BgFilled(), out);
    WriteColor(text->GetFgColor(), out);
    WriteColor(text->GetBgColor(), out);
    WriteFont(text->GetFont(), out);
    WriteTransformer(text->GetTransformer(), out);
}

/****************************************************************************/

TextComp* TextView::GetTextComp () { return (TextComp*) GetSubject(); }
ClassId TextView::GetClassId () { return TEXT_VIEW; }

boolean TextView::IsA (ClassId id) {
    return TEXT_VIEW == id || GraphicView::IsA(id);
}

TextView::TextView (TextComp* subj) : GraphicView(subj) { }

boolean TextView::TextChanged () {
    TextGraphic* gview = (TextGraphic*) GetGraphic();
    TextGraphic* gsubj = (TextGraphic*) GetTextComp()->GetGraphic();
    
    return *gview != *gsubj;
}

void TextView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        Transformer total;
        GetGraphic()->TotalTransformation(total);

        float tx0, ty0;
        total.Transform(0., 0., tx0, ty0);
        ((AlignToGridCmd*) cmd)->Align(this, tx0, ty0);

    } else {
        GraphicView::Interpret(cmd);
    }
}

void TextView::Update () {
    TextGraphic* gview = (TextGraphic*) GetGraphic();
    TextGraphic* gsubj = (TextGraphic*) GetTextComp()->GetGraphic();

    IncurDamage(gview);
    * (Graphic*) gview = * (Graphic*) gsubj;
    gview->SetLineHeight(gsubj->GetLineHeight());
    IncurDamage(gview);
    EraseHandles();
}

Manipulator* TextView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    Editor* ed = v->GetEditor();
    int tabWidth = round(.5*inch);

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        FontVar* fontVar = (FontVar*) ed->GetState("FontVar");
	ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
        PSFont* font = (fontVar == nil) ? psstdfont : fontVar->GetFont();
	PSColor* fg = (colVar == nil) ? psblack : colVar->GetFgColor();
        int lineHt = font->GetLineHt();

        Painter* painter = new Painter;
        painter->FillBg(false);
        painter->SetFont(font);
	painter->SetColors(fg, nil);
        painter->SetTransformer(rel);

        m = new TextManip(v, painter, lineHt, tabWidth, tool);

    } else if (tool->IsA(RESHAPE_TOOL)) {
        TextGraphic* textgr = (TextGraphic*) GetGraphic();
        Painter* painter = new Painter;
        int lineHt = textgr->GetLineHeight();
        Coord xpos, ypos;
        rel = new Transformer;
        const char* text = textgr->GetOriginal();
        int size = strlen(text);
        
        textgr->TotalTransformation(*rel);
        rel->Transform(0, 0, xpos, ypos);
        painter->FillBg(false);
        painter->SetFont(textgr->GetFont());
	painter->SetColors(textgr->GetFgColor(), nil);
        painter->SetTransformer(rel);
        Unref(rel);

        m = new TextManip(
            v, text, size, xpos, ypos, painter, lineHt, tabWidth, tool
        );

    } else {
        m = GraphicView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* TextView::InterpretManipulator (Manipulator* m) {
    Viewer* v = m->GetViewer();
    Editor* ed = v->GetEditor();
    Tool* tool = m->GetTool();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL) || tool->IsA(RESHAPE_TOOL)) {
        TextManip* tm = (TextManip*) m;
        int size;
        const char* text = tm->GetText(size);

        if (size == 0) {
            if (tool->IsA(RESHAPE_TOOL)) {
                cmd = new DeleteCmd(ed);
            } else {
                v->Update();          // to repair text display-incurred damage
            }

        } else {
            Coord xpos, ypos;
            tm->GetPosition(xpos, ypos);
            Painter* p = tm->GetPainter();
            Transformer* rel = tm->GetPainter()->GetTransformer();
            int lineHt = tm->GetLineHeight();

            Graphic* pg = GetGraphicComp()->GetGraphic();
            TextGraphic* textgr = new TextGraphic(text, lineHt, pg);

            if (tool->IsA(GRAPHIC_COMP_TOOL)) {
                textgr->SetTransformer(nil);
            }

            if (rel != nil) {
                rel->InvTransform(xpos, ypos);
            }
            textgr->Translate(xpos, ypos);
            textgr->FillBg(false);
            textgr->SetFont((PSFont*) p->GetFont());
            textgr->SetColors((PSColor*) p->GetFgColor(), nil);

            if (tool->IsA(GRAPHIC_COMP_TOOL)) {
                cmd = new PasteCmd(ed, new Clipboard(new TextComp(textgr)));
            } else {
                cmd = new ReplaceCmd(ed, new TextComp(textgr));
            }
        }

    } else {
        cmd = GraphicView::InterpretManipulator(m);
    }

    return cmd;
}

Graphic* TextView::GetGraphic () {
    TextComp* textComp;
    Graphic* graphic = GraphicView::GetGraphic();

    if (graphic == nil) {
        textComp = GetTextComp();
        graphic = textComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/****************************************************************************/

TextGraphic::TextGraphic (const char* s, int h, Graphic* gr) : ULabel(s, gr) { 
    _lineHt = h;
}

TextGraphic::TextGraphic (const char* s, Graphic* gr) : ULabel(s, gr) { 
    _lineHt = gr->GetFont()->Height();
}

boolean TextGraphic::operator == (TextGraphic& tg) {
    const char* tgstring = tg.GetOriginal();
    int tgcount = strlen(tgstring);
    int count = strlen(_string);

    return (tgcount == count) ? strcmp(tgstring, _string) == 0 : false;
}

boolean TextGraphic::operator != (TextGraphic& tg) { return !(*this == tg); }

Graphic* TextGraphic::Copy () { 
    return new TextGraphic(_string, _lineHt, this);
}

void TextGraphic::SetLineHeight (int h) { _lineHt = h; }
int TextGraphic::GetLineHeight () { return _lineHt; }

void TextGraphic::draw (Canvas* c, Graphic* gs) {
    int beg, end, lineSize, nextBeg, ypos = 0;
    int count = strlen(_string);

    update(gs);

    for (beg = 0; beg < count; beg = nextBeg) {
        GetLine(_string, count, beg, end, lineSize, nextBeg);
        _p->Text(c, &_string[beg], lineSize, 0, ypos);
        ypos -= _lineHt;
    }
}

void TextGraphic::getExtent (
    float& x0, float& y0, float& cx, float& cy, float& tol, Graphic* gs
) {
    PSFont* f = gs->GetFont();
    Coord l, b, r, t;

    CalcBox(l, b, r, t, f);
    if (gs->GetTransformer() == nil) {
	x0 = l;
	y0 = b;
	cx = float(l + r) / 2;
	cy = float(b + t) / 2;
    } else {
        transformRect(l, b, r, t, x0, y0, cx, cy, gs);
	cx = (cx + x0)/2;
	cy = (cy + y0)/2;
    }
    tol = 0;
}

void TextGraphic::CalcBox (Coord& l, Coord& b, Coord& r, Coord& t, PSFont* f) {
    int beg, end, lineSize, nextBeg;
    const char* s = GetOriginal();
    int size = strlen(s);

    l = r = 0;
    b = _lineHt;
    t = f->Height();
    
    for (beg = 0; beg < size; beg = nextBeg) {
        GetLine(s, size, beg, end, lineSize, nextBeg);
        r = max(r, f->Width(&s[beg], lineSize) - 1);
        b -= _lineHt;
    }
}

boolean TextGraphic::contains (PointObj& po, Graphic* gs) {
    PointObj pt (&po);
    PSFont* f = gs->GetFont();
    BoxObj box(0, 0, 0, f->Height());
    int beg, end, lineSize, nextBeg, ypos = 0;
    const char* s = GetOriginal();
    int size = strlen(s);

    invTransform(pt._x, pt._y, gs);
    
    for (beg = 0; beg < size; beg = nextBeg) {
        GetLine(s, size, beg, end, lineSize, nextBeg);
        box._right = f->Width(&s[beg], lineSize) - 1;
        if (box.Contains(pt)) {
            return true;
        }
        box._top -= _lineHt;
        box._bottom -= _lineHt;
    }
    return false;
}

boolean TextGraphic::RotatedIntersects (BoxObj& userb, Graphic* gs) {
    int beg, end, lineSize, nextBeg, ypos = 0;
    const char* s = GetOriginal();
    int size = strlen(s);
    PSFont* f = gs->GetFont();
    Coord x[4], tx[5];
    Coord y[4], ty[5];
    
    x[0] = x[1] = x[2] = x[3] = y[0] = y[1] = 0;
    y[2] = y[3] = f->Height();

    for (beg = 0; beg < size; beg = nextBeg) {
        GetLine(s, size, beg, end, lineSize, nextBeg);
        x[1] = x[2] = f->Width(&s[beg], lineSize) - 1;
	transformList(x, y, 4, tx, ty, gs);
	tx[4] = tx[0];
	ty[4] = ty[0];
	FillPolygonObj fp(tx, ty, 5);
	if (fp.Intersects(userb)) {
            return true;
        }
        y[0] -= _lineHt;
        y[1] -= _lineHt;
        y[2] -= _lineHt;
        y[3] -= _lineHt;
    }
    return false;
}

boolean TextGraphic::TransformedIntersects (BoxObj& userb, Graphic* gs) {
    int beg, end, lineSize, nextBeg, ypos = 0;
    const char* s = GetOriginal();
    int size = strlen(s);
    PSFont* f = gs->GetFont();
    Coord l, b, r, t = f->Height();
    Coord tl, tb, tr, tt;

    l = b = r = 0;
    
    for (beg = 0; beg < size; beg = nextBeg) {
        GetLine(s, size, beg, end, lineSize, nextBeg);
        r = f->Width(&s[beg], lineSize) - 1;
	transform(l, b, tl, tb, gs);
        transform(r, t, tr, tt, gs);
        BoxObj box(tl, tb, tr, tt);
	if (box.Intersects(userb)) {
            return true;
        }
        b -= _lineHt;
        t -= _lineHt;
    }
    return false;
}

boolean TextGraphic::UntransformedIntersects (BoxObj& userb, Graphic* gs) {
    int beg, end, lineSize, nextBeg, ypos = 0;
    const char* s = GetOriginal();
    int size = strlen(s);
    PSFont* f = gs->GetFont();
    BoxObj box(0, 0, 0, f->Height());

    for (beg = 0; beg < size; beg = nextBeg) {
        GetLine(s, size, beg, end, lineSize, nextBeg);
        box._right = f->Width(&s[beg], lineSize) - 1;
        if (box.Intersects(userb)) {
            return true;
        }
        box._top -= _lineHt;
        box._bottom -= _lineHt;
    }
    return false;
}

boolean TextGraphic::intersects (BoxObj& userb, Graphic* gs) {
    Transformer* t = gs->GetTransformer();
    boolean intersects;
    
    if (t != nil && t->Rotated()) {
        intersects = RotatedIntersects(userb, gs);
        
    } else if (t != nil) {
        intersects = TransformedIntersects(userb, gs);

    } else {
        intersects = UntransformedIntersects(userb, gs);
    }
    return intersects;
}

/****************************************************************************/

ClassId PSText::GetClassId () { return PS_TEXT; }

boolean PSText::IsA (ClassId id) { 
    return PS_TEXT == id || PostScriptView::IsA(id);
}

PSText::PSText (TextComp* subj) : PostScriptView(subj) { }

boolean PSText::Definition (ostream& out) {
    TextComp* comp = (TextComp*) GetSubject();
    TextGraphic* g = comp->GetText();
    const char* text = g->GetOriginal();
    int count = strlen(text);

    out << "Begin " << MARK << " Text\n";

    float sep = g->GetLineHeight() - 1;         // correct for vert shift
    Transformer corrected, *t = g->GetTransformer();
    corrected.Translate(0., sep);

    if (t == nil) {
        g->SetTransformer(&corrected);
        TextGS(out);
        g->SetTransformer(t);

    } else {
        t->Reference();
        corrected.Postmultiply(t);
        g->SetTransformer(&corrected);
        TextGS(out);
        g->SetTransformer(t);
        Unref(t);
    }

    out << MARK << "\n";
    out << "[\n";

    int beg, end, lineSize, nextBeg, ypos = 0;

    for (beg = 0; beg < count; beg = nextBeg) {
        GetLine(text, count, beg, end, lineSize, nextBeg);
        const char* string = Filter(&text[beg], end - beg + 1);
        out << "(" << string << ")\n";
    }

    out << "] Text\n";
    out << "End\n\n";

    return out.good();
}

// octal converts a character to the string \ddd where d is an octal digit.

static char* octal(unsigned char c, register char* p) {
    *p-- = '\0';		// backwards from terminating null...
    *p-- = (char)('0' + c%8);
    *p-- = (char)('0' + (c >>= 3)%8);
    *p-- = (char)('0' + (c >>= 3)%8);
    *p = '\\';			// ...to beginning backslash
    return p;
}

// Filter escapes embedded special characters that would cause syntax
// errors in a Postscript string.

const char* PSText::Filter (const char* string, int len) {
    TextBuffer stext(sbuf, 0, SBUFSIZE);

    for (int dot = 0; len--; string++) {
	char c = *string;

	if (!isascii(c) || iscntrl(c)) {
	    char buf[5];
	    octal(c, &buf[sizeof(buf) - 1]);
	    dot += stext.Insert(dot, buf, sizeof(buf) - 1);

	} else {
	    switch (c) {
	    case '(':
	    case ')':
	    case '\\':
		dot += stext.Insert(dot, "\\", 1);
		// fall through
	    default:
		dot += stext.Insert(dot, string, 1);
	    }
	}
    }
    stext.Insert(dot, "", 1);

    return stext.Text();
}
