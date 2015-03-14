/*
 * Copyright (c) 1991 Stanford University
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
 * StencilComp definitions.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/unidraw.h>

#include <Unidraw/Components/stencilcomp.h>

#include <Unidraw/Graphic/ustencil.h>

#include <InterViews/bitmap.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

static const int no_mask = 0;
static const int mask_equals_image = 1;
static const int valid_mask = 2;

/*****************************************************************************/

ClassId StencilComp::GetClassId () { return STENCIL_COMP; }

boolean StencilComp::IsA (ClassId id) {
    return STENCIL_COMP == id || GraphicComp::IsA(id);
}

Component* StencilComp::Copy () {
    return new StencilComp((UStencil*) GetGraphic()->Copy(), _filename);
}

StencilComp::StencilComp (UStencil* s, const char* filename) : GraphicComp(s) {
    _filename = (filename == nil) ? nil : strnew(filename);
}

StencilComp::~StencilComp () { delete _filename; }
UStencil* StencilComp::GetStencil () { return (UStencil*) GetGraphic(); }
const char* StencilComp::GetFileName () { return _filename; }

void StencilComp::Read (istream& in) {
    GraphicComp::Read(in);
    Bitmap* image = ReadBitmap(in);
    Bitmap* mask = nil;

    Skip(in);
    int m;
    in >> m;

    if (m == valid_mask) {
        mask = ReadBitmap(in);
    } else if (m == mask_equals_image) {
        mask = image;
    }

    UStencil* stencil = new UStencil(image, mask);
    stencil->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    stencil->SetColors(fg, bg);

    Transformer* t = ReadTransformer(in);
    stencil->SetTransformer(t);
    Unref(t);

    SetGraphic(stencil);
    _filename = ReadString(in);
}

void StencilComp::Write (ostream& out) {
    GraphicComp::Write(out);
    UStencil* stencil = GetStencil();
    Bitmap* image, *mask;
    stencil->GetOriginal(image, mask);

    WriteBitmap(image, out);
    Mark(out);

    if (mask == nil) {
        out << no_mask;
    } else if (mask == image) {
        out << mask_equals_image;
    } else {
        out << valid_mask;
        WriteBitmap(mask, out);
    }

    WriteBgFilled(stencil->BgFilled(), out);
    WriteColor(stencil->GetFgColor(), out);
    WriteColor(stencil->GetBgColor(), out);
    WriteTransformer(stencil->GetTransformer(), out);
    WriteString(_filename, out);
}

/*****************************************************************************/

StencilComp* StencilView::GetStencilComp () {
    return (StencilComp*) GetSubject();
}

ClassId StencilView::GetClassId () { return STENCIL_VIEW; }

boolean StencilView::IsA (ClassId id) {
    return STENCIL_VIEW == id || GraphicView::IsA(id);
}

StencilView::StencilView (StencilComp* subj) : GraphicView(subj) { }

void StencilView::Update () {
    Graphic* stencil = GetGraphic();

    IncurDamage(stencil);
    *stencil = *GetStencilComp()->GetGraphic();
    IncurDamage(stencil);
    EraseHandles();
}

Graphic* StencilView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();
    
    if (graphic == nil) {
        StencilComp* stencilComp = GetStencilComp();
        Bitmap* image, *mask;
        stencilComp->GetStencil()->GetOriginal(image, mask);
        graphic = new UStencil(image, mask, stencilComp->GetGraphic());
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

PSStencil::PSStencil (StencilComp* subj) : PostScriptView(subj) { }
ClassId PSStencil::GetClassId () { return PS_STENCIL; }

boolean PSStencil::IsA (ClassId id) { 
    return PS_STENCIL == id || PostScriptView::IsA(id);
}

boolean PSStencil::Definition (ostream& out) {
    StencilComp* comp = (StencilComp*) GetSubject();
    Bitmap* image, *mask;
    comp->GetStencil()->GetOriginal(image, mask);
    char* tag = (image == mask) ? "SSten" : "FSten";
    Coord w = image->Width();
    Coord h = image->Height();

    out << "Begin " << MARK << " " << tag << "\n";
    FgColor(out);
    BgColor(out);
    Transformation(out);

    out << MARK << "\n";
    out << w << " " << h << " " << tag << " ";
    out << "{ currentfile "<< (w + 7) / 8 << " string readhexstring pop }\n";
    out << "imagemask";

    unidraw->GetCatalog()->WriteBitmapData(image, out);

    out << "\nEnd\n\n";

    return out.good();
}
