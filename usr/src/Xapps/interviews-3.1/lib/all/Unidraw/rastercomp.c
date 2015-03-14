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
 * RasterComp definitions.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/unidraw.h>

#include <Unidraw/Components/rastercomp.h>

#include <Unidraw/Graphic/rasterrect.h>

#include <InterViews/raster.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

static const int color_depth = 8;               // bits per color in PostScript

/*****************************************************************************/

ClassId RasterComp::GetClassId () { return RASTER_COMP; }

boolean RasterComp::IsA (ClassId id) {
    return RASTER_COMP == id || GraphicComp::IsA(id);
}

Component* RasterComp::Copy () {
    return new RasterComp((RasterRect*) GetGraphic()->Copy(), _filename);
}

RasterComp::RasterComp (RasterRect* s, const char* filename) : GraphicComp(s) {
    _filename = (filename == nil) ? nil : strnew(filename);
}

RasterComp::~RasterComp () { delete _filename; }

RasterRect* RasterComp::GetRasterRect () { return (RasterRect*) GetGraphic(); }
const char* RasterComp::GetFileName () { return _filename; }

void RasterComp::Read (istream& in) {
    GraphicComp::Read(in);
    Raster* raster = ReadRaster(in);
    RasterRect* rr = new RasterRect(raster);

    Transformer* t = ReadTransformer(in);
    rr->SetTransformer(t);
    Unref(t);

    SetGraphic(rr);
    _filename = ReadString(in);
}

void RasterComp::Write (ostream& out) {
    GraphicComp::Write(out);
    RasterRect* rr = GetRasterRect();
    Raster* raster = rr->GetOriginal();

    WriteRaster(raster, out);
    WriteTransformer(rr->GetTransformer(), out);
    WriteString(_filename, out);
}

/*****************************************************************************/

RasterComp* RasterView::GetRasterComp () {
    return (RasterComp*) GetSubject();
}

ClassId RasterView::GetClassId () { return RASTER_VIEW; }

boolean RasterView::IsA (ClassId id) {
    return RASTER_VIEW == id || GraphicView::IsA(id);
}

RasterView::RasterView (RasterComp* subj) : GraphicView(subj) { }

void RasterView::Update () {
    Graphic* raster = GetGraphic();

    IncurDamage(raster);
    *raster = *GetRasterComp()->GetGraphic();
    IncurDamage(raster);
    EraseHandles();
}

Graphic* RasterView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();
    
    if (graphic == nil) {
        RasterRect* rr = GetRasterComp()->GetRasterRect();
        graphic = new RasterRect(rr->GetOriginal(), rr);
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

PSRaster::PSRaster (RasterComp* subj) : PostScriptView(subj) { }
ClassId PSRaster::GetClassId () { return PS_RASTER; }

boolean PSRaster::IsA (ClassId id) { 
    return PS_RASTER == id || PostScriptView::IsA(id);
}

boolean PSRaster::Definition (ostream& out) {
    RasterComp* comp = (RasterComp*) GetSubject();
    Raster* raster = comp->GetRasterRect()->GetOriginal();
    Coord w = raster->Width();
    Coord h = raster->Height();

    out << "Begin " << MARK << " " << "Rast\n";
    Transformation(out);

    out << MARK << "\n";
    out << w << " " << h << " " << color_depth << " Rast ";
    out << "{ currentfile ";
    out << (w * color_depth + 7) / 8 << " ";
    out << "string readhexstring pop }\n";
    out << "image";

    Catalog* catalog = unidraw->GetCatalog();
    catalog->WriteGraymapData(raster, out);

    catalog->Mark(out);
    out << "colorimage";
    catalog->WriteRasterData(raster, out);

    out << "\nEnd\n\n";

    return out.good();
}
