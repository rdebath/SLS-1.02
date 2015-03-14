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
 * PostScriptView - idraw-compatible PostScript external representation 
 * for graphical components.
 */

#ifndef unidraw_components_psview_h
#define unidraw_components_psview_h

#include <Unidraw/Components/externview.h>

#include <IV-2_6/_enter.h>

class PostScriptView : public PreorderView {
public:
    virtual boolean Emit(ostream&);
    GraphicComp* GetGraphicComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    PostScriptView(GraphicComp* = nil);

    virtual void Comments(ostream&);

    virtual void PSVersion(ostream&);
    virtual void Creator(ostream&);
    virtual void FontNames(ostream&);
    virtual void Pages(ostream&);
    virtual void BoundingBox(ostream&);

    virtual void Prologue(ostream&);
    virtual void Version(ostream&);
    virtual void GridSpacing(ostream&);
    virtual void Trailer(ostream&);

    virtual void MinGS(ostream&);
    virtual void FullGS(ostream&);
    virtual void TextGS(ostream&);
    virtual void Brush(ostream&);
    virtual void FgColor(ostream&);
    virtual void BgColor(ostream&);
    virtual void Font(ostream&);
    virtual void Pattern(ostream&);
    virtual void Transformation(ostream&);

    virtual void SetPSFonts(UList* = nil);
    virtual UList* GetPSFonts();

    virtual void GetBox(Coord&, Coord&, Coord&, Coord&);
    virtual void GetGridSpacing(float&, float&);

    virtual void ConstProcs(ostream&);
    virtual void BeginProc(ostream&);
    virtual void EndProc(ostream&);

    virtual void SetGSProcs(ostream&);
    virtual void SetBrushProc(ostream&);
    virtual void SetFgColorProc(ostream&);
    virtual void SetBgColorProc(ostream&);
    virtual void SetFontProc(ostream&);
    virtual void SetPatternProc(ostream&);

    virtual void ObjectProcs(ostream&);
    virtual void BSplineProc(ostream&);
    virtual void CircleProc(ostream&);
    virtual void ClosedBSplineProc(ostream&);
    virtual void EllipseProc(ostream&);
    virtual void LineProc(ostream&);
    virtual void MultiLineProc(ostream&);
    virtual void PolygonProc(ostream&);
    virtual void RectangleProc(ostream&);
    virtual void TextProc(ostream&);

    virtual void MiscProcs(ostream&);
    virtual void DefinitionProc(ostream&);
    virtual void FillProc(ostream&);
    virtual void StrokeProc(ostream&);
    virtual void ShowProc(ostream&);
    virtual void PatternProc(ostream&);
    virtual void MinMaxProcs(ostream&);
    virtual void MidpointProc(ostream&);
    virtual void ThirdpointProc(ostream&);
    virtual void SubsplineProc(ostream&);
    virtual void StoreVerticesProc(ostream&);

    PSFont* GetFont(UList*);
    PostScriptView* View(UList*);
    PostScriptView* CreatePSView(GraphicComp*);
private:
    UList* _fonts;
};

class PostScriptViews : public PostScriptView {
public:
    PostScriptViews(GraphicComps* = nil);
    virtual ~PostScriptViews();

    virtual boolean Emit(ostream&);
    virtual boolean Definition(ostream&);
    virtual void Update();
    GraphicComps* GetGraphicComps();

    virtual ExternView* GetView(Iterator);
    virtual void SetView(ExternView*, Iterator&);

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    UList* Elem(Iterator);
    void DeleteView(Iterator&);
    void DeleteViews();
protected:
    UList* _views;
};

#include <IV-2_6/_leave.h>

#endif
