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
 * IdrawComp implementation.
 */

#include "idarrows.h"
#include "idcomp.h"
#include "idcatalog.h"
#include "idclasses.h"

#include <Unidraw/grid.h>
#include <Unidraw/viewer.h>

#include <stream.h>

/*****************************************************************************/

static const float GRID_XINCR = 8;                 // default grid spacing
static const float GRID_YINCR = 8;

/*****************************************************************************/

IdrawComp::IdrawComp () { SetGridSpacing(GRID_XINCR, GRID_YINCR); }
ClassId IdrawComp::GetClassId () { return IDRAW_COMP; }

ClassId IdrawComp::GetSubstId (const char*& delim) {
    delim = "%END_IDRAW_COMP%";
    return GraphicComps::GetClassId();
}

boolean IdrawComp::IsA (ClassId id) {
    return IDRAW_COMP == id || GraphicComps::IsA(id);
}

void IdrawComp::SetGridSpacing (float xincr, float yincr) {
    _xincr = xincr;
    _yincr = yincr;
}

void IdrawComp::GetGridSpacing (float& xincr, float& yincr) {
    xincr = _xincr;
    yincr = _yincr;
}

/*****************************************************************************/

IdrawView::IdrawView (IdrawComp* subj) : GraphicViews(subj) { }
ClassId IdrawView::GetClassId () { return IDRAW_VIEW; }

boolean IdrawView::IsA (ClassId id) {
    return IDRAW_VIEW == id || GraphicViews::IsA(id);
}

/*****************************************************************************/

PSIdraw::PSIdraw (IdrawComp* subj) : PostScriptViews(subj) { }
ClassId PSIdraw::GetClassId () { return PS_IDRAW; }

boolean PSIdraw::IsA (ClassId id) { 
    return PS_IDRAW == id || PostScriptViews::IsA(id);
}

void PSIdraw::Creator (ostream& out) {
    out << "%%Creator: idraw\n";
}

void PSIdraw::Prologue (ostream& out) {
    out << "%%BeginIdrawPrologue\n";
    ArrowHeader(out);
    out << "%%EndIdrawPrologue\n\n";

    PostScriptView::Prologue(out);
}

void PSIdraw::GridSpacing (ostream& out) {
    float xincr, yincr;
    IdrawComp* comp = (IdrawComp*) GetSubject();
    comp->GetGridSpacing(xincr, yincr);

    out << "Grid " << xincr << " " << yincr << " ";
}

void PSIdraw::ConstProcs (ostream& out) {
    int arrowWidth = round(ARROWWIDTH*points);
    int arrowHeight = round(ARROWHEIGHT*points);

    out << "/arrowHeight " << arrowHeight << " def\n";
    out << "/arrowWidth " << arrowWidth << " def\n\n";

    PostScriptViews::ConstProcs(out);
}

void PSIdraw::LineProc (ostream& out) {
    out << "/Line {\n";
    out << "0 begin\n";
    out << "2 storexyn\n";
    out << "newpath\n";
    out << "x 0 get y 0 get moveto\n";
    out << "x 1 get y 1 get lineto\n";
    out << "brushNone not { istroke } if\n";
    out << "0 0 1 1 leftarrow\n";
    out << "0 0 1 1 rightarrow\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PSIdraw::MultiLineProc (ostream& out) {
    out << "/MLine {\n";
    out << "0 begin\n";
    out << "storexyn\n";
    out << "newpath\n";
    out << "n 1 gt {\n";
    out << "x 0 get y 0 get moveto\n";
    out << "1 1 n 1 sub {\n";
    out << "/i exch def\n";
    out << "x i get y i get lineto\n";
    out << "} for\n";
    out << "patternNone not brushLeftArrow not brushRightArrow not and and ";
    out << "{ ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "0 0 1 1 leftarrow\n";
    out << "n 2 sub dup n 1 sub dup rightarrow\n";
    out << "} if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PSIdraw::BSplineProc (ostream& out) {
    out << "/BSpl {\n";
    out << "0 begin\n";
    out << "storexyn\n";
    out << "newpath\n";
    out << "n 1 gt {\n";
    out << "0 0 0 0 0 0 1 1 true subspline\n";
    out << "n 2 gt {\n";
    out << "0 0 0 0 1 1 2 2 false subspline\n";
    out << "1 1 n 3 sub {\n";
    out << "/i exch def\n";
    out << "i 1 sub dup i dup i 1 add dup i 2 add dup false subspline\n";
    out << "} for\n";
    out << "n 3 sub dup n 2 sub dup n 1 sub dup 2 copy false subspline\n";
    out << "} if\n";
    out << "n 2 sub dup n 1 sub dup 2 copy 2 copy false subspline\n";
    out << "patternNone not brushLeftArrow not brushRightArrow not and and ";
    out << "{ ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "0 0 1 1 leftarrow\n";
    out << "n 2 sub dup n 1 sub dup rightarrow\n";
    out << "} if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PSIdraw::SetBrushProc (ostream& out) {
    out << "/SetB {\n";
    out << "dup type /nulltype eq {\n";
    out << "pop\n";
    out << "false /brushRightArrow idef\n";
    out << "false /brushLeftArrow idef\n";
    out << "true /brushNone idef\n";
    out << "} {\n";
    out << "/brushDashOffset idef\n";
    out << "/brushDashArray idef\n";
    out << "0 ne /brushRightArrow idef\n";
    out << "0 ne /brushLeftArrow idef\n";
    out << "/brushWidth idef\n";
    out << "false /brushNone idef\n";
    out << "} ifelse\n";
    out << "} def\n\n";
}

void PSIdraw::ArrowHeader (ostream& out) {
    out << "/arrowhead {\n";
    out << "0 begin\n";
    out << "transform originalCTM itransform\n";
    out << "/taily exch def\n";
    out << "/tailx exch def\n";
    out << "transform originalCTM itransform\n";
    out << "/tipy exch def\n";
    out << "/tipx exch def\n";
    out << "/dy tipy taily sub def\n";
    out << "/dx tipx tailx sub def\n";
    out << "/angle dx 0 ne dy 0 ne or { dy dx atan } { 90 } ifelse def\n";
    out << "gsave\n";
    out << "originalCTM setmatrix\n";
    out << "tipx tipy translate\n";
    out << "angle rotate\n";
    out << "newpath\n";
    out << "arrowHeight neg arrowWidth 2 div moveto\n";
    out << "0 0 lineto\n";
    out << "arrowHeight neg arrowWidth 2 div neg lineto\n";
    out << "patternNone not {\n";
    out << "originalCTM setmatrix\n";
    out << "/padtip arrowHeight 2 exp 0.25 arrowWidth 2 exp mul add sqrt ";
    out << "brushWidth mul\n";
    out << "arrowWidth div def\n";
    out << "/padtail brushWidth 2 div def\n";
    out << "tipx tipy translate\n";
    out << "angle rotate\n";
    out << "padtip 0 translate\n";
    out << "arrowHeight padtip add padtail add arrowHeight div dup scale\n";
    out << "arrowheadpath\n";
    out << "ifill\n";
    out << "} if\n";
    out << "brushNone not {\n";
    out << "originalCTM setmatrix\n";
    out << "tipx tipy translate\n";
    out << "angle rotate\n";
    out << "arrowheadpath\n";
    out << "istroke\n";
    out << "} if\n";
    out << "grestore\n";
    out << "end\n";
    out << "} dup 0 9 dict put def\n\n";
    out << "/arrowheadpath {\n";
    out << "newpath\n";
    out << "arrowHeight neg arrowWidth 2 div moveto\n";
    out << "0 0 lineto\n";
    out << "arrowHeight neg arrowWidth 2 div neg lineto\n";
    out << "} def\n\n";
    out << "/leftarrow {\n";
    out << "0 begin\n";
    out << "y exch get /taily exch def\n";
    out << "x exch get /tailx exch def\n";
    out << "y exch get /tipy exch def\n";
    out << "x exch get /tipx exch def\n";
    out << "brushLeftArrow { tipx tipy tailx taily arrowhead } if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
    out << "/rightarrow {\n";
    out << "0 begin\n";
    out << "y exch get /tipy exch def\n";
    out << "x exch get /tipx exch def\n";
    out << "y exch get /taily exch def\n";
    out << "x exch get /tailx exch def\n";
    out << "brushRightArrow { tipx tipy tailx taily arrowhead } if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}
