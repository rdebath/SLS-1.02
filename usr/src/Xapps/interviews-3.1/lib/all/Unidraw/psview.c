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
 * PostScriptView implementation.
 */

#include <Unidraw/classes.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/psformat.h>
#include <Unidraw/Components/psview.h>

#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/pspaint.h>

#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stdio.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

static const int MAXLINELEN = 256;

static char* reencodeISO[] = {
    "/reencodeISO {",
    "dup dup findfont dup length dict begin",
    "{ 1 index /FID ne { def }{ pop pop } ifelse } forall",
    "/Encoding ISOLatin1Encoding def",
    "currentdict end definefont",
    "} def",
    "",
    "/ISOLatin1Encoding [",
    "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
    "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
    "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
    "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
    "/space/exclam/quotedbl/numbersign/dollar/percent/ampersand/quoteright",
    "/parenleft/parenright/asterisk/plus/comma/minus/period/slash",
    "/zero/one/two/three/four/five/six/seven/eight/nine/colon/semicolon",
    "/less/equal/greater/question/at/A/B/C/D/E/F/G/H/I/J/K/L/M/N",
    "/O/P/Q/R/S/T/U/V/W/X/Y/Z/bracketleft/backslash/bracketright",
    "/asciicircum/underscore/quoteleft/a/b/c/d/e/f/g/h/i/j/k/l/m",
    "/n/o/p/q/r/s/t/u/v/w/x/y/z/braceleft/bar/braceright/asciitilde",
    "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
    "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
    "/.notdef/dotlessi/grave/acute/circumflex/tilde/macron/breve",
    "/dotaccent/dieresis/.notdef/ring/cedilla/.notdef/hungarumlaut",
    "/ogonek/caron/space/exclamdown/cent/sterling/currency/yen/brokenbar",
    "/section/dieresis/copyright/ordfeminine/guillemotleft/logicalnot",
    "/hyphen/registered/macron/degree/plusminus/twosuperior/threesuperior",
    "/acute/mu/paragraph/periodcentered/cedilla/onesuperior/ordmasculine",
    "/guillemotright/onequarter/onehalf/threequarters/questiondown",
    "/Agrave/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE/Ccedilla",
    "/Egrave/Eacute/Ecircumflex/Edieresis/Igrave/Iacute/Icircumflex",
    "/Idieresis/Eth/Ntilde/Ograve/Oacute/Ocircumflex/Otilde/Odieresis",
    "/multiply/Oslash/Ugrave/Uacute/Ucircumflex/Udieresis/Yacute",
    "/Thorn/germandbls/agrave/aacute/acircumflex/atilde/adieresis",
    "/aring/ae/ccedilla/egrave/eacute/ecircumflex/edieresis/igrave",
    "/iacute/icircumflex/idieresis/eth/ntilde/ograve/oacute/ocircumflex",
    "/otilde/odieresis/divide/oslash/ugrave/uacute/ucircumflex/udieresis",
    "/yacute/thorn/ydieresis",
    "] def",
    nil
};

/*****************************************************************************/

ClassId PostScriptView::GetClassId () { return POSTSCRIPT_VIEW; }

boolean PostScriptView::IsA (ClassId id) {
    return POSTSCRIPT_VIEW == id || PreorderView::IsA(id);
}

PostScriptView::PostScriptView (GraphicComp* subj) : PreorderView(subj) { 
    _fonts = nil;
}

GraphicComp* PostScriptView::GetGraphicComp () {
    return (GraphicComp*) GetSubject();
}

static Transformer* SaveTransformer (Graphic* g) {
    Transformer* orig = g->GetTransformer();
    Ref(orig);
    g->SetTransformer(new Transformer(orig));
    return orig;
}

static void RestoreTransformer (Graphic* g, Transformer* orig) {
    g->SetTransformer(orig);
    Unref(orig);
}

// ScaleToPostscriptCoords scales the picture to Postscript
// coordinates if screen and Postscript inches are different.

static void ScaleToPostScriptCoords (Graphic* g) {
    const double ps_inch = 72.;

    if (inch != ps_inch) {
	double factor = ps_inch / inch;
	g->Scale(factor, factor);
    }
}

boolean PostScriptView::Emit (ostream& out) {
    SetPSFonts();

    Graphic* g = GetGraphicComp()->GetGraphic();
    Transformer* t = SaveTransformer(g);
    ScaleToPostScriptCoords(g);

    Comments(out);
    Prologue(out);
    Version(out);
    GridSpacing(out);

    out << "\n\n%%Page: 1 1\n\n";
    out << "Begin\n";
    FullGS(out);
    out << "/originalCTM matrix currentmatrix def\n\n";

    boolean status = Definition(out);

    out << "End " << MARK << " eop\n\n";
    out << "showpage\n\n";

    Trailer(out);
    RestoreTransformer(g, t);

    return status;
}

void PostScriptView::Comments (ostream& out) {
    PSVersion(out);
    Creator(out);
    FontNames(out);
    Pages(out);
    BoundingBox(out);

    out << "%%EndComments\n\n";
}

void PostScriptView::PSVersion (ostream& out) {
    out << "%!PS-Adobe-2.0 EPSF-1.2\n";
}

void PostScriptView::Creator (ostream& out) {
    out << "%%Creator: unidraw\n";
}

void PostScriptView::Pages (ostream& out) {
    out << "%%Pages: 1\n";
}

void PostScriptView::BoundingBox (ostream& out) {
    Coord l, b, r, t;
    GetBox(l, b, r, t);
    out << "%%BoundingBox: " << l << " " << b << " " << r << " " << t << "\n";
}

void PostScriptView::Prologue (ostream& out) {
    ConstProcs(out);
    BeginProc(out);
    EndProc(out);
    SetGSProcs(out);
    ObjectProcs(out);
    MiscProcs(out);

    out << "%%EndProlog\n\n";
}

static int Count (UList* list) {
    int i = 0;

    for (UList* u = list->First(); u != list->End(); u = u->Next()) {
        ++i;
    }
    return i;
}

void PostScriptView::ConstProcs (ostream& out) {
    UList* fonts = GetPSFonts();
    int nfonts = Count(fonts);

    out << "/IdrawDict " << (50 + nfonts) << " dict def\n";
    out << "IdrawDict begin\n\n";

    if (nfonts > 0) {
	for (char** line = reencodeISO; *line != nil; ++line) {
	    out << *line << "\n";
	}

	for (UList* u = fonts->First(); u != fonts->End(); u = u->Next()) {
	    PSFont* font = GetFont(u);

	    // No way to check if the X font's encoding is iso8859-1, so...
	    if (strncmp(font->GetPrintFont(), "Symbol", 6) != 0) {
		out << "/" << font->GetPrintFont() << " reencodeISO def\n";
	    } else {
		out << "/" << font->GetPrintFont() << " dup findfont def\n";
	    }
	}
	out << "\n";
    }

    out << "/none null def\n";
    out << "/numGraphicParameters 17 def\n";
    out << "/stringLimit 65535 def\n\n";
}

void PostScriptView::BeginProc (ostream& out) {
    out << "/Begin {\n";
    out << "save\n";
    out << "numGraphicParameters dict begin\n";
    out << "} def\n\n";
}

void PostScriptView::EndProc (ostream& out) {
    out << "/End {\n";
    out << "end\n";
    out << "restore\n";
    out << "} def\n\n";
}

void PostScriptView::SetGSProcs (ostream& out) {
    SetBrushProc(out);
    SetFgColorProc(out);
    SetBgColorProc(out);
    SetFontProc(out);
    SetPatternProc(out);
}

void PostScriptView::SetBrushProc (ostream& out) {
    out << "/SetB {\n";
    out << "dup type /nulltype eq {\n";
    out << "pop\n";
    out << "true /brushNone idef\n";
    out << "} {\n";
    out << "/brushDashOffset idef\n";
    out << "/brushDashArray idef\n";
    out << "pop pop\n";
    out << "/brushWidth idef\n";
    out << "false /brushNone idef\n";
    out << "} ifelse\n";
    out << "} def\n\n";
}

void PostScriptView::SetFgColorProc (ostream& out) {
    out << "/SetCFg {\n";
    out << "/fgblue idef\n";
    out << "/fggreen idef\n";
    out << "/fgred idef\n";
    out << "} def\n\n";
}

void PostScriptView::SetBgColorProc (ostream& out) {
    out << "/SetCBg {\n";
    out << "/bgblue idef\n";
    out << "/bggreen idef\n";
    out << "/bgred idef\n";
    out << "} def\n\n";
}

void PostScriptView::SetFontProc (ostream& out) {
    out << "/SetF {\n";
    out << "/printSize idef\n";
    out << "/printFont idef\n";
    out << "} def\n\n";
}

void PostScriptView::SetPatternProc (ostream& out) {
    out << "/SetP {\n";
    out << "dup type /nulltype eq {\n";
    out << "pop true /patternNone idef\n";
    out << "} {\n";
    out << "dup -1 eq {\n";
    out << "/patternGrayLevel idef\n";
    out << "/patternString idef\n";
    out << "} {\n";
    out << "/patternGrayLevel idef\n";
    out << "} ifelse\n";
    out << "false /patternNone idef\n";
    out << "} ifelse\n";
    out << "} def\n\n";
}

void PostScriptView::ObjectProcs (ostream& out) {
    BSplineProc(out);
    CircleProc(out);
    ClosedBSplineProc(out);
    EllipseProc(out);
    LineProc(out);
    MultiLineProc(out);
    PolygonProc(out);
    RectangleProc(out);
    TextProc(out);
}    

void PostScriptView::BSplineProc (ostream& out) {
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
    out << "patternNone not { ";
    out << "ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "} if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::CircleProc (ostream& out) {
    out << "/Circ {\n";
    out << "newpath\n";
    out << "0 360 arc\n";
    out << "patternNone not { ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "} def\n\n";
}

void PostScriptView::ClosedBSplineProc (ostream& out) {
    out << "/CBSpl {\n";
    out << "0 begin\n";
    out << "dup 2 gt {\n";
    out << "storexyn\n";
    out << "newpath\n";
    out << "n 1 sub dup 0 0 1 1 2 2 true subspline\n";
    out << "1 1 n 3 sub {\n";
    out << "/i exch def\n";
    out << "i 1 sub dup i dup i 1 add dup i 2 add dup false subspline\n";
    out << "} for\n";
    out << "n 3 sub dup n 2 sub dup n 1 sub dup 0 0 false subspline\n";
    out << "n 2 sub dup n 1 sub dup 0 0 1 1 false subspline\n";
    out << "patternNone not { ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "} {\n";
    out << "Poly\n";
    out << "} ifelse\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::EllipseProc (ostream& out) {
    out << "/Elli {\n";
    out << "0 begin\n";
    out << "newpath\n";
    out << "4 2 roll\n";
    out << "translate\n";
    out << "scale\n";
    out << "0 0 1 0 360 arc\n";
    out << "patternNone not { ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "end\n";
    out << "} dup 0 1 dict put def\n\n";
}

void PostScriptView::LineProc (ostream& out) {
    out << "/Line {\n";
    out << "0 begin\n";
    out << "2 storexyn\n";
    out << "newpath\n";
    out << "x 0 get y 0 get moveto\n";
    out << "x 1 get y 1 get lineto\n";
    out << "brushNone not { istroke } if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::MultiLineProc (ostream& out) {
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
    out << "patternNone not { ";
    out << "ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "} if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::PolygonProc (ostream& out) {
    out << "/Poly {\n";
    out << "3 1 roll\n";
    out << "newpath\n";
    out << "moveto\n";
    out << "-1 add\n";
    out << "{ lineto } repeat\n";
    out << "closepath\n";
    out << "patternNone not { ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "} def\n\n";
}

void PostScriptView::RectangleProc (ostream& out) {
    out << "/Rect {\n";
    out << "0 begin\n";
    out << "/t exch def\n";
    out << "/r exch def\n";
    out << "/b exch def\n";
    out << "/l exch def\n";
    out << "newpath\n";
    out << "l b moveto\n";
    out << "l t lineto\n";
    out << "r t lineto\n";
    out << "r b lineto\n";
    out << "closepath\n";
    out << "patternNone not { ifill } if\n";
    out << "brushNone not { istroke } if\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::TextProc (ostream& out) {
    out << "/Text {\n";
    out << "ishow\n";
    out << "} def\n\n";
}

void PostScriptView::MiscProcs (ostream& out) {
    DefinitionProc(out);
    FillProc(out);
    StrokeProc(out);
    ShowProc(out);
    PatternProc(out);
    MinMaxProcs(out);
    MidpointProc(out);
    ThirdpointProc(out);
    SubsplineProc(out);
    StoreVerticesProc(out);

    out << "/SSten {\n";
    out << "fgred fggreen fgblue setrgbcolor\n";
    out << "dup true exch 1 0 0 -1 0 6 -1 roll matrix astore\n";
    out << "} def\n\n";

    out << "/FSten {\n";
    out << "dup 3 -1 roll dup 4 1 roll exch\n";
    out << "newpath\n";
    out << "0 0 moveto\n";
    out << "dup 0 exch lineto\n";
    out << "exch dup 3 1 roll exch lineto\n";
    out << "0 lineto\n";
    out << "closepath\n";
    out << "bgred bggreen bgblue setrgbcolor\n";
    out << "eofill\n";
    out << "SSten\n";
    out << "} def\n\n";

    out << "/Rast {\n";
    out << "exch dup 3 1 roll 1 0 0 -1 0 6 -1 roll matrix astore\n";
    out << "} def\n\n";
/*
    out << "/concatprocs {\n";
    out << "/proc2 exch cvlit def\n";
    out << "/proc1 exch cvlit def\n";
    out << "/newproc proc1 length proc2 length add array def\n";
    out << "newproc 0 proc1 putinterval\n";
    out << "newproc proc1 length proc2 putinterval\n";
    out << "newproc cvx\n";
    out << "} def\n\n";

    out << "/invertproc {\n";
    out << "{1 exch sub} currenttransfer concatprocs settransfer\n";
    out << "} def\n\n";
*/
}

void PostScriptView::DefinitionProc (ostream& out) {
    out << "/idef {\n";
    out << "dup where { pop pop pop } { exch def } ifelse\n";
    out << "} def\n\n";
}

void PostScriptView::FillProc (ostream& out) {
    out << "/ifill {\n";
    out << "0 begin\n";
    out << "gsave\n";
    out << "patternGrayLevel -1 ne {\n";
    out << "fgred bgred fgred sub patternGrayLevel mul add\n";
    out << "fggreen bggreen fggreen sub patternGrayLevel mul add\n";
    out << "fgblue bgblue fgblue sub patternGrayLevel mul add setrgbcolor\n";
    out << "eofill\n";
    out << "} {\n";
    out << "eoclip\n";
    out << "originalCTM setmatrix\n";
    out << "pathbbox /t exch def /r exch def /b exch def /l exch def\n";
    out << "/w r l sub ceiling cvi def\n";
    out << "/h t b sub ceiling cvi def\n";
    out << "/imageByteWidth w 8 div ceiling cvi def\n";
    out << "/imageHeight h def\n";
    out << "bgred bggreen bgblue setrgbcolor\n";
    out << "eofill\n";
    out << "fgred fggreen fgblue setrgbcolor\n";
    out << "w 0 gt h 0 gt and {\n";
    out << "l w add b translate w neg h scale\n";
    out << "w h true [w 0 0 h neg 0 h] { patternproc } imagemask\n";
    out << "} if\n";
    out << "} ifelse\n";
    out << "grestore\n";
    out << "end\n";
    out << "} dup 0 8 dict put def\n\n";
}

void PostScriptView::StrokeProc (ostream& out) {
    out << "/istroke {\n";
    out << "gsave\n";
    out << "brushDashOffset -1 eq {\n";
    out << "[] 0 setdash\n";
    out << "1 setgray\n";
    out << "} {\n";
    out << "brushDashArray brushDashOffset setdash\n";
    out << "fgred fggreen fgblue setrgbcolor\n";
    out << "} ifelse\n";
    out << "brushWidth setlinewidth\n";
    out << "originalCTM setmatrix\n";
    out << "stroke\n";
    out << "grestore\n";
    out << "} def\n\n";
}

void PostScriptView::ShowProc (ostream& out) {
    out << "/ishow {\n";
    out << "0 begin\n";
    out << "gsave\n";
    out << "fgred fggreen fgblue setrgbcolor\n";
    out << "/fontDict printFont printSize scalefont dup setfont def\n";
    out << "/descender fontDict begin 0 [FontBBox] 1 get FontMatrix end\n";
    out << "transform exch pop def\n";
    out << "/vertoffset 1 printSize sub descender sub def {\n";
    out << "0 vertoffset moveto show\n";
    out << "/vertoffset vertoffset printSize sub def\n";
    out << "} forall\n";
    out << "grestore\n";
    out << "end\n";
    out << "} dup 0 3 dict put def\n";
}

void PostScriptView::PatternProc (ostream& out) {
    out << "/patternproc {\n";
    out << "0 begin\n";
    out << "/patternByteLength patternString length def\n";
    out << "/patternHeight patternByteLength 8 mul sqrt cvi def\n";
    out << "/patternWidth patternHeight def\n";
    out << "/patternByteWidth patternWidth 8 idiv def\n";
    out << "/imageByteMaxLength imageByteWidth imageHeight mul\n";
    out << "stringLimit patternByteWidth sub min def\n";
    out << "/imageMaxHeight imageByteMaxLength imageByteWidth idiv ";
    out << "patternHeight idiv\n";
    out << "patternHeight mul patternHeight max def\n";
    out << "/imageHeight imageHeight imageMaxHeight sub store\n";
    out << "/imageString imageByteWidth imageMaxHeight mul patternByteWidth ";
    out << "add string def\n";
    out << "0 1 imageMaxHeight 1 sub {\n";
    out << "/y exch def\n";
    out << "/patternRow y patternByteWidth mul patternByteLength mod def\n";
    out << "/patternRowString patternString patternRow patternByteWidth ";
    out << "getinterval def\n";
    out << "/imageRow y imageByteWidth mul def\n";
    out << "0 patternByteWidth imageByteWidth 1 sub {\n";
    out << "/x exch def\n";
    out << "imageString imageRow x add patternRowString putinterval\n";
    out << "} for\n";
    out << "} for\n";
    out << "imageString\n";
    out << "end\n";
    out << "} dup 0 12 dict put def\n\n";
}

void PostScriptView::MinMaxProcs (ostream& out) {
    out << "/min {\n";
    out << "dup 3 2 roll dup 4 3 roll lt { exch } if pop\n";
    out << "} def\n\n";
    out << "/max {\n";
    out << "dup 3 2 roll dup 4 3 roll gt { exch } if pop\n";
    out << "} def\n\n";
}

void PostScriptView::MidpointProc (ostream& out) {
    out << "/midpoint {\n";
    out << "0 begin\n";
    out << "/y1 exch def\n";
    out << "/x1 exch def\n";
    out << "/y0 exch def\n";
    out << "/x0 exch def\n";
    out << "x0 x1 add 2 div\n";
    out << "y0 y1 add 2 div\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::ThirdpointProc (ostream& out) {
    out << "/thirdpoint {\n";
    out << "0 begin\n";
    out << "/y1 exch def\n";
    out << "/x1 exch def\n";
    out << "/y0 exch def\n";
    out << "/x0 exch def\n";
    out << "x0 2 mul x1 add 3 div\n";
    out << "y0 2 mul y1 add 3 div\n";
    out << "end\n";
    out << "} dup 0 4 dict put def\n\n";
}

void PostScriptView::SubsplineProc (ostream& out) {
    out << "/subspline {\n";
    out << "0 begin\n";
    out << "/movetoNeeded exch def\n";
    out << "y exch get /y3 exch def\n";
    out << "x exch get /x3 exch def\n";
    out << "y exch get /y2 exch def\n";
    out << "x exch get /x2 exch def\n";
    out << "y exch get /y1 exch def\n";
    out << "x exch get /x1 exch def\n";
    out << "y exch get /y0 exch def\n";
    out << "x exch get /x0 exch def\n";
    out << "x1 y1 x2 y2 thirdpoint\n";
    out << "/p1y exch def\n";
    out << "/p1x exch def\n";
    out << "x2 y2 x1 y1 thirdpoint\n";
    out << "/p2y exch def\n";
    out << "/p2x exch def\n";
    out << "x1 y1 x0 y0 thirdpoint\n";
    out << "p1x p1y midpoint\n";
    out << "/p0y exch def\n";
    out << "/p0x exch def\n";
    out << "x2 y2 x3 y3 thirdpoint\n";
    out << "p2x p2y midpoint\n";
    out << "/p3y exch def\n";
    out << "/p3x exch def\n";
    out << "movetoNeeded { p0x p0y moveto } if\n";
    out << "p1x p1y p2x p2y p3x p3y curveto\n";
    out << "end\n";
    out << "} dup 0 17 dict put def\n\n";
}

void PostScriptView::StoreVerticesProc (ostream& out) {
    out << "/storexyn {\n";
    out << "/n exch def\n";
    out << "/y n array def\n";
    out << "/x n array def\n";
    out << "n 1 sub -1 0 {\n";
    out << "/i exch def\n";
    out << "y i 3 2 roll put\n";
    out << "x i 3 2 roll put\n";
    out << "} for\n";
    out << "} def\n\n";
}

void PostScriptView::Version (ostream& out) {
    out << MARK << " Idraw " << PSV_LATEST << " ";
}

void PostScriptView::GridSpacing (ostream& out) {
    float xincr, yincr;
    GetGridSpacing(xincr, yincr);
    out << "Grid " << xincr << " " << yincr << " ";
}

void PostScriptView::Trailer (ostream& out) {
    out << "%%Trailer\n\n";
    out << "end\n";
}

void PostScriptView::MinGS (ostream& out) {
    Brush(out);
    FgColor(out);
    BgColor(out);
    Pattern(out);
    Transformation(out);
}

void PostScriptView::FullGS (ostream& out) {
    Brush(out);
    FgColor(out);
    BgColor(out);
    Font(out);
    Pattern(out);
    Transformation(out);
}

void PostScriptView::TextGS (ostream& out) {
    FgColor(out);
    Font(out);
    Transformation(out);
}

void PostScriptView::Brush (ostream& out) {
    PSBrush* brush = (PSBrush*) GetGraphicComp()->GetGraphic()->GetBrush();

    if (brush == nil) {
	out << MARK << " b u\n";

    } else if (brush->None()) {
	out << "none SetB " << MARK << " b n\n";

    } else {
	int p = brush->GetLinePattern();
	out << MARK << " b " << p << "\n";

	int w = brush->Width();
	out << w << " " << false << " " << false << " ";

	const int* dashpat = brush->GetDashPattern();
	int dashpatsize = brush->GetDashPatternSize();
	int dashoffset = brush->GetDashOffset();

	if (dashpatsize <= 0) {
	    out << "[] " << dashoffset << " ";
	} else {
	    out << "[";

	    for (int i = 0; i < dashpatsize - 1; i++) {
		out << dashpat[i] << " ";
	    }
	    out << dashpat[i] << "] " << dashoffset << " ";
	}
	out << "SetB\n";
    }
}

void PostScriptView::FgColor (ostream& out) {
    PSColor* fgcolor = (PSColor*) GetGraphicComp()->GetGraphic()->GetFgColor();

    if (fgcolor == nil) {
	out << MARK << " cfg u\n";

    } else {
	const char* name = fgcolor->GetName();
	out << MARK << " cfg " << name << "\n";

	if (strcmp(name, "white") == 0 || strcmp(name, "White") == 0) {
	    out << "1 1 1 SetCFg\n";
	} else {
	    ColorIntensity r, g, b;
	    fgcolor->GetIntensities(r, g, b);
	    out << r << " " << g << " " << b << " SetCFg\n";
	}
    }
}

void PostScriptView::BgColor (ostream& out) {
    PSColor* bgcolor = (PSColor*) GetGraphicComp()->GetGraphic()->GetBgColor();

    if (bgcolor == nil) {
	out << MARK << " cbg u\n";

    } else {
	const char* name = bgcolor->GetName();
	out << MARK << " cbg " << name << "\n";

	if (strcmp(name, "white") == 0 || strcmp(name, "White") == 0) {
	    out << "1 1 1 SetCBg\n";
	} else {
	    ColorIntensity r, g, b;
	    bgcolor->GetIntensities(r, g, b);
	    out << r << " " << g << " " << b << " SetCBg\n";
	}
    }
}

void PostScriptView::Font (ostream& out) {
    PSFont* font = (PSFont*) GetGraphicComp()->GetGraphic()->GetFont();

    if (font == nil) {
	out << MARK << " f u\n";
    } else {
	const char* name = font->GetName();
	const char* pf = font->GetPrintFont();
	const char* ps = font->GetPrintSize();
	out << MARK << " f " << name << "\n";
	out << pf << " " << ps << " SetF\n";
    }
}

void PostScriptView::Pattern (ostream& out) {
    PSPattern* pat = (PSPattern*) GetGraphicComp()->GetGraphic()->GetPattern();

    if (pat == nil) {
	out << MARK << " p u\n";

    } else if (pat->None()) {
	out << "none SetP " << MARK << " p n\n";

    } else if (pat->GetSize() > 0) {
	const int* data = pat->GetData();
	int size = pat->GetSize();
        char buf[CHARBUFSIZE];

	out << MARK << " p\n";
	out << "< ";

	if (size <= 8) {
	    for (int i = 0; i < 8; i++) {
		sprintf(buf, "%02x", data[i] & 0xff);
		out << buf << " ";
	    }

	} else {
	    for (int i = 0; i < patternHeight; i++) {
		sprintf(buf, "%0*x", patternWidth/4, data[i]);
		if (i != patternHeight - 2) {
		    out << buf << " ";
		} else {
		    out << buf << "\n  ";
		}
	    }
	}
	out << "> -1 SetP\n";

    } else {
	float graylevel = pat->GetGrayLevel();
	out << MARK << " p\n";
	out << graylevel << " SetP\n";
    }
}

void PostScriptView::Transformation (ostream& out) {
    Transformer* t = GetGraphicComp()->GetGraphic()->GetTransformer();
    Transformer identity;

    if (t == nil || *t == identity) {
	out << MARK << " t u\n";

    } else {
	float a00, a01, a10, a11, a20, a21;
	t->GetEntries(a00, a01, a10, a11, a20, a21);
	out << MARK << " t\n";
	out << "[ " << a00 << " " << a01 << " " << a10 << " ";
	out << a11 << " " << a20 << " " << a21 << " ] concat\n";
    }
}

static boolean Uncollected (const char* name, UList* fonts) {
    for (UList* u = fonts->First(); u != fonts->End(); u = u->Next()) {
        PSFont* font = (PSFont*) (*u)();

        if (strcmp(font->GetPrintFont(), name) == 0) {
            return false;
        }
    }
    return true;
}

static void CollectFonts (GraphicComp* comp, UList* fonts) {
    PSFont* font = comp->GetGraphic()->GetFont();

    if (font != nil && Uncollected(font->GetPrintFont(), fonts)) {
        fonts->Append(new UList(font));
    }

    Iterator i;

    for (comp->First(i); !comp->Done(i); comp->Next(i)) {
        CollectFonts(comp->GetComp(i), fonts);
    }
}

PSFont* PostScriptView::GetFont (UList* u) { return (PSFont*) (*u)(); }

void PostScriptView::SetPSFonts (UList* u) {
    delete _fonts;
    _fonts = u;
}

UList* PostScriptView::GetPSFonts () {
    if (_fonts == nil) {
        _fonts = new UList;

        CollectFonts(GetGraphicComp(), _fonts);
    }
    return _fonts;
}

void PostScriptView::FontNames (ostream& out) {
    UList* fonts = GetPSFonts();
    const char* comment = "%%DocumentFonts:";
    int linelen = strlen(comment);
    out << comment;

    for (UList* u = fonts->First(); u != fonts->End(); u = u->Next()) {
	PSFont* font = GetFont(u);

	if (linelen + strlen(font->GetPrintFont()) + 2 <= MAXLINELEN) {
	    out << " ";
	    ++linelen;
	} else {
	    out << "\n%%+ ";
	    linelen = strlen("%%+ ");
	}
	out << font->GetPrintFont();
	linelen += strlen(font->GetPrintFont());
    }
    out << "\n";
}

void PostScriptView::GetBox (Coord& l, Coord& b, Coord& r, Coord& t) {
    GetGraphicComp()->GetGraphic()->GetBox(l, b, r, t);
}

void PostScriptView::GetGridSpacing (float& xincr, float& yincr) {
    xincr = yincr = 8;                          // dumb default
}

PostScriptView* PostScriptView::View (UList* r) {
    return (PostScriptView*) (*r)();
}

PostScriptView* PostScriptView::CreatePSView (GraphicComp* comp) {
    PostScriptView* psv = (PostScriptView*) comp->Create(POSTSCRIPT_VIEW);

    if (psv != nil) {
        comp->Attach(psv);
        psv->Update();
    }
    return psv;
}

/*****************************************************************************/

ClassId PostScriptViews::GetClassId () { return POSTSCRIPT_VIEWS; }

boolean PostScriptViews::IsA (ClassId id) {
    return POSTSCRIPT_VIEWS == id || PostScriptView::IsA(id);
}

PostScriptViews::PostScriptViews (GraphicComps* subj) : PostScriptView(subj) {
    _views = new UList;
}

PostScriptViews::~PostScriptViews () {
    DeleteViews();
    delete _views;
}

boolean PostScriptViews::Emit (ostream& out) {
    SetPSFonts();

    Graphic* g = GetGraphicComp()->GetGraphic();
    Transformer* t = SaveTransformer(g);
    ScaleToPostScriptCoords(g);

    Comments(out);
    Prologue(out);
    Version(out);
    GridSpacing(out);

    out << "\n\n%%Page: 1 1\n\n";
    out << "Begin\n";
    FullGS(out);
    out << "/originalCTM matrix currentmatrix def\n\n";

    boolean status = PreorderView::Definition(out);

    out << "End " << MARK << " eop\n\n";
    out << "showpage\n\n";

    Trailer(out);
    RestoreTransformer(g, t);

    return status;
}

boolean PostScriptViews::Definition (ostream& out) {
    out << "Begin " << MARK << " Pict\n";
    FullGS(out);
    out << "\n";

    boolean status = PreorderView::Definition(out);

    out << "End " << MARK << " eop\n\n";

    return status;
}    

void PostScriptViews::Update () {
    DeleteViews();

    GraphicComp* comps = GetGraphicComp();
    Iterator i;

    for (comps->First(i); !comps->Done(i); comps->Next(i)) {
        GraphicComp* comp = comps->GetComp(i);
        PostScriptView* psv = CreatePSView(comp);

        if (psv != nil) {
            _views->Append(new UList(psv));
        }
    }
}

GraphicComps* PostScriptViews::GetGraphicComps () {
    return (GraphicComps*) GetSubject();
}

UList* PostScriptViews::Elem (Iterator i) { return (UList*) i.GetValue(); }
void PostScriptViews::First (Iterator& i) { i.SetValue(_views->First()); }
void PostScriptViews::Last (Iterator& i) { i.SetValue(_views->Last()); }
void PostScriptViews::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void PostScriptViews::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean PostScriptViews::Done (Iterator i) { return Elem(i) == _views->End(); }
ExternView* PostScriptViews::GetView (Iterator i) { return View(Elem(i)); }

void PostScriptViews::SetView (ExternView* ev, Iterator& i) {
    i.SetValue(_views->Find(ev));
}

void PostScriptViews::DeleteView (Iterator& i) {
    UList* doomed = Elem(i);
    ExternView* view = GetView(i);

    Next(i);
    _views->Remove(doomed);
    SetParent(view, nil);
    delete doomed;
    delete view;
}

void PostScriptViews::DeleteViews () {
    Iterator i;

    First(i);
    while (!Done(i)) {
        DeleteView(i);
    }
}    
