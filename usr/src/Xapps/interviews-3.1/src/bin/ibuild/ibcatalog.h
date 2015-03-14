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
 * IBuildCatalog - can read components in traditional idraw 
 * PostScript format.
 */

#ifndef ibcatalog_h
#define ibcatalog_h

#include <Unidraw/catalog.h>

class GraphicComp;

class IBuildCatalog : public Catalog{
public:
    IBuildCatalog(const char*, Creator*, float);
    
    virtual boolean Retrieve(const char*, EditorInfo*&);
    virtual boolean Retrieve(const char*, Component*&);
    virtual boolean Retrieve(const char*, Command*&);
    virtual boolean Retrieve(const char*, Tool*&);
private:
    boolean UnidrawFormat(const char*);

    void PSReadGridSpacing(istream&, float&, float&);
    void PSReadGS(istream&, Graphic*);
    void PSReadPictGS(istream&, Graphic*);
    void PSReadTextGS(istream&, Graphic*);
    void PSReadBrush(istream&, Graphic*);
    void PSReadFgColor(istream&, Graphic*);
    void PSReadBgColor(istream&, Graphic*);
    void PSReadFont(istream&, Graphic*);
    void PSReadPattern(istream&, Graphic*);
    void PSReadTransformer(istream&, Graphic*);
    void PSReadPoints(istream&, const Coord*&, const Coord*&, int&);

    void PSReadChildren(istream&, GraphicComp*);
    void PSReadTextData(istream&, char*, int);

    GraphicComp* ReadPostScript(istream&);
    GraphicComp* ReadPict(istream&);
    GraphicComp* ReadBSpline(istream&);
    GraphicComp* ReadCircle(istream&);
    GraphicComp* ReadClosedBSpline(istream&);
    GraphicComp* ReadEllipse(istream&);
    GraphicComp* ReadLine(istream&);
    GraphicComp* ReadMultiLine(istream&);
    GraphicComp* ReadPolygon(istream&);
    GraphicComp* ReadRect(istream&);
    GraphicComp* ReadText(istream&);
    GraphicComp* ReadSStencil(istream&);
    GraphicComp* ReadFStencil(istream&);
    GraphicComp* ReadRaster(istream&);

    void ScaleToScreenCoords(Graphic*);
    float CalcGrayLevel(int);
    void CorrectTextVPos(Graphic*, float);
private:
    static char _buf[CHARBUFSIZE]; // contains storage for reading data
    static float _psversion;       // stores version of drawing read from file
    boolean _valid;
};

#endif
