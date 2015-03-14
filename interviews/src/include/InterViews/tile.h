/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Tile - tiled layout
 */

#ifndef iv_tile_h
#define iv_tile_h

#include <InterViews/layout.h>

class Tile : public Layout {
public:
    Tile(DimensionName);
    virtual ~Tile();

    virtual void request(
        GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
        const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Requisition requisition_;
};

class TileReversed : public Layout {
public:
    TileReversed(DimensionName);
    virtual ~TileReversed();

    virtual void request(
        GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
        const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Requisition requisition_;
};

class TileFirstAligned : public Layout {
public:
    TileFirstAligned(DimensionName);
    virtual ~TileFirstAligned();

    virtual void request(
        GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
        const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Requisition requisition_;
};

class TileReversedFirstAligned : public Layout {
public:
    TileReversedFirstAligned(DimensionName);
    virtual ~TileReversedFirstAligned();

    virtual void request(
        GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
        const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Requisition requisition_;
};

#endif
