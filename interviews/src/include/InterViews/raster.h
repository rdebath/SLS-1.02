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
 * Raster - rasterized image
 */

#ifndef iv_raster_h
#define iv_raster_h

#include <InterViews/color.h>
#include <InterViews/coord.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class RasterRep;

class Raster : public Resource {
public:
    Raster(unsigned long width, unsigned long height);
    Raster(const Raster&);
    virtual ~Raster();

    virtual Coord width() const;
    virtual Coord height() const;

    virtual unsigned long pwidth() const;
    virtual unsigned long pheight() const;

    virtual Coord left_bearing() const;
    virtual Coord right_bearing() const;
    virtual Coord ascent() const;
    virtual Coord descent() const;

    virtual void peek(
	unsigned long x, unsigned long y,
	ColorIntensity& red, ColorIntensity& green, ColorIntensity& blue,
	float& alpha
    ) const;

    virtual void poke(
	unsigned long x, unsigned long y,
	ColorIntensity red, ColorIntensity green, ColorIntensity blue,
	float alpha
    );

    virtual void flush() const;

    RasterRep* rep() const;
protected:
    Raster(RasterRep*);
private:
    RasterRep* rep_;

    /* anachronisms */
public:
    unsigned int Width() const;
    unsigned int Height() const;
};

inline RasterRep* Raster::rep() const { return rep_; }

inline unsigned int Raster::Width() const { return (unsigned int)pwidth(); }
inline unsigned int Raster::Height() const { return (unsigned int)pheight(); }

#include <InterViews/_leave.h>

#endif
