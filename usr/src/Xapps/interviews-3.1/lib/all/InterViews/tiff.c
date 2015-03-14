/*
 * Copyright (c) 1991 Stanford University
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

#include <InterViews/color.h>
#include <InterViews/raster.h>
#include <InterViews/tiff.h>
#include <TIFF/tiffio.h>
#include <stdlib.h>

#define	howmany(x, y)	(((x)+((y)-1))/(y))

typedef	unsigned char u_char;
typedef	unsigned short u_short;
typedef	unsigned int u_int;
typedef	unsigned long u_long;
typedef unsigned char RGBvalue;

class TIFFRasterImpl;

typedef void (TIFFRasterImpl::*tileContigRoutine)(
    u_long*, const u_char*, const RGBvalue*, u_long, u_long, int, int
);

typedef void (TIFFRasterImpl::*tileSeparateRoutine)(
    u_long*, const u_char*, const u_char*, const u_char*,
    const RGBvalue*, u_long, u_long, int, int
);

class TIFFRasterImpl {
private:
    friend class TIFFRaster;

    TIFF*	tif_;
    u_long*	raster_;		/* packed image data */
    u_short	bitspersample_;
    u_short	samplesperpixel_;
    u_short	photometric_;
    u_short	orientation_;
    u_short*	redcmap_;		/* colormap for pallete images */
    u_short*	greencmap_;
    u_short*	bluecmap_;

    u_long**	BWmap_;			/* B&W mapping table */
    u_long**	PALmap_;		/* palette image mapping table */

    TIFFRasterImpl();
    ~TIFFRasterImpl();

    Raster* load(const char* filename);

    boolean gt(u_long w, u_long h);
    boolean gtTileContig(const RGBvalue* Map, u_long h, u_long w);
    boolean gtTileSeparate(const RGBvalue* Map, u_long h, u_long w);
    boolean gtStripContig(const RGBvalue* Map, u_long h, u_long w);
    boolean gtStripSeparate(const RGBvalue* Map, u_long h, u_long w);

    u_long setorientation(u_long h);
    boolean makebwmap(RGBvalue* Map);
    boolean makecmap(
	const u_short* rmap, const u_short* gmap, const u_short* bmap
    );

    void put8bitcmaptile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void put4bitcmaptile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void put2bitcmaptile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void put1bitcmaptile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void put1bitbwtile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void put2bitbwtile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void put4bitbwtile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void putRGBgreytile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void putRGBcontig8bittile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void putRGBcontig16bittile(
	u_long* dest, const u_char* src, const RGBvalue* Map,
	u_long w, u_long h, int fromskew, int toskew
    );
    void putRGBseparate8bittile(
	u_long* dest,
	const u_char* red, const u_char* green, const u_char* blue,
	const RGBvalue* Map, u_long w, u_long h, int fromskew, int toskew
    );
    void putRGBseparate16bittile(
	u_long* dest,
	const u_char* red, const u_char* green, const u_char* blue,
	const RGBvalue* Map, u_long w, u_long h, int fromskew, int toskew
    );

    tileContigRoutine pickTileContigCase(const RGBvalue* Map);
    tileSeparateRoutine pickTileSeparateCase(const RGBvalue* Map);
};

TIFFRasterImpl::TIFFRasterImpl() {}
TIFFRasterImpl::~TIFFRasterImpl() {}

Raster* TIFFRaster::load(const char* filename, boolean) {
    TIFFRasterImpl impl;
    return impl.load(filename);
}

Raster* TIFFRasterImpl::load(const char* filename) {
    tif_ = TIFFOpen(filename, "r");
    if (tif_ == nil) {
	return nil;
    }
    if (!TIFFGetField(tif_, TIFFTAG_BITSPERSAMPLE, &bitspersample_)) {
	bitspersample_ = 1;
    }
    switch (bitspersample_) {
    case 1: case 2: case 4:
    case 8: case 16:
	break;
    default:
	TIFFClose(tif_);
	return nil;
    }
    if (!TIFFGetField(tif_, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel_)) {
	samplesperpixel_ = 1;
    }
    switch (samplesperpixel_) {
    case 1: case 3: case 4:
	break;
    default:
	TIFFClose(tif_);
	return nil;
    }
    u_long width;
    TIFFGetField(tif_, TIFFTAG_IMAGEWIDTH, &width);
    u_long height;
    TIFFGetField(tif_, TIFFTAG_IMAGELENGTH, &height);
    if (!TIFFGetField(tif_, TIFFTAG_PHOTOMETRIC, &photometric_)) {
	switch (samplesperpixel_) {
	case 1:
	    photometric_ = PHOTOMETRIC_MINISBLACK;
	    break;
	case 3: case 4:
	    photometric_ = PHOTOMETRIC_RGB;
	    break;
	default:
	    TIFFClose(tif_);
	    return nil;
	}
    }
    Raster* r = nil;
    raster_ = new u_long[width * height];
    BWmap_ = nil;
    PALmap_ = nil;
    if (raster_ != nil && gt(width, height)) {
	/* create raster_ from packed image data */
	r = new Raster(width, height);
	for (long i = height - 1; i >= 0; i--) {
	    u_char* c = (u_char*) (raster_ + i*width);
	    /* should use a lookup table here */
	    for (long j = 0; j < width; j++) {
		r->poke(
		    j, i,
		    ColorIntensity(float(c[3]) / float(0xff)),
		    ColorIntensity(float(c[2]) / float(0xff)),
		    ColorIntensity(float(c[1]) / float(0xff)),
		    1.0
		);
		c += sizeof (u_long);
	    }
	}
    }
    TIFFClose(tif_);
    delete raster_;
    delete BWmap_;
    delete PALmap_;
    return r;
}

static int checkcmap(
    int n, const u_short* r, const u_short* g, const u_short* b
) {
    while (n-- > 0) {
	if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256) {
	    return 16;
	}
    }
    return 8;
}

boolean TIFFRasterImpl::gt(u_long w, u_long h) {
    u_short minsamplevalue;
    u_short maxsamplevalue;
    u_short planarconfig;
    RGBvalue* Map = nil;

    if (!TIFFGetField(tif_, TIFFTAG_MINSAMPLEVALUE, &minsamplevalue)) {
	minsamplevalue = 0;
    }
    if (!TIFFGetField(tif_, TIFFTAG_MAXSAMPLEVALUE, &maxsamplevalue)) {
	maxsamplevalue = (1<<bitspersample_)-1;
    }
    switch (photometric_) {
    case PHOTOMETRIC_RGB:
	if (minsamplevalue == 0 && maxsamplevalue == 255) {
	    break;
	}
	/* fall thru... */
    case PHOTOMETRIC_MINISBLACK:
    case PHOTOMETRIC_MINISWHITE: {
	register int x, range;

	range = maxsamplevalue - minsamplevalue;
	Map = new RGBvalue[range + 1];
	if (Map == nil) {
	    TIFFError(
		TIFFFileName(tif_),
		"No space for photometric conversion table"
	    );
	    return false;
	}
	if (photometric_ == PHOTOMETRIC_MINISWHITE) {
	    for (x = 0; x <= range; x++) {
		Map[x] = ((range - x) * 255) / range;
	    }
	} else {
	    for (x = 0; x <= range; x++) {
		Map[x] = (x * 255) / range;
	    }
	}
	if (photometric_ != PHOTOMETRIC_RGB && bitspersample_ != 8) {
	    /*
	     * Use photometric mapping table to construct
	     * unpacking tables for samples < 8 bits.
	     */
	    if (!makebwmap(Map)) {
		return false;
	    }
	    delete Map;			/* no longer need Map, free it */
	    Map = nil;
	}
	break;
    }
    case PHOTOMETRIC_PALETTE:
	if (!TIFFGetField(
	    tif_, TIFFTAG_COLORMAP, &redcmap_, &greencmap_, &bluecmap_)
	) {
	    TIFFError(TIFFFileName(tif_), "Missing required \"Colormap\" tag");
	    return (false);
	}
	/*
	 * Convert 16-bit colormap to 8-bit (unless it looks
	 * like an old-style 8-bit colormap).
	 */
	if (
	    checkcmap(
		1 << bitspersample_, redcmap_, greencmap_, bluecmap_
	    ) == 16
	) {
	    int i;
	    for (i = (1 << bitspersample_) - 1; i > 0; i--) {
#define	CVT(x)		(((x) * 255) / ((1L<<16)-1))
		redcmap_[i] = (u_short) CVT(redcmap_[i]);
		greencmap_[i] = (u_short) CVT(greencmap_[i]);
		bluecmap_[i] = (u_short) CVT(bluecmap_[i]);
	    }
	}
	if (bitspersample_ <= 8) {
	    /*
	     * Use mapping table and colormap to construct
	     * unpacking tables for samples < 8 bits.
	     */
	    if (!makecmap(redcmap_, greencmap_, bluecmap_)) {
		return false;
	    }
	}
	break;
    }
    TIFFGetField(tif_, TIFFTAG_PLANARCONFIG, &planarconfig);
    boolean e;
    if (planarconfig == PLANARCONFIG_SEPARATE && samplesperpixel_ > 1) {
	e = TIFFIsTiled(tif_) ?
	    gtTileSeparate(Map, h, w) : gtStripSeparate(Map, h, w);
    } else {
	e = TIFFIsTiled(tif_) ? 
	    gtTileContig(Map, h, w) : gtStripContig(Map, h, w);
    }
    delete Map;
    return e;
}

u_long TIFFRasterImpl::setorientation(u_long h) {
    u_long y;

    if (!TIFFGetField(tif_, TIFFTAG_ORIENTATION, &orientation_)) {
	orientation_ = ORIENTATION_TOPLEFT;
    }
    switch (orientation_) {
    case ORIENTATION_BOTRIGHT:
    case ORIENTATION_RIGHTBOT:	/* XXX */
    case ORIENTATION_LEFTBOT:	/* XXX */
	TIFFWarning(TIFFFileName(tif_), "using bottom-left orientation");
	orientation_ = ORIENTATION_BOTLEFT;
	/* fall thru... */
    case ORIENTATION_BOTLEFT:
	y = 0;
	break;
    case ORIENTATION_TOPRIGHT:
    case ORIENTATION_RIGHTTOP:	/* XXX */
    case ORIENTATION_LEFTTOP:	/* XXX */
    default:
	TIFFWarning(TIFFFileName(tif_), "using top-left orientation");
	orientation_ = ORIENTATION_TOPLEFT;
	/* fall thru... */
    case ORIENTATION_TOPLEFT:
	y = h-1;
	break;
    }
    return y;
}

/*
 * Get an tile-organized image that has
 *    PlanarConfiguration contiguous if SamplesPerPixel > 1
 * or
 *    SamplesPerPixel == 1
 */    
boolean TIFFRasterImpl::gtTileContig(const RGBvalue* Map, u_long h, u_long w) {
    u_char* buf = new u_char[TIFFTileSize(tif_)];
    if (buf == nil) {
	TIFFError(TIFFFileName(tif_), "No space for tile buffer");
	return false;
    }
    tileContigRoutine put = pickTileContigCase(Map);
    u_long tw;
    TIFFGetField(tif_, TIFFTAG_TILEWIDTH, &tw);
    u_long th;
    TIFFGetField(tif_, TIFFTAG_TILELENGTH, &th);
    u_long y = setorientation(h);
    int toskew = (int)(orientation_ == ORIENTATION_TOPLEFT ? -tw+-w : -tw+w);
    for (u_long row = 0; row < h; row += th) {
	u_long nrow = (row + th > h ? h - row : th);
	for (u_long col = 0; col < w; col += tw) {
	    if (TIFFReadTile(tif_, buf, col, row, 0, 0) < 0) {
		break;
	    }
	    if (col + tw > w) {
		/*
		 * Tile is clipped horizontally.  Calculate
		 * visible portion and skewing factors.
		 */
		u_long npix = w - col;
		int fromskew = (int)(tw - npix);
		(this->*put)(
		    raster_ + y*w + col, buf, Map,
		    npix, nrow, fromskew, toskew + fromskew
		);
	    } else
		(this->*put)(
		    raster_ + y*w + col, buf, Map, tw, nrow, 0, toskew
		);
	}
	y += (orientation_ == ORIENTATION_TOPLEFT ? -nrow : nrow);
    }
    delete buf;
    return true;
}

/*
 * Get an tile-organized image that has
 *     SamplesPerPixel > 1
 *     PlanarConfiguration separated
 * We assume that all such images are RGB.
 */    
boolean TIFFRasterImpl::gtTileSeparate(
    const RGBvalue* Map, u_long h, u_long w
) {
    u_long tilesize = TIFFTileSize(tif_);
    u_char* buf = new u_char[3*tilesize];
    if (buf == nil) {
	TIFFError(TIFFFileName(tif_), "No space for tile buffer");
	return false;
    }
    u_char* r = buf;
    u_char* g = r + tilesize;
    u_char* b = g + tilesize;
    tileSeparateRoutine put = pickTileSeparateCase(Map);
    u_long tw;
    TIFFGetField(tif_, TIFFTAG_TILEWIDTH, &tw);
    u_long th;
    TIFFGetField(tif_, TIFFTAG_TILELENGTH, &th);
    u_long y = setorientation(h);
    int toskew = (int)(orientation_ == ORIENTATION_TOPLEFT ? -tw+-w : -tw+w);
    for (u_long row = 0; row < h; row += th) {
	u_long nrow = (row + th > h ? h - row : th);
	for (u_long col = 0; col < w; col += tw) {
	    if (TIFFReadTile(tif_, r, col, row, 0, 0) < 0) {
		break;
	    }
	    if (TIFFReadTile(tif_, g, col, row, 0, 1) < 0) {
		break;
	    }
	    if (TIFFReadTile(tif_, b, col, row, 0, 2) < 0) {
		break;
	    }
	    if (col + tw > w) {
		/*
		 * Tile is clipped horizontally.  Calculate
		 * visible portion and skewing factors.
		 */
		u_long npix = w - col;
		int fromskew = (int)(tw - npix);
		(this->*put)(
		    raster_ + y*w + col, r, g, b, Map,
		    npix, nrow, fromskew, toskew + fromskew
		);
	    } else
		(this->*put)(
		    raster_ + y*w + col, r, g, b, Map,
		    tw, nrow, 0, toskew
		);
	}
	y += (orientation_ == ORIENTATION_TOPLEFT ? -nrow : nrow);
    }
    delete buf;
    return true;
}

/*
 * Get a strip-organized image that has
 *    PlanarConfiguration contiguous if SamplesPerPixel > 1
 * or
 *    SamplesPerPixel == 1
 */    
boolean TIFFRasterImpl::gtStripContig(
    const RGBvalue* Map, u_long h, u_long w
) {
    u_char* buf = new u_char[TIFFStripSize(tif_)];
    if (buf == nil) {
	TIFFError(TIFFFileName(tif_), "No space for strip buffer");
	return (false);
    }
    tileContigRoutine put = pickTileContigCase(Map);
    u_long y = setorientation(h);
    int toskew = (int)(orientation_ == ORIENTATION_TOPLEFT ? -w + -w : -w + w);
    u_long rowsperstrip = (u_long) -1L;
    TIFFGetField(tif_, TIFFTAG_ROWSPERSTRIP, &rowsperstrip);
    u_long imagewidth;
    TIFFGetField(tif_, TIFFTAG_IMAGEWIDTH, &imagewidth);
    int scanline = TIFFScanlineSize(tif_);
    int fromskew = (int)(w < imagewidth ? imagewidth - w : 0);
    for (u_long row = 0; row < h; row += rowsperstrip) {
	u_int nrow = u_int(row + rowsperstrip > h ? h - row : rowsperstrip);
	if (TIFFReadEncodedStrip(
	    tif_, TIFFComputeStrip(tif_, row, 0), buf, nrow*scanline) < 0
	) {
	    break;
	}
	(this->*put)(raster_ + y*w, buf, Map, w, nrow, fromskew, toskew);
	y += (orientation_ == ORIENTATION_TOPLEFT ? -nrow : nrow);
    }
    delete buf;
    return true;
}

/*
 * Get a strip-organized image with
 *     SamplesPerPixel > 1
 *     PlanarConfiguration separated
 * We assume that all such images are RGB.
 */
boolean TIFFRasterImpl::gtStripSeparate(
    const RGBvalue* Map, u_long h, u_long w
) {
    u_long stripsize = TIFFStripSize(tif_);
    u_char* buf = new u_char[3*stripsize];
    u_char* r = buf;
    u_char* g = r + stripsize;
    u_char* b = g + stripsize;
    tileSeparateRoutine put = pickTileSeparateCase(Map);
    u_long y = setorientation(h);
    int toskew = (int)(orientation_ == ORIENTATION_TOPLEFT ? -w + -w : -w + w);
    u_long rowsperstrip = (u_long) -1L;
    TIFFGetField(tif_, TIFFTAG_ROWSPERSTRIP, &rowsperstrip);
    u_long imagewidth;
    TIFFGetField(tif_, TIFFTAG_IMAGEWIDTH, &imagewidth);
    int scanline = TIFFScanlineSize(tif_);
    int fromskew = (int)(w < imagewidth ? imagewidth - w : 0);
    for (u_long row = 0; row < h; row += rowsperstrip) {
	u_int nrow = u_int(row + rowsperstrip > h ? h - row : rowsperstrip);
	if (TIFFReadEncodedStrip(
	    tif_, TIFFComputeStrip(tif_, row, 0), r, nrow*scanline) < 0
	) {
	    break;
	}
	if (TIFFReadEncodedStrip(
	    tif_, TIFFComputeStrip(tif_, row, 1), g, nrow*scanline) < 0
	) {
	    break;
	}
	if (TIFFReadEncodedStrip(
	    tif_, TIFFComputeStrip(tif_, row, 2), b, nrow*scanline) < 0
	) {
	    break;
	}
	(this->*put)(raster_ + y*w, r, g, b, Map, w, nrow, fromskew, toskew);
	y += (orientation_ == ORIENTATION_TOPLEFT ? -nrow : nrow);
    }
    delete buf;
    return true;
}

#define	PACK(r,g,b)	((u_long)(r))|(((u_long)(g))<<8)|(((u_long)(b))<<16)

/*
 * Greyscale images with less than 8 bits/sample are handled
 * with a table to avoid lots of shifts and masks.  The table
 * is setup so that put*bwtile (below) can retrieve 8/bitspersample_
 * pixel values simply by indexing into the table with one
 * number.
 */
boolean TIFFRasterImpl::makebwmap(RGBvalue* Map) {
    register int i;
    int nsamples = 8 / bitspersample_;

    BWmap_ = (u_long **)malloc(
	256*sizeof (u_long *)+(256*nsamples*sizeof(u_long))
    );
    if (BWmap_ == nil) {
	TIFFError(TIFFFileName(tif_), "No space for B&W mapping table");
	return false;
    }
    register u_long* p = (u_long*)(BWmap_ + 256);
    for (i = 0; i < 256; i++) {
	BWmap_[i] = p;
	switch (bitspersample_) {
	    register RGBvalue c;
#define	GREY(x)	c = Map[x]; *p++ = PACK(c,c,c);
	case 1:
	    GREY(i>>7);
	    GREY((i>>6)&1);
	    GREY((i>>5)&1);
	    GREY((i>>4)&1);
	    GREY((i>>3)&1);
	    GREY((i>>2)&1);
	    GREY((i>>1)&1);
	    GREY(i&1);
	    break;
	case 2:
	    GREY(i>>6);
	    GREY((i>>4)&3);
	    GREY((i>>2)&3);
	    GREY(i&3);
	    break;
	case 4:
	    GREY(i>>4);
	    GREY(i&0xf);
	    break;
	}
#undef	GREY
    }
    return true;
}

/*
 * Palette images with <= 8 bits/sample are handled
 * with a table to avoid lots of shifts and masks.  The table
 * is setup so that put*cmaptile (below) can retrieve 8/bitspersample_
 * pixel values simply by indexing into the table with one
 * number.
 */
boolean TIFFRasterImpl::makecmap(
    const u_short* rmap, const u_short* gmap, const u_short* bmap
) {
    register int i;
    int nsamples = 8 / bitspersample_;
    register u_long *p;

    PALmap_ = (u_long **)malloc(
	256*sizeof (u_long *)+(256*nsamples*sizeof(u_long))
    );
    if (PALmap_ == nil) {
	TIFFError(TIFFFileName(tif_), "No space for Palette mapping table");
	return (false);
    }
    p = (u_long *)(PALmap_ + 256);
    for (i = 0; i < 256; i++) {
	PALmap_[i] = p;
#define	CMAP(x)	\
c = x; *p++ = PACK(rmap[c]&0xff, gmap[c]&0xff, bmap[c]&0xff);
	switch (bitspersample_) {
	    register RGBvalue c;
	case 1:
	    CMAP(i>>7);
	    CMAP((i>>6)&1);
	    CMAP((i>>5)&1);
	    CMAP((i>>4)&1);
	    CMAP((i>>3)&1);
	    CMAP((i>>2)&1);
	    CMAP((i>>1)&1);
	    CMAP(i&1);
	    break;
	case 2:
	    CMAP(i>>6);
	    CMAP((i>>4)&3);
	    CMAP((i>>2)&3);
	    CMAP(i&3);
	    break;
	case 4:
	    CMAP(i>>4);
	    CMAP(i&0xf);
	    break;
	case 8:
	    CMAP(i);
	    break;
	}
#undef CMAP
    }
    return (true);
}

/*
 * The following routines move decoded data returned
 * from the TIFF library into rasters that are suitable
 * for passing to lrecwrite.  They do the necessary
 * conversions based on whether the drawing mode is RGB
 * colormap and whether or not there is a mapping table.
 *
 * The routines have been created according to the most
 * important cases and optimized.  pickTileContigCase and
 * pickTileSeparateCase analyze the parameters and select
 * the appropriate "put" routine to use.
 */
#define	REPEAT8(op)	REPEAT4(op); REPEAT4(op)
#define	REPEAT4(op)	REPEAT2(op); REPEAT2(op)
#define	REPEAT2(op)	op; op
#define	CASE8(x,op)				\
	switch (x) {				\
	case 7: op; case 6: op; case 5: op;	\
	case 4: op; case 3: op; case 2: op;	\
	case 1: op;				\
	}
#define	CASE4(x,op)	switch (x) { case 3: op; case 2: op; case 1: op; }

#define	UNROLL8(w, op1, op2) {		\
	u_long x;			\
	for (x = w; x >= 8; x -= 8) {	\
	    op1;			\
	    REPEAT8(op2);		\
	}				\
	if (x > 0) {			\
	    op1;			\
	    CASE8(x,op2);		\
	}				\
}
#define	UNROLL4(w, op1, op2) {		\
	u_long x;			\
	for (x = w; x >= 4; x -= 4) {	\
	    op1;			\
	    REPEAT4(op2);		\
	}				\
	if (x > 0) {			\
	    op1;			\
	    CASE4(x,op2);		\
	}				\
}
#define	UNROLL2(w, op1, op2) {		\
	u_long x;			\
	for (x = w; x >= 2; x -= 2) {	\
	    op1;			\
	    REPEAT2(op2);		\
	}				\
	if (x) {			\
	    op1;			\
	    op2;			\
	}				\
}
			
#define	SKEW(r,g,b,skew)	{ r += skew; g += skew; b += skew; }

/*
 * 8-bit palette => RGB
 */
void TIFFRasterImpl::put8bitcmaptile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    while (h-- > 0) {
	UNROLL8(w,, *cp++ = PALmap_[*pp++][0]);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 4-bit palette => RGB
 */
void TIFFRasterImpl::put4bitcmaptile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long *bw;

    fromskew /= 2;
    while (h-- > 0) {
	UNROLL2(w, bw = PALmap_[*pp++], *cp++ = *bw++);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 2-bit palette => RGB
 */
void TIFFRasterImpl::put2bitcmaptile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long *bw;

    fromskew /= 4;
    while (h-- > 0) {
	UNROLL4(w, bw = PALmap_[*pp++], *cp++ = *bw++);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 1-bit palette => RGB
 */
void TIFFRasterImpl::put1bitcmaptile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long *bw;

    fromskew /= 8;
    while (h-- > 0) {
	UNROLL8(w, bw = PALmap_[*pp++], *cp++ = *bw++);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 1-bit bilevel => RGB
 */
void TIFFRasterImpl::put1bitbwtile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long* bw;

    fromskew /= 8;
    while (h-- > 0) {
	UNROLL8(w, bw = BWmap_[*pp++], *cp++ = *bw++);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 2-bit greyscale => RGB
 */
void TIFFRasterImpl::put2bitbwtile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long* bw;

    fromskew /= 4;
    while (h-- > 0) {
	UNROLL4(w, bw = BWmap_[*pp++], *cp++ = *bw++);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 4-bit greyscale => RGB
 */
void TIFFRasterImpl::put4bitbwtile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue*,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long* bw;

    fromskew /= 2;
    while (h-- > 0) {
	UNROLL2(w, bw = BWmap_[*pp++], *cp++ = *bw++);
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * 8-bit packed samples => RGB
 */
void TIFFRasterImpl::putRGBcontig8bittile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue* Map,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    fromskew *= samplesperpixel_;
    if (Map) {
	while (h-- > 0) {
	    u_long x;
	    for (x = w; x-- > 0;) {
		*cp++ = PACK(Map[pp[0]], Map[pp[1]], Map[pp[2]]);
		pp += samplesperpixel_;
	    }
	    pp += fromskew;
	    cp += toskew;
	}
    } else {
	while (h-- > 0) {
	    UNROLL8(w,,
		*cp++ = PACK(pp[0], pp[1], pp[2]);
		pp += samplesperpixel_);
	    cp += toskew;
	    pp += fromskew;
	}
    }
}

/*
 * 16-bit packed samples => RGB
 */
void TIFFRasterImpl::putRGBcontig16bittile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue* Map,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long x;

    fromskew *= samplesperpixel_;
    if (Map) {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		*cp++ = PACK(Map[pp[0]], Map[pp[1]], Map[pp[2]]);
		pp += samplesperpixel_;
	    }
	    cp += toskew;
	    pp += fromskew;
	}
    } else {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		*cp++ = PACK(pp[0], pp[1], pp[2]);
		pp += samplesperpixel_;
	    }
	    cp += toskew;
	    pp += fromskew;
	}
    }
}

/*
 * 8-bit unpacked samples => RGB
 */
void TIFFRasterImpl::putRGBseparate8bittile(
    u_long* cp,
    const u_char* r, const u_char* g, const u_char* b,
    const RGBvalue* Map,
    u_long w, u_long h,
    int fromskew, int toskew
)
{
    if (Map) {
	while (h-- > 0) {
	    u_long x;
	    for (x = w; x > 0; x--)
		*cp++ = PACK(Map[*r++], Map[*g++], Map[*b++]);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    } else {
	while (h-- > 0) {
	    UNROLL8(w,, *cp++ = PACK(*r++, *g++, *b++));
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    }
}

/*
 * 16-bit unpacked samples => RGB
 */
void TIFFRasterImpl::putRGBseparate16bittile(
    u_long* cp,
    const u_char* r, const u_char* g, const u_char* b,
    const RGBvalue* Map,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    u_long x;

    if (Map) {
	while (h-- > 0) {
	    for (x = w; x > 0; x--)
		*cp++ = PACK(Map[*r++], Map[*g++], Map[*b++]);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    } else {
	while (h-- > 0) {
	    for (x = 0; x < w; x++)
		*cp++ = PACK(*r++, *g++, *b++);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    }
}

/*
 * 8-bit greyscale => RGB
 */
void TIFFRasterImpl::putRGBgreytile(
    u_long* cp,
    const u_char* pp,
    const RGBvalue* Map,
    u_long w, u_long h,
    int fromskew, int toskew
) {
    while (h-- > 0) {
	u_long x;
	for (x = w; x-- > 0;) {
	    RGBvalue c = Map[*pp++];
	    *cp++ = PACK(c,c,c);
	}
	cp += toskew;
	pp += fromskew;
    }
}

/*
 * Select the appropriate conversion routine for packed data.
 */
tileContigRoutine TIFFRasterImpl::pickTileContigCase(const RGBvalue*) {
    tileContigRoutine put = 0;
    switch (photometric_) {
    case PHOTOMETRIC_RGB:
	if (bitspersample_ == 8) {
	    put = &TIFFRasterImpl::putRGBcontig8bittile;
	} else {
	    put = &TIFFRasterImpl::putRGBcontig16bittile;
	}
	break;
    case PHOTOMETRIC_PALETTE:
	switch (bitspersample_) {
	case 8:	put = &TIFFRasterImpl::put8bitcmaptile; break;
	case 4: put = &TIFFRasterImpl::put4bitcmaptile; break;
	case 2: put = &TIFFRasterImpl::put2bitcmaptile; break;
	case 1: put = &TIFFRasterImpl::put1bitcmaptile; break;
	}
	break;
    case PHOTOMETRIC_MINISWHITE:
    case PHOTOMETRIC_MINISBLACK:
	switch (bitspersample_) {
	case 8:	put = &TIFFRasterImpl::putRGBgreytile; break;
	case 4: put = &TIFFRasterImpl::put4bitbwtile; break;
	case 2: put = &TIFFRasterImpl::put2bitbwtile; break;
	case 1: put = &TIFFRasterImpl::put1bitbwtile; break;
	}
	break;
    }
    return (put);
}

/*
 * Select the appropriate conversion routine for unpacked data.
 *
 * NB: we assume that unpacked single channel data is directed
 *     to the "packed routines.
 */
tileSeparateRoutine TIFFRasterImpl::pickTileSeparateCase(const RGBvalue*) {
    if (bitspersample_ == 8) {
	return &TIFFRasterImpl::putRGBseparate8bittile;
    }
    return &TIFFRasterImpl::putRGBseparate16bittile;
}
