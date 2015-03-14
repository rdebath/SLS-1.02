/*
 * cfb8bit.c
 *
 * 8 bit color frame buffer utility routines
 */

/* $XConsortium: cfb8bit.c,v 1.5 91/01/27 13:02:56 keith Exp $ */

#include	"X.h"
#include	"Xmd.h"
#include	"Xproto.h"
#include	"gcstruct.h"
#include	"windowstr.h"
#include	"scrnintstr.h"
#include	"pixmapstr.h"
#include	"regionstr.h"
#include	"cfb.h"
#include	"cfbmskbits.h"
#include	"cfb8bit.h"

#if (PPW == 4)

unsigned long cfb8StippleMasks[16] = {
    0x00000000, 0x000000ff, 0x0000ff00, 0x0000ffff,
    0x00ff0000, 0x00ff00ff, 0x00ffff00, 0x00ffffff,
    0xff000000, 0xff0000ff, 0xff00ff00, 0xff00ffff,
    0xffff0000, 0xffff00ff, 0xffffff00, 0xffffffff,
};

int	cfb8StippleMode, cfb8StippleAlu, cfb8StippleRRop;
unsigned long	cfb8StippleFg, cfb8StippleBg, cfb8StipplePm;
unsigned long	cfb8StippleAnd[16], cfb8StippleXor[16];

int
cfb8SetStipple (alu, fg, planemask)
int		alu;
unsigned long	fg, planemask;
{
    unsigned long   and, xor, rrop;
    int	s;
    unsigned long   c;

    cfb8StippleMode = FillStippled;
    cfb8StippleAlu = alu;
    cfb8StippleFg = fg & PMSK;
    rrop = cfbReduceRasterOp (alu, fg, planemask, &and, &xor);
    cfb8StippleRRop = rrop;
    /*
     * create the appropriate pixel-fill bits for current
     * foreground
     */
    for (s = 0; s < 16; s++)
    {
	c = cfb8StippleMasks[s];
	cfb8StippleAnd[s] = and | ~c;
	cfb8StippleXor[s] = xor & c;
    }
}

int
cfb8SetOpaqueStipple (alu, fg, bg, planemask)
int		alu;
unsigned long	fg, bg, planemask;
{
    unsigned long   andfg, xorfg, andbg, xorbg, rropfg, rropbg;
    int	s;
    unsigned long   c;

    cfb8StippleMode = FillOpaqueStippled;
    cfb8StippleAlu = alu;
    cfb8StippleFg = fg & PMSK;
    cfb8StippleBg = bg & PMSK;
    cfb8StipplePm = planemask & PMSK;
    rropfg = cfbReduceRasterOp (alu, cfb8StippleFg, cfb8StipplePm, &andfg, &xorfg);
    rropbg = cfbReduceRasterOp (alu, cfb8StippleBg, cfb8StipplePm, &andbg, &xorbg);
    if (rropfg == rropbg)
	cfb8StippleRRop = rropfg;
    else
	cfb8StippleRRop = GXset;
    /*
     * create the appropriate pixel-fill bits for current
     * foreground
     */
    for (s = 0; s < 16; s++)
    {
	c = cfb8StippleMasks[s];
	cfb8StippleAnd[s] = (andfg | ~c) & (andbg | c);
	cfb8StippleXor[s] = (xorfg & c) | (xorbg & ~c);
    }
}

/*
 * a grungy little routine.  This computes clip masks
 * for partial character blts.  Returns rgnOUT if the
 * entire character is clipped; returns rgnIN if the entire
 * character is unclipped; returns rgnPART if a portion of
 * the character is visible.  Computes clip masks for each
 * longword of the character -- and those with the
 * contents of the glyph to compute the visible bits.
 */

#if (BITMAP_BIT_ORDER == MSBFirst)
unsigned long	cfb8BitLenMasks[32] = {
    0xffffffff, 0x7fffffff, 0x3fffffff, 0x1fffffff,
    0x0fffffff, 0x07ffffff, 0x03ffffff, 0x01ffffff,
    0x00ffffff, 0x007fffff, 0x003fffff, 0x001fffff,
    0x000fffff, 0x0007ffff, 0x0003ffff, 0x0001ffff,
    0x0000ffff, 0x00007fff, 0x00003fff, 0x00001fff,
    0x00000fff, 0x000007ff, 0x000003ff, 0x000001ff,
    0x000000ff, 0x0000007f, 0x0000003f, 0x0000001f,
    0x0000000f, 0x00000007, 0x00000003, 0x00000001,
};
#else
unsigned long cfb8BitLenMasks[32] = {
    0xffffffff, 0xfffffffe, 0xfffffffc, 0xfffffff8,
    0xfffffff0, 0xffffffe0, 0xffffffc0, 0xffffff80,
    0xffffff00, 0xfffffe00, 0xfffffc00, 0xfffff800,
    0xfffff000, 0xffffe000, 0xffffc000, 0xffff8000,
    0xffff0000, 0xfffe0000, 0xfffc0000, 0xfff80000,
    0xfff00000, 0xffe00000, 0xffc00000, 0xff800000,
    0xff000000, 0xfe000000, 0xfc000000, 0xf8000000,
    0xf0000000, 0xe0000000, 0xc0000000, 0x80000000,
};
#endif

int
cfb8ComputeClipMasks32 (pBox, numRects, x, y, w, h, clips)
    BoxPtr	pBox;
    int		numRects;
    int		x, y, w, h;
    unsigned long   *clips;
{
    int	    yBand, yBandBot;
    int	    ch;
    unsigned long	    clip;
    int	    partIN = FALSE, partOUT = FALSE;
    int	    result;

    if (numRects == 0)
	return rgnOUT;
    while (numRects && pBox->y2 <= y)
    {
	--numRects;
	++pBox;
    }
    if (!numRects || pBox->y1 >= y + h)
	return rgnOUT;
    yBand = pBox->y1;
    while (numRects && pBox->y1 == yBand && pBox->x2 <= x)
    {
	--numRects;
	++pBox;
    }
    if (!numRects || pBox->y1 >= y + h)
	return rgnOUT;
    if (numRects &&
	x >= pBox->x1 &&
	x + w <= pBox->x2 &&
	y >= pBox->y1 &&
	y + h <= pBox->y2)
    {
	return rgnIN;
    }
    ch = 0;
    while (ch < h && y + ch < pBox->y1)
    {
	partOUT = TRUE;
	clips[ch++] = 0;
    }
    while (numRects && pBox->y1 < y + h)
    {
	yBand = pBox->y1;
	yBandBot = pBox->y2;
    	while (numRects && pBox->y1 == yBand && pBox->x2 <= x)
    	{
	    --numRects;
	    ++pBox;
    	}
    	if (!numRects)
	    break;
	clip = 0;
    	while (numRects && pBox->y1 == yBand && pBox->x1 < x + w)
    	{
	    if (x < pBox->x1)
		if (pBox->x2 < x + w)
		    clip |= cfb8BitLenMasks[pBox->x1 - x] & ~cfb8BitLenMasks[pBox->x2 - x];
		else
		    clip |= cfb8BitLenMasks[pBox->x1 - x];
 	    else
		if (pBox->x2 < x + w)
		    clip |= ~cfb8BitLenMasks[pBox->x2 - x];
		else
		    clip = ~0;
	    --numRects;
	    ++pBox;
    	}
	if (clip != 0)
		partIN = TRUE;
	if (clip != ~0)
		partOUT = TRUE;
	while (ch < h && y + ch < yBandBot)
	    clips[ch++] = clip;
	while (numRects && pBox->y1 == yBand)
	{
	    --numRects;
	    ++pBox;
	}
    }
    while (ch < h)
    {
	partOUT = TRUE;
	clips[ch++] = 0;
    }
    result = rgnOUT;
    if (partIN)
    {
	if (partOUT)
	    result = rgnPART;
	else
	    result = rgnIN;
    }
    return result;
}

#endif /* PPW == 4 */
