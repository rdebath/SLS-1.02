/* Combined Purdue/PurduePlus patches, level 2.0, 1/17/89 */
/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: mfbhrzvert.c,v 1.11 89/09/13 18:58:09 rws Exp $ */
#include "X.h"

#include "gc.h"
#include "window.h"
#include "pixmap.h"
#include "region.h"

#include "mfb.h"
#include "maskbits.h"

/* horizontal solid line
   abs(len) > 1
*/
mfbHorzS(rop, addrl, nlwidth, x1, y1, len)
int rop;		/* a reduced rasterop */
register int *addrl;	/* pointer to base of bitmap */
register int nlwidth;	/* width in longwords of bitmap */
int x1;			/* initial point */ 
int y1;
int len;		/* length of line */
{
    register int startmask;
    register int endmask;
    register int nlmiddle;


    /* force the line to go left to right
       but don't draw the last point
    */
    if (len < 0)
    {
	x1 += len;
	x1 += 1;
	len = -len;
    }

    addrl = addrl + (y1 * nlwidth) + (x1 >> 5);

    /* all bits inside same longword */
    if ( ((x1 & 0x1f) + len) < 32)
    {
	maskpartialbits(x1, len, startmask);
        if (rop == RROP_BLACK)
        {
	    *addrl &= ~startmask;
        }
        else if (rop == RROP_WHITE)
        {
	    *addrl |= startmask;
        }
        else if (rop == RROP_INVERT)
        {
	    *addrl ^= startmask;
        }
    }
    else
    {
	maskbits(x1, len, startmask, endmask, nlmiddle);
        if (rop == RROP_BLACK)
        {
	    if (startmask)
		*addrl++ &= ~startmask;
	    Duff (nlmiddle, *addrl++ = 0x0);
	    if (endmask)
		*addrl &= ~endmask;
        }
        else if (rop == RROP_WHITE)
        {
	    if (startmask)
		*addrl++ |= startmask;
	    Duff (nlmiddle, *addrl++ = 0xffffffff);
	    if (endmask)
		*addrl |= endmask;
        }
        else if (rop == RROP_INVERT)
        {
	    if (startmask)
		*addrl++ ^= startmask;
	    Duff (nlmiddle, *addrl++ ^= 0xffffffff);
	    if (endmask)
		*addrl ^= endmask;
        }
    }
}

/* vertical solid line
   this uses do loops because pcc (Ultrix 1.2, bsd 4.2) generates
   better code.  sigh.  we know that len will never be 0 or 1, so
   it's OK to use it.
*/

mfbVertS(rop, addrl, nlwidth, x1, y1, len)
int rop;		/* a reduced rasterop */
register int *addrl;	/* pointer to base of bitmap */
register int nlwidth;	/* width in longwords of bitmap */
int x1, y1;		/* initial point */
register int len;	/* length of line */
{
    register int bitmask;

    addrl = addrl + (y1 * nlwidth) + (x1 >> 5);

    if (len < 0)
    {
	nlwidth = -nlwidth;
	len = -len;
    }
 
    if (rop == RROP_BLACK)
    {
	bitmask = rmask[x1&0x1f];
        Duff(len, *addrl &= bitmask; addrl += nlwidth );
    }
    else if (rop == RROP_WHITE)
    {
	bitmask = mask[x1&0x1f];
        Duff(len, *addrl |= bitmask; addrl += nlwidth );
    }
    else if (rop == RROP_INVERT)
    {
	bitmask = mask[x1&0x1f];
        Duff(len, *addrl ^= bitmask; addrl += nlwidth );
    }
}

