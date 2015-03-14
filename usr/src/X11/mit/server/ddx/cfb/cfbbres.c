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
/* $XConsortium: cfbbres.c,v 1.10 91/07/10 14:53:48 keith Exp $ */
#include "X.h"
#include "misc.h"
#include "cfb.h"
#include "cfbmskbits.h"
#include "servermd.h"

/* Solid bresenham line */
/* NOTES
   e2 is used less often than e1, so it's not in a register
*/

cfbBresS(rop, and, xor, addrl, nlwidth, signdx, signdy, axis, x1, y1, e, e1, e2, len)
int rop;
unsigned long and, xor;
int *addrl;		/* pointer to base of bitmap */
int nlwidth;		/* width in longwords of bitmap */
register int signdx;
int signdy;		/* signs of directions */
int axis;		/* major axis (Y_AXIS or X_AXIS) */
int x1, y1;		/* initial point */
register int e;		/* error accumulator */
register int e1;	/* bresenham increments */
int e2;
int len;		/* length of line */
{
    register int    e3 = e2-e1;

#if (PPW == 4)
    register unsigned char *addrb;		/* bitmask long pointer 
					     	 * cast to char pointer */
    register unsigned char pix = xor;

    if (len == 0)
    	return;
    /* point to first point */
    nlwidth <<= 2;
    addrb = (unsigned char *)(addrl) + (y1 * nlwidth) + x1;
    if (signdy < 0)
    	nlwidth = -nlwidth;
    e = e-e1;			/* to make looping easier */
    
    if (axis == Y_AXIS)
    {
	int	t;

	t = nlwidth;
	nlwidth = signdx;
	signdx = t;
    }
    if (rop == GXcopy)
    {
	--len;
#define body {\
	    *addrb = pix; \
	    addrb += signdx; \
	    e += e1; \
	    if (e >= 0) \
	    { \
		addrb += nlwidth; \
		e += e3; \
	    } \
	}
#ifdef LARGE_INSTRUCTION_CACHE
	while (len >= 16)
	{
	    body body body body
	    body body body body
	    body body body body
	    body body body body
	    len -= 16;
	}
	switch (len)
	{
	case 15: body case 14: body case 13: body case 12: body
	case 11: body case 10: body case  9: body case  8: body
	case  7: body case  6: body case  5: body case  4: body
	case  3: body case  2: body case  1: body
	}
#else
	while (len >= 4)
	{
	    body body body body
	    len -= 4;
	}
	switch (len)
	{
	case  3: body case  2: body case  1: body
	}
#endif
#undef body
	*addrb = pix;
    }
    else
    {
	while(len--)
	{ 
	    *addrb = DoRRop (*addrb, and, xor);
	    e += e1;
	    if (e >= 0)
	    {
		addrb += nlwidth;
		e += e3;
	    }
	    addrb += signdx;
	}
    }
#else
    register unsigned long   tmp, bit;
    unsigned long leftbit, rightbit;

    /* point to longword containing first point */
    addrl = (addrl + (y1 * nlwidth) + (x1 >> PWSH));
    if (signdy < 0)
	    nlwidth = -nlwidth;
    e = e-e1;			/* to make looping easier */

    leftbit = cfbmask[0];
    rightbit = cfbmask[PPW-1];
    bit = cfbmask[x1 & PIM];

    if (axis == X_AXIS)
    {
	if (signdx > 0)
	{
	    while (len--)
	    { 
		*addrl = DoMaskRRop (*addrl, and, xor, bit);
		bit = SCRRIGHT(bit,1);
		e += e1;
		if (e >= 0)
		{
		    addrl += nlwidth;
		    e += e3;
		}
		if (!bit)
		{
		    bit = leftbit;
		    addrl++;
		}
	    }
	}
	else
	{
	    while (len--)
	    { 
		*addrl = DoMaskRRop (*addrl, and, xor, bit);
		e += e1;
		bit = SCRLEFT(bit,1);
		if (e >= 0)
		{
		    addrl += nlwidth;
		    e += e3;
		}
		if (!bit)
		{
		    bit = rightbit;
		    addrl--;
		}
	    }
	}
    } /* if X_AXIS */
    else
    {
	if (signdx > 0)
	{
	    while(len--)
	    {
		*addrl = DoMaskRRop (*addrl, and, xor, bit);
		e += e1;
		if (e >= 0)
		{
		    bit = SCRRIGHT(bit,1);
		    if (!bit)
		    {
			bit = leftbit;
			addrl++;
		    }
		    e += e3;
		}
		addrl += nlwidth;
	    }
	}
	else
	{
	    while(len--)
	    {
		*addrl = DoMaskRRop (*addrl, and, xor, bit);
		e += e1;
		if (e >= 0)
		{
		    bit = SCRLEFT(bit,1);
		    if (!bit)
		    {
			bit = rightbit;
			addrl--;
		    }
		    e += e3;
		}
		addrl += nlwidth;
	    }
	}
    } /* else Y_AXIS */
#endif
}
