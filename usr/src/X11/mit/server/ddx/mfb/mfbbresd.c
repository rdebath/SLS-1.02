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
/* $XConsortium: mfbbresd.c,v 1.3 89/11/08 17:12:27 keith Exp $ */
#include "X.h"
#include "misc.h"
#include "mfb.h"
#include "maskbits.h"

/* Dashed bresenham line */

#define StepDash\
    if (!--dashRemaining) { \
	if (++ dashIndex == numInDashList) \
	    dashIndex = 0; \
	dashRemaining = pDash[dashIndex]; \
	rop = fgrop; \
	if (dashIndex & 1) \
	    rop = bgrop; \
    }

mfbBresD(fgrop, bgrop,
	 pdashIndex, pDash, numInDashList, pdashOffset, isDoubleDash,
	 addrl, nlwidth,
	 signdx, signdy, axis, x1, y1, e, e1, e2, len)
int fgrop, bgrop;
int *pdashIndex;	/* current dash */
unsigned char *pDash;	/* dash list */
int numInDashList;	/* total length of dash list */
int *pdashOffset;	/* offset into current dash */
int isDoubleDash;
int *addrl;		/* pointer to base of bitmap */
int nlwidth;		/* width in longwords of bitmap */
int signdx, signdy;	/* signs of directions */
int axis;		/* major axis (Y_AXIS or X_AXIS) */
int x1, y1;		/* initial point */
register int e;		/* error accumulator */
register int e1;	/* bresenham increments */
int e2;
int len;		/* length of line */
{
    register int yinc;	/* increment to next scanline, in bytes */
    register unsigned char *addrb;
    register int e3 = e2-e1;
    register unsigned long bit;
    unsigned int leftbit = mask[0]; /* leftmost bit to process in new word */
    unsigned int rightbit = mask[31]; /* rightmost bit to process in new word */
    int dashIndex;
    int dashOffset;
    int dashRemaining;
    int	rop;

    dashOffset = *pdashOffset;
    dashIndex = *pdashIndex;
    dashRemaining = pDash[dashIndex] - dashOffset;
    rop = fgrop;
    if (!isDoubleDash)
	bgrop = -1;
    if (dashIndex & 1)
	rop = bgrop;

    /* point to longword containing first point */
    addrb = (unsigned char *)(addrl + (y1 * nlwidth) + (x1 >> 5));
    yinc = signdy * nlwidth * 4;                /* 4 == sizeof(int) */
    e = e-e1;			/* to make looping easier */
    bit = mask[x1 & 31];
    if (axis == X_AXIS)
    {
	if (signdx > 0)
	{
	    while(len--)
	    { 
		if (rop == RROP_BLACK)
		    *(unsigned long *)addrb &= ~bit;
		else if (rop == RROP_WHITE)
		    *(unsigned long *)addrb |= bit;
		else if (rop == RROP_INVERT)
		    *(unsigned long *)addrb ^= bit;
		e += e1;
		if (e >= 0)
		{
		    addrb += yinc;
		    e += e3;
		}
		bit = SCRRIGHT(bit,1);
		if (!bit) { bit = leftbit;addrb += 4; }
		StepDash
	    }
	}
	else
	{
	    while(len--)
	    { 
		if (rop == RROP_BLACK)
		    *(unsigned long *)addrb &= ~bit;
		else if (rop == RROP_WHITE)
		    *(unsigned long *)addrb |= bit;
		else if (rop == RROP_INVERT)
		    *(unsigned long *)addrb ^= bit;
		e += e1;
		if (e >= 0)
		{
		    addrb += yinc;
		    e += e3;
		}
		bit = SCRLEFT(bit,1);
		if (!bit) { bit = rightbit;addrb -= 4; }
		StepDash
	    }
	}
    } /* if X_AXIS */
    else
    {
	if (signdx > 0)
	{
	    while(len--)
	    {
		if (rop == RROP_BLACK)
		    *(unsigned long *)addrb &= ~bit;
		else if (rop == RROP_WHITE)
		    *(unsigned long *)addrb |= bit;
		else if (rop == RROP_INVERT)
		    *(unsigned long *)addrb ^= bit;
		e += e1;
		if (e >= 0)
		{
		    bit = SCRRIGHT(bit,1);
		    if (!bit) { bit = leftbit;addrb += 4; }
		    e += e3;
		}
		addrb += yinc;
		StepDash
	    }
	}
	else
	{
	    while(len--)
	    {
		if (rop == RROP_BLACK)
		    *(unsigned long *)addrb &= ~bit;
		else if (rop == RROP_WHITE)
		    *(unsigned long *)addrb |= bit;
		else if (rop == RROP_INVERT)
		    *(unsigned long *)addrb ^= bit;
		e += e1;
		if (e >= 0)
		{
		    bit = SCRLEFT(bit,1);
		    if (!bit) { bit = rightbit;addrb -= 4; }
		    e += e3;
		}
		addrb += yinc;
		StepDash
	    }
	}
    } /* else Y_AXIS */
    *pdashIndex = dashIndex;
    *pdashOffset = pDash[dashIndex] - dashRemaining;
} 
