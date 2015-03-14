/************************************************************
Copyright 1987 by Sun Microsystems, Inc. Mountain View, CA.

                    All Rights Reserved

Permission  to  use,  copy,  modify,  and  distribute   this
software  and  its documentation for any purpose and without
fee is hereby granted, provided that the above copyright no-
tice  appear  in all copies and that both that copyright no-
tice and this permission notice appear in  supporting  docu-
mentation,  and  that the names of Sun or MIT not be used in
advertising or publicity pertaining to distribution  of  the
software  without specific prior written permission. Sun and
M.I.T. make no representations about the suitability of this
software for any purpose. It is provided "as is" without any
express or implied warranty.

SUN DISCLAIMS ALL WARRANTIES WITH REGARD TO  THIS  SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SUN BE  LI-
ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

/* $XConsortium: cfbmskbits.c,v 4.6 91/07/05 10:52:59 rws Exp $ */

/*
 * ==========================================================================
 * Converted to Color Frame Buffer by smarks@sun, April-May 1987.  The "bit 
 * numbering" in the doc below really means "byte numbering" now.
 * ==========================================================================
 */

/*
   these tables are used by several macros in the cfb code.

   the vax numbers everything left to right, so bit indices on the
screen match bit indices in longwords.  the pc-rt and Sun number
bits on the screen the way they would be written on paper,
(i.e. msb to the left), and so a bit index n on the screen is
bit index 32-n in a longword

   see also cfbmskbits.h
*/
#include	<X.h>
#include	<Xmd.h>
#include	<servermd.h>

#if	(BITMAP_BIT_ORDER == MSBFirst)
/* NOTE:
the first element in starttab could be 0xffffffff.  making it 0
lets us deal with a full first word in the middle loop, rather
than having to do the multiple reads and masks that we'd
have to do if we thought it was partial.
*/
unsigned int cfbstarttab[] =
    {
	0x00000000,
	0x00FFFFFF,
	0x0000FFFF,
	0x000000FF
    };

unsigned int cfbendtab[] =
    {
	0x00000000,
	0xFF000000,
	0xFFFF0000,
	0xFFFFFF00
    };

/* a hack, for now, since the entries for 0 need to be all
   1 bits, not all zeros.
   this means the code DOES NOT WORK for segments of length
   0 (which is only a problem in the horizontal line code.)
*/
unsigned int cfbstartpartial[] =
    {
	0xFFFFFFFF,
	0x00FFFFFF,
	0x0000FFFF,
	0x000000FF
    };

unsigned int cfbendpartial[] =
    {
	0xFFFFFFFF,
	0xFF000000,
	0xFFFF0000,
	0xFFFFFF00
    };
#else		/* (BITMAP_BIT_ORDER == LSBFirst) */
/* NOTE:
the first element in starttab could be 0xffffffff.  making it 0
lets us deal with a full first word in the middle loop, rather
than having to do the multiple reads and masks that we'd
have to do if we thought it was partial.
*/
unsigned int cfbstarttab[] = 
	{
	0x00000000,
	0xFFFFFF00,
	0xFFFF0000,
	0xFF000000
	};

unsigned int cfbendtab[] = 
	{
	0x00000000,
	0x000000FF,
	0x0000FFFF,
	0x00FFFFFF
	};

/* a hack, for now, since the entries for 0 need to be all
   1 bits, not all zeros.
   this means the code DOES NOT WORK for segments of length
   0 (which is only a problem in the horizontal line code.)
*/
unsigned int cfbstartpartial[] = 
	{
	0xFFFFFFFF,
	0xFFFFFF00,
	0xFFFF0000,
	0xFF000000
	};

unsigned int cfbendpartial[] = 
	{
	0xFFFFFFFF,
	0x000000FF,
	0x0000FFFF,
	0x00FFFFFF
	};
#endif	/* (BITMAP_BIT_ORDER == MSBFirst) */


/* used for masking bits in bresenham lines
   mask[n] is used to mask out all but bit n in a longword (n is a
screen position).
   rmask[n] is used to mask out the single bit at position n (n
is a screen posiotion.)
*/

#if	(BITMAP_BIT_ORDER == MSBFirst)
unsigned int cfbmask[] =
    {
	0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF
    }; 
unsigned int cfbrmask[] = 
    {
	0x00FFFFFF, 0xFF00FFFF, 0xFFFF00FF, 0xFFFFFF00
    };
#else	/* (BITMAP_BIT_ORDER == LSBFirst) */
unsigned int cfbmask[] =
    {
	0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000
    }; 
unsigned int cfbrmask[] = 
    {
	0xFFFFFF00, 0xFFFF00FF, 0xFF00FFFF, 0x00FFFFFF
    };
#endif	/* (BITMAP_BIT_ORDER == MSBFirst) */

/*
 * QuartetBitsTable contains four masks whose binary values are masks in the
 * low order quartet that contain the number of bits specified in the
 * index.  This table is used by getstipplepixels.
 */
unsigned int QuartetBitsTable[5] = {
#if (BITMAP_BIT_ORDER == MSBFirst)
    0x00000000,                         /* 0 - 0000 */
    0x00000008,                         /* 1 - 1000 */
    0x0000000C,                         /* 2 - 1100 */
    0x0000000E,                         /* 3 - 1110 */
    0x0000000F                          /* 4 - 1111 */
#else /* (BITMAP_BIT_ORDER == LSBFirst */
    0x00000000,                         /* 0 - 0000 */
    0x00000001,                         /* 1 - 0001 */
    0x00000003,                         /* 2 - 0011 */
    0x00000007,                         /* 3 - 0111 */
    0x0000000F                          /* 4 - 1111 */
#endif /* (BITMAP_BIT_ORDER == MSBFirst) */
};

/*
 * QuartetPixelMaskTable is used by getstipplepixels to get a pixel mask
 * corresponding to a quartet of bits.
 */
unsigned int QuartetPixelMaskTable[16] = {
    0x00000000,
    0x000000FF,
    0x0000FF00,
    0x0000FFFF,
    0x00FF0000,
    0x00FF00FF,
    0x00FFFF00,
    0x00FFFFFF,
    0xFF000000,
    0xFF0000FF,
    0xFF00FF00,
    0xFF00FFFF,
    0xFFFF0000,
    0xFFFF00FF,
    0xFFFFFF00,
    0xFFFFFFFF
};

#ifdef	vax
#undef	VAXBYTEORDER
#endif
