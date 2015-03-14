/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/gBanks.c,v 1.4 1992/08/29 11:09:54 dawes Exp $ */
/*******************************************************************************
			Copyright 1992 by Glenn G. Lai 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Glenn G. Lai not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

Glenn G. Lai DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

Glenn G. Lai
P.O. Box 4314
Austin, Tx 78765
(glenn@cs.utexas.edu)
6/3/92
*******************************************************************************/
#include "misc.h"
#include "vgaBank.h"
unsigned SpeedUpRowsNext[17];
unsigned SpeedUpRowsPrev[17];
void SpeedUpComputeNext(unsigned dst, unsigned h) 
{
    if (vgaWriteFlag) {
	register unsigned int i, j = h, k = 0, l = vgaSegmentMask + 1 >> 10;
    
	i = l - ((dst & vgaSegmentMask) >> 10);
	do {
	    if (i > j) i = j;
	    j -= i;
	    SpeedUpRowsNext[k++] = i;
	i = l;
	} while (j);
	SpeedUpRowsNext[k] = 0;
    } else {
	SpeedUpRowsNext[0] = h;
	SpeedUpRowsNext[1] = 0;
    }
}
