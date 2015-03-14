/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/glennsBitBlt.c,v 1.5 1992/08/29 11:10:02 dawes Exp $ */
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
5/2/92
*******************************************************************************/

#include "misc.h"
#include "vgaBank.h"

void GlennsBitBlt(srcBase, dstBase, widthSrc, widthDst, x, y, x1, y1, w, h,
		  xdir, ydir)
unsigned char *srcBase, *dstBase;
int    widthSrc, widthDst;
int    x, y, x1, y1, w, h;
int    xdir, ydir;
{
    if (srcBase >= (unsigned char*)VGABASE)
	if (dstBase >= (unsigned char*)VGABASE) {
	    if (w < 6)
		if (ydir == 1)
		    WinWin(srcBase + y * widthSrc + x,
			   dstBase + y1 * widthDst + x1,
			   h, w, 1024, 1, xdir, 0);
		else
		    WinWin(srcBase - (y + h - 1) * widthSrc + x,
			   dstBase - (y1 + h - 1) * widthDst + x1,
			   h, w, -1024, -1, xdir, 0);
	    else if (ydir == 1)
		if ((y == y1) && (xdir == -1))
		    WinWin(srcBase + (y + h - 1) * widthSrc  + x + w - 1,
			   dstBase + (y1 + h - 1) * widthDst + x1 + w - 1,
			   h, w, -widthSrc + w, -1, -1, 1);
		else
		    WinWin(srcBase + y * widthSrc + x, dstBase + y1 * widthDst + x1,
			   h, w, widthSrc - w, 1, xdir, 0);
	    else
		WinWin(srcBase - (y + h - 1) * widthSrc + x + w - 1,
		       dstBase - (y1 + h - 1) * widthDst + x1 + w - 1,
		       h, w, widthSrc + w, -1, xdir, 0);
	} else {
	    widthSrc = widthSrc > 0 ? widthSrc: -widthSrc;
	    widthDst = widthDst > 0 ? widthDst: -widthDst;
	    WinPix(srcBase+(y*widthSrc)+x,dstBase+(y1*widthDst)+x1,
		   h, w, widthSrc - w, widthDst - w);
	}
    else if (dstBase >= (unsigned char*)VGABASE) {
	widthSrc = widthSrc > 0 ? widthSrc: -widthSrc;
	widthDst = widthDst > 0 ? widthDst: -widthDst;
	PixWin(srcBase+(y*widthSrc)+x,dstBase+(y1*widthDst)+x1,
	       h, w, widthSrc - w, widthDst - w);
    } else {
	if (ydir == 1)
	    if ((y == y1) && (xdir == -1))
		PixPix(srcBase+((y+h-1)*widthSrc)+x+w-1,
		       dstBase+((y1+h-1)*widthDst)+x1+w-1,
		       h, w, -widthSrc + w, -widthDst + w, -1);
	    else
		PixPix(srcBase+(y*widthSrc)+x,dstBase+(y1*widthDst)+x1,
		       h, w, widthSrc - w, widthDst - w, 1);
	else
	    if (w > 4)
		PixPix(srcBase-((y+h-1)*widthSrc)+x+w-1,
		       dstBase-((y1+h-1)*widthDst)+x1+w-1,
		       h, w, widthSrc + w, widthDst + w, -1);
	    else
		PixPix(srcBase-((y+h-1)*widthSrc)+x,
		       dstBase-((y1+h-1)*widthDst)+x1,
		       h, w, widthSrc - w, widthDst - w, -1);
    }
}
