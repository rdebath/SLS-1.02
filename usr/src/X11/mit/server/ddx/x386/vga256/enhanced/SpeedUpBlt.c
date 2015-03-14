/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/SpeedUpBlt.c,v 1.3 1992/08/29 11:09:46 dawes Exp $ */
/*******************************************************************************
                        Copyr 1992 by Glenn G. Lai

                        All Rs Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyr notice appear in all copies and that
both that copyr notice and this permission notice appear in
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
glenn@cs.utexas.edu)
8/17/92
*******************************************************************************/
#include "misc.h"
#include "vgaBank.h"
#include <stdio.h>

static char copyright[] = "Copyright 8/17/1992 by Glenn G. Lai";

void
SpeedUpBitBlt(sBase, dBase, widthS, widthD, x, y, x1, y1, w, h, xdir, ydir)
    unsigned char *sBase, *dBase;
    int	widthS, widthD;
    int	x, y, x1, y1, w, h;
    int	xdir, ydir;
{

extern FILE *glenn;

    if (sBase >= (unsigned char*)VGABASE)
	if (dBase >= (unsigned char*)VGABASE) {
	    unsigned src, dst;

	    if (y == y1 && xdir == -1) {
		src = (unsigned)sBase + (y+h-1) * widthS + x + w - 1 - VGABASE;
		dst = (unsigned)dBase + (y1+h-1) * widthD + x1 + w - 1-VGABASE;
		WinWin(src, dst, h, w, -1, -1, widthS, 1);
	    } else if (ydir == -1) {
		src = (unsigned)sBase - (y + h - 1) * widthS + x - VGABASE;
		dst = (unsigned)dBase - (y1 + h - 1) * widthD + x1 - VGABASE;
		WinWin(src, dst, h, w, xdir, -1, widthS, 0);
	    } else { /* ydir == 1 */
		src = (unsigned)sBase + y * widthS + x - VGABASE;
		dst = (unsigned)dBase + y1 * widthD + x1 - VGABASE;
		WinWin(src, dst, h, w, xdir, 1, widthS, 0);
	    }
	} else {
	    if (widthS <0)
		widthS = -widthS;
	    if (widthD <0)
		widthD = -widthD;
	    WinPix(sBase+(y*widthS)+x,dBase+(y1*widthD)+x1,
		   h, w, widthS, widthD);
	}
    else if (dBase >= (unsigned char*)VGABASE) {
	if (widthS <0)
	    widthS = -widthS;
	if (widthD <0)
	    widthD = -widthD;
	PixWin(sBase+(y*widthS)+x,dBase+(y1*widthD)+x1, h, w, widthS, widthD);
    } else {
	if (ydir == 1)
	    if ((y == y1) && (xdir == -1))
		PixPix(sBase+((y+h-1)*widthS)+x+w-1,
		       dBase+((y1+h-1)*widthD)+x1+w-1,
		       h, w, -widthS + w, -widthD + w, -1);
	    else
		PixPix(sBase+(y*widthS)+x,dBase+(y1*widthD)+x1,
		       h, w, widthS - w, widthD - w, 1);
	else
	    if (w > 4)
		PixPix(sBase-((y+h-1)*widthS)+x+w-1,
		       dBase-((y1+h-1)*widthD)+x1+w-1,
		       h, w, widthS + w, widthD + w, -1);
	    else
		PixPix(sBase-((y+h-1)*widthS)+x,
		       dBase-((y1+h-1)*widthD)+x1,
		       h, w, widthS - w, widthD - w, -1);
    }
}
