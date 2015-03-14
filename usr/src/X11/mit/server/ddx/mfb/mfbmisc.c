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
/* $XConsortium: mfbmisc.c,v 5.2 91/06/08 15:07:28 rws Exp $ */
#include "X.h"
#include "misc.h"
#include "cursor.h"
#include "scrnintstr.h"

#include "mfb.h"

/*ARGSUSED*/
void
mfbQueryBestSize(class, pwidth, pheight, pScreen)
int class;
short *pwidth;
short *pheight;
ScreenPtr pScreen;
{
    unsigned width, test;

    switch(class)
    {
      case CursorShape:
	  if (*pwidth > pScreen->width)
	     *pwidth = pScreen->width;
	  if (*pheight > pScreen->height)
	     *pheight = pScreen->height;
	  break;
      case TileShape:
      case StippleShape:
	  width = *pwidth;
	  if (!width) break;
	  /* Return the closes power of two not less than what they gave me */
	  test = 0x80000000;
	  /* Find the highest 1 bit in the width given */
	  while(!(test & width))
	     test >>= 1;
	  /* If their number is greater than that, bump up to the next
	   *  power of two */
	  if((test - 1) & width)
	     test <<= 1;
	  *pwidth = test;
	  /* We don't care what height they use */
	  break;
    }
}

