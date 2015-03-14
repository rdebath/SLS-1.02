/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/cfb.banked/cfbhrzvert.c,v 1.6 1992/08/29 11:12:55 dawes Exp $ */
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
/* $XConsortium: cfbhrzvert.c,v 1.5 90/01/31 12:31:37 keith Exp $ */
#include "X.h"

#include "gc.h"
#include "window.h"
#include "pixmap.h"
#include "region.h"

#include "fastblt.h"
#include "cfb.h"
#undef BitRight
#undef BitLeft
#include "cfbmskbits.h"
#include "vgaBank.h"

/* horizontal solid line
   abs(len) > 1
*/
cfbHorzS(rop, and, xor, addrl, nlwidth, x1, y1, len)
int rop;
unsigned long and;
register unsigned long xor;
register int *addrl;	/* pointer to base of bitmap */
unsigned int nlwidth;		/* width in longwords of bitmap */
int x1;			/* initial point */ 
int y1;
unsigned int len;		/* length of line */
{
    register short count;
    short nlmiddle;
    register unsigned int startmask;
    register unsigned int endmask;
    int tmp, vcount;

    BANK_FLAG(addrl)

    addrl = addrl + (y1 * nlwidth) + (x1 >> PWSH);
    SETRW(addrl);

    /* all bits inside same longword */
    if ( ((x1 & PIM) + len) < PPW)
    {
	maskpartialbits(x1, len, startmask);
	*addrl = DoMaskRRop (*addrl, and, xor, startmask);
    }
    else
    {
	maskbits(x1, len, startmask, endmask, nlmiddle);

        /* If we are going to the cfb then we need to check for a bank switch
           this is done cheaply by finding the maximum vcount defore switching
	   in needed.
	*/
	if (vgaWriteFlag)
	  vcount = min(nlmiddle, (int *)vgaWriteTop - addrl - 1);
	else 
	  vcount = nlmiddle;

	count = vcount;
	 
	if (rop == GXcopy)
	{
	    if (startmask)
	    {
		*addrl = (*addrl & ~startmask) | (xor & startmask);
		addrl++; CHECKRWO(addrl);
	    }

	    DuffL (count, label0, *addrl++ = xor;)
	    
	    /* We've gove as far as we could without switching. Now we
               switch and finish the job.
	    */
	    if (nlmiddle -= vcount)
	      {
		addrl = vgaReadWriteNext(addrl);
		while (nlmiddle--)
		  *addrl++ = xor;
	      }
	    else
	      CHECKRWO(addrl);

	    if (endmask)
		*addrl = (*addrl & ~endmask) | (xor & endmask);
	}
	else
	{
	    if (startmask)
	    {
		*addrl = DoMaskRRop (*addrl, and, xor, startmask);
		addrl++; CHECKRWO(addrl);
	    }
	    if (rop == GXxor)
	    {
	      DuffL (count, label1, *addrl++ ^= xor;)		 
		if (nlmiddle -= vcount)
		  {
		    addrl = vgaReadWriteNext(addrl);
		    while (nlmiddle--)
		      *addrl++ ^= xor;
		  }
		else
		  CHECKRWO(addrl);
	    }
	    else
	    {
		while (count--)
		{
		    *addrl = DoRRop (*addrl, and, xor);
		    addrl++;
		}

		if (nlmiddle -= vcount)
		  {
		    addrl = vgaReadWriteNext(addrl);
		    while (nlmiddle--)
		      {
			*addrl = DoRRop (*addrl, and, xor);
			addrl++;
		      }
		  }
		else
		  CHECKRWO(addrl);
	    }
	    if (endmask)
		*addrl = DoMaskRRop (*addrl, and, xor, endmask);
	}
    }
}

/* vertical solid line */

cfbVertS(rop, and, xor, addrl, nlwidth, x1, y1, len)
int rop;
unsigned char and;
register unsigned char xor;
int *addrl;	/* pointer to base of bitmap */
register unsigned int nlwidth;	/* width in longwords of bitmap */
int x1, y1;		/* initial point */
register unsigned int len;	/* length of line */
{
  unsigned char *bits = (unsigned char *) addrl;
  register unsigned char *bits0;
  register short len0;
  int len1;
  
  BANK_FLAG(addrl);
  
  nlwidth <<= 2;
  bits = bits + (y1 * nlwidth) + x1;
  
  /*
   * special case copy and xor to avoid a test per pixel
   */
  if (rop == GXcopy) {
    while (len) {
      bits0 = bits;
      SETRW(bits0);
      if (vgaWriteFlag)
	len0 = min (len, ((unsigned char *)vgaWriteTop - bits0) / 
		    nlwidth);
      else
	len0 = len;
      len1 = len0;
      DuffL (len0, label2,
	     *bits0 = xor;
	     bits0 += nlwidth;)
      if (len -= len1) {
	len--;
	*bits0 = xor;
	bits += len1 * nlwidth + nlwidth;
      }
    }
  } else 
    if (rop == GXxor) {
      while (len) {
	bits0 = bits;
	SETRW(bits0);
	if (vgaWriteFlag)
	  len0 = min (len, ((unsigned char *)vgaWriteTop - bits0) / 
		      nlwidth);
	else
	  len0 = len;
	len1 = len0;
	DuffL (len0, label3,
	       *bits0 ^= xor;
	       bits0 += nlwidth;)
	if (len -= len1) {
	  len--;
	  *bits0 ^= xor;
	  bits += len1 * nlwidth + nlwidth;
	}
      }
    }
    else
      while (len) {
	bits0 = bits;
	SETRW(bits0);
	if (vgaWriteFlag)
	  len0 = min (len, ((unsigned char *)vgaWriteTop - bits0) / 
		      nlwidth);
	else
	  len0 = len;
	len1 = len0;
	while (len0--) {
	  *bits0 = DoRRop(*bits0, and, xor);
	  bits0 += nlwidth;
	}
	if (len -= len1) {
	  len--;
	  *bits0 = DoRRop(*bits0, and, xor);
	  bits += len1 * nlwidth + nlwidth;
	}
      }
}

