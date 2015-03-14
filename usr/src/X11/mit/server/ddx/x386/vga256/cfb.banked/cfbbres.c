/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/cfb.banked/cfbbres.c,v 1.9 1992/08/29 11:12:43 dawes Exp $ */
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
/* $XConsortium: cfbbres.c,v 1.9 90/01/31 12:31:15 keith Exp $ */
#include "X.h"
#include "misc.h"
#include "cfb.h"
#include "cfbmskbits.h"
#include "servermd.h"
#include "fastblt.h"
#include "vgaBank.h"

/* Solid bresenham line */
/* NOTES
   e2 is used less often than e1, so it's not in a register
*/

stdcfbBresS(rop, and, xor, addrl, nlwidth, signdx, signdy, axis, x1, y1,
            e, e1, e2, len)
int rop;
register unsigned char and, xor;
int *addrl;		/* pointer to base of bitmap */
int nlwidth;		/* width in longwords of bitmap */
register short signdx;
int signdy;		/* signs of directions */
int axis;		/* major axis (Y_AXIS or X_AXIS) */
int x1, y1;		/* initial point */
register short e;		/* error accumulator */
register short e1;	/* bresenham increments */
int e2;
register short len;		/* length of line */
{
  register unsigned char *addrb;		/* bitmask long pointer 
					     	 * cast to char pointer */
  register unsigned char Oflag, Uflag;
  register short e3 = e2-e1;
  int gendir = signdx + signdy;
  
  
  if (len == 0)
    return;
  
  BANK_FLAG(addrl)
  Oflag = (vgaWriteFlag && ((gendir == 2) || (gendir == 0)));
  Uflag = (vgaWriteFlag && ((gendir == -2) || (gendir == 0)));

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
  
  SETRW(addrb);

#ifdef SPEEDUP
if (rop == GXcopy)
  while(len--)
    { 
      *addrb = xor; 
      e += e1;
      if (e >= 0)
	{
	  addrb += nlwidth;
	  e += e3;
	}
      addrb += signdx;
      CHECKRWO(addrb);
      CHECKRWU(addrb);
    }
else
#endif

  DuffL (len, label1,
	 *addrb = DoRRop (*addrb, and, xor);
	 e += e1;
	 if (e >= 0)
	 {
	   addrb += nlwidth;
	   e += e3;
	 }
	 addrb += signdx;
	 CHECKRWOF(Oflag, addrb);
	 CHECKRWUF(Uflag, addrb);)
}

