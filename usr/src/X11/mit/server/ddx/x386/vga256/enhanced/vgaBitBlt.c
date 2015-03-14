/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/vgaBitBlt.c,v 1.6 1992/09/11 13:39:06 dawes Exp $ */
/*
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Thomas Roell, roell@informatik.tu-muenchen.de
 *
 * /proj/X11/mit/server/ddx/at386/vga/RCS/vgaBitBlt.c,v 1.5 91/02/10 16:44:40 root Exp
 */

#include	"X.h"
#include	"Xmd.h"
#include	"Xproto.h"
#include	"gcstruct.h"
#include	"windowstr.h"
#include	"scrnintstr.h"
#include	"pixmapstr.h"
#include	"regionstr.h"
#include	"cfb.h"
#include	"cfbmskbits.h"
#include	"cfb8bit.h"
#include	"fastblt.h"
#include	"mergerop.h"
#include        "vgaBank.h"


extern void fastBitBltCopy();

void (*ourvgaBitBlt)();
void vgaImageRead();
void vgaImageWrite();

void
vgaBitBlt(pdstBase, psrcBase, widthSrc, widthDst, x, y,
	    x1, y1, w, h, xdir, ydir, alu, planemask)
     unsigned char *psrcBase, *pdstBase;  /* start of src and dst bitmaps */
     int    widthSrc, widthDst;
     int    x, y, x1, y1, w, h;
     int    xdir, ydir;
     int    alu;
     unsigned long  planemask;

{
  unsigned char *psrc, *pdst;
  int hcount, vcount, count, srcPitch, dstPitch, tmp;

  if (alu == GXcopy && (planemask & 0xFF) == 0xFF) {
    if (xdir == 1) { /* left to right */
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x;
	  pdst = pdstBase+(y1*widthDst)+x1;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1;
	}
      srcPitch = widthSrc - w;
      dstPitch = widthDst - w;
      
    } else  {/* right to left */
      
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x+w;
	  pdst = pdstBase+(y1 * widthDst)+x1+w;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x+w;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1+w;
	}
      srcPitch = widthSrc + w;
      dstPitch = widthDst + w;
    }
    
    psrc = (unsigned char *)vgaSetRead(psrc);
    pdst = (unsigned char *)vgaSetWrite(pdst);
    
    vcount = 0;
    while (h || vcount) {
      /* 
       * compute here the maximal linecount(hcount) before a
       * segment switch has to be made
       */
      if (!vcount) {
	if (ydir == 1) {
	  tmp = ((unsigned char *)vgaReadTop - psrc) / widthSrc;
	  if ((xdir == -1) &&
	      ((psrc - (unsigned char *)vgaReadBottom) <= w)) tmp = 0;
	} else {
	  tmp = (psrc - (unsigned char *)vgaReadBottom) / (-widthSrc);
	  if ((xdir == 1) &&
	      (((unsigned char *)vgaReadTop - psrc) <= w)) tmp = 0;
	}
	hcount=min( h, tmp );
	if(!hcount) vcount = w;
	
	if (ydir == 1) {
	  tmp = ((unsigned char *)vgaWriteTop - pdst) / widthDst;
	  if ((xdir == -1) &&
	      ((pdst - (unsigned char *)vgaWriteBottom) <= w)) tmp = 0;
	} else {
	  tmp = (pdst - (unsigned char *)vgaWriteBottom) / (-widthDst);
	  if ((xdir == 1) &&
	      (((unsigned char *)vgaWriteTop - pdst) <= w)) tmp = 0;
	}
	hcount=min( hcount, tmp );
	if(!hcount) vcount = w;
      }

      /*
       * if we now have to make a segment switch within a line,
       * divide this line into small parts.
       */
      if (vcount) {
	count = vcount;
	
	/* check which segment to switch first */
	if (xdir == 1)  {
	  if (tmp = (unsigned char *)vgaReadTop - psrc)
	    count = min(count, tmp);
	  else
	    psrc = (unsigned char *)vgaReadNext(psrc);
	} else {
	  if (tmp = psrc - (unsigned char *)vgaReadBottom)
	    count = min(count, tmp);
	  else
	    psrc = (unsigned char *)vgaReadPrev(psrc);
	}
	
	if (xdir == 1) {
	  if (tmp = (unsigned char *)vgaWriteTop - pdst)
	    count = min(count, tmp);
	  else
	    pdst = (unsigned char *)vgaWriteNext(pdst);
	} else {
	  if (tmp = pdst - (unsigned char *)vgaWriteBottom)
	    count = min(count, tmp);
	  else
	    pdst = (unsigned char *)vgaWritePrev(pdst);
	}
	
	fastBitBltCopy(xdir,psrc, pdst, 1, count, 0, 0);
	
	if (!(vcount -= count)) {
	  h--;  /* partial line finish */
	  if (xdir == 1) {
	    psrc += (srcPitch + count);
	    pdst += (dstPitch + count);
	  } else {
	    psrc += (srcPitch - count);
	    pdst += (dstPitch - count);
	  }

	  if ( psrc >= (unsigned char *)vgaReadTop )
	    psrc = (unsigned char *)vgaReadNext(psrc);
	  if ( psrc < (unsigned char *)vgaReadBottom )
	    psrc = (unsigned char *)vgaReadPrev(psrc);

	  if ( pdst >= (unsigned char *)vgaWriteTop )
	    pdst = (unsigned char *)vgaWriteNext(pdst);
	  if ( pdst < (unsigned char *)vgaWriteBottom )
	    pdst = (unsigned char *)vgaWritePrev(pdst);

	} else {
	  if (xdir == 1) {
	    psrc += count;
	    pdst += count;
	  } else {
	    psrc -= count;
	    pdst -= count;
	  }
	}
      } else {
	
	h -= hcount;
	
	fastBitBltCopy(xdir, psrc, pdst, hcount, w, srcPitch, dstPitch);
	
	psrc += (hcount*widthSrc);
	pdst += (hcount*widthDst);
      }
    }
  }
}

void
OneBankvgaBitBlt(pdstBase, psrcBase, widthSrc, widthDst, x, y,
	    x1, y1, w, h, xdir, ydir, alu, planemask)
     unsigned char *psrcBase, *pdstBase;  /* start of src and dst bitmaps */
     int    widthSrc, widthDst;
     int    x, y, x1, y1, w, h;
     int    xdir, ydir;
     int    alu;
     unsigned long  planemask;

{
#define BUFSIZE 4096

      unsigned char buf[BUFSIZE];
      int m,n;

      if (alu == GXcopy && (planemask & 0xFF) == 0xFF) {
              if (w == 0)     
                      return;
              if ((m=BUFSIZE/w) <= 0)
                      return;
              if (ydir == 1) {
                      while (h) {
                              n = m>h ? h : m;
                              (void)vgaImageRead(buf, psrcBase, widthSrc,w,x,y,0,0,w,n,xdir,ydir,alu,planemask);
                              (void)vgaImageWrite(pdstBase, buf,w,widthDst,0,0,x1,y1,w,n,xdir,ydir,alu,planemask);
                              y += n;
                              y1 += n;
                              h -= n;
                      }
              }
              else {
                      y += h;
                      y1 += h;
                      while (h) {
                              n = m>h ? h : m;
                              y -= n;
                              y1 -= n;
                              h -= n;
                              (void)vgaImageRead(buf+BUFSIZE-w,psrcBase,widthSrc,w,x,y,0,0,w,n,xdir,ydir,alu,planemask);
                              (void)vgaImageWrite(pdstBase,buf+BUFSIZE-w,w,widthDst,0,0,x1,y1,w,n,xdir,ydir,alu,planemask);
                      }
              }
      }
}

void
vgaImageRead(pdstBase, psrcBase, widthSrc, widthDst, x, y,
	    x1, y1, w, h, xdir, ydir, alu, planemask)
     unsigned char *psrcBase, *pdstBase;  /* start of src and dst bitmaps */
     int    widthSrc, widthDst;
     int    x, y, x1, y1, w, h;
     int    xdir, ydir;
     int    alu;
     unsigned long  planemask;

{
  register unsigned char *psrc, *pdst;
  int hcount, vcount, count, srcPitch, dstPitch, tmp;

  if (alu == GXcopy && (planemask & 0xFF) == 0xFF) {
    if (xdir == 1) { /* left to right */
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x;
	  pdst = pdstBase+(y1*widthDst)+x1;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1;
	}
      srcPitch = widthSrc - w;
      dstPitch = widthDst - w;
      
    } else  {/* right to left */
      
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x+w;
	  pdst = pdstBase+(y1 * widthDst)+x1+w;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x+w;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1+w;
	}
      srcPitch = widthSrc + w;
      dstPitch = widthDst + w;
    }
    
    psrc = (unsigned char *)vgaSetRead(psrc);
    
    vcount = 0;
    while (h || vcount) {
      /* 
       * compute here the maximal linecount(hcount) before a
       * segment switch has to be made
       */
      if (!vcount) {
	if (ydir == 1) {
	  tmp = ((unsigned char *)vgaReadTop - psrc) / widthSrc;
	  if ((xdir == -1) &&
	      ((psrc - (unsigned char *)vgaReadBottom) <= w)) tmp = 0;
	} else {
	  tmp = (psrc - (unsigned char *)vgaReadBottom) / (-widthSrc);
	  if ((xdir == 1) &&
	      (((unsigned char *)vgaReadTop - psrc) <= w)) tmp = 0;
	}
	hcount=min( h, tmp );
	if(!hcount) vcount = w;
      }

      /*
       * if we now have to make a segment switch within a line,
       * divide this line into small parts.
       */
      if (vcount) {
	count = vcount;
	
	/* check which segment to switch first */
	if (xdir == 1)  {
	  if (tmp = (unsigned char *)vgaReadTop - psrc)
	    count = min(count, tmp);
	  else
	    psrc = (unsigned char *)vgaReadNext(psrc);
	} else {
	  if (tmp = psrc - (unsigned char *)vgaReadBottom)
	    count = min(count, tmp);
	  else
	    psrc = (unsigned char *)vgaReadPrev(psrc);
	}
	
	fastBitBltCopy(xdir,psrc, pdst, 1, count, 0, 0);
	
	if (!(vcount -= count)) {
	  h--;  /* partial line finish */
	  if (xdir == 1) {
	    psrc += (srcPitch + count);
	    pdst += (dstPitch + count);
	  } else {
	    psrc += (srcPitch - count);
	    pdst += (dstPitch - count);
	  }

	  if ( psrc >= (unsigned char *)vgaReadTop )
	    psrc = (unsigned char *)vgaReadNext(psrc);
	  if ( psrc < (unsigned char *)vgaReadBottom )
	    psrc = (unsigned char *)vgaReadPrev(psrc);

	} else {
	  if (xdir == 1) {
	    psrc += count;
	    pdst += count;
	  } else {
	    psrc -= count;
	    pdst -= count;
	  }
	}
      } else {
	
	h -= hcount;
	
	fastBitBltCopy(xdir, psrc, pdst, hcount, w, srcPitch, dstPitch);
	
	psrc += (hcount*widthSrc);
	pdst += (hcount*widthDst);
      }
    }
  }
}


void
vgaImageWrite(pdstBase, psrcBase, widthSrc, widthDst, x, y,
	    x1, y1, w, h, xdir, ydir, alu, planemask)
     unsigned char *psrcBase, *pdstBase;  /* start of src and dst bitmaps */
     int    widthSrc, widthDst;
     int    x, y, x1, y1, w, h;
     int    xdir, ydir;
     int    alu;
     unsigned long  planemask;

{
  register unsigned char *psrc, *pdst;
  int hcount, vcount, count, srcPitch, dstPitch, tmp;

  if (alu == GXcopy && (planemask & 0xFF) == 0xFF) {
    if (xdir == 1) { /* left to right */
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x;
	  pdst = pdstBase+(y1*widthDst)+x1;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1;
	}
      srcPitch = widthSrc - w;
      dstPitch = widthDst - w;
      
    } else  {/* right to left */
      
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x+w;
	  pdst = pdstBase+(y1 * widthDst)+x1+w;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x+w;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1+w;
	}
      srcPitch = widthSrc + w;
      dstPitch = widthDst + w;
    }
    
    pdst = (unsigned char *)vgaSetWrite(pdst);
    
    vcount = 0;
    while (h || vcount) {
      /* 
       * compute here the maximal linecount(hcount) before a
       * segment switch has to be made
       */
      if (!vcount) {
	if (ydir == 1) {
	  tmp = ((unsigned char *)vgaWriteTop - pdst) / widthDst;
	  if ((xdir == -1) &&
	      ((pdst - (unsigned char *)vgaWriteBottom) <= w)) tmp = 0;
	} else {
	  tmp = (pdst - (unsigned char *)vgaWriteBottom) / (-widthDst);
	  if ((xdir == 1) &&
	      (((unsigned char *)vgaWriteTop - pdst) <= w)) tmp = 0;
	}
	hcount=min( h, tmp );
	if(!hcount) vcount = w;
      }

      /*
       * if we now have to make a segment switch within a line,
       * divide this line into small parts.
       */
      if (vcount) {
	count = vcount;
	
	/* check which segment to switch first */
	if (xdir == 1) {
	  if (tmp = (unsigned char *)vgaWriteTop - pdst)
	    count = min(count, tmp);
	  else
	    pdst = (unsigned char *)vgaWriteNext(pdst);
	} else {
	  if (tmp = pdst - (unsigned char *)vgaWriteBottom)
	    count = min(count, tmp);
	  else
	    pdst = (unsigned char *)vgaWritePrev(pdst);
	}
	
	fastBitBltCopy(xdir,psrc, pdst, 1, count, 0, 0);
	
	if (!(vcount -= count)) {
	  h--;  /* partial line finish */
	  if (xdir == 1) {
	    psrc += (srcPitch + count);
	    pdst += (dstPitch + count);
	  } else {
	    psrc += (srcPitch - count);
	    pdst += (dstPitch - count);
	  }

	  if ( (void *)pdst >= vgaWriteTop )
	    pdst = (unsigned char *)vgaWriteNext(pdst);
	  if ( (void *)pdst < vgaWriteBottom )
	    pdst = (unsigned char *)vgaWritePrev(pdst);

	} else {
	  if (xdir == 1) {
	    psrc += count;
	    pdst += count;
	  } else {
	    psrc -= count;
	    pdst -= count;
	  }
	}
      } else {
	
	h -= hcount;
	
	fastBitBltCopy(xdir, psrc, pdst, hcount, w, srcPitch, dstPitch);
	
	psrc += (hcount*widthSrc);
	pdst += (hcount*widthDst);
      }
    }
  }
}


void
vgaPixBitBlt(pdstBase, psrcBase, widthSrc, widthDst, x, y,
	    x1, y1, w, h, xdir, ydir, alu, planemask)
     unsigned char *psrcBase, *pdstBase;  /* start of src and dst bitmaps */
     int    widthSrc, widthDst;
     int    x, y, x1, y1, w, h;
     int    xdir, ydir;
     int    alu;
     unsigned long  planemask;

{
  register unsigned char *psrc, *pdst;
  int hcount, vcount, count, srcPitch, dstPitch, tmp;

  if (alu == GXcopy && (planemask & 0xFF) == 0xFF) {
    if (xdir == 1) { /* left to right */
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x;
	  pdst = pdstBase+(y1*widthDst)+x1;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1;
	}
      srcPitch = widthSrc - w;
      dstPitch = widthDst - w;
      
    } else  {/* right to left */
      
      if (ydir == 1) /* top to bottom */
	{
	  psrc = psrcBase+(y*widthSrc)+x+w;
	  pdst = pdstBase+(y1 * widthDst)+x1+w;
	}
      else /* bottom to top */
	{
	  psrc = psrcBase-((y+h-1)*widthSrc)+x+w;
	  pdst = pdstBase-((y1+h-1)*widthDst)+x1+w;
	}
      srcPitch = widthSrc + w;
      dstPitch = widthDst + w;
    }
    
    fastBitBltCopy(xdir, psrc, pdst, h, w, srcPitch, dstPitch);
	
  }
}
