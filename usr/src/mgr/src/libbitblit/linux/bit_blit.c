/*{{{}}}*/
/*{{{  #includes*/
#include "screen.h"
#include "share.h"
#include "do.h"
/*}}}  */

/*{{{  Precomputed shift mask*/
unsigned char _mask[9] = { 0xFF, 0x7F, 0x3F, 0x1F, 0x0F, 0x07, 0x03, 0x01, 0x00 };
/*}}}  */

/*{{{  old code for bitplanes*/
#if 0
  /*  General memory-to-memory rasterop */

  void bit_blit(dest, dx, dy, width, height, rfunc, source, sx, sy)
  int sx, sy, dx, dy;		/* properly clipped source and dest */
  int width, height;		/* rectangle to be transferred */
  BITMAP *source, *dest;		/* bit map pointers */
  int rfunc;			/* rasterop function */
  {
    int func,xfunc,mapmask,plane,fmask;

    xfunc = OPCODE(rfunc);
    if (!(fmask = mapmask = (0x0f&(~GETFCOLOR(rfunc)))))
    fmask = 0x0f; /* Empty map mask same as full one! */

    if (source) 	/* Same depth... */
    if (BIT_DEPTH(source) == BIT_DEPTH(dest))
    {
      for (plane = 0;plane<BIT_DEPTH(dest);plane++)
      single_blit(dest,dx,dy,width,height,xfunc,source,sx,sy,plane);
      return;
    }

    for (plane=0;plane<BIT_DEPTH(dest);plane++)
    {
      switch (xfunc)
      {
        case BIT_SRC:
        if ((1<<plane)&fmask)
        func = xfunc;
        else
        func = BIT_CLR;
        break;
        case BIT_CLR:
        func = BIT_CLR;
        break;
        case BIT_SET:
        if ((1<<plane)&mapmask)
        func = xfunc;
        else
        func = ~xfunc;
        break;
        case ~BIT_SRC:
        if ((1<<plane)&fmask)
        func = BIT_SET;
        else
        func = ~BIT_SRC;
        break;
        default:
        if ((1<<plane)&fmask)
        func = xfunc;
        else
        func = BIT_DST;
      }
      if (func != BIT_DST)
      single_blit(dest,dx,dy,width,height,xfunc,source,sx,sy,plane);
    }
  }
#endif
/*}}}  */

/*{{{  bit_blit*/
void bit_blit(dest, dx, dy, width, height, func, source, sx,sy)
int sx, sy, dx, dy;		/* properly clipped source and dest */
int width, height;		/* rectangle to be transferred */
BITMAP *source, *dest;		/* bit map pointers */
int func;			/* rasterop function */
{
  /*{{{  variables*/
  int dwwidth = BIT_LINE(dest);
  int swwidth;
  unsigned int h_cnt, h_cnt_m1, shift;
  unsigned int lmask, rmask;
  register char *dst;
  register char *src;
#  ifdef NEED_ADJUST
  register char *Dst;
  register char *Src;
#  endif
  register int t;
  /*}}}  */

  /*{{{  Clipping, should already be done ?*/
  if (width < 0) dx += width, width = -width;
  if (height < 0) dy += height, height = -height;
  if (dx < 0)
  {
    if (source) sx -= dx;
    width += dx; dx = 0;
  }
  if (dy < 0)
  {
    if (source) sy -= dy;
    height += dy, dy = 0;
  }
    if (source)
    {
      if (sx < 0) dx -= sx, width += sx, sx = 0;
      if (sy < 0) dy -= sy, height += sy, sy = 0;
      if ((t = sx + width - source->wide) > 0) width -= t;
      if ((t = sy + height - source->high) > 0) height -= t;

      swwidth = BIT_LINE(source);
      sx += source->x0, sy += source->y0;

    }
    if ((t = dx + width - dest->wide) > 0) width -= t;
    if ((t = dy + height - dest->high) > 0) height -= t;

    if (width < 1 || height < 1) return;
  /*}}}  */

  dx += dest->x0, dy += dest->y0;
  h_cnt = ((dx + width - 1)/8) - (dx/8) + 1;
  h_cnt_m1 = h_cnt-1;
  func = OPCODE(func);
  dst = (char *) (BIT_DATA(dest) + dy * dwwidth + (dx/8));
  /*{{{  old code to pick the bitplane to use*/
#  if 0
  if (IS_SCREEN(dest)) setplane(plane);
  else if (IS_PRIMARY(dest)) dst += BIT_OFFSET(dest)*(plane%BIT_DEPTH(dest));
  else dst += BIT_OFFSET(dest->primary)*(plane%BIT_DEPTH(dest->primary));
#  endif
  /*}}}  */
  lmask = _mask[dx&7];
  rmask = ~_mask[((dx+width-1) & 7) + 1];

  _cld();	/* default direction is 'up' */
#ifdef MOVIE
  log_blit(dest,dx,dy,width,height,func,src,sx,sy);
#endif
  if (!source)
  /*{{{  no source bitmap*/
  {
    if (h_cnt > 1)
    {
      while (height-- > 0)
      {
#        ifdef NEED_ADJUST
        Dst=IS_SCREEN(dest) ? adjust(dst) : dst;
        _do_mask(Dst, lmask, func);
        _do_blit(Dst+1, h_cnt_m1-1, func);
        _do_mask(Dst+h_cnt_m1, rmask, func);
#        else
        _do_mask(dst, lmask, func);
        _do_blit(dst+1, h_cnt_m1-1, func);
        _do_mask(dst+h_cnt_m1, rmask, func);
#        endif
        dst += dwwidth;
      }
    } else
    {
      lmask &= rmask;
      while (height-- > 0)
      {
#        ifdef NEED_ADJUST
        _do_mask(IS_SCREEN(dest) ? adjust(dst) : dst, lmask, func);
#        else
        _do_mask(dst, lmask, func);
#        endif
        dst += dwwidth;
      }
    }
  }
  /*}}}  */
  else
  /*{{{  Source (op) dest bitmap*/
  {
    src = BIT_DATA(source) + sy * swwidth + (sx/8);
    /*{{{  old code to pick the bitplane to use*/
#    if 0
    if (IS_SCREEN(source)) setplane(plane);
    else if (IS_PRIMARY(source)) src += BIT_OFFSET(source)*(plane%BIT_DEPTH(source));
    else src += BIT_OFFSET(source->primary)*(plane%BIT_DEPTH(source->primary));
#    endif
    /*}}}  */
    if ((sx&7) >= (dx&7))
    shift = 8 - (sx&7) + (dx&7);
    else
    shift = (dx&7) - (sx&7), src -= 1;

    if (dy >= sy) 
    {
      src += (height - 1) * swwidth;
      dst += (height - 1) * dwwidth;
      swwidth = -swwidth, dwwidth = -dwwidth;
    }

    if (h_cnt > 1) 
    {
      if (dx < sx)
      {		/* left to right */
        while (height-- > 0) 
        {
#          ifdef NEED_ADJUST
          Dst=(IS_SCREEN(dest) ? adjust(dst) : dst);
          Src=(IS_SCREEN(source) ? adjust(src) : src);
          _do_2mask(Dst, Src, lmask, func, shift);
          _do_2blit(Dst+1, Src+1, h_cnt_m1-1, func, shift);
          _do_2mask(Dst+h_cnt_m1, Src+h_cnt_m1, rmask, func, shift);
#          else
          _do_2mask(dst, src, lmask, func, shift);
          _do_2blit(dst+1, src+1, h_cnt_m1-1, func, shift);
          _do_2mask(dst+h_cnt_m1, src+h_cnt_m1, rmask, func, shift);
#          endif
          src += swwidth;
          dst += dwwidth;
        }
      }
      else 
      {		/* right to left */
        _std();
        src += h_cnt_m1, dst += h_cnt_m1;
        while (height-- > 0) 
        {
#          ifdef NEED_ADJUST
          Dst=(IS_SCREEN(dest) ? adjust(dst) : dst);
          Src=(IS_SCREEN(source) ? adjust(src) : src);
          _do_2mask(Dst, Src, rmask, func, shift);
          _do_2blit(Dst-1, Src-1, h_cnt-2, func, shift);
          _do_2mask(Dst-h_cnt+1, Src-h_cnt+1, lmask, func, shift);
#          else
          _do_2mask(dst, src, rmask, func, shift);
          _do_2blit(dst-1, src-1, h_cnt-2, func, shift);
          _do_2mask(dst-h_cnt+1, src-h_cnt+1, lmask, func, shift);
#          endif
          src += swwidth;
          dst += dwwidth;
        }
      }
    }
    else 
    {	/* h_cnt = 1 */
      lmask &= rmask;
      if (dx >= sx)
      {		/* left to right */
        _std();
        src += h_cnt_m1, dst += h_cnt_m1;
      }
      while (height-- > 0) 
      {
#        ifdef NEED_ADJUST
        _do_2mask(IS_SCREEN(dest) ? adjust(dst) : dst, IS_SCREEN(source) ? adjust(src) : src, lmask, func, shift);
#        else
        _do_2mask(dst, src, lmask, func, shift);
#        endif
        dst += dwwidth;
        src += swwidth;
      }
    }
    _cld();	/* better to restore this */
  }
  /*}}}  */
}
/*}}}  */
