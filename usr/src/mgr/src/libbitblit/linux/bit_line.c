/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include "screen.h"
#include "share.h"
#include "do.h"

#define CLIP /* */

void bit_line(dest, x0, y0, x1, y1, func)
BITMAP *dest;
int x0, y0, x1, y1;
int func;
{
  register int r, rincr, rdecr, d_incr, count;
  register unsigned char bit;
  register char *dst;
#ifdef NEED_ADJUST
  register char *Dst;
#else
#define Dst dst
#endif
  int temp, dx, dy;

  /* Clip here */

#define TOP	1
#define BOTTOM	2
#define LEFT	4
#define RIGHT	8

#define crossings(x,y) \
  (x<0 ? LEFT : x>= (dest->wide) ? RIGHT : 0) + \
  (y<0 ? TOP : y>=(dest->high) ? BOTTOM : 0)

  {
#ifdef CLIP
	/* The classic clipping algorithm */

	int Cross0 = crossings(x0, y0);
	int Cross1 = crossings(x1, y1);

	while (Cross0 || Cross1) {
		int Cross, x, y;
		if (Cross0 & Cross1) return;
		if (Cross0 != 0)
			Cross = Cross0;
		else
			Cross = Cross1;
		if (Cross & (LEFT | RIGHT)) {
			int edge = (Cross & LEFT) ? 0 : dest->wide - 1;
			y = y0 + (y1 - y0) * (edge - x0) / (x1 - x0);
			x = edge;
		} else if (Cross & (TOP | BOTTOM)) {
			int edge = (Cross & TOP) ? 0 : dest->high - 1;
			x = x0 + (x1 - x0) * (edge - y0) / (y1 - y0);
			y = edge;
		}
		if (Cross == Cross0) {
			x0 = x;
			y0 = y;
			Cross0 = crossings(x, y);
		} else {
			x1 = x;
			y1 = y;
			Cross1 = crossings(x, y);
		}
	}
#endif CLIP
	x0 += dest->x0;
	y0 += dest->y0;
	x1 += dest->x0;
	y1 += dest->y0;
  }

#ifdef MOVIE
  log_line(dest,x0,y0,x1,y1,func);
#endif

  /* Always left to right */

  if (x1 < x0) {
	temp = x1, x1 = x0, x0 = temp;
	temp = y1, y1 = y0, y0 = temp;
  }
  dx = x1 - x0;
  dy = y1 - y0;
  if (dy > 0)
	d_incr = BIT_LINE(dest);
  else
	d_incr = -BIT_LINE(dest), dy = -dy;

  dst = y0 * (BIT_LINE(dest)) + (x0 >> 3) + (char*)(BIT_DATA(dest));
#ifdef NEED_ADJUST
  Dst=(IS_SCREEN(dest) ? adjust(dst) : dst);
#endif
  bit = (0x80 >> (x0 & 7));

  /* */

  if (dx > dy) {

	rincr = (dx - dy) << 1;
	rdecr = -(dy << 1);
	r = dx + rdecr;
	for (count = dx; count >= 0; count--) {
		_do_mask(Dst, bit, func);
		if ((bit >>= 1) == 0) {
			bit = 0x80;
#ifdef NEED_ADJUST
                        Dst++;
#endif
			dst++;
		};
		if (r < 0) {
			dst += d_incr;
#ifdef NEED_ADJUST
                        Dst=(IS_SCREEN(dest) ? adjust(dst) : dst);
#endif
			r += rincr;
		} else
			r += rdecr;
	}
  } else {
	rincr = (dy - dx) << 1;
	rdecr = -(dx << 1);
	r = dy + rdecr;
	for (count = dy; count >= 0; count--) {
		_do_mask(Dst, bit, func);
		dst += d_incr;
#ifdef NEED_ADJUST
                Dst=(IS_SCREEN(dest) ? adjust(dst) : dst);
#endif
		if (r < 0) {
			if ((bit >>= 1) == 0) {
				bit = 0x80;
#ifdef NEED_ADJUST
                                Dst++;
#endif
				dst++;
			};
			r += rincr;
		} else
			r += rdecr;
	}
  }
}
