/*
 * Draw a point at position (dx, dy) in 'map' using bitblit function 'func'.
 */

/*{{{}}}*/
/*{{{  #includes*/
#include "screen.h"
#include "share.h"
#include "do.h"
/*}}}  */

int bit_point(map, dx, dy, func)
BITMAP *map;
register int dx, dy;
int func;
{
  register unsigned char *dst;
  int x = BIT_X(map) + dx;
  int y = BIT_Y(map) + dy;

  if (dx < 0 || dy < 0 || dx >= BIT_WIDE(map) || dy >= BIT_HIGH(map))
	return(-1);
#ifdef MOVIE
  log_point(map,x,y,func);
#endif
  dst = (char *) (BIT_DATA(map)) + y * BIT_LINE(map) + (x >> 3);
#ifdef NEED_ADJUST
  _do_mask(IS_SCREEN(map) ? adjust(dst) : dst, 0x80 >> (x & 7), OPCODE(func));
  /* untested */
  if (IS_SCREEN(map)) dst=adjust(dst);
  return ((*dst)&(0x80>>(x&7)));
#else
  _do_mask(dst, 0x80 >> (x & 7), OPCODE(func));
  /* untested */
  return ((*dst)&(0x80>>(x&7)));
#endif
}
