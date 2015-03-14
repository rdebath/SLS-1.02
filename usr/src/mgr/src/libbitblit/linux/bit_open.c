/*{{{}}}*/
/*{{{  #includes*/
#include <stdlib.h>

#include "screen.h"
#include "share.h"
/*}}}  */

/*{{{  bit_open() -- open the screen 'name' as a bitmap.*/
BITMAP *bit_open(name) char *name;
{
  BITMAP *result;
  DATA *addr;

  if ((addr=initscreen(name))==(DATA*)0) return (BITMAP*)0;
  if ((result = (BITMAP *) malloc(sizeof(BITMAP))) == (BITMAP *) 0) return(BITMAP *) 0;
  setscreen();
  result->primary = result;
  result->data = addr;
  result->x0 = 0;
  result->y0 = 0;
  result->wide = screen_width();
  result->high = screen_height();
  result->depth = screen_depth();
  result->type = _SCREEN;
#ifdef MOVIE
  log_open(result);
#endif
  return result;
}
/*}}}  */
