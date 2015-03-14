/*{{{}}}*/
/*{{{  #includes*/
#include <stdlib.h>

#include "screen.h"
#include "share.h"
/*}}}  */

/*{{{  bit_destroy*/
void bit_destroy(bitmap) BITMAP *bitmap;
{
  if (bitmap==(BITMAP*)0) return;

  /* destroy bitmap cache for primary bitmaps */
  if (IS_PRIMARY(bitmap) && bitmap->cache)
  {
    bit_destroy(bitmap->cache);
    bitmap->cache=NULL;
  }

  if (IS_MEMORY(bitmap) && IS_PRIMARY(bitmap))
  {
#ifdef MOVIE
    log_destroy(bitmap);
#endif
    free(bitmap->data);
    bitmap->data=(BITMAP*)0;
  }
  else if (IS_SCREEN(bitmap) && IS_PRIMARY(bitmap))
  {
#ifdef MOVIE
    log_destroy(bitmap);
#endif
    resetscreen();
  }

  /* dont free static bitmaps */
  if (IS_STATIC(bitmap) && IS_PRIMARY(bitmap)) return;
  free(bitmap);
}
/*}}}  */
