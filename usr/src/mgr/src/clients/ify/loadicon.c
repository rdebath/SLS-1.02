#include "bitblit.h"
#include "term.h"

#include "icondata.h"

void loadicon(n, w, h) int n, *w, *h;
{
  BITMAP *b=&ify_icon;

  m_ttyset();
  m_func(BIT_SRC);
  *w=BIT_WIDE(b);
  *h=BIT_HIGH(b);
  m_bitcreate(n,*w,*h);
  m_bitldto(*w,*h,0,0,n,bit_size(*w,*h,BIT_DEPTH(b)));
  fwrite(BIT_DATA(b),bit_size(*w,*h,BIT_DEPTH(b)),1,m_termout);
  m_flush();
  m_ttyreset();
}
