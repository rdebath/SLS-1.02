#include "defs.h"

draw_shadow(shape_no, xpos, rot )
  int shape_no, xpos, rot;
  
{
  rotshape_ptr sh = &shape[shape_no].forms[rot];
  
  XClearArea( XtDisplay(shadow), XtWindow(shadow), 0, 0, 0, 0, False );

  XFillRectangle(XtDisplay(shadow), XtWindow(shadow), shape[shape_no].gc,
		 xpos*resources.boxsize + sh->shadowx, 0, sh->shadowwidth, resources.boxsize);
  XFlush(XtDisplay(toplevel));
}

show_next()
{
  XClearArea( XtDisplay(nextobject), XtWindow(nextobject), 0,0,0,0, False );
  print_shape( nextobject, next_no, 0, 0, next_rot, False );
}

print_shape_window( d, w, shape_no, x, y, rot, clear )
  Display *d;
  Drawable w;
  int shape_no, x, y, rot;
  unsigned char clear;
{
  rotshape_ptr sh = &shape[shape_no].forms[rot];
  long xmin, ymin;
  XRectangle rect[2];
  int i;
  
  ymin = y* resources.boxsize;
  xmin = x*resources.boxsize;

  /* Fill or clear the rectangles */
      
  for (i = 0; i < sh->nrect; i++)
    {
      XRectangle *r = &rect[i], *shr = &sh->rect[i];
      
      *r = *shr;
      r->x += xmin;
      r->y += ymin;
      if (clear) 
	XClearArea( d, w, r->x, r->y, r->width, r->height, FALSE );
    }
  
  if (!clear)
    {
      XFillRectangles( d, w, shape[shape_no].gc, rect, sh->nrect );
      XFlush(d);
    }
}

print_shape( w, shape_no, x, y, rot, clear )
  Widget w;
  int shape_no, x, y, rot;
  unsigned char clear;
{
  print_shape_window( XtDisplay(w), XtWindow(w), shape_no, x, y, rot, clear );
}
