#include "defs.h"

shape_type shape[7] = {

/*      bitmap    score X Y wid ht */
  { /* Shape 0 */
    { { 0x00000f00, 5,  { {0,1,4,1}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  0,4,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x00004444, 8,  { {1,0,1,4}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  1,1,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* #### */
      { 0x00000f00, 5,  { {0,1,4,1}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  0,4,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x00004444, 8,  { {1,0,1,4}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  1,1,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  },

  { /* Shape 1 */
    { { 0x0000cc00, 6,  { {0,0,2,2}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ##   */
      { 0x0000cc00, 6,  { {0,0,2,2}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ##   */
      { 0x0000cc00, 6,  { {0,0,2,2}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x0000cc00, 6,  { {0,0,2,2}, {0,0,0,0} }, {{0,0,0,0},{0,0,0,0}}, 1,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  },

  { /* Shape 2 */
    { { 0x00004e00, 5,  { {0,1,3,1}, {1,0,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*  #   */
      { 0x00004640, 5,  { {1,0,1,3}, {2,1,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  1,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ###  */
      { 0x00000e40, 6,  { {0,1,3,1}, {1,2,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x00004c40, 5,  { {1,0,1,3}, {0,1,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  },

  { /* Shape 3 */
    { { 0x0000c600, 6,  { {0,0,2,1}, {1,1,2,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ##   */
      { 0x00004c80, 7,  { {1,0,1,2}, {0,1,1,2} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*  ##  */
      { 0x0000c600, 6,  { {0,0,2,1}, {1,1,2,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x00004c80, 7,  { {1,0,1,2}, {0,1,1,2} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  },

  { /* Shape 4 */
    { { 0x00006c00, 6,  { {1,0,2,1}, {0,1,2,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*  ##  */
      { 0x00008c40, 7,  { {0,0,1,2}, {1,1,1,2} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ##   */
      { 0x00006c00, 6,  { {1,0,2,1}, {0,1,2,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x00008c40, 7,  { {0,0,1,2}, {1,1,1,2} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  },

  { /* Shape 5 */
    { { 0x00002e00, 6,  { {2,0,1,1}, {0,1,3,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*   #  */
      { 0x000088c0, 7,  { {0,0,1,3}, {1,2,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ###  */
      { 0x0000e800, 6,  { {0,0,3,1}, {0,1,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x0000c440, 7,  { {1,0,1,3}, {0,0,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  },

  { /* Shape 6 */
    { { 0x0000e200, 6,  { {0,0,3,1}, {2,1,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /* ###  */
      { 0x000044c0, 7,  { {1,0,1,3}, {0,2,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*   #  */
      { 0x00008e00, 6,  { {0,1,3,1}, {0,0,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,3,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },   /*      */
      { 0x0000c880, 7,  { {0,0,1,3}, {1,0,1,1} }, {{0,0,0,0},{0,0,0,0}}, 2,  0,2,  {0,0,0,0}, {0,0,0,0}, {0,0,0,0} } }, /*      */
  }
};

void define_shapes()
{
  Pixmap boxstipple;
  int s, r, x, y;
  
  /* First create the stipple for the boxes, depending on the box size. */
  {
    XGCValues gcv;
    GC cleargc, setgc, checkgc;
    Pixmap graytile;
    static XPoint points[] = 
    { 
      { 0,1 }, 
      { 1,0 }, 
    };

    /* This long, complicated thing is intended to reduce traffic to the server: 
       create a gray stipple first, then spray it over the box to make the final
       stipple. */

    graytile = XCreatePixmap( XtDisplay(canvas), XtWindow(canvas), 2, 2, 1 );
    gcv.foreground = gcv.background = 0;
    cleargc = XCreateGC( XtDisplay(canvas), graytile, (unsigned long) GCForeground|GCBackground, &gcv );
    gcv.foreground = 1; 
    setgc = XCreateGC( XtDisplay(canvas), graytile, (unsigned long) GCForeground|GCBackground, &gcv );
    XFillRectangle( XtDisplay(canvas), graytile, cleargc, 0, 0, 2, 2 );
    XDrawPoints( XtDisplay(canvas), graytile, setgc, points, 2, CoordModeOrigin );

    /* Now create the box. */

    boxstipple = XCreatePixmap( XtDisplay(canvas), XtWindow(canvas), 
			       resources.boxsize, resources.boxsize, 1 );
    gcv.tile = graytile;
    gcv.fill_style = FillTiled;
    checkgc = XCreateGC( XtDisplay(canvas), boxstipple, 
			(unsigned long) GCForeground|GCBackground|GCTile|GCFillStyle, &gcv );
    XFillRectangle( XtDisplay(canvas), boxstipple, checkgc, 0, 0, 
		   resources.boxsize-1, resources.boxsize-1 );
    XDrawLine( XtDisplay(canvas), boxstipple, setgc, 0, 0, resources.boxsize-2, 0 );
    XDrawLine( XtDisplay(canvas), boxstipple, setgc, 0, 1, 0, resources.boxsize-2 );
    XDrawLine( XtDisplay(canvas), boxstipple, cleargc, resources.boxsize-1, 0, 
	      resources.boxsize-1, resources.boxsize-1 );
    XDrawLine( XtDisplay(canvas), boxstipple, cleargc, 0, resources.boxsize-1,
	      resources.boxsize-2, resources.boxsize-1 );
    
    /* Now free up all this garbage. */

    XFreeGC( XtDisplay(canvas), setgc );
    XFreeGC( XtDisplay(canvas), cleargc );
    XFreeGC( XtDisplay(canvas), checkgc );
    XFreePixmap( XtDisplay(canvas), graytile );
  }

  /* First set the highesty values (could have specified these statically, but the
     human cost is too high.  This is one-shot anyway). */

  for (s = 0; s < 7; s++)
    {
      shape_ptr sh = &shape[s];
      XGCValues gcv;

      for (r = 0; r < 4; r++)
	{
	  /* working on shape s */

	  rotshape_ptr rot = &sh->forms[r];
	  unsigned long unitson;
	  int i;


	  /* Scale the x,y,width,length */

	  for (i = 0; i < rot->nrect; i++)
	    {
	      XRectangle *rec = &rot->rect[i];

	      *rec = rot->urect[i];

	      rec->x *= resources.boxsize;
	      rec->y *= resources.boxsize;
	      rec->width *= resources.boxsize;
	      rec->height *= resources.boxsize;
	    }

	  rot->shadowx *= resources.boxsize;
	  rot->shadowwidth *= resources.boxsize;

	  /* set the highesty values. */

	  unitson = rot->unitson;
	  for (x = 0; x < 4; x++)
	    {
	      rot->highesty[x] = rot->highestx[x] = 0;
	      rot->lowestx[x] = -2;

	    }

	  for (y = 3; y >= 0; y -- )
	    for (x = 3; x >= 0; x--)
	      {
		if (unitson & 1)
		  {
		    if (rot->highesty[x] == 0)
		      rot->highesty[x] = y+1;
		    if (rot->highestx[y] == 0)
		      rot->highestx[y] = x+1;
		    rot->lowestx[y] = x-1;
		  }
		unitson >>= 1;
	      }
	}

      /* Now allocate the graphics context, and set it properly. */

      gcv.foreground = sh->foreground;
      gcv.background = sh->background;
      gcv.stipple = boxstipple;
      gcv.fill_style = FillOpaqueStippled;

      sh->gc = XCreateGC( XtDisplay(canvas), XtWindow(canvas),
			 (unsigned long) GCForeground|GCBackground|GCStipple|GCFillStyle,
			 &gcv );
    }
}


void store_shape(shape_no, xpos, ypos, rot)
  int     shape_no, xpos, ypos, rot;
{
  register unsigned long unitson = shape[shape_no].forms[rot].unitson;

  int y, x;

  for (y = ypos+3; y >= ypos; y--)
    for (x = xpos+3; x >= xpos; x--)
      {

	if ((x >= 0) && (y >= 0))
	  if (unitson & 0x00000001)
	    grid[x][y] = &shape[shape_no];
	unitson >>= 1;
      }
}

extern long random();

void create_shape()
{
  shape_no = next_no;
  rot = next_rot;
  next_no = random() % 7;
  next_rot = random() % 4;
  xpos = (UWIDTH / 2) - 1;
  ypos = -4;
}
/*
emacs mode: indented-text
 
emacs Local Variables: 
emacs mode: c 
emacs c-indent-level: 2
emacs c-continued-statement-offset: 2
emacs c-continued-brace-offset: -2
emacs c-tab-always-indent: nil
emacs c-brace-offset: 0 
emacs tab-width: 8
emacs tab-stop-list: (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84)
emacs End:
*/
