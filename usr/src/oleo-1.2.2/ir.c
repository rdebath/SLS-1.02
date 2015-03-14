/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "ir.h"
int
xx_IRencloses (r1, r2)
     xx_IntRectangle r1;
     xx_IntRectangle r2;
{
  return (xx_IRhits_point (r1, xx_IRllx (r2), xx_IRlly (r2))
	  && (!xx_IRw (r2) || !xx_IRh (r2)
	      || (xx_IRhits_point (r1, xx_IRurx (r2), xx_IRlly (r2))
		  && xx_IRhits_point (r1, xx_IRllx (r2), xx_IRury (r2))
		  && xx_IRhits_point (r1, xx_IRurx (r2), xx_IRury (r2)))));
}

int
xx_IRequiv (r1, r2)
     xx_IntRectangle r1;
     xx_IntRectangle r2;
{
  return (xx_IRllx (r2) == xx_IRllx (r1) &&
	  xx_IRlly (r2) == xx_IRlly (r1) &&
	  xx_IRurx (r2) == xx_IRurx (r1) &&
	  xx_IRury (r2) == xx_IRury (r1));
}


#define MAX(A,B)	((A) > (B) ? (A) : (B))
#define MIN(A,B)	((A) < (B) ? (A) : (B))
#define ROUND(F) 	(int)((F) + 0.5)
void
xx_IRbound (r1, r2)
     xx_IntRectangle r1;
     xx_IntRectangle r2;
{
  int x, y;
  int X, Y;

  x = MIN (xx_IRllx (r1), xx_IRllx (r2));
  y = MIN (xx_IRlly (r1), xx_IRlly (r2));
  X = MAX (xx_IRurx (r1), xx_IRurx (r2));
  Y = MAX (xx_IRury (r1), xx_IRury (r2));

  r1->x = x;
  r1->y = y;
  r1->w = X - x + 1;
  r1->h = Y - y + 1;
}


int
xx_IRarea (r)
     xx_IntRectangle r;
{
  return r->w * r->h;
}


int
xx_IRhits_point (rect, x, y)
     xx_IntRectangle rect;
     int x;
     int y;
{
  return (x >= rect->x
	  && y >= rect->y
	  && x < rect->x + rect->w
	  && y < rect->y + rect->h);
}

void
xx_IRclip (r1, r2)
     xx_IntRectangle r1;
     xx_IntRectangle r2;
{
  int x = MAX (r1->x, r2->x);
  int y = MAX (r1->y, r2->y);
  int X = MIN (xx_IRurx (r1), xx_IRurx (r2));
  int Y = MIN (xx_IRury (r1), xx_IRury (r2));
  int t;
  r1->x = x;
  r1->y = y;
  t = X - x + 1;
  if (t < 0)
    r1->w = 0;
  else
    r1->w = t;
  t = Y - y + 1;
  if (t < 0)
    r1->h = 0;
  else
    r1->h = t;
}




int
xx_IRsubtract (outv, a, b)
     xx_IntRectangle outv;
     xx_IntRectangle a;
     xx_IntRectangle b;
{
  struct xx_sIntRectangle arect;
  struct xx_sIntRectangle brect;
  int outp = 0;

  if (!(a->w && a->h))
    return 0;
  if (!(b->w && b->h))
    {
      *outv = *a;
      return 1;
    }

  arect = *a;
  brect = *b;

  xx_IRclip (&brect, &arect);

  if (xx_IRhits_point (&arect,
		       xx_IRllx (&brect),
		       xx_IRlly (&brect)))
    {
      /*

	 -----------------.
	|        |//////brect
	|        |////////|
        |        |---------
	|   I    |  II    |
	| 	 |        |
         -----------------  arect
	*/

      /* I  */
      outv[outp] = arect;
      outv[outp].w = xx_IRllx (&brect) - xx_IRllx (&arect);

      if (outv[outp].w)
	++outp;

      /* II */
      outv[outp] = arect;
      outv[outp].x = xx_IRllx (&brect);
      outv[outp].w = xx_IRurx (&arect) - xx_IRllx (&brect) + 1;
      outv[outp].h = xx_IRlly (&brect) - xx_IRlly (&arect);

      if (outv[outp].h)
	++outp;

      arect.w = xx_IRurx (&arect) - xx_IRllx (&brect) + 1;
      arect.h = xx_IRury (&arect) - xx_IRlly (&brect) + 1;
      arect.x = xx_IRllx (&brect);
      arect.y = xx_IRlly (&brect);
    }

  if (xx_IRhits_point (&arect,
		       xx_IRurx (&brect),
		       xx_IRury (&brect)))
    {
      /*
	          --------------
                 |           I  | arect
                 |--------------|
                 |////////|     |
                 |////////|  II |
                  --------------
             brect


	*/

      /* I */
      outv[outp] = arect;
      outv[outp].h = xx_IRury (&arect) - xx_IRury (&brect);
      outv[outp].y = xx_IRury (&brect) + 1;
      if (outv[outp].h)
	++outp;

      /* II */
      outv[outp] = arect;
      outv[outp].h = xx_IRury (&brect) - xx_IRlly (&arect) + 1;
      outv[outp].w = xx_IRurx (&arect) - xx_IRurx (&brect);
      outv[outp].x = xx_IRurx (&brect) + 1;

      if (outv[outp].w)
	++outp;
    }
  return outp;
}
