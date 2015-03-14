/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* graphics subs  - mostly from the BLIT */
/*}}}  */

/*{{{  #includes*/
#include "bitblit.h"
/*}}}  */
/*{{{  #defines*/
#define labs(x,y)		if((x=y)<0) x= -x
#define	BIG			0x7fff
#define	HUGE			0x3fffffffL
#define sq(x) ((long)(x)*(x))
#define	sgn(x)	((x)<0? -1 : (x)==0? 0 : 1)
/*}}}  */

/*{{{  resid -- calculate b*b*x*x + a*a*y*y - a*a*b*b avoiding ovfl*/
static long resid(a,b,x,y) register int a,b,x,y;
   {
   long result = 0;
   long u = b*((long)a*a - (long)x*x);
   long v = (long)a*y*y;
   register q = u>BIG? HUGE/u: BIG;
   register r = v>BIG? HUGE/v: BIG;
   while(a || b) {
      if(result>=0 && b) {
         if(q>b) q = b;
         result -= q*u;
         b -= q;
         }
      else {
         if(r>a) r = a;
         result += r*v;
         a -= r;
         }
      }
   return(result);
   }
/*}}}  */
/*{{{  ellip1*/
/*
 * clockwise ellipse centered at x0,y0 with half-axes a,b.
 * from x1,y1 to x2,y2
 */

void ellip1(screen, x0,y0, a, b, x1,y1, x2,y2, f)
BITMAP *screen;
int x0,y0, a,b,x1,y1, x2, y2, f;
   {
   int z;
   int dx = y1>0? 1: y1<0? -1: x1>0? -1: 1;
   int dy = x1>0? -1: x1<0? 1: y1>0? -1: 1;
   long a2 = (long)a*a;
   long b2 = (long)b*b;
   register long dex = b2*(2*dx*x1+1);
   register long e;
   register long dey = a2*(2*dy*y1+1);
   register long ex, ey, exy;

   e = resid(a, b, x1, y1);
   a2 *= 2;
   b2 *= 2;
   do {
      labs(ex, e+dex);
      labs(ey, e+dey);
      labs(exy, e+dex+dey);
      if(exy<=ex || ey<ex) {
         y1 += dy;
         e += dey;
         dey += a2;
         }
      if(exy<=ey || ex<ey) {
         x1 += dx;
         e += dex;
         dex += b2;
         }
      bit_point(screen, x0+x1, y0+y1, f);
      if(x1 == 0) {
         dy = -dy;
         dey = -dey + a2;
         }
       else if(y1 == 0) {
         for(z=x1; abs(z+=dx)<=a; )
            bit_point(screen, x0+z, y0+y1, f);
         dx = -dx;
         dex = -dex + b2;
         }
      } while(x1!=x2 || y1!=y2);
   }
/*}}}  */
/*{{{  Labs*/
static long
Labs(x)
long x;
   {
   return(x<0? -x : x);
   }
/*}}}  */
/*{{{  nearby*/
/*   Note1: the equality end test is justified
 *   because it is impossible that
 *   abs(x^2+y^2-r^2)==abs((x+\010+-1)^2+y^2-r^2) or
 *   abs(x^2+y^2-r^2)==abs(x^2+(y+\010+-1)-r^2),
 *   and no values of x or y are skipped.
 *
 */

void nearby(x1,y1, x2,y2, rx, ry)
register int x1, y1, x2, y2;
int *rx, *ry;
   {
   long eps, exy;   /*integers but many bits*/
   int d, dy;
   register dx;
   eps = sq(x2) + sq(y2) - sq(x1) - sq(y1);
   d = eps>0? -1: 1;
   for( ; ; eps=exy, x2+=dx, y2+=dy) {
      if(abs(y2) > abs(x2)) {
         dy = d*sgn(y2);
         dx = 0;
         }
      else {
         dy = 0;
         dx = d*sgn(x2);
         if(dx==0)
            dx = 1;
         }
      exy = eps + (2*x2+dx)*dx + (2*y2+dy)*dy;
      if(Labs(eps) <= Labs(exy))
         break;
      }
   *rx = x2;
   *ry = y2;
   }
/*}}}  */

/*{{{  circle  -- circle of radius r centered at x1,y1*/
void circle(b,x1,y1,r,f)
BITMAP  *b;
int x1;
register int y1;
int r, f;
{
   register err = 0;		/* x^2 + y^2 - r^2 */
   register dxsq = 1;		/* (x+dx)^2-x^2*/
   register dysq = 1 - 2*r;
   register exy;
   int x0 = x1;
   register y0 = y1 - r;

   y1 += r;
   while(y1 > y0) {
      bit_point(b,x0,y0,f);
      bit_point(b,x0,y1,f);
      bit_point(b,x1,y0,f);
      bit_point(b,x1,y1,f);
      exy = err + dxsq + dysq;
      if(-exy <= err+dxsq) {
         y1--;
         y0++;
         err += dysq;
         dysq += 2;
         }
      if(exy <= -err) {
         x1++;
         x0--;
         err += dxsq;
         dxsq += 2;
         }
      }
   bit_point(b,x0,y0,f);
   bit_point(b,x1,y0,f);
   }
/*}}}  */
/*{{{  ellipse -- draw an ellipse centered at x0,y0 with half-axes a,b*/
void ellipse(screen, x, y, a, b, f)
BITMAP *screen;
int x, y;
int a, b;
int f;
   {
   if(a==0 || b==0)
      bit_line(screen, x-a, y-b, x+a, y+b, f);
   else
      ellip1(screen, x, y, a, b, 0, b, 0, b, f);
   }
/*}}}  */
/*{{{  arc*/
/*	Draw an approximate arc centered at x0,y0 of an
 *	integer grid and running counter-clockwise from
 *	x1,y1 to the vicinity of x2,y2.
 *	If the endpoints coincide, draw a complete circle.
 *
 *	The "arc" is a sequence of vertically, horizontally,
 *	or diagonally adjacent points that minimize 
 *	abs(x^2+y^2-r^2).
 *
 *	The circle is guaranteed to be symmetric about
 *	the horizontal, vertical, and diagonal axes
 */

void arc(bp, x0,y0, x2,y2, x1,y1, f)
register BITMAP *bp;
int x0,y0,x2,y2,x1,y1,f;
   {
   register dx, dy;
   register eps;   /* x^2 + y^2 - r^2 */
   int dxsq, dysq;   /* (x+dx)^2-x^2, ...*/
   int ex, ey, exy;

   x1=x1-x0;
   y1=y1-y0;
   x2=x2-x0;
   y2=y2-y0;

   nearby(x1,y1,x2,y2, &x2, &y2);

   dx = -sgn(y1);
   dy = sgn(x1);
   dxsq = (2*x1 + dx)*dx;
   dysq = (2*y1 + dy)*dy;
   eps = 0;
   do {
      if(x1 == 0) {
         dy = -sgn(y1);
         dysq = (2*y1 + dy)*dy;
         }
      else if(y1 == 0) {
         dx = -sgn(x1);
         dxsq = (2*x1 + dx)*dx;
         }
      ex = abs(eps + dxsq);
      ey = abs(eps + dysq);
      exy = abs(eps + dxsq + dysq);
      if(ex<ey || exy<=ey) {
         x1 += dx;
         eps += dxsq;
         dxsq += 2;
         }
      if(ey<ex || exy<=ex) {
         y1 += dy;
         eps += dysq;
         dysq += 2;
         }
      bit_point(bp,x0+x1, y0+y1,f);
      /* bit_blit(bp,x0+x1-1,y0+y1-1,2,2,f,NULL_DATA,0,0); */
      }
   while(!(x1==x2 && y1==y2));   /* Note1 */
   }
/*}}}  */
