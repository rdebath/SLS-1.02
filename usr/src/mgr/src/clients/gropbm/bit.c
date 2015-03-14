/*{{{}}}*/
/*{{{  #includes*/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef MGR
#include "bitmap.h"
#endif
/*}}}  */

/*{{{  variables*/
static int bitwidth, bitheight, bytewidth;
unsigned char *bitmap;
/*}}}  */

/*{{{  SETDOT*/
static int bitmask[8]= { 128,64,32,16,8,4,2,1 };

#define SETDOT(x,y) ((x)>=0 && (x)<=bitwidth && (y)>=0 && (y)<= bitheight ? *(bitmap+(y)*bytewidth+((x)>>3))|=bitmask[(x)&7] : 0)
/*}}}  */
/*{{{  bitmalloc*/
int bitmalloc(int width, int height)
{
  bytewidth=width&7 ? (width+1)>>3 : width>>3;
  bitwidth=width;
  bitheight=height;
  return ((bitmap=malloc(bytewidth*bitheight))!=(void*)0);
}
/*}}}  */
/*{{{  bitfree*/
void bitfree(void)
{
  free(bitmap);
}
/*}}}  */
/*{{{  bitclear*/
void bitclear(void)
{
  memset(bitmap,0,bytewidth*bitheight);
}
/*}}}  */
/*{{{  bitline*/
void bitline(int x1, int y1, int x2, int y2)
{
  int x,y,z,dx,dy,dz,i1,i2;

  if (x1 == x2)
  /*{{{  draw a vertical line*/
  {
    if (y1 > y2) for (i1=y1; i1>=y2; i1--) SETDOT(x1,i1);
    else for (i1=y1; i1<=y2; i1++) SETDOT(x1,i1);
  }
  /*}}}  */
  else if (y1 == y2)
  /*{{{  draw a horizontal line*/
  {
    if (x1 > x2) for (i1=x1; i1>=x2; i1--) SETDOT(i1,y1);
    else for (i1=x1; i1<=x2; i1++) SETDOT(i1,y1);
  }
  /*}}}  */
  else
  /*{{{  draw a normal line*/
  {
    dx=x2-x1; if (dx<0) dx = -dx;
    dy=y2-y1; if (dy<0) dy = -dy;
    if (x1 <= x2)
    /*{{{  from (x1,y1) to (x2,y2)*/
    {
      x=x1; y=y1;
      if (y1>y2) z = -1; else z=1;
    }
    /*}}}  */
    else
    /*{{{  from (x2,y2) to (x1,y1)*/
    {
      x=x2; y=y2;
      if (y2 > y1) z = -1; else z=1;
    }
    /*}}}  */
    if (dx>dy) i2=dx; else i2=dy;
    SETDOT(x,y);
    dz=i2 >> 1;
    for (i1=1; i1<=i2; i1++)
    {
      if (dz<dx) { dz+=dy; x++; }
      if (dz>=dx) { dz-=dx; y+=z; }
      SETDOT(x,y);
    }
  }
  /*}}}  */
}
/*}}}  */
/*{{{  bitcircle*/
void bitcircle(int x0, int y0, int rx, int ry)
{
}
/*}}}  */
/*{{{  bitpbmwrite*/
void bitpbmwrite(FILE *fp)
{
  fprintf(fp,"P4\n");
  fprintf(fp,"%d %d\n",bitwidth,bitheight);
  fwrite(bitmap,bytewidth*bitheight,1,fp);
}
/*}}}  */
#ifdef MGR
/*{{{  bitmgrwrite*/
void bitmgrwrite(FILE *fp)
{
  struct b_header header;

  B_PUTHDR8(&header,bitwidth,bitheight,1);
  fwrite(&header,sizeof(header),1,fp);
  fwrite(bitmap,bytewidth*bitheight,1,fp);
}
/*}}}  */
#endif

/*{{{  #defines*/
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
  while(a || b) 
  {
    if(result>=0 && b) 
    {
      if(q>b) q = b;
      result -= q*u;
      b -= q;
    }
    else 
    {
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

static void ellip1(x0,y0, a, b, x1,y1, x2,y2)
int x0,y0, a,b,x1,y1, x2, y2;
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
  do 
  {
    ex=labs(e+dex);
    ey=labs(e+dey);
    exy=labs(e+dex+dey);
    if (exy<=ex || ey<ex)
    {
      y1 += dy;
      e += dey;
      dey += a2;
    }
    if (exy<=ey || ex<ey)
    {
      x1 += dx;
      e += dex;
      dex += b2;
    }
    SETDOT(x0+x1, y0+y1);
    if (x1 == 0)
    {
      dy = -dy;
      dey = -dey + a2;
    }
    else if (y1 == 0)
    {
      for(z=x1; abs(z+=dx)<=a; ) SETDOT(x0+z, y0+y1);
      dx = -dx;
      dex = -dex + b2;
    }
  } while(x1!=x2 || y1!=y2);
}
/*}}}  */
void bitellipse(int x0, int y0, int a, int b)
{
  ellip1(x0, y0, a, b, 0, b, 0, b);
}
