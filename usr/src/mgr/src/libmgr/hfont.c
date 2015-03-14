/*{{{}}}*/
/*{{{  #includes*/
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "term.h"
#include "hfont.h"
/*}}}  */
/*{{{  #defines*/
/* hfont uses fixpoint arithmetic */
#define PRECISION(x) ((x)<<7)
#define SINGLE(x) ((x)>>7)

#define PENUP 0x7fff
/*}}}  */

/*{{{  isin, icos*/
/*	sine and cosine routines
 *	input:	degrees (integer)
 *	output:	sine/cosine << 10
 */

/*	sin table 0-90 degrees <<10 */

static int sintab[] = {
	0, 18, 36, 54, 71, 89, 107, 125, 143, 160,
	178, 195, 213, 230, 248, 265, 282, 299, 316, 333,
	350, 367, 384, 400, 416, 433, 449, 465, 481, 496,
	512, 527, 543, 558, 573, 587, 602, 616, 630, 644,
	658, 672, 685, 698, 711, 724, 737, 749, 761, 773,
	784, 796, 807, 818, 828, 839, 849, 859, 868, 878,
	887, 896, 904, 912, 920, 928, 935, 943, 949, 956,
	962, 968, 974, 979, 984, 989, 994, 998, 1002, 1005,
	1008, 1011, 1014, 1016, 1018, 1020, 1022, 1023, 1023,
	1024, 1024,
	} ;

static int isin(n) register int n;		/* angle in degrees */
   {
   if (n < 0)
      return(-isin(-n));

   while (n >= 360)
      n -= 360;

   if (n < 90)
      return( sintab[n]);
   else if (n < 180)
      return( sintab[180-n]);
   else if (n < 270)
      return( -sintab[n-180]);
   else
      return( -sintab[360-n]);
   }

static int
icos(n)
register int n;
   {
   if (n < 0)
      n=-n;

   while (n >= 360)
      n -= 360;

   if (n < 90)
      return( sintab[90-n]);
   else if (n < 180)
      return( -sintab[n-90]);
   else if (n < 270)
      return( -sintab[270-n]);
   else
      return( sintab[n-270]);
   }
/*}}}  */

/*{{{  hfont_open -- read hershey font from file*/
hfont_raw *hfont_open(char *name)
{
  /*{{{  variables*/
  FILE *fp;
  int c,c2;
  int i,pairs,leftmargin;
  int num=0, *ofs;
  hfont_raw *f=malloc(sizeof(hfont_raw));
  struct hfont_vector *font;
  char fullname[_POSIX_PATH_MAX];
  /*}}}  */

  if (f==(hfont_raw*)0) { errno=ENOMEM; return (hfont_raw*)0; }
  if (*name!='/')
  {
    strcpy(fullname,HFONTDIR);
    strcat(fullname,"/");
    strcat(fullname,name);
    fp=fopen(fullname,"r");
  }
  else fp=fopen(name,"r");
  if (fp==(FILE*)0) return((hfont_raw*)0);
  font=&(f->vectors[0]);
  ofs=f->offset;
  for (i=0; i<HFONT_MAXCHARS; i++)
  {
    /*{{{  note offset of character in vector table*/
    *ofs++=num;
    /*}}}  */
    /*{{{  ignore character name*/
    if (fgetc(fp)==EOF) break; fgetc(fp); fgetc(fp); fgetc(fp); fgetc(fp);
    /*}}}  */
    /*{{{  read number of pairs*/
    c=fgetc(fp); pairs=(c==' ' ? 0 : c-'0');
    c=fgetc(fp); if (c!=' ') pairs=10*pairs+c-'0';
    c=fgetc(fp); if (c!=' ') pairs=10*pairs+c-'0'-1;
    font->y=pairs;
    /*}}}  */
    /*{{{  read character width and left margin*/
    c=fgetc(fp); c2=fgetc(fp);
    font->x=c2 - c;
    leftmargin='R' - c;
    font++;
    num++;
    /*}}}  */
    /*{{{  read character vector pairs*/
    while (pairs-->0)
    {
      c=fgetc(fp);
      if (c==' ') font->x=PENUP; else font->x=c-'R'+leftmargin;
      c=fgetc(fp);
      font->y=9-(c-'R');
      font++;
      num++;
    }
    /*}}}  */
    /*{{{  read newline*/
    if (fgetc(fp)!='\n') { fclose(fp); return 0; }
    /*}}}  */
  }
  fclose(fp);
  f->maxchars=i;
  f->scale=36;
  return f;
}
/*}}}  */
/*{{{  hfont_scale -- size hershey font to requested point size*/
hfont_scaled *hfont_scale(hfont_raw *f, int xres_u, int yres_u, int point_p)
{
  /*{{{  variables*/
  int c1=xres_u*point_p,c2=yres_u*point_p,b=72*f->scale;
  int count,i;
  struct hfont_vector *h_font,*u_font;
  hfont_scaled *g=malloc(sizeof(hfont_scaled));
  /*}}}  */

  if (g==(hfont_scaled*)0) { errno=ENOMEM; return g; }
  memcpy(g->offset,f->offset,sizeof(g->offset));
  g->x_overlay=(int)((10*point_p * xres_u) / (72L*f->scale)+8)/10;
  if (g->x_overlay==1) g->x_overlay=0;
  g->y_overlay=(int)((10*point_p * yres_u) / (72L*f->scale)+8)/10;
  if (g->y_overlay==1) g->y_overlay=0;
  h_font=f->vectors;
  u_font=g->vectors;
  g->height=((10*c2*f->scale)/b+5)/10;
  for (i=0; i<f->maxchars; i++)
  {
    u_font->x=(PRECISION(c1)*h_font->x)/b;
    u_font->y=h_font->y;
    count=h_font->y;
    h_font++; u_font++;
    while (count--)
    {
      if (h_font->x != PENUP)
      {
        u_font->x=(((c1<<4)*h_font->x)/b+8)>>4;
        u_font->y=(((c2<<4)*h_font->y)/b+8)>>4;
      }
      else u_font->x=PENUP;
      h_font++; u_font++;
    }
  }
  return g;
}
/*}}}  */
/*{{{  hfont_print -- print a string as hershey font characters*/
void hfont_printto(buf, f, x, y, angle, s) int buf; hfont_scaled *f; int *x, *y, angle; unsigned char *s;
{
  int cx, cy, num, l, xc, yc,i,j;
  struct hfont_vector *c;
  int sinx, cosx, siny, cosy;
  int width;

  siny=isin(angle);
  cosy=icos(angle);
  while (l=*s++)
  {
    c=&f->vectors[f->offset[l-' ']];
    width=c->x; num=c->y-1; c++;
    sinx=isin(angle);
    cosx=icos(angle);
    xc=(c->x * cosx - c->y * siny)>>10;
    yc=(c->x * sinx + c->y * cosy)>>10;
    cx=SINGLE(*x)+xc; cy=SINGLE(*y)-yc; c++;
    while (num-->0)
    {
      if (c->x==PENUP)
      {
        c++;
        num--;
        xc=(c->x * cosx - c->y * siny)>>10;
        yc=(c->x * sinx + c->y * cosy)>>10;
      }
      else
      {
        xc=(c->x * cosx - c->y * siny)>>10;
        yc=(c->x * sinx + c->y * cosy)>>10;
        for (i=0; i<=f->x_overlay; i++) for (j=0; j<=f->y_overlay; j++) m_lineto(buf,cx+i,cy+j,i+SINGLE(*x)+xc,SINGLE(*y)-yc+j);
      }
      cx=SINGLE(*x)+xc; cy=SINGLE(*y)-yc; c++;
    }
    *x=*x+((width*cosx)>>10);
    *y=*y-((sinx*width)>>10);
  }
}
/*}}}  */
