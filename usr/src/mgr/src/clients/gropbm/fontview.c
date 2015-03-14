/*{{{}}}*/
/*{{{  Notes*/
/*

fontview - display a Hershey font using mroff device independent output
format.  This program reads a font and displays 4 characters per DIN A4
page in vectors.  Typical usage: "fontview hindi.d | oki180 | lpr".
fontview isn't part of the official mroff sources yet.

*/
/*}}}  */
/*{{{  #includes*/
#include <limits.h>
#include <string.h>
#include <stdio.h>

#include "term.h"
/*}}}  */
/*{{{  #defines*/
#define X_START 'A'
#define X_END 'a'
#define Y_START '@'
#define Y_END 'a'

#define PAGE_BITMAP 0
/*}}}  */

/*{{{  virtual device functions*/
/*{{{  open*/
void vd_open(void)
{
  ckmgrterm("fontview");
  m_setup(M_MODEOK);
  m_push(P_FLAGS);
  m_setmode(M_ABS);
}
/*}}}  */
/*{{{  close*/
void vd_close(void)
{
  m_pop();
}
/*}}}  */
/*{{{  erase*/
void vd_erase(void)
{
  m_clear();
}
/*}}}  */
/*{{{  update*/
void vd_update(void)
{
  m_flush();
}
/*}}}  */
/*{{{  draw a line*/
void do_line(int to, int x1, int y1, int x2, int y2)
{
  m_lineto(to,x1,y1,x2,y2);
}
/*}}}  */
/*{{{  select a font*/
void vd_font(char *s)
{
}
/*}}}  */
/*{{{  move*/
void vd_move(int x, int y)
{
}
/*}}}  */
/*{{{  draw text*/
void vd_text(char *s)
{
}
/*}}}  */
/*{{{  draw char*/
void vd_char(char c)
{
}
/*}}}  */
/*}}}  */

/*{{{  type definitions*/
typedef enum { FALSE, TRUE } Bool;

struct pair
{
  int x,y;
};

#define PENUP           0x7fff
/*}}}  */
/*{{{  variable declarations*/
struct pair vectors[200];
char left, right;
char label[6];
/*}}}  */

/*{{{  read a letter*/
Bool readletter(FILE *fp)
{
  static char ln[156],*s;
  struct pair *font;
  int i;

  font=vectors;
  if (fgets(ln,sizeof(ln),fp)==NULL) return FALSE;
  /*{{{  store label*/
  strncpy(label,ln,5);
  /*}}}  */
  /*{{{  store number of following pairs*/
  font->y= (ln[5]==' ' ? 0 : ln[5]-'0');
  font->y=10*font->y+(ln[6]==' ' ? 0 : ln[6]-'0');
  font->y=10*font->y+ln[7]-'0'-2;
  /*}}}  */
  /*{{{  store margins*/
  left=ln[8]-'R';
  right=ln[9]-'R';
  /*}}}  */
  s=ln+10;
  for (i=1; i<=font->y+1; i++)
  /*{{{  store pair*/
  {
    if (*s=='\n') return FALSE;
    if (*s==' ') font[i].x=PENUP;
    else font[i].x= *s-'R';
    s++;
    font[i].y='R'-*s++;
  }
  /*}}}  */
  return TRUE;
}
/*}}}  */
/*{{{  draw a letter*/
int drawletter(int to, int x, int y, int m)
{
  /*{{{  variable declarations*/
  static int cx, cy, num;
  struct pair *c;
  /*}}}  */

  /*{{{  draw character*/
  c=vectors; num=c->y; c++;
  cx=x+(c->x)*m; cy=y-(c->y)*m; c++;
  while (num-->0)
  {
    if (c->x == PENUP) { c++; num--; }
    else do_line(to,cx,cy,x+(c->x)*m,y-(c->y)*m);
    cx=x+(c->x)*m; cy=y-(c->y)*m; c++;
  }
  /*}}}  */
  /*{{{  draw margin markers*/
  /*{{{  upper left marker*/
  do_line(to,x+left*m,y+('F'-'R')*m,x+left*m+5,y+('F'-'R')*m);
  do_line(to,x+left*m,y+('F'-'R')*m,x+left*m,y+('F'-'R')*m+5);
  /*}}}  */
  /*{{{  upper right marker*/
  do_line(to,x+right*m-5,y+('F'-'R')*m,x+right*m,y+('F'-'R')*m);
  do_line(to,x+right*m,y+('F'-'R')*m,x+right*m,y+('F'-'R')*m+5);
  /*}}}  */
  /*{{{  bottom left marker*/
  do_line(to,x+left*m,y+('['-'R')*m,x+left*m+5,y+('['-'R')*m);
  do_line(to,x+left*m,y+('['-'R')*m,x+left*m,y+('['-'R')*m-5);
  /*}}}  */
  /*{{{  bottom right marker*/
  do_line(to,x+right*m-5,y+('['-'R')*m,x+right*m,y+('['-'R')*m);
  do_line(to,x+right*m,y+('['-'R')*m,x+right*m,y+('['-'R')*m-5);
  /*}}}  */
  /*}}}  */
  /*{{{  draw crosses and letters on y axis*/
  for (cy=Y_START; cy<=Y_END; cy++)
  {
    int cym=y+(cy-'R')*m;

    /*{{{  draw letters on y axis*/
    vd_move(x+(X_START-'R')*m-12,cym+3);
    vd_char(cy);
    /*}}}  */
    /*{{{  draw crosses*/
    for (cx=X_START; cx<=X_END; cx++)
    {
      int cxm=x+(cx-'R')*m;

      do_line(to,cxm-1,cym,cxm+1,cym);
      do_line(to,cxm,cym-1,cxm,cym+1);
    }
    /*}}}  */
  }
  /*}}}  */
  /*{{{  draw letters on x axis*/
  for (cx=X_START; cx<=X_END; cx++)
  {
    int cxm=x+(cx-'R')*m;

    vd_move(cxm-2,y+(Y_END-'R')*m+12);
    vd_char(cx);
  }
  /*}}}  */
  /*{{{  draw label*/
  vd_move(x+(X_START-'R')*m-12,y+(Y_START-'R')*m-12);
  vd_text("Label: ");
  vd_text(label);
  /*}}}  */
}
/*}}}  */

/*{{{  main*/
int main(int argc, char *argv[])
{
  /*{{{  variable declarations*/
  FILE *fd;
  Bool finished=FALSE;
  char font[_POSIX_PATH_MAX];
  /*}}}  */

  /*{{{  check number of arguments*/
  if (argc!=2)
  {
    fprintf(stderr,"Usage: fontview font\n");
    exit(1);
  }
  /*}}}  */
  /*{{{  try to open font file*/
  if ((fd=fopen(argv[1],"r"))==NULL)
  {
    fprintf(stderr,"fontview: Can't open font %s\n",argv[1]);
    exit(2);
  }
  /*}}}  */
  vd_open();
  strcpy(font,"roman.s");
  vd_font(font);
  do
  {
    vd_erase();
    if (readletter(fd))
    {
      drawletter(PAGE_BITMAP,150,180,8);
      if (readletter(fd))
      {
        drawletter(PAGE_BITMAP,440,180,8);
        if (readletter(fd))
        {
          drawletter(PAGE_BITMAP,150,520,8);
          if (readletter(fd)) drawletter(PAGE_BITMAP,440,520,8); else finished=TRUE;
        }
        else finished=TRUE;
      }
      else finished=TRUE;
    }
    else finished=TRUE;
    vd_update();
  } while (!finished);
  vd_close();
  exit(0);
}
/*}}}  */
