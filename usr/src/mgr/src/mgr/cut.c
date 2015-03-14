/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* cut and paste text */
/*}}}  */
/*{{{  #includes*/
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "event.h"

#include "Write.h"
#include "do_button.h"
#include "do_event.h"
#include "font_subs.h"
#include "get_text.h"
#include "icon_server.h"
/*}}}  */
/*{{{  #defines*/
#define MAXROWS		64		/* greatest char height */
#define MAXCOLS		32		/* widest char (bits in u-long) */
/*}}}  */

/*{{{  variables*/
static BITMAP *glyph;				/* spot for glyph comparison */
static unsigned long data[MAXROWS];		/* bit data for glyph */
static BITMAP *check;				/* other spot for glyph comparison */
static unsigned long data2[MAXROWS];		/* bit data for other glyph */

static struct entry **table;		/* hash table */
/*}}}  */

/*{{{  get_hash -- given bitmap, get hash code*/
int get_hash(map,x,y,w,h,how)
BITMAP *map;
int x,y,w,h;		/* where in map */
int how;		/* 0-> normal, 1->inverted */
{
  register unsigned long sum = 0;
  register int j;
  bit_blit(glyph,0,0,32,h,BIT_CLR,NULL_DATA,0,0);
  bit_blit(glyph,32-w,0,w,h,how ? BIT_NOT(BIT_SRC) : BIT_SRC,map,x,y);
  for (j=0;j<h;j++)
  sum += data[j]*(j+1);
  return(sum%H_SIZE);
}
/*}}}  */

/*{{{  enter -- enter a glyph into the hash table*/
static void enter(item,value,type) int item; unsigned char value; int type;
{
  register struct entry *entry;

  if (table[item] == (struct entry *) 0)
  {
    table[item] = malloc(sizeof (struct entry));
    table[item] -> value = value;
    table[item] -> type = type;
    table[item] -> next = (struct entry *) 0;
  }
  else 
  {
    for(entry = table[item];entry->next;entry = entry -> next);
    entry -> next  = malloc(sizeof (struct entry));
    entry -> next -> value = value;
    entry -> next -> type = type;
    entry -> next -> next = (struct entry *) 0;
  }
}
/*}}}  */
/*{{{  get_match -- find a character match in current font*/
static unsigned char get_match(map,x,y,w,h)
BITMAP *map;				/* bitmap containing text */
int x,y,w,h;				/* position of glyph in "map" */
{
  register struct entry *entry;
  register WINDOW *win = active;
  int code;				/* hash code */
  int size = sizeof(long) * h;

  code = get_hash(map,x,y,w,h,0);		/* leaves char in glyph */
  for(entry=table[code];entry;entry=entry->next) 
  {
    bit_blit(check,32-w,0,w,h,BIT_SRC,W(font)->glyph[entry->value+entry->type*MAXGLYPHS],0,0);
    if (bcmp(data,data2,size)==0)
    {
      return(entry->value);
    }
  }

  /* try inverse video version */

  code = get_hash(map,x,y,w,h,1);		/* leaves char in glyph */
  for(entry=table[code];entry;entry=entry->next) 
  {
    bit_blit(check,32-w,0,w,h,BIT_SRC,W(font)->glyph[entry->value],0,0);
    if (bcmp(data,data2,size)==0) 
    {
      return(entry->value);
    }
  }
  return('\0');
}
/*}}}  */
/*{{{  fixline -- change trailing white space into \n*/
static unsigned char *fixline(s,pnt) unsigned char *s; register unsigned char *pnt;
{
  while (*--pnt == ' ' && pnt > s);
  *++pnt = '\n';
  return(++pnt);
}
/*}}}  */
/*{{{  to_tabs -- change spaces to tabs*/
static unsigned char *to_tabs(pos,in,out)
register int pos;				/* starting col # */
register unsigned char *in;				/* input str */
register unsigned char *out;				/* output str - tabs */
{
  unsigned char *s = out;				/* start of out str */
  register unsigned char c;				/* current input char */
  register int spaces = 0;			/* # pending spaces */
   
#ifdef DEBUG
  dprintf(C)(stderr,"-> TABS");
#endif
  while(pos++,c = *in++) 
  {

    if (c=='\n' || c=='\r' || c=='\f')	/* reset column counter */
    pos =0;

    if (c == ' ') 
    {
      spaces++;
      if (pos && pos%8 == 0) {		/* ' ' -> \t */
        c = '\t';
#ifdef DEBUG
        dprintf(C)(stderr,".");
#endif
        spaces=0;
      }
    }
    else for(;spaces>0;spaces--) {		/* output spaces */
      *out++ = ' ';
    }
         
    if (spaces == 0)
    *out++ = c;
  }
  *out = '\0';
#ifdef DEBUG
  dprintf(C)(stderr,"\n");
#endif
  return(s);
}
/*}}}  */
/*{{{  oops -- can't cut, ring bell*/
static void oops()
{
  register WINDOW *win = active;

  CLEAR(W(window),BIT_NOT(BIT_DST));
  write(2,"\007",1);
  CLEAR(W(window),BIT_NOT(BIT_DST));
}
/*}}}  */

/*{{{  paste -- stuff global buffer into input stream*/
int paste()
{
  if (snarf) 
  {
    do_event(EVENT_PASTE,active,E_MAIN);
    Write(ACTIVE(to_fd),snarf,strlen(snarf));
#ifdef DEBUG
    dprintf(y)(stderr,"%s: Pasting [%s]\n",ACTIVE(tty), snarf?snarf:"EMPTY");
#endif
  }
#ifdef DEBUG
  else 
  {
    dprintf(y)(stderr,"%s: Nothing to paste\n",ACTIVE(tty));
  }
#endif
  return(0);
}
/*}}}  */
/*{{{  cut -- cut text from a window, put in global buffer*/
int cut()
{
  register int i,j;
  register WINDOW *win = active;		/* window to cut text from */
  int count;					/* # of snarfed chars */
  int errors = 0;				/* number of misses */
  int cols=1, rows=0;				/* rows and cols swept */
  int col,row;					/* starting col and row */
  int maxcol;					/* # of cols in row */
  int x,y;					/* bit position in bitmap */
  int hcode;					/* hash code */
  int button;					/* button from move_mouse */
  unsigned char c;				/* matched char */
  unsigned char *pntr;				/* current char in line */
  unsigned char *line;				/* buffer to receive text */
  BITMAP *src = W(window);	/* source bitmap, usually W(window) */
  BITMAP *tmp = NULL;

  /* return immediately if window is not snarffable */

  if ((W(flags) & W_SNARFABLE) ==0) return(0);

  /* initialize comparison registers */

  glyph = bit_alloc(32,FSIZE(high),data,1);
  check = bit_alloc(32,FSIZE(high),data2,1);

  bit_blit(check,0,0,32,FSIZE(high),BIT_CLR,NULL_DATA,0,0);

  /* build hash table */

  if ((table = W(font)->table) == NULL) 
  {
#ifdef DEBUG
    dprintf(C)(stderr,"building cut table\n");
#endif
    table = W(font)->table = malloc (sizeof (struct entry) * H_SIZE);
    bzero(table, sizeof(struct entry) * H_SIZE);
   
    count =  W(font)->head.type & 0x80 ? 4 : 1;
    for(j=0;j<count;j++) for(i = FSIZE(start);i<FSIZE(start)+FSIZE(count);i++)
    {
      if (W(font)->glyph[i+MAXGLYPHS*j] && i >= ' ')
      {
        hcode=get_hash(W(font)->glyph[i+MAXGLYPHS*j],0,0,FSIZE(wide),FSIZE(high),0);
        enter(hcode,i,j);
      }
    }
  }

  /* find cut region */

  SETMOUSEICON(&mouse_cut);
  button = move_mouse(screen,mouse,&mousex,&mousey,0);
  SETMOUSEICON(&mouse_arrow);
  i = get_text(screen,mouse,mousex,mousey,&cols,&rows,win,E_SWTEXTT);
  if (i == 0) 
  {
    do_button(0);
    return(0);
  }

  /* find extent of cut region */

  col = (mousex-(W(x0)+SUM_BDR+W(text.x)))/FSIZE(wide);
  maxcol = (W(text.wide) ? W(text.wide) : BIT_WIDE(W(window)))/FSIZE(wide);
  row = (mousey-(W(y0)+SUM_BDR+W(text.y)))/FSIZE(high);

  if (W(flags)&W_SNARFLINES) {		/* snarf lines only */
#ifdef DEBUG
    dprintf(C)(stderr,"Cutting lines only\n");
#endif
    col = 0;
    cols = maxcol;
  }

#ifdef DEBUG
  dprintf(C)(stderr,"Cut got %d,%d  %d x %d\n",col,row,cols,rows);
#endif

  /* prepare src bitmap */

#if 0
#if DEPTH > 1
  if (BIT_DEPTH(src) > 1) { 	/* uh-oh! */
    tmp = bit_shrink(src,GETBCOLOR(W(style)));
    src = bit_create(tmp,BIT_X(src),BIT_Y(src),BIT_WIDE(src),BIT_HIGH(src));
  }
#endif
#endif

  /* look up characters */

  pntr = line = malloc(1+(1+maxcol)*(rows+1));	/* max possible cut */
  switch(rows) 
  {
    case 0:			/* 1 row */
    y = W(text.y)+row*FSIZE(high);
    for(x=W(text.x)+col*FSIZE(wide),i=0;i<cols;i++,x+=FSIZE(wide)) 
    {
      c = get_match(src,x,y,FSIZE(wide),FSIZE(high));
      *pntr++ = c ? c : (errors++,C_NOCHAR);
    }
    if (col+cols == maxcol && c==' ')
    pntr = fixline(line,pntr);
    break;
    case 1:			/* 2 rows */
    y = W(text.y)+row*FSIZE(high);
    for(x=W(text.x)+col*FSIZE(wide),i=0;i<maxcol;i++,x+=FSIZE(wide)) 
    {
      c = get_match(src,x,y,FSIZE(wide),FSIZE(high));
      *pntr++ = c ? c : (errors++,C_NOCHAR);
    }
    pntr = fixline(line,pntr);

    y += FSIZE(high);
    for(x=W(text.x),i=0;i<col+cols;i++,x+=FSIZE(wide)) 
    {
      c = get_match(src,x,y,FSIZE(wide),FSIZE(high));
      *pntr++ = c ? c : (errors++,C_NOCHAR);
    }
    if (col+cols == maxcol && c==' ')
    pntr = fixline(line,pntr);
    break;

    default:			/* > 2 rows */
    y = W(text.y)+row*FSIZE(high);
    for(x=W(text.x)+col*FSIZE(wide),i=0;i<maxcol;i++,x+=FSIZE(wide)) 
    {
      c = get_match(src,x,y,FSIZE(wide),FSIZE(high));
      *pntr++ = c ? c : (errors++,C_NOCHAR);
    }
    pntr = fixline(line,pntr);

    for(j=0;j<rows-1;j++) 
    {
      y += FSIZE(high);
      for(x=W(text.x),i=0;i<maxcol;i++,x+=FSIZE(wide)) 
      {
        c = get_match(src,x,y,FSIZE(wide),FSIZE(high));
        *pntr++ = c ? c : (errors++,C_NOCHAR);
      }
      pntr = fixline(line,pntr);
    }

    y += FSIZE(high);
    for(x=W(text.x),i=0;i<col+cols;i++,x+=FSIZE(wide)) 
    {
      c = get_match(src,x,y,FSIZE(wide),FSIZE(high));
      *pntr++ = c ? c : (errors++,C_NOCHAR);
    }
    if (col+cols == maxcol && c==' ')
    pntr = fixline(line,pntr);

    break;
  }
  *pntr = '\0';

  /* dont use bit_free */
  free(check);
  free(glyph);

  /* put text into snarf buffer */

  count = pntr-line;
#ifdef DEBUG
  dprintf(C)(stderr,"snarfed %d chars, %d errors\n",count,errors);
  dprintf(C)(stderr,"snarfed [%s]\n",line);
#endif

  if ((!(W(flags)&W_SNARFHARD) && errors > 0) || 2*errors > count)
  {
    oops();
    count = 0;
  }
  else 
  {
    if (W(flags)&W_SNARFTABS)
    to_tabs(col,line,line);

    if (snarf && button < BUTTON_SYS) {			/* add to cut buffer */
      unsigned char *tmp = malloc(strlen(snarf) + strlen(line) +1);
      count += strlen(snarf);
      strcpy(tmp,snarf);
      strcat(tmp,line);
      free(snarf);
      free(line);
      snarf = tmp;
    }
    else if (snarf) {					/* replace cut buffer */
      free(snarf);
      snarf = line;
    }
    else						/* new cut buffer */
    snarf = line;

    /* send snarf events (if any) */
    id_message = W(pid);
    for(win=active;win != (WINDOW *) 0;win=W(next))
    do_event(EVENT_SNARFED,win,E_MAIN);
  }
  do_button(0);
  if (tmp) {	
    bit_destroy(src);
    bit_destroy(tmp);
  }
  return(count);
}
/*}}}  */
/*{{{  zap_fhash -- zap a font hash table*/
void zap_fhash(fnt) struct font *fnt;
{
  register struct entry *entry, *next;
  register int i;

  if (fnt->table) 
  {
    for(i=0;i<H_SIZE;i++)
    for(entry=table[i];entry;entry=next) 
    {
      next = entry->next;
      free(entry);
    }
    free(fnt->table);
  }
}
/*}}}  */
