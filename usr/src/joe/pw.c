/* Prompt windows
   Copyright (C) 1992 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the 
terms of the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.  

JOE is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
details.  

You should have received a copy of the GNU General Public License along with 
JOE; see the file COPYING.  If not, write to the Free Software Foundation, 
675 Mass Ave, Cambridge, MA 02139, USA.  */ 

#include "config.h"
#include "heap.h"
#include "w.h"
#include "vfile.h"
#include "toomany.h"
#include "termcap.h"
#include "b.h"
#include "edfuncs.h"
#include "kbd.h"
#include "scrn.h"
#include "bw.h"
#include "zstr.h"
#include "help.h"
#include "tab.h"
#include "undo.h"
#include "pw.h"

CONTEXT cprmpt={"prompt",0};
CONTEXT cfprmpt={"fprompt",0};

/* Move prompt window */

static void movepw(w,x,y)
W *w;
int x,y;
{
}

/* Resize prompt window */

static void resizepw(w,wi,he)
W *w;
int wi,he;
{
}

/* Abort prompt window */

static void killpw(w)
W *w;
{
BW *bw=(BW *)w->object;
PW *pw=(PW *)bw->object;
bwrm(bw);
free(pw->prompt);
free(pw);
}

/* Update a prompt window */

static void followpw(w)
W *w;
{
BW *bw=(BW *)w->object;
bwfllw(bw);
}

static void disppw(w)
W *w;
{
int x;
BW *bw=(BW *)w->object;
PW *pw=(PW *)bw->object;

/* Scroll buffer and position prompt */
if(pw->promptlen>w->w/2+w->w/4)
 {
 pw->promptofst=pw->promptlen-w->w/2;
 if(bw->cursor->col<w->w-(pw->promptlen-pw->promptofst))
  bw->offset=0;
 else
  bw->offset=bw->cursor->col-(w->w-(pw->promptlen-pw->promptofst)-1);
 }
else
 { 
 if(bw->cursor->col<w->w-pw->promptlen) pw->promptofst=0, bw->offset=0;
 else if(bw->cursor->col>=w->w)
  pw->promptofst=pw->promptlen, bw->offset=bw->cursor->col-(w->w-1);
 else
  pw->promptofst=pw->promptlen-(w->w-bw->cursor->col-1),
  bw->offset=bw->cursor->col-(w->w-(pw->promptlen-pw->promptofst)-1);
 }

/* Set cursor position */
w->curx=bw->cursor->col-bw->offset+pw->promptlen-pw->promptofst;
w->cury=0;

/* Generate prompt */
w->t->t->updtab[w->y]=1;
genfmt(w->t->t,w->x,w->y,pw->promptofst,pw->prompt,0);

/* Position and size buffer */
bwmove(bw,w->x+pw->promptlen-pw->promptofst,w->y);
bwresz(bw,w->w-(pw->promptlen-pw->promptofst),1);

/* Generate buffer */
bwgen(bw);
}

/* When user hits return in a prompt window */

void upromptrtn(w)
W *w;
{
BW *bw=(BW *)w->object;
PW *pw=(PW *)bw->object;
char *s;
W *win;
void (*pfunc)();
peol(bw->cursor);
s=brvs(bw->top,bw->cursor->byte-bw->top->byte);
if(pw->hist)
 if(bw->b->chnged)
  {
  P *q=pdup(pw->hist->eof);
  binsm(q,s,bw->cursor->byte-bw->top->byte);
  peof(q);
  binsc(q,'\n');
  prm(q);
  }
 else
  {
  P *q=pdup(pw->hist->bof);
  P *r;
  P *t;
  pline(q,bw->top->line);
  r=pdup(q);
  pnextl(r);
  t=pdup(pw->hist->eof);
  binsb(t,q,r);
  bdel(q,r);
  prm(q); prm(r); prm(t);
  }
win=w->win;

pfunc=pw->pfunc;
wabort(w);
pfunc(win,s);
}

/* When user aborts a prompt window ^C */

void uabortpw(w)
W *w;
{
wabort(w);
}

static void inspw(w,b,l,n,flg)
W *w;
B *b;
long l,n;
int flg;
{
BW *bw=(BW *)w->object;
if(b==bw->b) bwins(bw,l,n,flg);
}

static void delpw(w,b,l,n,flg)
W *w;
B *b;
long l,n;
int flg;
{
BW *bw=(BW *)w->object;
if(b==bw->b) bwdel(bw,l,n,flg);
}

static WATOM watompw=
{
&cprmpt,
disppw,
followpw,
killpw,
resizepw,
movepw,
inspw,
delpw,
TYPEPW
};

static WATOM watomfpw=
{
&cfprmpt,
disppw,
followpw,
killpw,
resizepw,
movepw,
inspw,
delpw,
TYPEPW
};

/* Create a prompt window */

W *wmkpw(w,prompt,history,func,huh)
W *w;
char *prompt;
B **history;
void (*func)();
char *huh;
{
W *new;
PW *pw;
BW *bw;
new=wcreate(w->t,&watompw,w,w,w->main,1,huh);
if(!new) return 0;
new->object=(void *)(bw=bwmk(new->t,bmk(ctab),new->x,new->y,new->w,1));
bw->object=(void *)(pw=(PW *)malloc(sizeof(PW)));
pw->prompt=zdup(prompt);
pw->promptlen=fmtlen(prompt);
pw->promptofst=0;
pw->pfunc=func;
if(history)
 {
 if(!*history) *history=bmk(ctab);
 pw->hist= *history;
 binsb(bw->cursor,pw->hist->bof,pw->hist->eof);
 bw->b->chnged=0;
 peof(bw->cursor); peof(bw->top); pbol(bw->top);
 }
else pw->hist=0;
w->t->curwin=new;
return new;
}

W *wmkfpw(w,prompt,history,func,huh)
W *w;
char *prompt;
B **history;
void (*func)();
char *huh;
{
W *new;
PW *pw;
BW *bw;
new=wcreate(w->t,&watomfpw,w,w,w->main,1,huh);
if(!new) return 0;
new->object=(void *)(bw=bwmk(new->t,bmk(ctab),new->x,new->y,new->w,1));
bw->object=(void *)(pw=(PW *)malloc(sizeof(PW)));
pw->prompt=zdup(prompt);
pw->promptlen=fmtlen(prompt);
pw->promptofst=0;
pw->pfunc=func;
if(history)
 {
 if(!*history) *history=bmk(ctab);
 pw->hist= *history;
 binsb(bw->cursor,pw->hist->bof,pw->hist->eof);
 bw->b->chnged=0;
 peof(bw->cursor); peof(bw->top); pbol(bw->top);
 }
else pw->hist=0;
w->t->curwin=new;
return new;
}
