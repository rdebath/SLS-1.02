/* Text editing windows
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
#include "toomany.h"
#include "termcap.h"
#include "vfile.h"
#include "b.h"
#include "edfuncs.h"
#include "tty.h"
#include "kbd.h"
#include "scrn.h"
#include "bw.h"
#include "zstr.h"
#include "vs.h"
#include "help.h"
#include "undo.h"
#include "main.h"
#include "macro.h"
#include "tw.h"

extern char *exmsg;

/* Update a text window */

static void followtw(w)
W *w;
{
BW *bw=(BW *)w->object;
bwfllw(bw);
}

int starow=0;
int stacol=0;

static void disptw(w)
W *w;
{
P *p;
char buf[40];
BW *bw=(BW *)w->object;
TW *tw=(TW *)bw->object;

w->cury=bw->cursor->line-bw->top->line+bw->y-w->y;
w->curx=bw->cursor->xcol-bw->offset+bw->x-w->x;

if(starow!=tw->starow || stacol!=tw->stacol ||
   starow || stacol) w->t->t->updtab[w->y]=1;
if(bw->b->name==tw->stanam) goto nosta0;
if(bw->b->name && tw->stanam && !zcmp(bw->b->name,tw->stanam)) goto nosta0;
if(tw->stanam) free(tw->stanam), tw->stanam=0;
if(bw->b->name) tw->stanam=zdup(bw->b->name);
w->t->t->updtab[w->y]=1;
nosta0:
if(bw->b->chnged!=tw->stamod) w->t->t->updtab[w->y]=1;
if(tw->stahlp!=!!w->t->wind) w->t->t->updtab[w->y]=1;
if(tw->starec!=recmac) w->t->t->updtab[w->y]=1;

if(!w->t->t->updtab[w->y]) goto nosta;

tw->stahlp=!!w->t->wind;
tw->stamod=bw->b->chnged;
tw->starec=recmac;
tw->stalin=vstrunc(tw->stalin,0);

tw->starow=starow;
if(starow)
 {
 tw->stalin=vsncpy(tw->stalin,0,sc("R="));
 sprintf(buf,"%4ld",bw->cursor->line+1);
 tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sz(buf));
 tw->stalin=vsadd(tw->stalin,' ');
 }
tw->stacol=stacol;
if(stacol)
 {
 tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sc("C="));
 sprintf(buf,"%3ld",bw->cursor->col+1);
 tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sz(buf));
 tw->stalin=vsadd(tw->stalin,' ');
 }
if(bw->b->name) tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sz(bw->b->name));
else tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sc("(Unnamed)"));
if(bw->b->chnged) tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sc(" (Modified)"));
if(recmac)
 {
 sprintf(buf," (Macro %d recording...)",recmac->n);
 tw->stalin=vsncpy(tw->stalin,sLEN(tw->stalin),sz(buf));
 }

tw->stalin=vstrunc(tw->stalin,w->w);
if(!w->t->wind && w->w>=40)
 tw->stalin=vsncpy(tw->stalin,w->w-17,sc("Ctrl-K H for help"));

/* Output status line */
 {
 int z;
 int *s=w->t->t->scrn+w->x+w->t->t->co*w->y;
 for(z=0;tw->stalin[z];++z)
  {
  if(have) goto nosta;
  if(s[z]!=(unsigned char)tw->stalin[z]+INVERSE)
   {
   int c=s[z]=(unsigned char)tw->stalin[z]+INVERSE;
   outatr(w->t->t,w->x+z,w->y,c);
   }
  }
 }
w->t->t->updtab[w->y]=0;
nosta:

bwgen(bw);
}

/* Abort text window */

static void killtw(w)
W *w;
{
BW *bw=(BW *)w->object;
TW *tw=(TW *)bw->object;
bwrm(bw);
vsrm(tw->stalin);
free(tw);
}

/* Move text window */

static void movetw(w,x,y)
W *w;
int x,y;
{
BW *bw=(BW *)w->object;
bwmove(bw,x,y+1);
}

/* Resize text window */

static void resizetw(w,wi,he)
W *w;
int wi,he;
{
BW *bw=(BW *)w->object;
bwresz(bw,wi,he-1);
}

/* Split current window */

void usplitw(w)
W *w;
{
BW *bw=(BW *)w->object;
TW *tw=(TW *)bw->object;
int newh=getgrouph(w);
W *new=wcreate(w->t,w->watom,findbotw(w),NULL,w,newh/2+(newh&1),NULL);
TW *newtw;
BW *newbw;
if(!new) return;
new->object=(void *)(newbw=bwmk(w->t,bw->b,new->x,new->y+1,new->w,new->h-1));
++bw->b->count;
newbw->lmargin=bw->lmargin;
newbw->rmargin=bw->rmargin;
newbw->autoindent=bw->autoindent;
newbw->wordwrap=bw->wordwrap;
newbw->overtype=bw->overtype;
newbw->offset=bw->offset;
newbw->object=(void *)(newtw=(TW *)malloc(sizeof(TW)));
newtw->staupd=tw->staupd;
newtw->stanam=0;
newtw->stalin=0;
newtw->stamod=0;
newtw->stahlp=0;
pset(newbw->top,bw->top);
pset(newbw->cursor,bw->cursor);
new->t->curwin=new;
}

/* User routine for aborting a text window */

void uaborttw(w)
W *w;
{
BW *bw=(BW *)w->object;
TW *tw=(TW *)bw->object;
if(bw->b->chnged && bw->b->count==1)
 {
 int c=query(w,"Loose changes to this file (y,n)? ");
 if(c!='y' && c!='Y') return;
 if(bw->b->name)
  {
  exmsg=vsncpy(NULL,0,sc("File "));
  exmsg=vsncpy(exmsg,sLEN(exmsg),sz(bw->b->name));
  exmsg=vsncpy(exmsg,sLEN(exmsg),sc(" not saved."));
  }
 else exmsg=vsncpy(NULL,0,sc("File (Unnamed) not saved."));
 }
else if(!exmsg)
 {
 if(bw->b->name)
  {
  exmsg=vsncpy(NULL,0,sc("File "));
  exmsg=vsncpy(exmsg,sLEN(exmsg),sz(bw->b->name));
  exmsg=vsncpy(exmsg,sLEN(exmsg),sc(" not changed so no update needed."));
  }
 else exmsg=vsncpy(NULL,0,sc("File (Unnamed) not changed so no update needed."));
 }
wabort(w);		/* Eliminate this window and it's children */
if(!leave) if(exmsg) vsrm(exmsg), exmsg=0;
}

void ucheckp(w)
W *w;
{
BW *bw=(BW *)w->object;
checkp(bw->b);
}

void ucheck(w)
W *w;
{
BW *bw=(BW *)w->object;
check(bw->b);
}

CONTEXT cmain={"main",0};

static void instw(w,b,l,n,flg)
W *w;
B *b;
long l,n;
int flg;
{
BW *bw=(BW *)w->object;
if(b==bw->b) bwins(bw,l,n,flg);
}

static void deltw(w,b,l,n,flg)
W *w;
B *b;
long l,n;
int flg;
{
BW *bw=(BW *)w->object;
if(b==bw->b) bwdel(bw,l,n,flg);
}

static WATOM watomtw=
{
&cmain,
disptw,
followtw,
killtw,
resizetw,
movetw,
instw,
deltw,
TYPETW
};

/* Create a text window.  It becomes the last window on the screen */

W *wmktw(t,b)
SCREEN *t;
B *b;
{
W *w;
BW *bw;
TW *tw;
w=wcreate(t,&watomtw,NULL,NULL,NULL,t->h,NULL);
w->object=(void *)(bw=bwmk(t,b,w->x,w->y+1,w->w,w->h-1));
bw->object=(void *)(tw=(TW *)malloc(sizeof(TW)));
tw->staupd=1; /* Unneeded? */
tw->stanam=0;
tw->starec=recmac;
tw->stalin=0;
tw->stamod=0;
tw->stahlp=0;
tw->starow=starow;
tw->stacol=stacol;
return w;
}
