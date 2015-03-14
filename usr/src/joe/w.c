/* Window system
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
#include "vfile.h"
#include "toomany.h"
#include "b.h"
#include "termcap.h"
#include "scrn.h"
#include "tty.h"
#include "queue.h"
#include "kbd.h"
#include "main.h"
#include "poshist.h"
#include "w.h"

/* Redraw a window */

void wredraw(w)
W *w;
{
msetI(w->t->t->updtab+w->y,1,w->h);
}

/* Scroll an update array */

void scrldn(ary,top,bot,amnt)
int *ary,top,bot,amnt;
{
if(!amnt || top>=bot || amnt>bot-top) return;
if(bot>25)
 {
 signal(0,5);
 }
/* mbkwd(ary+top+amnt,ary+top,(bot-top-amnt)*sizeof(int)); */
msetI(ary+top,1,amnt);
}

void scrlup(ary,top,bot,amnt)
int *ary,top,bot,amnt;
{
if(!amnt || top>=bot || amnt>bot-top) return;
if(bot>25)
 {
 signal(0,5);
 }
/* mfwrd(ary+top,ary+top+amnt,(bot-top-amnt)*sizeof(int)); */
msetI(ary+bot-amnt,1,amnt);
}

/* Find first window in a group */

W *findtopw(w)
W *w;
{
W *x;
for(x=w;x->link.prev->main==w->main && x->link.prev!=w;x=x->link.prev);
return x;
}

/* Determine height of a family of windows */

int getgrouph(w)
W *w;
{
W *x;
int h;

/* Find first window in family */
x=findtopw(w);

/* Add heights of all windows in family */
for(w=x, h=(w->reqh?w->reqh:w->h);
    w->link.next!=x && w->link.next->main==x->main;
    w=w->link.next, h+=(w->reqh?w->reqh:w->h));

return h;
}

/* Find last window in a group */

W *findbotw(w)
W *w;
{
W *x;
for(x=w;x->link.next->main==w->main && x->link.next!=w;x=x->link.next);
return x;
}

W *lastw(t)
SCREEN *t;
{
W *x;
for(x=t->topwin;x->link.next!=t->topwin && x->link.next->y>=0;x=x->link.next);
return x;
}

/* Create a screen object */

SCREEN *scr;

SCREEN *screate(scrn)
SCRN *scrn;
{
SCREEN *t=(SCREEN *)malloc(sizeof(SCREEN));
t->pattern=0;
t->replace=0;
t->t=scrn;
t->w=scrn->co;
t->h=scrn->li;
t->topwin=0;
t->curwin=0;
t->wind=0;
t->markb=0;
t->markk=0;
t->arg=0;
scr=t;
return t;
}

void sresize(t)
SCREEN *t;
{
SCRN *scrn=t->t;
W *w;
int base=0;
int n=0;
t->w=scrn->co;
t->h=scrn->li;
if(t->wind>t->h-4) t->wind=t->h-4;
if(t->wind<0) t->wind=0;
w=t->topwin; do
 {
 if(w->y>=0)
  if(w->win) base+=w->h;
  else ++n, base+=2;
 w->w=t->w-1;
 }
 while(w=w->link.next, w!=t->topwin);
if(!base)
 {
 wfit(t);
 return;
 }
if(base>=t->h-t->wind) n=0;
else n=(t->h-t->wind-base)/n;
w=t->topwin; do
 if(w->y>=0)
  {
  w->y= -1;
  if(!w->win) w->reqh=2+n;
  }
 while(w=w->link.next, w!=t->topwin);
wfit(t);
}

void updall()
{
int y;
for(y=0;y!=scr->h;++y) scr->t->updtab[y]=1;
}

void scrins(b,l,n,flg)
B *b;
long l,n;
int flg;
{
W *w;
if(w=scr->topwin) do
 if(w->y>=0) w->watom->ins(w,b,l,n,flg);
 while(w=w->link.next, w!=scr->topwin);
}

void scrdel(b,l,n,flg)
B *b;
long l,n;
int flg;
{
W *w;
if(w=scr->topwin) do
 if(w->y>=0) w->watom->del(w,b,l,n,flg);
 while(w=w->link.next, w!=scr->topwin);
}

/* Fit as many windows on the screen as is possible beginning with the window
 * at topwin.  Give any extra space which couldn't be used to fit in another
 * window to the last text window on the screen.  This function guarentees
 * to fit on the window with the cursor in it (moves topwin to next group
 * of windows until window with cursor fits on screen).
 */

static int doabort();

void wfit(t)
SCREEN *t;
{
int y;		/* Where next window goes */
int left;	/* Lines left on screen */
W *w;		/* Current window we're fitting */
W *pw;		/* Main window of previous family */
int req;	/* Amount this family needs */
int adj;	/* Amount family needs to be adjusted */
int flg=0;	/* Set if cursor window was placed on screen */

tryagain:
y=t->wind; left=t->h-y; pw=0;

w=t->topwin; do
 w->ny= -1, w->nh=(w->reqh?w->reqh:w->h);
 while((w=w->link.next)!=t->topwin);

/* Fit a group of windows on the screen */
w=t->topwin; do
 {
 req=getgrouph(w);
 if(req>left)		/* If group is taller than lines left */
  if(pw) break;		/* Give space to previous family */
  else adj=req-left;	/* This family gets shorter */
 else adj=0;
 
 /* Fit a family of windows on the screen */
 do
  {
  w->ny=y;			/* Set window's y position */
  if(!w->win) pw=w, w->nh-=adj;	/* Adjust main window of the group */
  if(!w->win && w->nh<2) while(w->nh<2) w->nh+=doabort(w->link.next);
  if(w==t->curwin) flg=1;	/* Set if we got window with cursor */
  y+=w->nh; left-=w->nh;	/* Increment y value by height of window */
  w=w->link.next;		/* Next window */
  } while(w!=t->topwin && w->main==w->link.prev->main);
 } while(w!=t->topwin);

/* We can't extra space to fit a new family on, so give space to parent of
 * previous family */
pw->nh+=left;

/* Adjust that family's children which are below the parent */
while((pw=pw->link.next)!=w) pw->ny+=left;

/* Make sure the cursor window got on the screen */
if(!flg)
 {
 t->topwin=findbotw(t->topwin)->link.next;
 goto tryagain;
 }

/* All of the windows are now on the screen.  Scroll the screen to reflect what
 * happened
 */
w=t->topwin; do
 if(w->y>=0 && w->ny>=0)
  if(w->ny>w->y)
   {
   W *l=pw=w;
   while(pw->link.next!=t->topwin &&
         (pw->link.next->y<0 || pw->link.next->ny<0 ||
         pw->link.next->ny>pw->link.next->y))
    {
    pw=pw->link.next;
    if(pw->ny>=0 && pw->y>=0) l=pw;
    }
   /* Scroll windows between l and w */
   loop1:
   if(l->ny>=0 && l->y>=0)
    {
    nscrldn(t->t,l->y,l->ny+Umin(l->h,l->nh),l->ny-l->y);
    scrldn(t->t->updtab,l->y,l->ny+Umin(l->h,l->nh),l->ny-l->y);
    }
   if(w!=l)
    {
    l=l->link.prev;
    goto loop1;
    }
   w=pw->link.next;
   }
  else if(w->ny<w->y)
   {
   W *l=pw=w;
   while(pw->link.next!=t->topwin &&
         (pw->link.next->y<0 || 
         pw->link.next->ny<0 || 
         pw->link.next->ny<pw->link.next->y))
    {
    pw=pw->link.next;
    if(pw->ny>=0 && pw->y>=0) l=pw;
    }
   /* Scroll windows between l and w */
   loop0:
   if(w->ny>=0 && w->y>=0)
    {
    nscrlup(t->t,w->ny,w->y+Umin(w->h,w->nh),w->y-w->ny);
    scrlup(t->t->updtab,w->ny,w->y+Umin(w->h,w->nh),w->y-w->ny);
    }
   if(w!=l)
    {
    w=w->link.next;
    goto loop0;
    }
   w=pw->link.next;
   }
  else w=w->link.next;
 else w=w->link.next;
 while(w!=t->topwin);

/* Update current height and position values */
w=t->topwin; do
 {
 if(w->ny>=0)
  {
  if(w->object) w->watom->move(w,w->x,w->ny),
                w->watom->resize(w,w->w,w->nh);
  if(w->y== -1) msetI(t->t->updtab+w->ny,1,w->nh);
  w->y=w->ny;
  }
 else w->y= -1;
 w->h=w->nh;
 w->reqh=0;
 }
 while(w=w->link.next, w!=t->topwin);
}

/* Goto next window */

int wnext(t)
SCREEN *t;
{
if(t->curwin->link.next!=t->curwin)
 {
 t->curwin=t->curwin->link.next;
 if(t->curwin->y== -1) wfit(t);
 return 0;
 }
else return -1;
}

/* Goto previous window */

int wprev(t)
SCREEN *t;
{
if(t->curwin->link.prev!=t->curwin)
 {
 t->curwin=t->curwin->link.prev;
 if(t->curwin->y== -1)
  {
  int req;
  int adj;
  req=getgrouph(t->curwin);
  if(req>t->h-t->wind) adj=req-(t->h-t->wind);
  else adj=0;
  req-=adj;
  do
   {
   t->topwin=t->topwin->link.prev;
   if(!t->topwin->win) t->topwin->h-=adj;
   }
   while(t->topwin->link.prev->main==t->topwin->main);
  wfit(t);
  }
 return 0;
 }
else return -1;
}

/* Grow window */

int wgrow(w)
W *w;
{
W *nextw, *z;
/* Is there enough space to grow window? */
for(nextw=w->link.next;
    nextw->win && nextw!=w->t->topwin;
    nextw=nextw->link.next);
if(nextw==w->t->topwin) return -1;
if(nextw->y== -1 || nextw->h<=2) return -1;

w->reqh=w->h+1;		/* Increase this window's height */
nextw->reqh=nextw->h-1;	/* Decrease this window's height and move it down */

wfit(w->t);

return 0;
}

/* Shrink window */

int wshrink(w)
W *w;
{
W *nextw, *z;
/* Is this window too small already? */
if(w->h<=2) return -1;

/* Is there a window we can grow with this window's space? */
for(nextw=w->link.next;
    nextw!=w->t->topwin && nextw->win;
    nextw=nextw->link.next);
if(nextw==w->t->topwin) return -1;

w->reqh=w->h-1;			/* Decrease window size */
nextw->reqh=nextw->h+1;		/* Give space to this window */

wfit(w->t);
return 0;
}

/* Show all windows */

void wshowall(t)
SCREEN *t;
{
int base=0;
int n=0;
W *w=t->topwin; do
 if(w->win) base+=w->h;
 else ++n, base+=2;
 while(w=w->link.next, w!=t->topwin);
if(base>=t->h-t->wind) n=0;
else n=(t->h-t->wind-base)/n;
w=t->topwin; do
 if(!w->win) w->reqh=2+n;
 while(w=w->link.next, w!=t->topwin);
wfit(t);
}

void wspread(t)
SCREEN *t;
{
int base=0;
int n=0;
W *w=t->topwin; do
 if(w->y>=0)
  if(w->win) base+=(w->reqh?w->reqh:w->h);
  else ++n, base+=2;
 while(w=w->link.next, w!=t->topwin);
if(!base)
 {
 wfit(t);
 return;
 }
if(base>=t->h-t->wind) n=0;
else n=(t->h-t->wind-base)/n;
w=t->topwin; do
 if(w->y>=0) if(!w->win) w->reqh=2+n;
 while(w=w->link.next, w!=t->topwin);
wfit(t);
}

/* Show just one family of windows */

void wshowone(w)
W *w;
{
W *q=w->t->topwin; do
 if(!q->win) q->reqh=w->t->h-(getgrouph(q)-q->h);
 while(q=q->link.next, q!=w->t->topwin);
wfit(w->t);

/*
int req=getgrouph(w);
int adj=w->t->h-req;
w->t->curwin=w;
w->t->topwin=findtopw(w);
w->main->reqh=w->main->h+adj;
wfit(w->t);
*/
}

/* Create a window */

W *wcreate(t,watom,where,target,original,height,huh)
SCREEN *t;
WATOM *watom;
W *where, *target, *original;
int height;
char *huh;
{
W *new;

if(height<1) return 0;

/* Create the window */
new=(W *)malloc(sizeof(W));
new->t=t;
new->w=t->w-1;
new->hh=new->h=height;
new->y= -1;
new->ny=0; new->nh=0; new->reqh=0;
new->x=0;
new->huh=huh;
new->orgwin=original;
new->watom=watom;
new->object=0;
new->msgb=0;
new->msgt=0;

/* Set window's target and family */
if(new->win=target) new->main=target->main;
else new->main=new;

/* Get space for window */
if(original)
 if(original->h-height<=2)
  {
  /* Not enough space for window */
  free(new);
  return 0;
  }
 else original->h-=height;

/* Create new keyboard handler for window */
if(watom->context) new->kbd=mkkbd(watom->context,new);
else new->kbd=0;

/* Put window on the screen */
if(where) enquef(W,link,where,new);
else
 {
 if(t->topwin) enqueb(W,link,t->topwin,new);
 else izque(W,link,new), t->curwin=t->topwin=new;
 }

wfit(t);
return new;
}

/* Abort group of windows */

static int doabort(w)
W *w;
{
int amnt=w->h;
W *z, *zn;
w->y= -2;
if(w->t->topwin==w) w->t->topwin=w->link.next;
loop:
z=w->t->topwin; do
 {
 if(z->orgwin==w) z->orgwin=0;
 if((z->win==w || z->main==w) && z->y!= -2)
  {
  amnt+=doabort(z);
  goto loop;
  }
 }
 while(z=z->link.next, z!=w->t->topwin);
if(w->orgwin)
 {
 if(w->orgwin->reqh) w->orgwin->reqh+=(w->reqh?w->reqh:w->h);
 else w->orgwin->reqh=w->orgwin->h+(w->reqh?w->reqh:w->h);
 w->orgwin->hh+=w->hh;
 }
if(w->t->curwin==w)
 if(w->t->curwin->win) w->t->curwin=w->t->curwin->win;
 else
  if(w->orgwin) w->t->curwin=w->orgwin;
  else w->t->curwin=w->link.next;
if(qempty(W,link,w))
 {
 leave=1;
 amnt=0;
 }
deque(W,link,w);
w->watom->kill(w);
rmkbd(w->kbd);
free(w);
windie(w);
return amnt;
}

/* Abort a window and its children */

void wabort(w)
W *w;
{
SCREEN *t=w->t;
if(w->orgwin && w!=w->main)
 {
/* w->orgwin->reqh=(w->orgwin->reqh?w->orgwin->reqh:w->orgwin->h)+w->h; */
 doabort(w);
 if(!leave) wfit(t);
 }
else
 {
 doabort(w);
 if(!leave) wspread(t);
 }
}

/* Generate text with formatting escape sequences */

void genfmt(t,x,y,ofst,s,flg)
SCRN *t;
char *s;
{
int *scrn=t->scrn+y*t->co+x;
int atr=0;
int col=0;
while(*s)
 {
 int c;
 if(*s=='\\')
  {
  ++s;
  if(!*s) break;
  if(*s=='\\') if(col++>=ofst) c= (unsigned char)*s++ + atr; else { ++s; continue; }
  else if(*s=='u' || *s=='U') { atr^=UNDERLINE; ++s; continue; }
  else if(*s=='i' || *s=='I') { atr^=INVERSE; ++s; continue; }
  else { ++s; continue; }
  }
 else if(col++>=ofst) c= (unsigned char)*s++ + atr; else { ++s; continue; }
 if(c!=*scrn) *scrn=c, outatr(t,col-ofst+x-1,y,c);
 ++scrn;
 }
if(flg) eraeol(t,col-ofst+x,y);
}

/* Determine column width of string with format codes */

int fmtlen(s)
char *s;
{
int col=0;
while(*s)
 {
 if(*s=='\\')
  {
  ++s;
  if(!*s) break;
  if(*s=='\\') { ++col; ++s; continue; }
  if(*s=='u' || *s=='U') { ++s; continue; }
  if(*s=='i' || *s=='I') { ++s; continue; }
  ++s; continue;
  }
 ++col; ++s;
 }
return col;
}

/* Display a message and skip the next key */

int msgout(t,y,s)
SCRN *t;
char *s;
{
int ofst;
int len;
len=fmtlen(s);
if(len<=(t->co-1)) ofst=0;
else ofst=len-(t->co-1);
genfmt(t,0,y,ofst,s,1);
return len-ofst;
}

void msg(w,s)
W *w;
char *s;
{
cpos(w->t->t,msgout(w->t->t,w->y+w->h-1,s),w->y+w->h-1);
w->t->t->updtab[w->y+w->h-1]=1;
engetc(w->t->t);
}

/* Set temporary message */

void msgnw(w,s)
W *w;
char *s;
{
w->msgb=s;
}

void msgnwt(w,s)
W *w;
char *s;
{
w->msgt=s;
}

/* Single key query */

int query(w,s)
W *w;
char *s;
{
int ofst;
int c;
int len;
len=fmtlen(s);
if(len<=w->w-2) ofst=0;
else ofst=len-(w->w-2);
genfmt(w->t->t,w->x,w->y+w->h-1,ofst,s,1);
cpos(w->t->t,w->x+len-ofst,w->y+w->h-1);
c=engetc(w->t->t);
w->t->t->updtab[w->y+w->h-1]=1;
return c;
}

/* Single key query - leave cursor in curwin */

static void dumb()
{
}

static WATOM dummy=
{
0,dumb,dumb,dumb,dumb,dumb,dumb,dumb
};

int queryn(w,s)
W *w;
char *s;
{
int ofst;
int len;
int c;
W *new=wcreate(w->t,&dummy,w,w,w,1,NULL);
if(!new) return MAXINT;
len=fmtlen(s);
if(len<=w->w-1) ofst=0;
else ofst=len-(w->w-1);
genfmt(w->t->t,w->x,new->y,ofst,s,1);
c=edgetc();
wabort(new);
return c;
}
