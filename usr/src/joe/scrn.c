/* Device independant TTY interface for JOE
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

#include <stdio.h>
#include <signal.h>
#include "blocks.h"
#include "heap.h"
#include "vs.h"
#include "termcap.h"
#include "tty.h"
#include "zstr.h"
#include "scrn.h"

extern int mid;

/* Table of key sequences which we will translate to single codes */

SEQ seqs[NKEYS]=
{
 { "kd", KEYDOWN, "DOWN" },
 { "ku", KEYUP, "UP" },
 { "kl", KEYLEFT, "LEFT" },
 { "kr", KEYRIGHT, "RIGHT" },
 { "k0", KEYF0, "F0" },
 { "k1", KEYF1, "F1" },
 { "k2", KEYF2, "F2" },
 { "k3", KEYF3, "F3" },
 { "k4", KEYF4, "F4" },
 { "k5", KEYF5, "F5" },
 { "k6", KEYF6, "F6" },
 { "k7", KEYF7, "F7" },
 { "k8", KEYF8, "F8" },
 { "k9", KEYF9, "F9" },
 { "kD", KEYDEL, "DEL" },
 { "kI", KEYINS, "INS" },
 { "kh", KEYHOME, "HOME" },
 { "kH", KEYEND, "END" },
 { "kN", KEYPGDN, "PGDN" },
 { "kP", KEYPGUP, "PGUP" }
};

/* Set attributes */

void attr(t,c)
SCRN *t;
int c;
{
int e;
c&=~255;
e=(t->attrib&~c);
if(e&UNDERLINE)
 {
 if(t->ue) texec(t->cap,t->ue,1), e&=~UNDERLINE;
 t->attrib&=~UNDERLINE;
 }
if(e&INVERSE)
 {
 if(t->se) texec(t->cap,t->se,1), e&=~INVERSE;
 else if(t->me) texec(t->cap,t->me,1), e=0, t->attrib=0;
 t->attrib&=~INVERSE;
 }
if(e)
 {
 if(t->me) texec(t->cap,t->me,1);
 t->attrib=0;
 }
e=(c&~t->attrib);
if(e&INVERSE)
 if(t->mr) texec(t->cap,t->mr,1);
 else if(t->so) texec(t->cap,t->so,1);
if(e&UNDERLINE)
 if(t->us) texec(t->cap,t->us,1);
if(e&BLINK)
 if(t->mb) texec(t->cap,t->mb,1);
if(e&BOLD)
 if(t->md) texec(t->cap,t->md,1);
if(e&DIM)
 if(t->mh) texec(t->cap,t->mh,1);
t->attrib=c;
}

/* Erase from given screen coordinate to end of line */

int eraeol(t,x,y)
SCRN *t;
{
int *s, *ss;
int w=t->co-x-1;			/* Don't worry about last column */
if(w<=0) return 0;
s=t->scrn+y*t->co+x;
ss=s+w;
do if(*--ss!=' ') { ++ss; break; } while(ss!=s);
if(ss-s>3 && t->ce)
 {
 cpos(t,x,y);
 attr(t,0);
 texec(t->cap,t->ce,1);
 msetI(s,' ',w);
 }
else while(s!=ss) outatr(t,x,y,' '), ++x, *s++=' ';
return 0;
}

/* Output a character with attributes */


/*
void outatr(t,x,y,c)
SCRN *t;
{
unsigned char ch;
if(c== -1) c=' ';
ch=c; c-=ch;
if(t->x!=x || t->y!=y) cpos(t,x,y);
if(c!=t->attrib) attr(t,c);
if(t->hz && ch=='~') ch='\\';
ttputc(ch);
++t->x;
}
*/

void outatr1(t,x,y,c)
SCRN *t;
{
if(t->os && t->eo &&
   (t->scrn[x+t->co*y]!=' ' || (t->scrn[x+t->co*y]&~255)!=(c&~255)) ||
   t->ul && (c&255)=='_' && (!t->os || t->eo)
  )
 outatr(t,x,y,' ');
outatr(t,x,y,c);
if(c&UNDERLINE && !t->us)
 {
 cpos(t,x,y), texec(t->cap,t->uc,1);
 if(++t->x==t->co)
  if(t->am) t->x=0, ++t->y;
  else if(t->xn) t->x= -1, t->y= -1;
  else --t->x;
 }
}

/* Set scrolling region */

void setregn(t,top,bot)
SCRN *t;
int top,bot;
{
if(!t->cs)
 {
 t->top=top;
 t->bot=bot;
 return;
 }
if(t->top!=top || t->bot!=bot)
 {
 t->top=top;
 t->bot=bot;
 texec(t->cap,t->cs,1,top,bot-1);
 t->x= -1; t->y= -1;
 }
}

/* Enter insert mode */

void setins(t)
SCRN *t;
{
if(t->ins!=1 && t->im)
 {
 t->ins=1;
 texec(t->cap,t->im,1);
 }
}

/* Exit insert mode */

void clrins(t)
SCRN *t;
{
if(t->ins!=0)
 {
 texec(t->cap,t->ei,1);
 t->ins=0;
 }
}

void out(t,c)
char *t;
char c;
{
ttputc(c);
}

SCRN *nopen()
{
SCRN *t=(SCRN *)malloc(sizeof(SCRN));
int x,y;
char *p;
ttopen();

if(!(t->cap=getcap(NULL,baud,out,NULL)))
 {
 free(t);
 ttclose();
 fprintf(stdout,"Couldn't load termcap/terminfo entry\n");
 return 0;
 }

t->li=getnum(t->cap,"li"); if(t->li<1) t->li=24;
t->co=getnum(t->cap,"co"); if(t->co<2) t->co=80;
x=y=0;
ttgtsz(&x,&y);
if(x>3 && y>3) t->li=y, t->co=x;
x=y=0;
if(p=getenv("LINES")) sscanf(p,"%d",&y);
if(p=getenv("COLUMNS")) sscanf(p,"%d",&x);
if(x>3) t->co=x;
if(y>3) t->li=y;

t->hz=getflag(t->cap,"hz");
t->os=getflag(t->cap,"os");
t->eo=getflag(t->cap,"eo");
if(getflag(t->cap,"hc")) t->os=1;
if(t->os || getflag(t->cap,"ul")) t->ul=1;
else t->ul=0;

t->xn=getflag(t->cap,"xn");
t->am=getflag(t->cap,"am");

t->ti=getstr(t->cap,"ti");
t->cl=getstr(t->cap,"cl");
t->cd=getstr(t->cap,"cd");

t->te=getstr(t->cap,"te");

t->mb=0; t->md=0; t->mh=0; t->mr=0; t->avattr=0;
if(!(t->me=getstr(t->cap,"me"))) goto oops;
if((t->mb=getstr(t->cap,"mb"))) t->avattr|=BLINK;
if((t->md=getstr(t->cap,"md"))) t->avattr|=BOLD;
if((t->mh=getstr(t->cap,"mh"))) t->avattr|=DIM;
if((t->mr=getstr(t->cap,"mr"))) t->avattr|=INVERSE;
oops:

t->so=0; t->se=0;
if(getnum(t->cap,"sg")<=0 && !t->mr && getstr(t->cap,"se"))
 {
 if(t->so=getstr(t->cap,"so")) t->avattr|=INVERSE;
 t->se=getstr(t->cap,"se");
 }
if(getflag(t->cap,"xs") || getflag(t->cap,"xt")) t->so=0;

t->us=0; t->ue=0;
if(getnum(t->cap,"ug")<=0 && getstr(t->cap,"ue"))
 {
 if(t->us=getstr(t->cap,"us")) t->avattr|=UNDERLINE;
 t->ue=getstr(t->cap,"ue");
 }

if(!(t->uc=getstr(t->cap,"uc"))) if(t->ul) t->uc="_";
if(t->uc) t->avattr|=UNDERLINE;

t->ms=getflag(t->cap,"ms");

if(baud<38400)
 {
 t->da=getflag(t->cap,"da");
 t->db=getflag(t->cap,"db");
 t->cs=getstr(t->cap,"cs");
 t->rr=getflag(t->cap,"rr");
 t->sf=getstr(t->cap,"sf");
 t->sr=getstr(t->cap,"sr");
 t->SF=getstr(t->cap,"SF");
 t->SR=getstr(t->cap,"SR");
 t->al=getstr(t->cap,"al");
 t->dl=getstr(t->cap,"dl");
 t->AL=getstr(t->cap,"AL");
 t->DL=getstr(t->cap,"DL");
 if(!getflag(t->cap,"ns") && !t->sf) t->sf="\12";
 }
else
 {
 t->da=0; t->db=0;
 t->al=0; t->dl=0; t->AL=0; t->DL=0;
 t->cs=0; t->rr=0;
 t->sf=0; t->SF=0; t->sr=0; t->SR=0;
 }

if(!getflag(t->cap,"in") && baud<38400)
 {
 t->dc=getstr(t->cap,"dc");
 t->DC=getstr(t->cap,"DC");
 t->dm=getstr(t->cap,"dm");
 t->ed=getstr(t->cap,"ed");

 t->im=getstr(t->cap,"im");
 t->ei=getstr(t->cap,"ei");
 t->ic=getstr(t->cap,"ic");
 t->IC=getstr(t->cap,"IC");
 t->ip=getstr(t->cap,"ip");
 t->mi=getflag(t->cap,"mi");
 }
else
 {
 t->dm=0; t->dc=0; t->DC=0; t->ed=0;
 t->im=0; t->ic=0; t->IC=0; t->ip=0; t->ei=0;
 t->mi=1;
 }

t->bs=0;
if(getstr(t->cap,"bc")) t->bs=getstr(t->cap,"bc");
else if(getstr(t->cap,"le")) t->bs=getstr(t->cap,"le");
if(getflag(t->cap,"bs")) t->bs="\10";

t->cbs=tcost(t->cap,t->bs,1,2,2);

t->lf="\12";
if(getstr(t->cap,"do")) t->lf=getstr(t->cap,"do");
t->clf=tcost(t->cap,t->lf,1,2,2);

t->up=getstr(t->cap,"up");
t->cup=tcost(t->cap,t->up,1,2,2);

t->nd=getstr(t->cap,"nd");

t->tw=8;
if(getnum(t->cap,"it")>0) t->tw=getnum(t->cap,"it");
else if(getnum(t->cap,"tw")>0) t->tw=getnum(t->cap,"tw");

if(!(t->ta=getstr(t->cap,"ta"))) if(getflag(t->cap,"pt")) t->ta="\11";
t->bt=getstr(t->cap,"bt");
if(getflag(t->cap,"xt")) t->ta=0, t->bt=0;

t->cta=tcost(t->cap,t->ta,1,2,2);
t->cbt=tcost(t->cap,t->bt,1,2,2);

t->ho=getstr(t->cap,"ho");
t->cho=tcost(t->cap,t->ho,1,2,2);
t->ll=getstr(t->cap,"ll");
t->cll=tcost(t->cap,t->ll,1,2,2);

t->cr="\15";
if(getstr(t->cap,"cr")) t->cr=getstr(t->cap,"cr");
if(getflag(t->cap,"nc") || getflag(t->cap,"xr")) t->cr=0;
t->ccr=tcost(t->cap,t->cr,1,2,2);

t->cRI=tcost(t->cap,t->RI=getstr(t->cap,"RI"),1,2,2);
t->cLE=tcost(t->cap,t->LE=getstr(t->cap,"LE"),1,2,2);
t->cUP=tcost(t->cap,t->UP=getstr(t->cap,"UP"),1,2,2);
t->cDO=tcost(t->cap,t->DO=getstr(t->cap,"DO"),1,2,2);
t->cch=tcost(t->cap,t->ch=getstr(t->cap,"ch"),1,2,2);
t->ccv=tcost(t->cap,t->cv=getstr(t->cap,"cv"),1,2,2);
t->ccb=tcost(t->cap,t->cb=getstr(t->cap,"cb"),1,2,2);
t->ccm=tcost(t->cap,t->cm=getstr(t->cap,"cm"),1,2,2);

t->cce=tcost(t->cap,t->ce=getstr(t->cap,"ce"),1,2,2);

x=0;
for(y=0;y!=NKEYS;++y)
 if(getstr(t->cap,seqs[y].seq))
  {
  char *s=tcompile(t->cap,getstr(t->cap,seqs[y].seq));
  if(s)
   {
   t->ktab[x].s=s;
   t->ktab[x].l=sLen(s);
   t->ktab[x].n=seqs[y].code;
   ++x;
   }
  }
t->tabsize=x;
t->kbufp=0;
t->dumpptr= -1;

/* Make sure terminal can do absolute positioning */
if(t->cm) goto ok;
if(t->ch && t->cv) goto ok;
if(t->ho && (t->lf || t->DO || t->cv)) goto ok;
if(t->ll && (t->up || t->UP || t->cv)) goto ok;
if(t->cr && t->cv) goto ok;
leave=1;
ttclose();
signrm();
fprintf(stderr,"Sorry, your terminal can't do absolute cursor positioning\n");
fprintf(stderr,"It\'s broken\n");
return 0;
ok:

t->scrn=0; t->sary=0; t->updtab=0;
nresize(t,t->co,t->li);

if(t->ti) texec(t->cap,t->ti,1);

/* Check if terminal can't scroll */
if(t->al || t->AL || t->cs) goto sok1;
if(baud<38400) mid=1;
sok1:
if(t->dl || t->DL || t->cs) goto sok2;
if(baud<38400) mid=1;
sok2:

return t;
} 

/* Change size of screen */

void nresize(t,w,h)
SCRN *t;
{
if(h<1) h=24;
if(w<2) w=80;
t->li=h;
t->co=w;
if(t->sary) free(t->sary);
if(t->updtab) free(t->updtab);
if(t->scrn) free(t->scrn);
t->scrn=(int *)malloc(t->li*t->co*sizeof(int));
t->sary=(int *)calloc(t->li,sizeof(int));
t->updtab=(int *)malloc(t->li*sizeof(int));
nredraw(t);
}

/* Calculate cost of positioning the cursor using only relative cursor
 * positioning functions: t->(lf, DO, up, UP, bs, LE, RI, ta, bt) and rewriting
 * characters (to move right)
 *
 * This doesn't use the am and bw capabilities although it probably could.
 */

static int relcost(t,x,y,ox,oy)
register SCRN *t;
register int x,y,ox,oy;
{
int cost=0, c;

/* If we don't know the cursor position, force use of absolute positioning */
if(oy== -1 || ox== -1) return 10000;

/* First adjust row */
if(y>oy)
 /* Have to go down */
 if(t->lf)
  if(t->cDO<(c=(y-oy)*t->clf)) cost+=t->cDO;
  else cost+=c;
 else if(t->DO) cost+=t->cDO;
 else return 10000;
else if(y<oy)
 /* Have to go up */
 if(t->up)
  if(t->cUP<(c=(oy-y)*t->cup)) cost+=t->cUP;
  else cost+=c;
 else if(t->UP) cost+=t->cUP;
 else return 10000;

/* Now adjust column */

/* Use tabs */
if(x>ox && t->ta)
 {
 int ntabs=(x-ox+ox%t->tw)/t->tw;
 int cstunder=x%t->tw+t->cta*ntabs, cstover;
 if(x+t->tw<t->co && t->bs) cstover=t->cbs*(t->tw-x%t->tw)+t->cta*(ntabs+1);
 else cstover=10000;
 if(cstunder<t->cRI && cstunder<x-ox && cstover>cstunder)
  return cost+cstunder;
 else if(cstover<t->cRI && cstover<x-ox) return cost+cstover;
 }
else if(x<ox && t->bt)
 {
 int ntabs=(ox-x+t->tw-ox%t->tw)/t->tw;
 int cstunder,cstover;
 if(t->bs) cstunder=t->cbt*ntabs+t->cbs*(t->tw-x%t->tw); else cstunder=10000;
 if(x-t->tw>=0) cstover=t->cbt*(ntabs+1)+x%t->tw; else cstover=10000;
 if(cstunder<t->cLE && (t->bs?cstunder<(ox-x)*t->cbs:1) && cstover>cstunder)
  return cost+cstunder;
 else if(cstover<t->cRI && (t->bs?cstover<(ox-x)*t->cbs:1)) return cost+cstover;
 }

/* Use simple motions */
if(x<ox)
 /* Have to go left */
 if(t->bs) 
  if(t->cLE<(c=(ox-x)*t->cbs)) cost+=t->cLE;
  else cost+=c;
 else if(t->LE) cost+=t->cLE;
 else return 10000;
else if(x>ox)
 /* Have to go right */
 /* Hmm.. this should take into account possible attribute changes */
 if(t->cRI<x-ox) cost+=t->cRI;
 else cost+=x-ox;

return cost;
}

/* Find optimal set of cursor positioning commands to move from the current
 * cursor row and column (either or both of which might be unknown) to the
 * given new row and column and execute them.
 */

static void cposs(t,x,y)
register SCRN *t;
register int x,y;
{
register int bestcost,cost;
int bestway;
int hy;
int hl;

/* Home y position is usually 0, but it is 'top' if we have scrolling region
 * relative addressing
 */
if(t->rr) hy=t->top, hl=t->bot-1;
else hy=0, hl=t->li-1;

/* Assume best way is with only using relative cursor positioning */

bestcost=relcost(t,x,y,t->x,t->y); bestway=0;

/* Now check if combinations of absolute cursor positioning functions are
 * better (or necessary in case one or both cursor positions are unknown)
 */

if(t->ccr<bestcost)
 {
 cost=relcost(t,x,y,0,t->y)+t->ccr;
 if(cost<bestcost) bestcost=cost, bestway=1;
 }
if(t->cho<bestcost)
 {
 cost=relcost(t,x,y,0,hy)+t->cho;
 if(cost<bestcost) bestcost=cost, bestway=2;
 }
if(t->cll<bestcost)
 {
 cost=relcost(t,x,y,0,hl)+t->cll;
 if(cost<bestcost) bestcost=cost, bestway=3;
 }
if(t->cch<bestcost && x!=t->x)
 {
 cost=relcost(t,x,y,x,t->y)+tcost(t->cap,t->ch,1,x);
 if(cost<bestcost) bestcost=cost, bestway=4;
 }
if(t->ccv<bestcost && y!=t->y)
 {
 cost=relcost(t,x,y,t->x,y)+tcost(t->cap,t->cv,1,y);
 if(cost<bestcost) bestcost=cost, bestway=5;
 }
if(t->ccb<bestcost)
 {
 cost=relcost(t,x,y,0,y)+tcost(t->cap,t->cb,1,y);
 if(cost<bestcost) bestcost=cost, bestway=13;
 }
if(t->ccm<bestcost)
 {
 cost=tcost(t->cap,t->cm,1,y,x);
 if(cost<bestcost) bestcost=cost, bestway=6;
 }
if(t->cch+t->ccv<bestcost && x!=t->x && y!=t->y)
 {
 cost=tcost(t->cap,t->cv,1,y-hy)+tcost(t->cap,t->ch,1,x);
 if(cost<bestcost) bestcost=cost, bestway=7;
 }
if(t->ccv+t->ccr<bestcost && y!=t->y)
 {
 cost=tcost(t->cap,t->cv,1,y)+tcost(t->cap,t->cr,1)+
      relcost(t,x,y,0,y);
 if(cost<bestcost) bestcost=cost, bestway=8;
 }
if(t->cll+t->cch<bestcost)
 {
 cost=tcost(t->cap,t->ll,1)+tcost(t->cap,t->ch,1,x)+
      relcost(t,x,y,x,hl);
 if(cost<bestcost) bestcost=cost, bestway=9;
 }
if(t->cll+t->ccv<bestcost)
 {
 cost=tcost(t->cap,t->ll,1)+tcost(t->cap,t->cv,1,y)+
      relcost(t,x,y,0,y);
 if(cost<bestcost) bestcost=cost, bestway=10;
 }
if(t->cho+t->cch<bestcost)
 {
 cost=tcost(t->cap,t->ho,1)+tcost(t->cap,t->ch,1,x)+
      relcost(t,x,y,x,hy);
 if(cost<bestcost) bestcost=cost, bestway=11;
 }
if(t->cho+t->ccv<bestcost)
 {
 cost=tcost(t->cap,t->ho,1)+tcost(t->cap,t->cv,1,y)+
      relcost(t,x,y,0,y);
 if(cost<bestcost) bestcost=cost, bestway=12;
 }

/* Do absolute cursor positioning if we don't know the cursor position or
 * if it is faster than doing only relative cursor positioning
 */

switch(bestway)
 {
case 1: texec(t->cap,t->cr,1); t->x=0; break;
case 2: texec(t->cap,t->ho,1); t->x=0; t->y=hy; break;
case 3: texec(t->cap,t->ll,1); t->x=0; t->y=hl; break;
case 9: texec(t->cap,t->ll,1); t->x=0; t->y=hl; goto doch;
case 11: texec(t->cap,t->ho,1); t->x=0; t->y=hy;
  doch:
case 4: texec(t->cap,t->ch,1,x); t->x=x; break;
case 10: texec(t->cap,t->ll,1); t->x=0; t->y=hl; goto docv;
case 12: texec(t->cap,t->ho,1); t->x=0; t->y=hy; goto docv;
case 8: texec(t->cap,t->cr,1); t->x=0;
  docv:
case 5: texec(t->cap,t->cv,1,y); t->y=y; break;
case 6: texec(t->cap,t->cm,1,y,x); t->y=y, t->x=x; break;
case 7: texec(t->cap,t->cv,1,y); t->y=y;
        texec(t->cap,t->ch,1,x); t->x=x;
        break;
case 13: texec(t->cap,t->cb,1,y); t->y=y; t->x=0; break;
 }

/* Use relative cursor position functions if we're not there yet */

/* First adjust row */
if(y>t->y)
 /* Have to go down */
 if(!t->lf || t->cDO<(y-t->y)*t->clf)
  texec(t->cap,t->DO,1,y-t->y), t->y=y;
 else while(y>t->y) texec(t->cap,t->lf,1), ++t->y;
else if(y<t->y)
 /* Have to go up */
 if(!t->up || t->cUP<(t->y-y)*t->cup)
  texec(t->cap,t->UP,1,t->y-y), t->y=y;
 else while(y<t->y) texec(t->cap,t->up,1), --t->y;

/* Use tabs */
if(x>t->x && t->ta)
 {
 int ntabs=(x-t->x+t->x%t->tw)/t->tw;
 int cstunder=x%t->tw+t->cta*ntabs, cstover;
 if(x+t->tw<t->co && t->bs) cstover=t->cbs*(t->tw-x%t->tw)+t->cta*(ntabs+1);
 else cstover=10000;
 if(cstunder<t->cRI && cstunder<x-t->x && cstover>cstunder)
  {
  t->x=x-x%t->tw;
  while(ntabs--) texec(t->cap,t->ta,1);
  }
 else if(cstover<t->cRI && cstover<x-t->x)
  {
  t->x=t->tw+x-x%t->tw;
  ++ntabs;
  while(ntabs--) texec(t->cap,t->ta,1);
  }
 }
else if(x<t->x && t->bt)
 {
 int ntabs=(t->x-x+t->tw-t->x%t->tw)/t->tw;
 int cstunder,cstover;
 if(t->bs) cstunder=t->cbt*ntabs+t->cbs*(t->tw-x%t->tw); else cstunder=10000;
 if(x-t->tw>=0) cstover=t->cbt*(ntabs+1)+x%t->tw; else cstover=10000;
 if(cstunder<t->cLE && (t->bs?cstunder<(t->x-x)*t->cbs:1) && cstover>cstunder)
  {
  t->x=x+t->tw-x%t->tw;
  while(ntabs--) texec(t->cap,t->bt,1);
  }
 else if(cstover<t->cRI && (t->bs?cstover<(t->x-x)*t->cbs:1))
  {
  t->x=x-x%t->tw; ++ntabs;
  while(ntabs--) texec(t->cap,t->bt,1);
  }
 }

/* Now adjust column */
if(x<t->x)
 /* Have to go left */
 if(!t->bs || t->cLE<(t->x-x)*t->cbs)
  texec(t->cap,t->LE,1,t->x-x), t->x=x;
 else while(x<t->x) texec(t->cap,t->bs,1), --t->x;
else if(x>t->x)
 /* Have to go right */
 /* Hmm.. this should take into account possible attribute changes */
 if(t->cRI<x-t->x) texec(t->cap,t->RI,1,x-t->x), t->x=x;
 else
  {
  if(t->ins) clrins(t);
  while(x>t->x)
   {
   int c=t->scrn[t->x+t->y*t->co];
   outatr(t,t->x,y,c);
   }
  }
}

void cpos(t,x,y)
register SCRN *t;
register int x,y;
{
if(y==t->y)
 {
 if(x==t->x) return;
 if(x>t->x && x-t->x<4)
  {
  int *cs=t->scrn+t->x+t->co*t->y;
  if(t->ins)
   if(t->nd)
    {
    do texec(t->cap,t->nd,1); while(++t->x!=x);
    return;
    }
   else clrins(t);
  do { int c=*cs++; outatr(t,t->x,t->y,c); } while(x!=t->x);
  return;
  }
 }
if(!t->ms && t->attrib&(INVERSE|UNDERLINE))
 attr(t,t->attrib&~(INVERSE|UNDERLINE));
if(y<t->top || y>=t->bot) setregn(t,0,t->li);
cposs(t,x,y);
}

static void doinschr(t,x,y,s,n)
SCRN *t;
int x,y,*s,n;
{
int a;
if(x>=t->co-1 || !n) return;
if(t->im || t->ic || t->IC)
 {
 cpos(t,x,y);
 setins(t);
 if(n==1 && t->ic || !t->IC)
  for(a=0;a!=n;++a)
   {
   texec(t->cap,t->ic,1);
   outatr(t,x+a,y,s[a]);
   texec(t->cap,t->ip,1);
   }
 else
  {
  texec(t->cap,t->IC,1,n);
  for(a=0;a!=n;++a) outatr(t,x+a,y,s[a]);
  }
 if(!t->mi) clrins(t);
 }
mmove(t->scrn+x+t->co*y+n,t->scrn+x+t->co*y,(t->co-(x+n))*sizeof(int));
mcpy(t->scrn+x+t->co*y,s,n*sizeof(int));
}

static void dodelchr(t,x,y,n)
SCRN *t;
int x,y,n;
{
int a;
if(!n || x>=t->co-1) return;
if(t->dc || t->DC)
 {
 cpos(t,x,y);
 texec(t->cap,t->dm,1);		/* Enter delete mode */
 if(n==1 && t->dc || !t->DC)
  for(a=n;a;--a) texec(t->cap,t->dc,1);
 else texec(t->cap,t->DC,1,n);
 texec(t->cap,t->ed,1);		/* Exit delete mode */
 }
mmove(t->scrn+t->co*y+t->x,t->scrn+t->co*y+t->x+n,(t->co-(x+n))*sizeof(int));
}

static void doupscrl(t,top,bot,amnt)
SCRN *t;
int top,bot,amnt;
{
int a=amnt, x;
if(!amnt) return;
attr(t,0);
if(top==0 && bot==t->li && (t->sf || t->SF))
 {
 setregn(t,0,t->li);
 cpos(t,0,t->li-1);
 if(amnt==1 && t->sf || !t->SF) while(a--) texec(t->cap,t->sf,1);
 else texec(t->cap,t->SF,a,a);
 goto done;
 }
if(bot==t->li && (t->dl || t->DL))
 {
 setregn(t,0,t->li);
 cpos(t,0,top);
 if(amnt==1 && t->dl || !t->DL) while(a--) texec(t->cap,t->dl,1);
 else texec(t->cap,t->DL,a,a);
 goto done;
 }
if(t->cs && ( t->sf || t->SF ))
 {
 setregn(t,top,bot);
 cpos(t,0,bot-1);
 if(amnt==1 && t->sf || !t->SF) while(a--) texec(t->cap,t->sf,1);
 else texec(t->cap,t->SF,a,a);
 goto done;
 }
if((t->dl || t->DL) && (t->al || t->AL))
 {
 cpos(t,0,top);
 if(amnt==1 && t->dl || !t->DL) while(a--) texec(t->cap,t->dl,1);
 else texec(t->cap,t->DL,a,a);
 a=amnt;
 cpos(t,0,bot-amnt);
 if(amnt==1 && t->al || !t->AL) while(a--) texec(t->cap,t->al,1);
 else texec(t->cap,t->AL,a,a);
 goto done;
 }
msetI(t->updtab+top,1,bot-top);
return;

done:
mfwrd(t->scrn+top*t->co,t->scrn+(top+amnt)*t->co,
      (bot-top-amnt)*t->co*sizeof(int));
if(bot==t->li && t->db)
 {
 msetI(t->scrn+(t->li-amnt)*t->co,-1,amnt*t->co);
 msetI(t->updtab+t->li-amnt,1,amnt);
 }
else msetI(t->scrn+(bot-amnt)*t->co,' ',amnt*t->co);
}

static void dodnscrl(t,top,bot,amnt)
SCRN *t;
int top,bot,amnt;
{
int a=amnt,x;
if(!amnt) return;
attr(t,0);
if(top==0 && bot==t->li && (t->sr || t->SR))
 {
 setregn(t,0,t->li);
 cpos(t,0,0);
 if(amnt==1 && t->sr || !t->SR)
  while(a--) texec(t->cap,t->sr,1);
 else texec(t->cap,t->SR,a,a);
 goto done;
 }
if(bot==t->li && (t->al || t->AL))
 {
 setregn(t,0,t->li);
 cpos(t,0,top);
 if(amnt==1 && t->al || !t->AL)
  while(a--) texec(t->cap,t->al,1);
 else texec(t->cap,t->AL,a,a);
 goto done;
 }
if(t->cs && (t->sr || t->SR))
 {
 setregn(t,top,bot);
 cpos(t,0,top);
 if(amnt==1 && t->sr || !t->SR)
  while(a--) texec(t->cap,t->sr,1);
 else texec(t->cap,t->SR,a,a);
 goto done;
 }
if((t->dl || t->DL) && (t->al || t->AL))
 {
 cpos(t,0,bot-amnt);
 if(amnt==1 && t->dl || !t->DL)
  while(a--) texec(t->cap,t->dl,1);
 else texec(t->cap,t->DL,a,a);
 a=amnt;
 cpos(t,0,top);
 if(amnt==1 && t->al || !t->AL)
  while(a--) texec(t->cap,t->al,1);
 else texec(t->cap,t->AL,a,a);
 goto done;
 }
msetI(t->updtab+top,1,bot-top);
return;
done:
mbkwd(t->scrn+(top+amnt)*t->co,t->scrn+top*t->co,
      (bot-top-amnt)*t->co*sizeof(int));
if(!top && t->da)
 {
 msetI(t->scrn,-1,amnt*t->co);
 msetI(t->updtab,1,amnt);
 }
else msetI(t->scrn+t->co*top,' ',amnt*t->co);
}

void nscroll(t)
SCRN *t;
{
int y,z,q,r,p;
for(y=0;y!=t->li;++y)
 {
 q=t->sary[y];
 if(have) return;
 if(q && q!=t->li)
  if(q>0)
   {
   for(z=y;z!=t->li && t->sary[z]==q;++z) t->sary[z]=0;
   doupscrl(t,y,z+q,q), y=z-1;
   }
  else
   {
   for(r=y;r!=t->li && (t->sary[r]<0 || t->sary[r]==t->li);++r);
   p=r-1; do
    {
    q=t->sary[p];
    if(q && q!=t->li)
     {
     for(z=p;t->sary[z]=0, (z && t->sary[z-1]==q);--z);
     dodnscrl(t,z+q,p+1,-q);
     p=z+1;
     }
    }
    while(p--!=y);
   y=r-1;
   }
 }
msetI(t->sary,0,t->li);
}

void nescape(t)
SCRN *t;
{
cpos(t,0,t->li-1);
eraeol(t,0,t->li-1);
attr(t,0);
clrins(t,0);
setregn(t,0,t->li);
if(t->te) texec(t->cap,t->te,1);
}

void nreturn(t)
SCRN *t;
{
if(t->ti) texec(t->cap,t->ti,1);
nredraw(t);
}

void nclose(t)
SCRN *t;
{
int x;
leave=1;
attr(t,0);
clrins(t);
setregn(t,0,t->li);
if(t->te) texec(t->cap,t->te,1);
ttclose();
rmcap(t->cap);
free(t->scrn);
free(t->sary);
for(x=0;x!=t->tabsize;++x) vsrm(t->ktab[x].s);
free(t);
}

int ngetc(t)
SCRN *t;
{
int c,w,h,x;
wayup:
if(t->dumpptr>=0)
 {
 c=t->kbuf[t->dumpptr++];
 if(t->dumpptr==t->kbufp)
  {
  t->dumpptr= -1;
  t->kbufp=0;
  }
 return c;
 }
up:
c=ttgetc();
if(t->kbufp==32) t->kbufp=0;
t->kbuf[t->kbufp++]=c;
w=0;
for(h=0;h!=t->tabsize;++h)
 {
 for(x=0;x!=t->kbufp && x!=t->ktab[h].l;++x)
  if(t->ktab[h].s[x]!=t->kbuf[x]) goto nomatch;
 if(x==t->ktab[h].l)
  {
  c=t->ktab[h].n;
  goto found;
  }
 else if(x==t->kbufp) w=1;
 nomatch:;
 }
if(w) goto up;
/* Have to dump each char */
t->dumpptr=0;
goto wayup;
found:
t->kbufp=0;

/* ttgtsz(&w,&h);
if((w!=t->co || h!=t->li) && w>=3 && h>=3) tchsize(t,w,h); */
return c;
}

void nscrldn(t,top,bot,amnt)
SCRN *t;
int top,bot,amnt;
{
int x;
if(!amnt || top>=bot || bot>t->li) return;
if(amnt<bot-top)
 {
 for(x=bot;x!=top+amnt;--x)
  t->sary[x-1]=(t->sary[x-amnt-1]==t->li?t->li:t->sary[x-amnt-1]-amnt),
  t->updtab[x-1]=t->updtab[x-amnt-1];
 for(x=top;x!=top+amnt;++x) t->updtab[x]=0;
 }
if(amnt>bot-top) amnt=bot-top;
msetI(t->sary+top,t->li,amnt);
if(amnt==bot-top) msetI(t->updtab+top,1,amnt);
}

void nscrlup(t,top,bot,amnt)
SCRN *t;
int top,bot,amnt;
{
int x;
if(!amnt || top>=bot || bot>t->li) return;
if(amnt<bot-top)
 {
 for(x=top+amnt;x!=bot;++x)
  t->sary[x-amnt]=(t->sary[x]==t->li?t->li:t->sary[x]+amnt),
  t->updtab[x-amnt]=t->updtab[x];
 for(x=bot-amnt;x!=bot;++x) t->updtab[x]=0;
 }
if(amnt>bot-top) amnt=bot-top;
msetI(t->sary+bot-amnt,t->li,amnt);
if(amnt==bot-top) msetI(t->updtab+bot-amnt,1,amnt);
}

/*
void nchsize(t,w,h)
SCRN *t;
int w,h;
{
int x,y,*tmp;
if(w==t->co && h==t->li) return;

t->scrn=(int *)realloc(t->scrn,w*h*sizeof(int));
msetI(t->scrn,-1,w*h);

tmp=(int *)malloc(w*h*sizeof(int));
for(x=0;x!=w*h;++x) tmp[x]=' ';
for(y=0;y!=t->li;++y)
 {
 if(y==h) break;
 for(x=0;x!=t->co;++x)
  {
  if(x==w) break;
  tmp[x+y*h]=t->screen[x+y*t->co];
  }
 }
free(t->screen); t->screen=tmp;

if(t->li!=h) t->sary=(int *)realloc(t->sary,h*sizeof(int));
msetI(t->sary,0,h);
t->x= -1;
t->y= -1;
t->top=h;
t->bot=0;
t->co=w; t->li=h;
if(t->placex>=w) t->placex=w-1;
if(t->placey>=h) t->placey=h-1;
}
*/

void nredraw(t)
SCRN *t;
{
msetI(t->scrn,-1,t->li*t->co);
msetI(t->sary,0,t->li);
msetI(t->updtab,-1,t->li);
t->x= -1;
t->y= -1;
t->top=t->li;
t->bot=0;
t->attrib= -1;
t->ins= -1;
attr(t,0);
clrins(t);
if(t->cl)
 {
 texec(t->cap,t->cl,1);
 t->x=0; t->y=0;
 msetI(t->scrn,' ',t->li*t->co);
 }
else if(t->cd)
 {
 cpos(t,0,0);
 texec(t->cap,t->cd,1);
 msetI(t->scrn,' ',t->li*t->co);
 }
}
