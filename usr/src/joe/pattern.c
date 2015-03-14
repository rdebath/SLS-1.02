/* Pattern matching system
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
#include <sys/types.h>
#include <sys/stat.h>
#include "config.h"
#include "heap.h"
#include "termcap.h"
#include "toomany.h"
#include "vfile.h"
#include "scrn.h"
#include "tty.h"
#include "queue.h"
#include "b.h"
#include "regex.h"
#include "kbd.h"
#include "bw.h"
#include "vs.h"
#include "w.h"
#include "pathfunc.h"
#include "va.h"
#include "edfuncs.h"
#include "pattern.h"

B *findhist=0;

void pfnext();

#define SRBACKWARDS 1
#define SRIGNORE 2
#define SRREPLACE 4

static void set_replace(w,s)
 W *w;
 char *s;
 { 
 if(w->t->replace) vsrm(w->t->replace);
 w->t->replace=s;
 pfnext(w);
 }

static void set_options(w,s)
 W *w;
 char *s;
 {
 int x;
 w->t->options=0;
 w->t->repeat= -1;
 for(x=0;s[x];++x)
  switch(s[x])
   {
  case 'r': case 'R': w->t->options|=SRREPLACE; break;
  case 'b': case 'B': w->t->options|=SRBACKWARDS; break;
  case 'i': case 'I': w->t->options|=SRIGNORE; break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
   if(w->t->repeat== -1) w->t->repeat=0;
   w->t->repeat=w->t->repeat*10+s[x]-'0';
   break;
   }
 vsrm(s);
 if(w->t->options&SRREPLACE) wmkpw(w,"Replace with: ",&findhist,set_replace,"Search");
 else pfnext(w);
 }

static void set_pattern(w,s)
 W *w;
 char *s;
 {
 if(w->t->pattern) vsrm(w->t->pattern);
 w->t->pattern=s;
 wmkpw(w,"(I)gnore case (R)eplace (B)ackwards NNN: ",NULL,set_options,"Search");
 }

void pffirst(w)
 W *w;
 {
 wmkpw(w,"Find: ",&findhist,set_pattern,"Search"); 
 }

char *entire=0;

static int searchf(w)
W *w;
{
BW *bw=(BW *)w->object;
P *start=pdup(bw->cursor);
P *end=pdup(bw->cursor);
int x;
for(x=0;x!=sLEN(w->t->pattern) && w->t->pattern[x]!='\\';++x);
if(w->t->options&SRIGNORE)
 while(pfindfni(start,w->t->pattern,x))
  {
  pset(end,start);
  pfwrd(end,x);
  if(pimatch(w->t->pattern+x,sLEN(w->t->pattern)-x,end,0))
   {
   w->t->foundlen=end->byte-start->byte;
   entire=vstrunc(entire,w->t->foundlen);
   brmem(start,entire,w->t->foundlen);
   pset(bw->cursor,end);
   prm(start); prm(end);
   pfcol(bw->cursor);
   bw->cursor->xcol=bw->cursor->col;
   return 1;
   }
  if(pgetc(start)== -1) break;
  }
else
 while(pfindfn(start,w->t->pattern,x))
  {
  pset(end,start);
  pfwrd(end,x);
  if(pmatch(w->t->pattern+x,sLEN(w->t->pattern)-x,end,0))
   {
   w->t->foundlen=end->byte-start->byte;
   entire=vstrunc(entire,w->t->foundlen);
   brmem(start,entire,w->t->foundlen);
   pset(bw->cursor,end);
   prm(start); prm(end);
   pfcol(bw->cursor);
   bw->cursor->xcol=bw->cursor->col;
   return 1;
   }
  if(pgetc(start)== -1) break;
  }
prm(start); prm(end);
return 0;
}

static int searchb(w)
W *w;
{
BW *bw=(BW *)w->object;
P *start=pdup(bw->cursor);
P *end=pdup(bw->cursor);
int x;
for(x=0;x!=sLEN(w->t->pattern) && w->t->pattern[x]!='\\';++x);
if(w->t->options&SRIGNORE)
 while(pbkwdf(start,1L) && pfindrni(start,w->t->pattern,x))
  {
  pset(end,start);
  pfwrd(end,x);
  if(pimatch(w->t->pattern+x,sLEN(w->t->pattern)-x,end,0))
   {
   w->t->foundlen=end->byte-start->byte;
   entire=vstrunc(entire,w->t->foundlen);
   brmem(start,entire,w->t->foundlen);
   pset(bw->cursor,start);
   prm(start); prm(end);
   pfcol(bw->cursor);
   bw->cursor->xcol=bw->cursor->col;
   return 1;
   }
  }
else
 while(pbkwdf(start,1L) && pfindrn(start,w->t->pattern,x))
  {
  pset(end,start);
  pfwrd(end,x);
  if(pmatch(w->t->pattern+x,sLEN(w->t->pattern)-x,end,0))
   {
   w->t->foundlen=end->byte-start->byte;
   entire=vstrunc(entire,w->t->foundlen);
   brmem(start,entire,w->t->foundlen);
   pset(bw->cursor,start);
   prm(start); prm(end);
   pfcol(bw->cursor);
   bw->cursor->xcol=bw->cursor->col;
   return 1;
   }
  }
prm(start); prm(end);
return 0;
}

static void insert(bw,s,len)
BW *bw;
char *s;
{
int x;
while(len)
 {
 for(x=0;x!=len && s[x]!='\\';++x);
 if(x)
  {
  binsm(bw->cursor,s,x);
  pfwrd(bw->cursor,x);
  len-=x;
  s+=x;
  }
 else if(len>=2)
  {
  if(s[1]=='\\') binsc(bw->cursor,'\\'), pgetc(bw->cursor);
  else if(s[1]=='n') binsc(bw->cursor,'\n'), pgetc(bw->cursor);
  else if((s[1]>='a' && s[1]<='z' ||
          s[1]>='A' && s[1]<='Z') && pieces[(s[1]&0x1f)-1])
   {
   binsm(bw->cursor,sv(pieces[(s[1]&0x1f)-1]));
   pfwrd(bw->cursor,sLEN(pieces[(s[1]&0x1f)-1]));
   }
  else if(s[1]>='0' && s[1]<='9' && pieces[s[1]-'0'])
   {
   binsm(bw->cursor,sv(pieces[s[1]-'0']));
   pfwrd(bw->cursor,sLEN(pieces[s[1]-'0']));
   }
  else if(s[1]=='&' && entire)
   {
   binsm(bw->cursor,sv(entire));
   pfwrd(bw->cursor,sLEN(entire));
   }
  s+=2; len-=2;
  }
 else len=0;
 }
}

static void replace(w)
W *w;
{
BW *bw=(BW *)w->object;
P *q=pdup(bw->cursor);
if(w->t->options&SRBACKWARDS)
 {
 q=pfwrd(q,w->t->foundlen);
 bdel(bw->cursor,q);
 prm(q);
 }
else
 {
 q=pbkwd(q,w->t->foundlen);
 bdel(q,bw->cursor);
 prm(q);
 }
insert(bw,sv(w->t->replace));
}

void pfnext(w)
 W *w;
 {
 BW *bw=(BW *)w->object;
 int c;
 int rest=0;
 int flg=0;
 int orgmid=mid;
 mid=1;
 if(!w->t->pattern) { pffirst(w); goto done; }
 next:
 if(w->t->repeat!= -1)
  if(!w->t->repeat) goto done;
  else --w->t->repeat;
 if(w->t->options&SRBACKWARDS)
  { if(!searchb(w)) { if(!flg || !(w->t->options&SRREPLACE)) msgnw(w,"Not found"); w->t->repeat= -1; goto done; } }
 else
  if(!searchf(w)) { if(!flg || !(w->t->options&SRREPLACE)) msgnw(w,"Not found"); w->t->repeat= -1; goto done; }
 flg=1;
 if(w->t->options&SRREPLACE)
  if(rest) { replace(w); goto next; }
  else
   {
   do
    {
    P *mb=w->t->markb, *mk=w->t->markk;
    if(w->t->options&SRBACKWARDS)
     {
     w->t->markb=pdup(bw->cursor);
     w->t->markk=pdup(bw->cursor);
     pfwrd(w->t->markk,w->t->foundlen);
     }
    else
     {
     w->t->markk=pdup(bw->cursor);
     w->t->markb=pdup(bw->cursor);
     pbkwd(w->t->markb,w->t->foundlen);
     }
    updall();
    c=queryn(w,"Replace (Y)es (N)o (R)est (^C) to abort?");
    prm(w->t->markb); prm(w->t->markk);
    w->t->markb=mb; w->t->markk=mk;
    if(c=='N' || c=='n') { goto next; }
    if(c=='Y' || c=='y') { replace(w); goto next; }
    if(c=='R' || c=='r') { replace(w); rest=1; goto next; }
    }
    while(c!=MAXINT && c!='C'-'@');
   }
 else if(w->t->repeat!= -1) goto next;
 done:
 updall();
 bw->cursor->xcol=bw->cursor->col;
 mid=orgmid;
 }
