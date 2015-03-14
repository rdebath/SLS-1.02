/* UNDO system
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

#include "b.h"
#include "w.h"
#include "bw.h"
#include "undo.h"

static UNDO undos={{&undos,&undos}};

int inundo=0;

void undomk(b)
B *b;
{
UNDO *undo=(UNDO *)malloc(sizeof(UNDO));
undo->nrecs=0;
undo->ptr=0;
undo->first=0;
undo->last=0;
izque(UNDOREC,link,&undo->recs);
b->undo=undo;
enquef(UNDO,link,&undos,undo);
}

void undorm(b)
B *b;
{
UNDO *undo=b->undo;
while(!qempty(UNDOREC,link,&undo->recs))
 {
 free(undo->recs.link.next->data);
 free(deque(UNDOREC,link,undo->recs.link.next));
 }
free(deque(UNDO,link,undo));
}

void uundo(w)
W *w;
{
UNDOREC *upto;
BW *bw=(BW *)w->object;
UNDO *undo=bw->b->undo;
if(!undo->nrecs) return;
if(!undo->ptr)
 {
 if(undo->recs.link.prev->where!=bw->cursor->byte)
  pfwrd(pbof(bw->cursor),undo->recs.link.prev->where);
 undo->ptr=&undo->recs;
/* return; */
 }
if(undo->ptr->link.prev==&undo->recs) return;
upto=undo->ptr->link.prev->unit;
loop:
undo->ptr=undo->ptr->link.prev;
if(undo->ptr->where!=bw->cursor->byte)
 pfwrd(pbof(bw->cursor),undo->ptr->where);
inundo=1;
if(undo->ptr->del)
 {
 binsm(bw->cursor,undo->ptr->data,undo->ptr->len);
 undo->ptr->del=0;
 }
else
 {
 P *q=pdup(bw->cursor);
 pfwrd(q,undo->ptr->len);
 bdel(bw->cursor,q);
 prm(q);
 undo->ptr->del=1;
 }
inundo=0;
if(upto && upto!=undo->ptr) goto loop;
if(undo->ptr->first) bw->b->chnged=0;
}

void uredo(w)
W *w;
{
UNDOREC *upto;
BW *bw=(BW *)w->object;
UNDO *undo=bw->b->undo;
if(!undo->ptr) return;
if(undo->ptr==&undo->recs) return;
upto=undo->ptr->unit;
loop:
if(undo->ptr->where!=bw->cursor->byte)
 pfwrd(pbof(bw->cursor),undo->ptr->where);
inundo=1;
if(undo->ptr->del)
 {
 binsm(bw->cursor,undo->ptr->data,undo->ptr->len);
 undo->ptr->del=0;
 }
else
 {
 P *q=pdup(bw->cursor);
 pfwrd(q,undo->ptr->len);
 bdel(bw->cursor,q);
 prm(q);
 undo->ptr->del=1;
 }
inundo=0;
undo->ptr=undo->ptr->link.next;
if(upto && upto!=undo->ptr->link.prev) goto loop;
}

static void undogc(undo)
UNDO *undo;
{
UNDOREC *unit=undo->recs.link.next->unit;
if(unit)
 while(unit!=undo->recs.link.next)
  {
  free(undo->recs.link.next->data);
  free(deque(UNDOREC,link,undo->recs.link.next));
  }
free(undo->recs.link.next->data);
free(deque(UNDOREC,link,undo->recs.link.next));
--undo->nrecs;
}

static void undomark1(undo)
UNDO *undo;
{
if(undo->first)
 {
 undo->first->unit=undo->last;
 undo->last->unit=undo->first;
 undo->first=undo->last=0;
 if(++undo->nrecs==UNDOKEEP) undogc(undo);
 }
}

void umclear()
{
UNDO *undo;
for(undo=undos.link.next;undo!=&undos;undo=undo->link.next)
 {
 UNDOREC *rec;
 for(rec=undo->recs.link.next;rec!=&undo->recs;rec=rec->link.next)
  rec->min=0;
 }
}

void undomark()
{
UNDO *undo;
for(undo=undos.link.next;undo!=&undos;undo=undo->link.next) undomark1(undo);
}

static void undoover(undo)
UNDO *undo;
{
if(undo->ptr && undo->ptr!=&undo->recs)
 {
 while(undo->recs.link.prev!=undo->ptr)
  {
  free(undo->recs.link.prev->data);
  free(deque(UNDOREC,link,undo->recs.link.prev));
  }
 free(undo->recs.link.prev->data);
 free(deque(UNDOREC,link,undo->recs.link.prev));
 }
undo->ptr=0;
}

void undoins(p,size)
P *p;
long size;
{
UNDOREC *rec;
if(inundo) return;
undoover(p->b->undo);
rec=p->b->undo->recs.link.prev;
if(rec->min && rec!=&p->b->undo->recs && rec->del==0 && p->byte==rec->where+rec->len)
 {
 rec->data=(char *)realloc(rec->data,rec->len+size);
 brmem(p,rec->data+rec->len,size);
 rec->len+=size;
 }
else if(rec->min &&
        rec!=&p->b->undo->recs && rec->del==0 && p->byte==rec->where)
 {
 rec->data=(char *)realloc(rec->data,rec->len+size);
 mmove(rec->data+size,rec->data,rec->len);
 brmem(p,rec->data,size);
 rec->len+=size;
 }
else
 {
 rec=(UNDOREC *)malloc(sizeof(UNDOREC));
 rec->data=(char *)malloc(size);
 if(!p->b->undo->first) p->b->undo->first=rec;
 p->b->undo->last=rec;
 rec->where=p->byte;
 rec->min=1;
 rec->unit=0;
 rec->len=size;
 rec->del=0;
 if(qempty(UNDOREC,link,&p->b->undo->recs) && !p->b->chnged) rec->first=1;
 else rec->first=0;
 brmem(p,rec->data,rec->len);
 enqueb(UNDOREC,link,&p->b->undo->recs,rec);
 }
}

void undodel(p,size)
P *p;
long size;
{
UNDOREC *rec;
if(inundo) return;
undoover(p->b->undo);
rec=p->b->undo->recs.link.prev;
if(rec->min && rec!=&p->b->undo->recs && rec->del==1
   && p->byte==rec->where)
 {
 rec->data=(char *)realloc(rec->data,rec->len+size);
 brmem(p,rec->data+rec->len,size);
 rec->len+=size;
 }
else if(rec->min &&
        rec!=&p->b->undo->recs && rec->del==1 && p->byte+size==rec->where)
 {
 rec->data=(char *)realloc(rec->data,rec->len+size);
 mmove(rec->data+size,rec->data,rec->len);
 brmem(p,rec->data,size);
 rec->len+=size;
 rec->where=p->byte;
 }
else
 {
 rec=(UNDOREC *)malloc(sizeof(UNDOREC));
 rec->data=(char *)malloc(size);
 if(!p->b->undo->first) p->b->undo->first=rec;
 p->b->undo->last=rec;
 rec->where=p->byte;
 rec->min=1;
 rec->unit=0;
 rec->len=size;
 rec->del=1;
 if(qempty(UNDOREC,link,&p->b->undo->recs) && !p->b->chnged) rec->first=1;
 else rec->first=0;
 brmem(p,rec->data,rec->len);
 enqueb(UNDOREC,link,&p->b->undo->recs,rec);
 }
}
