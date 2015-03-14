/* Keyboard macros
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

#include "main.h"
#include "macro.h"

/* Create a macro */

MACRO *mkmacro(k,arg,n)
{
MACRO *macro=(MACRO *)malloc(sizeof(MACRO));
macro->steps=0;
macro->size=0;
macro->arg=arg;
macro->n=n;
macro->k=k;
return macro;
}

/* Eliminate a macro */

void rmmacro(macro)
MACRO *macro;
{
if(macro)
 {
 if(macro->steps)
  {
  int x;
  for(x=0;x!=macro->n;++x) rmmacro(macro->steps[x]);
  free(macro->steps);
  }
 free(macro);
 }
}

/* Add a step to block macro */

void addmacro(macro,m)
MACRO *macro, *m;
{
if(macro->n==macro->size)
 macro->steps=(MACRO **)realloc(macro->steps,(macro->size+=5)*sizeof(MACRO *));
macro->steps[macro->n++]=m;
}

/* Duplicate a macro */

MACRO *dupmacro(mac)
MACRO *mac;
{
MACRO *m=(MACRO *)malloc(sizeof(MACRO));
int x;
m->k=mac->k;
m->n=mac->n;
m->arg=mac->arg;
if(mac->steps)
 {
 int x;
 m->steps=(MACRO **)malloc((m->size=mac->n)*sizeof(MACRO *));
 for(x=0;x!=m->n;++x) m->steps[x]=dupmacro(mac->steps[x]);
 }
else m->steps=0, m->size=0;
return m;
}

MACRO *macstk(m,k)
MACRO *m;
{
m->k=k;
return m;
}

MACRO *macsta(m,a)
MACRO *m;
{
m->arg=a;
return m;
}

/* Keyboard macro recorder */

MACRO *kbdmacro[10];
int playmode[10];

struct recmac *recmac=0;

void unmac()
{
if(recmac) rmmacro(recmac->m->steps[--recmac->m->n]);
}

void record(m)
MACRO *m;
{
if(recmac) addmacro(recmac->m,dupmacro(m));
}

void urecord(w)
W *w;
{
int c;
int n;
struct recmac *r;
n=query(w,"Macro to record (0-9): ");
if(n>'9' || n<'0') return;
unmac(); unmac();
r=(struct recmac *)malloc(sizeof(struct recmac));
r->m=mkmacro(0,1,0);
r->next=recmac;
r->n=n-'0';
recmac=r;
}

void ustop()
{
unmac();
if(recmac)
 {
 struct recmac *r=recmac;
 MACRO *m;
 recmac=r->next;
 if(kbdmacro[r->n]) rmmacro(kbdmacro[r->n]);
 kbdmacro[r->n]=r->m;
 if(recmac) record(m=mkmacro(r->n+'0',1,findcmd(&cmdtab,"play"))), rmmacro(m);
 free(r);
 }
}

void uplay(w,c)
W *w;
{
if(c>'9' || c<'0') return;
c-='0';
if(playmode[c] || !kbdmacro[c]) return;
playmode[c]=1;
exmacro(kbdmacro[c]);
playmode[c]=0;
}
