/* Menu selection window
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
#include "scrn.h"
#include "vs.h"
#include "va.h"
#include "menu.h"

void menufllw(m)
MENU *m;
{
m->top=m->cursor-m->cursor%m->perline;
}

void menugen(m)
MENU *m;
{
int col;
int x;
int *s=m->t->t->scrn+m->x+m->y*m->t->t->co;
col=0;
for(x=0;x!=m->perline && m->list[x+m->top];++x)
 {
 int atr,z;
 if(x+m->top==m->cursor) atr=INVERSE;
 else atr=0;
 if(col==m->w) break;
 for(z=0;m->list[x+m->top][z];++z)
  {
  if(col==m->w) break;
  if(s[col]!=(unsigned char)m->list[x+m->top][z]+atr)
   {
   s[col]=(unsigned char)m->list[x+m->top][z]+atr;
   outatr(m->t->t,m->x+col,m->y,s[col]);
   }
  ++col;
  }
 while(z<m->width)
  {
  if(col==m->w) break;
  if(s[col]!=' ')
   s[col]=' ', outatr(m->t->t,m->x+col,m->y,' ');
  ++col; ++z;
  }
 if(col!=m->w)
  {
  if(s[col]!=' ')
   s[col]=' ', outatr(m->t->t,m->x+col,m->y,' ');
  ++col;
  }
 }
if(col!=m->w) eraeol(m->t->t,m->x+col,m->y);
}

void menumove(m,x,y)
MENU *m;
{
m->x=x;
m->y=y;
}

void menuresz(m,wi,he)
MENU *m;
{
m->w=wi;
m->h=he;
}

void mconfig(m)
MENU *m;
{
/* Configure menu display parameters */
int x;
for(x=0,m->width=0;m->list[x];++x)
 if(zlen(m->list[x])>m->width) m->width=zlen(m->list[x]);
if(m->width>m->w) m->width=m->w-1;
m->perline=m->w/(m->width+1);
}

MENU *mkmenu(t,s,x,y,wi,h)
SCREEN *t;
char **s;
{
MENU *m=(MENU *)malloc(sizeof(MENU));
m->list=s;
m->top=0;
m->cursor=0;
m->t=t;
m->h=h; m->w=wi; m->x=x; m->y=y;
m->object=0;
mconfig(m);
return m;
}

void menurm(m)
MENU *m;
{
free(m);
}

void mbol(m)
MENU *m;
{
m->cursor=m->top;
}

void mbof(m)
MENU *m;
{
m->cursor=0;
}

void meof(m)
MENU *m;
{
if(aLEN(m->list)) m->cursor=aLEN(m->list)-1;
}

void meol(m)
MENU *m;
{
if(m->top+m->perline<aLEN(m->list))
 m->cursor=m->top+m->perline-1;
else meof(m);
}

void mrtarw(m)
MENU *m;
{
if(m->cursor+1<aLEN(m->list)) ++m->cursor;
}

void mltarw(m)
MENU *m;
{
if(m->cursor) --m->cursor;
}

void muparw(m)
MENU *m;
{
if(m->cursor>=m->perline) m->cursor-=m->perline;
}

void mdnarw(m)
MENU *m;
{
if(m->cursor+m->perline<aLEN(m->list)) m->cursor+=m->perline;
else if(m->top+m->perline<aLEN(m->list)) meof(m);
}
