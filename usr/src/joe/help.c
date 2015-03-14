/* Help system
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
#include "tty.h"
#include "toomany.h"
#include "b.h"
#include "termcap.h"
#include "kbd.h"
#include "scrn.h"
#include "w.h"
#include "menu.h"
#include "help.h"

/* The loaded help screen */

char *hlptxt=0;
int hlpsiz=0;
int hlpbsz=0;
int hlplns=0;

static int uphelp=0;

/* Display help text */

void dsphlp(t)
SCREEN *t;
{
char *str=hlptxt;
int y,x,c;
int atr=0;
for(y=0;y!=t->wind;++y)
 {
 if(t->t->updtab[y])
  {
  for(x=0;x!=t->w-1;++x)
   if(*str=='\n' || !*str)
    if(eraeol(t->t,x,y)) return;
    else break;
   else
    {
    if(*str=='\\')
     if(str[1]=='i' || str[1]=='u' || str[1]=='I' || str[1]=='U')
      {
      if(str[1]=='i' || str[1]=='I') atr^=INVERSE;
      else atr^=UNDERLINE;
      str+=2;
      --x; continue;
      }
     else if(str[1]=='\\') c='\\'+atr, str+=2;
     else { ++str; --x; continue; }
    else c= (unsigned char)*str++ +atr;
    if(t->t->scrn[x+y*t->w]!=c)
     {
     if(have) return;
     t->t->scrn[x+y*t->w]=c;
     outatr(t->t,x,y,c);
     }
    }
  atr=0; t->t->updtab[y]=0;
  }
 while(*str && *str!='\n') ++str;
 if(*str=='\n') ++str;
 }
}

/* Create the help window */

void helpon(t)
SCREEN *t;
{
int z;
W *w;
t->wind=hlplns;
if(t->wind>t->h-4) t->wind=t->h-4;
if(t->wind<0)
 {
 t->wind=0;
 return;
 }
w=t->topwin; do
 {
 w->hh=w->h;
 w->reqh=w->h*(t->h-t->wind);
 w->reqh/=t->h;
 if(w->win && !w->reqh) w->reqh=1;
 else if(w->reqh<2) w->reqh=2;
 w=(W *)w->link.next;
 } while(w!=t->topwin);
wfit(t);
msetI(t->t->updtab,1,t->wind);
}

/* Eliminate the help window */

void helpoff(t)
SCREEN *t;
{
W *w;
/*
w=t->topwin; do
 {
 w->reqh=w->hh;
 w=(W *)w->link.next;
 } while(w!=t->topwin);
*/
t->wind=0;
/*
wfit(w->t);*/
wspread(t);
}

/* Toggle help on/off */

void uhelp(w)
W *w;
{
struct help *h;
if(w->huh) if(h=get_help(w->huh))
 {
 if(w->t->wind) helpoff(w->t);
 hlptxt=h->hlptxt;
 hlpsiz=h->hlpsiz;
 hlpbsz=h->hlpbsz;
 hlplns=h->hlplns;
 helpon(w->t);
 return;
 }
uhelpme(w);
}

/* Help selection menu */

void uheol(w) W *w; { MENU *m=(MENU *)w->object; meol(m); }
void uhbol(w) W *w; { MENU *m=(MENU *)w->object; mbol(m); }
void uheof(w) W *w; { MENU *m=(MENU *)w->object; meof(m); }
void uhbof(w) W *w; { MENU *m=(MENU *)w->object; mbof(m); }
void uhdnarw(w) W *w; { MENU *m=(MENU *)w->object; mdnarw(m); }
void uhuparw(w) W *w; { MENU *m=(MENU *)w->object; muparw(m); }
void uhltarw(w) W *w; { MENU *m=(MENU *)w->object; mltarw(m); }
void uhrtarw(w) W *w; { MENU *m=(MENU *)w->object; mrtarw(m); }
void hdumb() {}
void movehelp(w,x,y) W *w; { MENU *m=(MENU *)w->object; menumove(m,x,y); }
void resizehelp(w,x,y) W *w; { MENU *m=(MENU *)w->object; menuresz(m,x,y); }
void disphelp(w) W *w;
 {
 MENU *m=(MENU *)w->object;
 menugen(m);
 w->cury=0;
 w->curx=(m->cursor-m->top)*(m->width+1);
 }
void followhelp(w) W *w; { MENU *m=(MENU *)w->object; menufllw(m); }
void wkillhelp(w) W *w; { MENU *m=(MENU *)w->object; if(m) menurm(m); }

void uhrtn(w)
W *w;
{
MENU *m=(MENU *)w->object;
hlptxt=help_structs[m->cursor]->hlptxt;
hlpsiz=help_structs[m->cursor]->hlpsiz;
hlpbsz=help_structs[m->cursor]->hlpbsz;
hlplns=help_structs[m->cursor]->hlplns;
wabort(w);
helpon(w->t);
}

void uhabort(w)
W *w;
{
wabort(w);
}

CONTEXT cthelp= {"help",0};

static WATOM watomhelp=
{
&cthelp,
disphelp,
followhelp,
wkillhelp,
resizehelp,
movehelp,
hdumb,
hdumb,
TYPEHELP
};

void uhelpme(w)
W *w;
{
W *new;
MENU *m;
if(w->t->wind)
 {
 helpoff(w->t);
 return;
 }
if(!(new=wcreate(w->t,&watomhelp,w,w,w->main,1,NULL))) return;
new->object=(void *)(m=mkmenu(new->t,help_names,new->x,new->y,new->w,new->h));
w->t->curwin=new;
}
