/* File selection menu
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
#include "scrn.h"
#include "kbd.h"
#include "vs.h"
#include "w.h"
#include "bw.h"
#include "zstr.h"
#include "pathfunc.h"
#include "va.h"
#include "menu.h"
#include "edfuncs.h"
#include "tty.h"
#include "tab.h"

CONTEXT cttab={"tab",0};

#define F_DIR		1
#define F_NORMAL	2
#define F_EXEC		4

char **rexpnd();

static int get_entries(tab,prv)
TAB *tab;
{
int a;
int which=0;
char *oldpwd;
char **files=(char **)rexpnd(tab->path,tab->pattern);
if(!files) return -1;
if(!aLEN(files)) return -1;
tab->len=aLEN(files);
vsrm(tab->files); tab->files=files;
vasort(files,tab->len);
if(tab->type) free(tab->type);
tab->type=(char *)malloc(tab->len);
oldpwd=pwd(); chdir(tab->path);
for(a=0;a!=tab->len;a++)
 {
 struct stat buf;
 mset(&buf,0,sizeof(struct stat));
 stat(files[a],&buf);
 if(buf.st_ino==prv) which=a;
 if((buf.st_mode&S_IFMT)==S_IFDIR) tab->type[a]=F_DIR;
 else if(buf.st_mode&(0100|0010|0001)) tab->type[a]=F_EXEC;
 else tab->type[a]=F_NORMAL;
 }
chdir(oldpwd);
return which;
}

static void wkilltab(w)
W *w;
{
MENU *m=(MENU *)w->object;
TAB *tab;
if(m) tab=(TAB *)m->object;
else tab=0;
if(tab)
 {
 vsrm(tab->path);
 vsrm(tab->pattern);
 varm(tab->files);
 free(tab->type);
 free(tab);
 }
if(m)
 {
 varm(m->list);
 menurm(m);
 }
}

static void followtab(w)
W *w;
{
MENU *m=(MENU *)w->object;
menufllw(m);
}

static void disptab(w)
W *w;
{
MENU *m=(MENU *)w->object;
menugen(m);
w->cury=0;
w->curx=(m->cursor-m->top)*(m->width+1);
}

static void resizetab(w,wi,he)
W *w;
{
MENU *m=(MENU *)w->object;
menuresz(m,wi,he);
}
 
static void movetab(w,x,y)
W *w;
{
MENU *m=(MENU *)w->object;
menumove(m,x,y);
}
 
static void tdumb() {}

void tltarw(w)
W *w;
{
MENU *m=(MENU *)w->object;
mltarw(m);
}
 
void trtarw(w)
W *w;
{
MENU *m=(MENU *)w->object;
mrtarw(m);
}
 
void tuparw(w)
W *w;
{
MENU *m=(MENU *)w->object;
muparw(m);
}
 
void tdnarw(w)
W *w;
{
MENU *m=(MENU *)w->object;
mdnarw(m);
}
 
void tbof(w)
W *w;
{
MENU *m=(MENU *)w->object;
mbof(m);
}
 
void teof(w)
W *w;
{
MENU *m=(MENU *)w->object;
meof(m);
}
 
void tbol(w)
W *w;
{
MENU *m=(MENU *)w->object;
mbol(m);
}
 
void teol(w)
W *w;
{
MENU *m=(MENU *)w->object;
meol(m);
}

int treload(w,m,tab,flg)
W *w;
MENU *m;
TAB *tab;
{
BW *bw;
P *p;
int x;
int which;
char **list;
struct stat buf;
if((which=get_entries(tab,tab->prv))<0) return 1;
if(tab->path && tab->path[0]) stat(tab->path,&buf);
else stat(".",&buf);
tab->prv=buf.st_ino;
if(!flg) which=0;
if(m) { vsrm(m->list); menurm(m); }
list=vaensure(NULL,aLEN(tab->files));
for(x=0;tab->files[x];++x)
 {
 vaset(list,x,vsncpy(NULL,0,sv(tab->files[x])));
 if(tab->type[x]==F_DIR) list[x]=vsncpy(list[x],sLEN(list[x]),sc("/"));
 else if(tab->type[x]==F_EXEC) list[x]=vsncpy(list[x],sLEN(list[x]),sc("*"));
 }
w->object=(void *)(m=mkmenu(w->t,list,w->x,w->y,w->w,w->h));
m->object=tab;
m->cursor=which;
bw=(BW *)w->win->object;
p=pdup(bw->cursor); pbol(p);
peol(bw->cursor);
bdel(p,bw->cursor);
if(sLEN(tab->path))
 {
 binsm(bw->cursor,sv(tab->path)), peol(bw->cursor);
 if(tab->path[sLEN(tab->path)-1]!='/')
  binsm(bw->cursor,sc("/")), peol(bw->cursor);
 }
binsm(bw->cursor,sv(tab->pattern)); peol(bw->cursor);
prm(p);
return 0;
}
 
void trtn(w)
W *w;
{
MENU *m=(MENU *)w->object;
TAB *tab=(TAB *)m->object;
if(tab->type[m->cursor]==F_DIR)
 { /* Switch directories */
 char *orgpath=tab->path;
 char *orgpattern=tab->pattern;
 char *e=endprt(tab->path);
 if(!zcmp(tab->files[m->cursor],"..") && sLEN(e) &&
    !(e[0]=='.' && e[1]=='.' && (!e[2] || e[2]=='/')))
  tab->path=begprt(tab->path);
 else
  {
  tab->path=vsncpy(NULL,0,sv(tab->path));
  tab->path=vsncpy(tab->path,sLEN(tab->path),sv(m->list[m->cursor]));
  }
 vsrm(e);
 tab->pattern=vsncpy(NULL,0,sc("*"));
 if(treload(w,m,tab,0))
  {
  msgnw(w,"Couldn\'t read directory ");
  vsrm(tab->pattern); tab->pattern=orgpattern;
  vsrm(tab->path); tab->path=orgpath;
  }
 else
  {
  vsrm(orgpattern);
  vsrm(orgpath);
  }
 }
else
 { /* Select name */
 BW *bw=(BW *)w->win->object;
 P *p;
 p=pdup(bw->cursor); pbol(p);
 peol(bw->cursor);
 bdel(p,bw->cursor);
 if(sLEN(tab->path))
  {
  binsm(bw->cursor,sv(tab->path)), peol(bw->cursor);
  if(tab->path[sLEN(tab->path)-1]!='/')
   binsm(bw->cursor,sc("/")), peol(bw->cursor);
  }
 binsm(bw->cursor,sv(tab->files[m->cursor])); peol(bw->cursor);
 prm(p);
 wabort(w);
 }
}

void tbacks(w)
W *w;
{
MENU *m=(MENU *)w->object;
TAB *tab=(TAB *)m->object;
char *orgpath=tab->path;
char *orgpattern=tab->pattern;
char *e=endprt(tab->path);
if(sLEN(e)) tab->path=begprt(tab->path);
else
 {
 vsrm(e);
 return;
 }
vsrm(e);
tab->pattern=vsncpy(NULL,0,sc("*"));
if(treload(w,m,tab,1))
 {
 msgnw(w,"Couldn\'t read directory ");
 vsrm(tab->pattern); tab->pattern=orgpattern;
 vsrm(tab->path); tab->path=orgpath;
 }
else
 {
 vsrm(orgpattern);
 vsrm(orgpath);
 }
}

void tuabort(w)
W *w;
{
BW *bw=(BW *)w->win->object;
P *p=pdup(bw->cursor); pbol(p);
peol(bw->cursor);
bdel(p,bw->cursor);
prm(p);
wabort(w);
}

static WATOM watomtab=
{
&cttab,
disptab,
followtab,
wkilltab,
resizetab,
movetab,
tdumb,
tdumb,
TYPETAB
};

/* Create a tab window */

void ucmplt(w)
W *w;
{
W *new;
TAB *tab;
char **list;
MENU *m;
P *p, *q;
char *cline, *tmp;
BW *bw;
long a,b;
if(!(new=wcreate(w->t,&watomtab,w,w,w->main,1,NULL))) return;
tab=(TAB *)malloc(sizeof(TAB));
tab->files=0;
tab->type=0;
bw=(BW *)w->object;
p=pdup(bw->cursor); pbol(p);
q=pdup(bw->cursor); peol(q);
tmp=brvs(p,q->byte-p->byte);
cline=parsens(tmp,&a,&b);
vsrm(tmp);
prm(p); prm(q);
tmp=namprt(cline);
tab->pattern=vsncpy(sv(tmp),sc("*"));
tab->path=dirprt(cline);
tab->prv=0;
vsrm(cline);
if(treload(new,NULL,tab,0))
 {
 msgnw(w,"Couldn\'t read directory ");
 vsrm(tab->path);
 vsrm(tab->pattern);
 free(tab);
 wabort(w);
 }
else w->t->curwin=new;
}
