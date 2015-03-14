/* Software virtual memory system
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
#include <fcntl.h>
#include "config.h"
#include "vs.h"
#include "blocks.h"
#include "heap.h"
#include "queue.h"
#include "pathfunc.h"
#include "toomany.h"
#include "random.h"
#include "vfile.h"

static VFILE vfiles={{&vfiles,&vfiles}};	/* Known vfiles */
static VPAGE *freepages=0;	/* Linked list of free pages */
static VPAGE *htab[HTSIZE];	/* Hash table of page headers */
static long curvalloc=0;	/* Amount of memory in use */
static long maxvalloc= ILIMIT;	/* Maximum allowed */
char *vbase;			/* Data first entry in vheader refers to */
VPAGE **vheaders=0;		/* Array of header addresses */
static int vheadsz=0;		/* No. entries allocated to vheaders */

void vflsh()
{
VPAGE *vp;
VPAGE *vlowest;
long addr;
VFILE *vfile;
int x;
for(vfile=vfiles.link.next;
    vfile!=&vfiles;
    vfile=vfile->link.next)
 {
 loop:
 addr= MAXLONG;
 vlowest=0;
 for(x=0;x!=HTSIZE;x++)
  for(vp=htab[x];vp;vp=vp->next)
   if(vp->addr<addr && vp->dirty && vp->vfile==vfile && !vp->count)
    addr=vp->addr, vlowest=vp;
 if(vlowest)
  {
  if(!vfile->name) vfile->name=mktmp("/tmp/");
  if(!vfile->fd)
   {
   vfile->fd=Fopen(vfile->name);
   }
  Fseek(vfile->fd,addr);
  if(addr+PGSIZE>vsize(vfile))
   {
   Fwrite(vfile->fd,vlowest->data,(int)(vsize(vfile)-addr));
   vfile->size=vsize(vfile);
   }
  else
   {
   Fwrite(vfile->fd,vlowest->data,PGSIZE);
   if(addr+PGSIZE>vfile->size) vfile->size=addr+PGSIZE;
   }
  vlowest->dirty=0;
  goto loop;
  }
 }
}

void vflshf(vfile)
VFILE *vfile;
{
VPAGE *vp;
VPAGE *vlowest;
long addr;
int x;
loop:
addr= MAXLONG;
vlowest=0;
for(x=0;x!=HTSIZE;x++)
 for(vp=htab[x];vp;vp=vp->next)
  if(vp->addr<addr && vp->dirty && vp->vfile==vfile && !vp->count)
   addr=vp->addr, vlowest=vp;
if(vlowest)
 {
 if(!vfile->name) vfile->name=mktmp("/tmp/");
 if(!vfile->fd)
  {
  vfile->fd=Fopen(vfile->name);
  }
 Fseek(vfile->fd,addr);
 if(addr+PGSIZE>vsize(vfile))
  {
  Fwrite(vfile->fd,vlowest->data,(int)(vsize(vfile)-addr));
  vfile->size=vsize(vfile);
  }
 else
  {
  Fwrite(vfile->fd,vlowest->data,PGSIZE);
  if(addr+PGSIZE>vfile->size) vfile->size=addr+PGSIZE;
  }
 vlowest->dirty=0;
 goto loop;
 }
}

char *vlock(vfile,addr)
VFILE *vfile;
long addr;
{
VPAGE *vp, *pp;
int x,y;
int ofst=(addr&(PGSIZE-1));
addr-=ofst;

for(vp=htab[((addr>>LPGSIZE)+(int)vfile)&(HTSIZE-1)];vp;vp=vp->next)
 if(vp->vfile==vfile && vp->addr==addr) return ++vp->count, vp->data+ofst;

if(freepages)
 {
 vp=freepages;
 freepages=vp->next;
 goto gotit;
 }

if(curvalloc+PGSIZE<=maxvalloc)
 {
 vp=(VPAGE *)malloc(sizeof(VPAGE)*INC);
 if(vp)
  {
  vp->data=(char *)memalign(PGSIZE,PGSIZE*INC);
  if(vp->data)
   {
   int q;
   curvalloc+=PGSIZE*INC;
   if(!vheaders)
    vheaders=(VPAGE **)malloc((vheadsz=INC)*sizeof(VPAGE *)),
    vbase=vp->data;
   else
    if(vp->data<vbase)
     {
     VPAGE **t=vheaders;
     int amnt=(physical(vbase)-physical(vp->data))>>LPGSIZE;
     vheaders=(VPAGE **)malloc((amnt+vheadsz)*sizeof(VPAGE *));
     mcpy(vheaders+amnt,t,vheadsz*sizeof(VPAGE *));
     vheadsz+=amnt;
     vbase=vp->data;
     free(t);
     }
    else if(((physical(vp->data+PGSIZE*INC)-physical(vbase))>>LPGSIZE)>vheadsz)
     {
     vheaders=(VPAGE **)realloc(vheaders,
      (vheadsz=(((physical(vp->data+PGSIZE*INC)-physical(vbase))>>LPGSIZE)))*sizeof(VPAGE *));
     }
   for(q=1;q!=INC;++q)
    {
    vp[q].next=freepages;
    freepages=vp+q;
    vp[q].data=vp->data+q*PGSIZE;
    vheader(vp->data+q*PGSIZE)=vp+q;
    }
   vheader(vp->data)=vp;
   goto gotit;
   }
  free(vp);
  vp=0;
  }
 }

for(y=HTSIZE, x=(random()&(HTSIZE-1));y;x=((x+1)&(HTSIZE-1)), --y)
 for(pp=(VPAGE *)(htab+x),vp=pp->next;vp;pp=vp, vp=vp->next)
  if(!vp->count && !vp->dirty)
   {
   pp->next=vp->next;
   goto gotit;
   }
vflsh();
for(y=HTSIZE, x=(random()&(HTSIZE-1));y;x=((x+1)&(HTSIZE-1)), --y)
 for(pp=(VPAGE *)(htab+x),vp=pp->next;vp;pp=vp, vp=vp->next)
  if(!vp->count && !vp->dirty)
   {
   pp->next=vp->next;
   goto gotit;
   }
write(2,"Out of memory\r\n",15);
exit(1);

gotit:
vp->addr=addr;
vp->vfile=vfile;
vp->dirty=0;
vp->count=1;
vp->next=htab[((addr>>LPGSIZE)+(int)vfile)&(HTSIZE-1)];
htab[((addr>>LPGSIZE)+(int)vfile)&(HTSIZE-1)]=vp;

if(addr<vfile->size)
 {
 if(!vfile->fd)
  {
  vfile->fd=Fopen(vfile->name);
  }
 Fseek(vfile->fd,addr);
 if(addr+PGSIZE>vfile->size)
  {
  Fread(vfile->fd,vp->data,(int)(vfile->size-addr));
  mset(vp->data+vfile->size-addr,0,PGSIZE-(int)(vfile->size-addr));
  }
 else Fread(vfile->fd,vp->data,PGSIZE);
 }
else mset(vp->data,0,PGSIZE);

return vp->data+ofst;
}

VFILE *vtmp()
{
VFILE *new=(VFILE *)malloc(sizeof(VFILE));
new->fd= 0;
new->name=0;
new->alloc=0;
new->size=0;
new->left=0;
new->lv=0;
new->vpage=0;
new->flags=1;
new->vpage1=0;
new->addr= -1;
return enqueb(VFILE,link,&vfiles,new);
}

VFILE *vopen(name)
char *name;
{
VFILE *new=(VFILE *)malloc(sizeof(VFILE));
new->name=vsncpy(NULL,0,sz(name));
new->fd=Fopen(name);
if(!new->fd)
 {
 fprintf(stderr,"Couldn\'t open \'%s\'\n",name);
 free(new);
 return 0;
 }
new->size=new->fd->size;
new->alloc=new->size;
new->left=0;
new->lv=0;
new->vpage=0;
new->flags=0;
new->vpage1=0;
new->addr= -1;
return enqueb(VFILE,link,&vfiles,new);
}

void vclose(vfile)
VFILE *vfile;
{
VPAGE *vp, *pp;
int x;
if(vfile->name)
 {
 if(vfile->flags) unlink(vfile->name);
 else vflshf(vfile);
 vsrm(vfile->name);
 }
if(vfile->fd) Fclose(vfile->fd);
free(deque(VFILE,link,vfile));
for(x=0;x!=HTSIZE;x++)
 for(pp=(VPAGE *)(htab+x), vp=pp->next;vp;)
  if(vp->vfile==vfile)
   {
   pp->next=vp->next;
   vp->next=freepages;
   freepages=vp;
   vp=pp->next;
   }
  else pp=vp, vp=vp->next;
}

/* this is now broken */
void vlimit(amount)
long amount;
{
VPAGE *vp, *pp;
int x,y;
maxvalloc=amount;
while(curvalloc>maxvalloc)
 if(freepages)
  {
  vp=freepages;
  freepages=vp->next;
  free(vp->data);
  free(vp);
  curvalloc-=PGSIZE;
  }
 else
  {
  again:
  for(y=HTSIZE, x=(random()&(HTSIZE-1));y;x=((x+1)&(HTSIZE-1)), --y)
   for(pp=(VPAGE *)(htab+x),vp=pp->next;vp;pp=vp, vp=vp->next)
    if(!vp->count && !vp->dirty)
     {
     pp->next=vp->next;
     free(vp->data);
     free(vp);
     if((curvalloc-=PGSIZE)<=maxvalloc) return;
     goto again;
     }
  vflsh();
  again1:
  for(y=HTSIZE, x=(random()&(HTSIZE-1));y;x=((x+1)&(HTSIZE-1)), --y)
   for(pp=(VPAGE *)(htab+x),vp=pp->next;vp;pp=vp, vp=vp->next)
    if(!vp->count && !vp->dirty)
     {
     pp->next=vp->next;
     free(vp->data);
     free(vp);
     if((curvalloc-=PGSIZE)<=maxvalloc) return;
     goto again1;
     }
  return;
  }
}

long valloc(vfile,size)
VFILE *vfile;
long size;
{
long start=vsize(vfile);
vfile->alloc=start+size;
if(vfile->lv)
 {
 if(vheader(vfile->vpage)->addr+PGSIZE>vfile->alloc)
  vfile->lv=PGSIZE-(vfile->alloc-vheader(vfile->vpage)->addr);
 else vfile->lv=0;
 }
return start;
}

void vseek(vfile,addr)
VFILE *vfile;
long addr;
{
vfile->alloc=vsize(vfile);
if(addr>vfile->alloc) vfile->alloc=addr;
if(!vfile->vpage) vfile->vpage=vlock(vfile,addr&~(long)(PGSIZE-1));
else if(vheader(vfile->vpage)->addr!=(addr&~(long)(PGSIZE-1)))
 {
 vunlock(vfile->vpage);
 vfile->vpage=vlock(vfile,addr&~(long)(PGSIZE-1));
 }
vfile->bufp=vfile->vpage+(addr&(PGSIZE-1));
vfile->left=vfile->vpage+PGSIZE-vfile->bufp;
if(vheader(vfile->vpage)->addr+PGSIZE>vfile->alloc)
 vfile->lv=PGSIZE-(vfile->alloc-vheader(vfile->vpage)->addr);
else vfile->lv=0;
}

int _vrgetc(vfile)
VFILE *vfile;
{
if(vtell(vfile)==0) return MAXINT;
vseek(vfile,vtell(vfile)-1);
++vfile->bufp;
--vfile->left;
return vrgetc(vfile);
}

int _vgetc(vfile)
VFILE *vfile;
{
if(vtell(vfile)==vsize(vfile)) return MAXINT;
vseek(vfile,vtell(vfile));
return vgetc(vfile);
}

int nmvgetc(v) VFILE *v; { return vgetc(v); } 

int _vputc(vfile,c)
VFILE *vfile;
char c;
{
vseek(vfile,vtell(vfile));
return vputc(vfile,c);
}

short vgetw(vfile)
VFILE *vfile;
{
short w;
if(vtell(vfile)+2>vsize(vfile)) return -1;
w=vgetc(vfile);
w+=((short)vgetc(vfile)<<8);
return w;
}

short vputw(vfile,w)
VFILE *vfile;
short w;
{
vputc(vfile,w);
vputc(vfile,w>>8);
return w;
}

long vgetl(vfile)
VFILE *vfile;
{
long w;
if(vtell(vfile)+4>vsize(vfile)) return -1;
w=vgetc(vfile);
w+=((long)vgetc(vfile)<<8);
w+=((long)vgetc(vfile)<<16);
w+=((long)vgetc(vfile)<<24);
return w;
}

long vputl(vfile,w)
VFILE *vfile;
long w;
{
vputc(vfile,w);
vputc(vfile,w>>8);
vputc(vfile,w>>16);
vputc(vfile,w>>24);
return w;
}

int _rc(vfile,addr)
VFILE *vfile;
long addr;
{
if(vfile->vpage1) vunlock(vfile->vpage1);
vfile->vpage1=vlock(vfile,vfile->addr=(addr&~(long)(PGSIZE-1)));
return rc(vfile,addr);
}

int _wc(vfile,addr,c)
VFILE *vfile;
long addr;
char c;
{
if(addr+1>vsize(vfile)) valloc(vfile,addr+1-vsize(vfile));
if(vfile->vpage1) vunlock(vfile->vpage1);
vfile->vpage1=vlock(vfile,vfile->addr=(addr&~(long)(PGSIZE-1)));
return wc(vfile,addr,c);
}

short rw(vfile,addr)
VFILE *vfile;
long addr;
{
short c;
if(addr+2>vsize(vfile)) return -1;
c=rc(vfile,addr);
c+=((short)rc(vfile,addr+1)<<8);
return c;
}

short ww(vfile,addr,c)
VFILE *vfile;
long addr;
short c;
{
if(addr+2>vsize(vfile)) valloc(vfile,addr+2-vsize(vfile));
wc(vfile,addr,c);
wc(vfile,addr+1,c>>8);
return c;
}

long rl(vfile,addr)
VFILE *vfile;
long addr;
{
long c;
if(addr+4>vsize(vfile)) return -1;
c=rc(vfile,addr);
c+=((long)rc(vfile,addr+1)<<8);
c+=((long)rc(vfile,addr+2)<<16);
c+=((long)rc(vfile,addr+3)<<24);
return c;
}

long wl(vfile,addr,c)
VFILE *vfile;
long addr;
long c;
{
if(addr+4>vsize(vfile)) valloc(vfile,addr+4-vsize(vfile));
wc(vfile,addr,c);
wc(vfile,addr+1,c>>8);
wc(vfile,addr+2,c>>16);
wc(vfile,addr+3,c>>24);
return c;
}

void vread(v,blk,size)
VFILE *v;
char *blk;
int size;
{
long addr=vtell(v);
char *src;
int x;
while(size)
 {
 src=vlock(v,addr);
 x=PGSIZE-(addr&(PGSIZE-1));
 if(x>=size)
  {
  vseek(v,addr+size);
  mcpy(blk,src,size);
  vunlock(src);
  return;
  }
 size-=x;
 addr+=x;
 mcpy(blk,src,x);
 blk+=x;
 vunlock(src);
 }
vseek(v,addr);
}

void vwrite(v,blk,size)
VFILE *v;
char *blk;
int size;
{
long addr=vtell(v);
char *src;
int x;
if(addr+size>vsize(v)) valloc(v,addr+size-vsize(v));
while(size)
 {
 src=vlock(v,addr);
 x=PGSIZE-(addr&(PGSIZE-1));
 if(x>=size)
  {
  vseek(v,addr+size);
  mcpy(src,blk,size);
  vchanged(src);
  vunlock(src);
  return;
  }
 size-=x;
 addr+=x;
 mcpy(src,blk,x);
 blk+=x;
 vchanged(src);
 vunlock(src);
 }
vseek(v,addr);
}

/* Write string to vfile */

void vputs(v,s)
VFILE *v;
char *s;
{
while(*s) vputc(v,*s), ++s;
}

/* Read a line from a file */

char *vgets(v,s)
VFILE *v;
char *s;
{
int c;
int x=0;
if(!sSIZ(s)) s=vstrunc(s,0);
while(c=vgetc(v), c!= MAXINT && c!='\n')
 {
 if(x==sSiz(s)) s=vsensure(s,x+10);
 s[x++]=c;
 }
if(x==sSiz(s)) s=vsensure(s,x+1);
sLen(s)=x;
s[x]=0;
return s;
}
