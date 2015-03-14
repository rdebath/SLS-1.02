/* Buffer management
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
#include <pwd.h>
#include <errno.h>
#include "config.h"
#include "heap.h"
#include "blocks.h"
#include "w.h"
#include "tty.h"
#include "scrn.h"
#include "undo.h"
#include "vs.h"
#include "zstr.h"
#include "main.h"
#include "bw.h"
#include "b.h"

extern int errno;
int force=0;

char buffer[1024];

/********************/
/* Buffer GAP stuff */
/********************/

/* Get size of gap (amount of free space) */

#define GGAPSZ(hdr) ((hdr)->ehole-(hdr)->hole)

/* Get number of characters in gap buffer */

#define GSIZE(hdr) (SEGSIZ-GGAPSZ(hdr))

/* Set position of gap */

static void gstgap(hdr,ptr,ofst)
H *hdr;
char *ptr;
int ofst;
{
if(ofst>hdr->hole)
 mfwrd(ptr+hdr->hole,ptr+hdr->ehole,ofst-hdr->hole), vchanged(ptr);
else if(ofst<hdr->hole)
 mbkwd(ptr+hdr->ehole-(hdr->hole-ofst),ptr+ofst,hdr->hole-ofst), vchanged(ptr);
hdr->ehole=ofst+hdr->ehole-hdr->hole;
hdr->hole=ofst;
}

/* Insert a block */

static void ginsm(hdr,ptr,ofst,blk,size)
H *hdr;
char *ptr;
int ofst;
char *blk;
int size;
{
if(ofst!=hdr->hole) gstgap(hdr,ptr,ofst);
mcpy(ptr+hdr->hole,blk,size);
hdr->hole+=size;
vchanged(ptr);
}

/* Delete characters */

static void gdel(hdr,ptr,ofst,size)
H *hdr;
char *ptr;
int ofst;
int size;
{
if(ofst!=hdr->hole) gstgap(hdr,ptr,ofst);
hdr->ehole+=size;
}

/* Read block */

static void grmem(hdr,ptr,ofst,blk,size)
H *hdr;
char *ptr;
int ofst;
char *blk;
int size;
{
if(ofst<hdr->hole)
 if(size>hdr->hole-ofst)
  mcpy(blk,ptr+ofst,hdr->hole-ofst),
  mcpy(blk+hdr->hole-ofst,ptr+hdr->ehole,size-(hdr->hole-ofst));
 else mcpy(blk,ptr+ofst,size);
else mcpy(blk,ptr+ofst+hdr->ehole-hdr->hole,size);
}

/**************************************/
/* Header and text segment allocation */
/**************************************/

static long salloc(b)
B *b;
{
if(b->fretxt)
 {
 long addr=b->fretxt;
 long *adr=(long *)vlock(b->text,addr);
 b->fretxt= *adr;
 vunlock(adr);
 return addr;
 }
else return valloc(b->text,(long)SEGSIZ);
}

static H frhdrs={{&frhdrs,&frhdrs}};

static H *halloc()
{
if(qempty(H,link,&frhdrs))
 {
 H *h;
 int x;
 h=(H *)malloc(sizeof(H)*64);
 for(x=0;x!=64;++x) enquef(H,link,&frhdrs,h+x);
 }
return deque(H,link,frhdrs.link.next);
}

static void sfree(b,addr)
B *b;
long addr;
{
long *adr=(long *)vlock(b->text,addr);
*adr=b->fretxt;
vchanged(adr); vunlock(adr);
b->fretxt=addr;
}

/**********************/
/* Pointer allocation */
/**********************/

static P frptrs={{&frptrs,&frptrs}};

static P *palloc()
{
if(qempty(P,link,&frptrs))
 {
 P *h;
 int x;
 h=(P *)malloc(sizeof(P)*64);
 for(x=0;x!=64;++x) enquef(P,link,&frptrs,h+x);
 }
return deque(P,link,frptrs.link.next);
}

static void pfree(p)
P *p;
{
enquef(P,link,&frptrs,p);
}

/****************************/
/* Buffer creation/deletion */
/****************************/

/* Doubly linked list of buffers */

static B bufs={{&bufs,&bufs}};

B *bmk(ctab)
char **ctab;
{
B *new=(B *)malloc(sizeof(B));
new->ctab=ctab;
new->backup=1;
new->chnged=0;
new->count=1;
new->name=0;
new->fretxt=0;
new->bof=palloc();
izque(P,link,new->bof);
new->text=vtmp();
new->bof->b=new;
new->bof->owner=0;
new->bof->ofst=0;
new->bof->byte=0;
new->bof->line=0;
new->bof->col=0;
new->bof->xcol=0;
new->bof->lbyte=0;
new->bof->hdr=halloc();
new->bof->hdr->hole=0;
new->bof->hdr->ehole=SEGSIZ;
izque(H,link,new->bof->hdr);
new->bof->hdr->seg=salloc(new);
new->bof->ptr=vlock(new->text,new->bof->hdr->seg);
new->eof=pdup(new->bof);
undomk(new);
enquef(B,link,&bufs,new);
return new;
}

/* Find loaded file */

B *bfind(name)
char *name;
{
B *b;
for(b=bufs.link.next;b!=&bufs;b=b->link.next)
 if(b->name && !zcmp(name,b->name))
  {
  ++b->count;
  return b;
  }
return 0;
}

void brm(b)
B *b;
{
if(!--b->count)
 {
 undorm(b);
 splicef(H,link,&frhdrs,b->bof->hdr);
 while(!qempty(P,link,b->bof)) prm(b->bof->link.next);
 prm(b->bof);
 if(b->name) free(b->name);
 vclose(b->text);
 free(deque(B,link,b));
 }
}

/**********************/
/* Pointer management */
/**********************/

P *pset(n,p)
P *n, *p;
{
n->b=p->b;
n->ofst=p->ofst;
n->hdr=p->hdr;
if(n->ptr) vunlock(n->ptr); n->ptr=p->ptr; vupcount(n->ptr);
n->byte=p->byte;
n->line=p->line;
n->col=p->col;
n->xcol=p->xcol;
n->lbyte=p->lbyte;
return n;
}

P *pdup(p)
P *p;
{
P *n=palloc();
n->ptr=0;
n->owner=0;
enquef(P,link,p,n);
return pset(n,p);
}

P *pdupown(p,o)
P *p;
P **o;
{
P *n=palloc();
n->ptr=0;
n->owner=o;
enquef(P,link,p,n);
pset(n,p);
if(*o) prm(*o);
*o=n;
return n;
}

P *pbof(p)
P *p;
{
return pset(p,p->b->bof);
}

P *peof(p)
P *p;
{
return pset(p,p->b->eof);
}

void prm(p)
P *p;
{
if(!p) return;
if(p->owner) *p->owner=0;
vunlock(p->ptr);
pfree(deque(P,link,p));
}

int pisbof(p)
P *p;
{
return p->hdr==p->b->bof->hdr && !p->ofst;
}

int piseof(p)
P *p;
{
return p->hdr==p->b->bof->hdr->link.prev;
}

int piseol(p)
P *p;
{
if(p->hdr==p->b->bof->hdr->link.prev) return 1;
if(p->ofst>=p->hdr->hole)
 { if(p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole]=='\n') return 1; }
else if(p->ptr[p->ofst]=='\n') return 1;
return 0;
}

int pisbol(p)
P *p;
{
char c;
if(p->hdr==p->b->bof->hdr && !p->ofst) return 1;
c=prgetcn(p); pfwrdn(p,(long)1);
return c=='\n';
}

int pisbow(p)
P *p;
{
P *q=pdup(p);
int c=brc(p);
int d=prgetcn(q);
prm(q);
if((c>='a' && c<='z' || c>='A' && c<='Z' || c>='0' && c<='9') &&
 !(d>='a' && d<='z' || d>='A' && d<='Z' || d>='0' && d<='9')) return 1;
return 0;
}

int piseow(p)
P *p;
{
P *q=pdup(p);
int d=brc(q);
int c=prgetcn(q);
prm(q);
if((c>='a' && c<='z' || c>='A' && c<='Z' || c>='0' && c<='9') &&
 !(d>='a' && d<='z' || d>='A' && d<='Z' || d>='0' && d<='9')) return 1;
return 0;
}

int pnext(p)
P *p;
{
if(p->hdr==p->b->bof->hdr->link.prev) return 0;
p->hdr=p->hdr->link.next; p->ofst=0;
vunlock(p->ptr); p->ptr=vlock(p->b->text,p->hdr->seg);
return GSIZE(p->hdr)!=0;
}

int pprev(p)
P *p;
{
if(p->hdr==p->b->bof->hdr) return 0;
p->hdr=p->hdr->link.prev;
p->ofst=GSIZE(p->hdr);
vunlock(p->ptr); p->ptr=vlock(p->b->text,p->hdr->seg);
return p->ofst;
}

int pgetcn(p)
P *p;
{
unsigned char c;
if(p->ofst==GSIZE(p->hdr)) return MAXINT;
if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
else c=p->ptr[p->ofst];
if(++p->ofst==GSIZE(p->hdr)) pnext(p); 
return c;
}

int pgetc(p)
P *p;
{
unsigned char c;
if(p->ofst==GSIZE(p->hdr)) return MAXINT;
if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
else c=p->ptr[p->ofst];
if(++p->ofst==GSIZE(p->hdr)) pnext(p); 
++p->byte;
if(c=='\n') ++p->line, p->col=0, p->lbyte=0;
else
 {
 ++p->lbyte;
 if(c=='\t') p->col+=TABSIZ-p->col%TABSIZ;
 else p->col+=((dspattr||dspasis&&c>=160&&c<=254)?1:zlen(p->b->ctab[c]));
 }
return c;
}

P *pfwrdn(p,n)
P *p;
long n;
{
while(n>=GSIZE(p->hdr)-p->ofst)
 {
 int adj=GSIZE(p->hdr)-p->ofst;
 if(!adj) break;
 n-=adj;
 pnext(p);
 }
if(n>GSIZE(p->hdr)-p->ofst) return 0;
p->ofst+=n;
return p;
}

P *pfwrd(p,n)
P *p;
long n;
{
int c;
while(n--) if(pgetc(p)== MAXINT) return 0;
return p;
}

int prgetcn(p)
P *p;
{
unsigned char c;
if(!p->ofst) if(!pprev(p)) return MAXINT;
--p->ofst;
if(p->ofst>=p->hdr->hole)
 c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
else c=p->ptr[p->ofst];
return c;
}

int prgetc(p)
P *p;
{
unsigned char c;
if(!p->ofst) if(!pprev(p)) return MAXINT;
--p->ofst;
if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
else c=p->ptr[p->ofst];
--p->byte;
if(c=='\n')
 {
 --p->line;
 pfcol(p);
 }
else if(c=='\t') pfcol(p);
else
 {
 --p->lbyte;
 if(dspattr||dspasis&&c>=160&&c<=254) --p->col;
 else p->col-=zlen(p->b->ctab[c]);
 }
return c;
}

P *pbkwdn(p,n)
P *p;
long n;
{
while(n>p->ofst)
 {
 n-=p->ofst;
 if(!pprev(p)) break;
 }
if(n>p->ofst) return 0;
p->ofst-=n;
return p;
}

P *pbkwdf(p,n)
P *p;
long n;
{
while(n--)
 {
 if(!p->ofst) if(!pprev(p)) return 0;
 --p->ofst;
 --p->byte;
 if(p->ofst>=p->hdr->hole)
  {
  if('\n'==p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole])
  --p->line;
  }
 else if('\n'==p->ptr[p->ofst]) --p->line;
 }
return p;
}

P *pfcol(p)
P *p;
{
H *hdr=p->hdr;
int ofst=p->ofst;
pboln(p);
while(p->ofst!=ofst || p->hdr!=hdr) pgetc(p);
return p;
}

P *pbkwd(p,n)
P *p;
long n;
{
P *y=pbkwdf(p,n);
pfcol(p);
return y;
}

P *pbol(p)
P *p;
{
if(p->lbyte) pbkwdn(p,p->lbyte);
p->byte-=p->lbyte; p->lbyte=0; p->col=0;
return p;
}

P *pboln(p)
P *p;
{
if(pprevl(p)) pgetc(p);
p->col=0; p->lbyte=0;
return p;
}

P *peol(p)
P *p;
{
while(p->hdr!=p->b->bof->hdr->link.prev)
 {
 unsigned char c;
 if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
 else c=p->ptr[p->ofst];
 if(c=='\n') break;
 else
  {
  ++p->byte;
  ++p->ofst;
  ++p->lbyte;
  if(c=='\t') p->col+=TABSIZ-p->col%TABSIZ;
  else p->col+=((dspattr||dspasis&&c>=160&&c<=254)?1:zlen(p->b->ctab[c]));
  if(p->ofst==GSIZE(p->hdr)) pnext(p); 
  }
 }
return p;
}

P *pnextl(p)
P *p;
{
char c;
do
 {
 if(p->ofst==GSIZE(p->hdr)) if(!pnext(p)) return 0;
 if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
 else c=p->ptr[p->ofst];
 ++p->byte; ++p->ofst;
 }
 while(c!='\n');
++p->line;
p->col=0; p->lbyte=0;
if(p->ofst==GSIZE(p->hdr)) pnext(p);
return p;
}

P *pprevl(p)
P *p;
{
char c;
do
 {
 if(!p->ofst) if(!pprev(p)) return 0;
 --p->ofst; --p->byte;
 if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
 else c=p->ptr[p->ofst];
 }
 while(c!='\n');
--p->line;
return p;
}

P *pline(p,line)
P *p;
long line;
{
if(line>p->b->eof->line) { pset(p,p->b->eof); return p; }
if(line<Iabs(p->line-line)) pset(p,p->b->bof);
if(Iabs(p->b->eof->line-line)<Iabs(p->line-line)) pset(p,p->b->eof);
if(p->line==line) { pbol(p); return p; }
while(line>p->line) pnextl(p);
if(line<p->line)
 {
 while(line<p->line) pprevl(p);
 pboln(p);
 }
p->lbyte=0; p->col=0;
return p;
}

P *pcol(p,goalcol)
P *p;
long goalcol;
{
if(p->lbyte) pbol(p);
do
 {
 unsigned char c;
 int wid;
 if(p->ofst==GSIZE(p->hdr)) break;
 if(p->ofst>=p->hdr->hole) c=p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
 else c=p->ptr[p->ofst];
 if(c=='\n') break;
 if(c=='\t') wid=TABSIZ-p->col%TABSIZ;
 else wid=((dspattr||dspasis&&c>=160&&c<=254)?1:zlen(p->b->ctab[c]));
 if(p->col+wid>goalcol) break;
 if(++p->ofst==GSIZE(p->hdr)) pnext(p); 
 ++p->byte; ++p->lbyte; p->col+=wid;
 } while(p->col!=goalcol);
return p;
}

P *pfindrn(p,s,len)
P *p;
char *s;
{
P *q=pdup(p);
char *os;
int olen;
if(!len) return p;
q=pdup(p);
do
 {
 pset(q,p);
 os=s; olen=len;
 while(olen--) if(*os++!=pgetc(q)) break;
 if(olen== -1)
  {
  prm(q);
  return p;
  }
 } while(pbkwdf(p,1L));
prm(q);
return 0;
}

P *pfindrni(p,s,len)
P *p;
char *s;
{
P *q=pdup(p);
char *os;
int olen;
if(!len) return p;
q=pdup(p);
do
 {
 pset(q,p);
 os=s; olen=len;
 while(olen--) if(toup(*os++)!=toup(pgetc(q))) break;
 if(olen== -1)
  {
  prm(q);
  return p;
  }
 } while(pbkwdf(p,1L));
prm(q);
return 0;
}

P *pfindfn(p,s,len)
P *p;
char *s;
{
P *q=pdup(p);
char *os;
int olen;
int c;
if(!len) return p;
q=pdup(p);
while( (c=pgetc(p)) != MAXINT )
 if(c==s[0])
  {
  pset(q,p);
  os=s+1; olen=len-1;
  while(olen--) if(*os++!=pgetc(q)) break;
  if(olen== -1)
   {
   prm(q);
   prgetc(p);
   return p;
   }
  }
prm(q);
return 0;
}

P *pfindfni(p,s,len)
P *p;
char *s;
{
P *q=pdup(p);
char *os;
int olen;
int c;
if(!len) return p;
q=pdup(p);
while( (c=pgetc(p)) != MAXINT )
 if(toup(c)==toup(s[0]))
  {
  pset(q,p);
  os=s+1; olen=len-1;
  while(olen--) if(toup(*os++)!=toup(pgetc(q))) break;
  if(olen== -1)
   {
   prm(q);
   prgetc(p);
   return p;
   }
  }
prm(q);
return 0;
}

/******************************/
/* Getting data from a buffer */
/******************************/

int brc(p)
P *p;
{
if(p->hdr==p->b->bof->hdr->link.prev) return MAXINT;
if(p->ofst>=p->hdr->hole) return p->ptr[p->ofst+p->hdr->ehole-p->hdr->hole];
else return p->ptr[p->ofst];
}

char *brmem(p,blk,size)
P *p;
char *blk;
int size;
{
char *bk=blk;
P *np;
int amnt;
np=pdup(p);
while(size>(amnt=GSIZE(np->hdr)-np->ofst))
 {
 grmem(np->hdr,np->ptr,np->ofst,bk,amnt);
 bk+=amnt;
 size-=amnt;
 if(!pnext(np)) break;
 }
if(size<(amnt=GSIZE(np->hdr)-np->ofst)) amnt=size;
if(amnt) grmem(np->hdr,np->ptr,np->ofst,bk,amnt);
prm(np);
return blk;
}

char *brs(p,size)
P *p;
int size;
{
char *s=(char *)malloc(size+1);
s[size]=0;
return brmem(p,s,size);
}

char *brvs(p,size)
P *p;
int size;
{
char *s=vstrunc(NULL,size);
return brmem(p,s,size);
}

int bsavefd(p,fd,size)
P *p;
long size;
{
P *np=pdup(p);
int amnt;
while(size>(amnt=GSIZE(np->hdr)-np->ofst))
 {
 if(np->ofst<np->hdr->hole)
  {
  if(write(fd,np->ptr+np->ofst,np->hdr->hole-np->ofst)<0) return -5;
  if(write(fd,np->ptr+np->hdr->ehole,SEGSIZ-np->hdr->ehole)<0) return -5;
  }
 else if(write(fd,np->ptr+np->ofst+GGAPSZ(np->hdr),amnt)<0) return -5;
 size-=amnt;
 if(!pnext(np)) break;
 }
if(size<(amnt=GSIZE(np->hdr)-np->ofst)) amnt=size;
if(amnt)
 if(np->ofst<np->hdr->hole)
  if(amnt>np->hdr->hole-np->ofst)
   {
   if(write(fd,np->ptr+np->ofst,np->hdr->hole-np->ofst)<0) return -5;
   if(write(fd,np->ptr+np->hdr->ehole,amnt-np->hdr->hole+np->ofst)<0) return -5;
   }
  else
   {
   if(write(fd,np->ptr+np->ofst,amnt)<0) return -5;
   }
 else if(write(fd,np->ptr+np->ofst+GGAPSZ(np->hdr),amnt)<0) return -5;
prm(np);
return 0; /* Check status returned by fwrite */
}

char *parsens(s,skip,amnt)
char *s;
long *skip, *amnt;
{
char *n=vsncpy(NULL,0,sz(s));
int x,y;
*skip=0;
*amnt= MAXLONG;
for(x=sLEN(n)-1;x>0 && (n[x]>='0' && n[x]<='9' || n[x]=='x' || n[x]=='X');--x);
if(n[x]==',')
 {
 int c;
 n[x]=0;
 if(n[x+1]=='x' || n[x+1]=='X') sscanf(n+x+2,"%lx",skip);
 else if(n[x+1]=='0' && (n[x+2]=='x' || n[x+2]=='X')) sscanf(n+x+3,"%lx",skip);
 else if(n[x+1]=='0') sscanf(n+x+1,"%lo",skip);
 else sscanf(n+x+1,"%d",skip);
 for(--x;x>0 && (n[x]>='0' && n[x]<='9' || n[x]=='x' || n[x]=='X');--x);
 if(n[x]==',')
  {
  n[x]=0;
  *amnt= *skip;
  if(n[x+1]=='x' || n[x+1]=='X') sscanf(n+x+2,"%lx",skip);
  else if(n[x+1]=='0' && (n[x+2]=='x' || n[x+2]=='X')) sscanf(n+x+3,"%lx",skip);
  else if(n[x+1]=='0') sscanf(n+x+1,"%lo",skip);
  else sscanf(n+x+1,"%d",skip);
  }
 }
if(n[0]=='~')
 {
 for(x=1;n[x] && n[x]!='/';++x);
 if(n[x]=='/')
  if(x==1)
   {
   char *z;
   s=getenv("HOME");
   z=vsncpy(NULL,0,sz(s));
   z=vsncpy(z,sLEN(z),sz(n+x));
   vsrm(n);
   n=z;
   }
  else
   {
   struct passwd *passwd;
   n[x]=0;
   passwd=getpwnam(n+1);
   n[x]='/';
   if(passwd)
    {
    char *z=vsncpy(NULL,0,sz(passwd->pw_dir));
    z=vsncpy(z,sLEN(z),sz(n+x));
    vsrm(n);
    n=z;
    }
   }
 }
return n;
}

int bsave(p,s,size)
P *p;
char *s;
long size;
{
FILE *f;
int flg;
long skip,amnt;
s=parsens(s,&skip,&amnt);
if(amnt<size) size=amnt;
if(s[0]=='!')
 {
 nescape(maint->t);
 ttclsn();
 f=popen(s+1,"w");
 }
else if(s[0]=='>' && s[1]=='>') f=fopen(s+2,"a");
else if(!zcmp(s,"-"))
 {
 nescape(maint->t);
 ttclsn();
 f=stdout;
 }
else
 if(skip || amnt!=MAXLONG) f=fopen(s,"r+");
 else f=fopen(s,"w");
if(!f)
 {
 if(s[0]=='!') ttopnn(), nreturn(maint->t);
 return -4;
 }
fflush(f);

if(skip && lseek(fileno(f),skip,0)<0) { flg= -3; goto err; }

flg=bsavefd(p,fileno(f),size);
if(force && size && !skip && amnt==MAXINT)
 {
 P *q=pdup(p);
 char nl='\n';
 pfwrdn(p,size-1);
 if(brc(p)!='\n')
  if(write(fileno(f),&nl,1)<0) flg= -5;
 }
err:;
if(s[0]=='!') pclose(f);
else if(zcmp(s,"-")) fclose(f);
else fflush(f);
if(s[0]=='!' || !zcmp(s,"-")) ttopnn(), nreturn(maint->t);
return flg;
}

/*******************************/
/* Deleting data from a buffer */
/*******************************/

static void fixup(p,amnt,nlines,hdr,hdramnt)
P *p;
long amnt;
long nlines;
H *hdr;
int hdramnt;
{
P *pp;
int flg;
if(!p->lbyte) flg=0;
else flg=1;
scrdel(p->b,p->line,nlines,flg);
for(pp=p->link.next;pp!=p;pp=pp->link.next)
 if(pp->byte>=p->byte)
  if(pp->byte<=p->byte+amnt) pset(pp,p);
  else
   {
   if(pp->hdr==hdr) pp->ofst-=hdramnt;
   if(pp->line==p->line+nlines) pp->col=~(long)0;
   pp->byte-=amnt;
   pp->line-=nlines;
   }
for(pp=p->link.next;pp!=p;pp=pp->link.next)
 if(pp->col==~(long)0) pfcol(pp);
p->b->chnged=1;
}

static void frchn(b,h)
B *b;
H *h;
{
H *a;
a=h; do
 sfree(b,a->seg),
 a=a->link.next;
 while(a!=h);
splicef(H,link,&frhdrs,h);
}

P *bdel(from,to)
P *from, *to;
{
long nlines=to->line-from->line;/* No. EOLs to delete */
long amnt=to->byte-from->byte;	/* No. bytes to delete */
int toamnt;			/* Amount delete from segment in 'to' */
if(from->byte==to->byte) return from;
undodel(from,amnt);
if(from->hdr==to->hdr)
 {
 gdel(from->hdr,from->ptr,from->ofst,(int)amnt);
 toamnt=amnt;
 }
else
 {
 H *a;
 toamnt=to->ofst;
 /* Delete beginning of to */
 gstgap(to->hdr,to->ptr,to->ofst);
 to->hdr->hole=0;

 /* Delete end of from */
 if(from->ofst)
  {
  a=from->hdr;
  gstgap(from->hdr,from->ptr,from->ofst);
  from->hdr->ehole=SEGSIZ;
  }
 else a=from->hdr->link.prev;

 /* From now points to header/segment of to */
 from->hdr=to->hdr;
 vunlock(from->ptr); from->ptr=to->ptr; vupcount(to->ptr);
 from->ofst=0;

 /* Delete headers/segments between a and to->hdr */
 if(a->link.next!=to->hdr)
  frchn(to->b,snip(H,link,a->link.next,to->hdr->link.prev));
 }

fixup(from,amnt,nlines,to->hdr,toamnt);
return from;
}

/********************************/
/* Inserting data into a buffer */
/********************************/

/* Split a block at p's ofst */
/* p is placed in the new block such that it points to the same text but with
 * p->ofst==0
 */

static void bsplit(p)
P *p;
{
if(p->ofst)
 {
 H *hdr;
 char *ptr;
 P *pp;

 hdr=halloc();
 hdr->seg=salloc(p->b);
 ptr=vlock(p->b->text,hdr->seg);

 gstgap(p->hdr,p->ptr,p->ofst);
 mcpy(ptr,p->ptr+p->hdr->ehole,SEGSIZ-p->hdr->ehole);
 vchanged(ptr);
 hdr->hole=SEGSIZ-p->hdr->ehole;
 hdr->ehole=SEGSIZ;
 p->hdr->ehole=SEGSIZ;

 enquef(H,link,p->hdr,hdr);
 
 vunlock(p->ptr);

 for(pp=p->link.next;pp!=p;pp=pp->link.next)
  if(pp->hdr==p->hdr && pp->ofst>=p->ofst)
   {
   pp->hdr=hdr;
   vunlock(pp->ptr); pp->ptr=ptr; vupcount(ptr);
   pp->ofst-=p->ofst;
   }

 p->ptr=ptr;
 p->hdr=hdr;
 p->ofst=0;
 }
}

static void inschn(p,a)
P *p;
H *a;
{
bsplit(p);
p->hdr=spliceb(H,link,p->hdr,a);
vunlock(p->ptr); p->ptr=vlock(p->b->text,a->seg);
}

static H *bldchn(b,blk,size)
B *b;
char *blk;
int size;
{
H anchor, *l;
izque(H,link,&anchor);
do
 {
 char *ptr;
 int amnt;
 ptr=vlock(b->text,(l=halloc())->seg=salloc(b));
 if(size>SEGSIZ) amnt=SEGSIZ;
 else amnt=size;
 mcpy(ptr,blk,amnt);
 vchanged(ptr); vunlock(ptr);
 l->hole=amnt; l->ehole=SEGSIZ;
 enqueb(H,link,&anchor,l);
 blk+=amnt; size-=amnt;
 }
 while(size);
l=anchor.link.next;
deque(H,link,&anchor);
return l;
}

static void fixup1(p,amnt,nlines,hdr,hdramnt)
P *p;
long amnt;
long nlines;
H *hdr;
int hdramnt;
{
P *pp;
int flg;
if(!p->lbyte) flg=0;
else flg=1;
scrins(p->b,p->line,nlines,flg);
for(pp=p->link.next;pp!=p;pp=pp->link.next)
 if(pp->byte==p->byte && pp!=p->b->eof) pset(pp,p);
 else if(pp->byte>p->byte || pp==p->b->eof)
  {
  pp->byte+=amnt;
  if(pp->line==p->line) pp->col=~(long)0;
  pp->line+=nlines;
  if(pp->hdr==hdr) pp->ofst+=hdramnt;
  }
for(pp=p->link.next;pp!=p;pp=pp->link.next) if(pp->col==~(long)0) pfcol(pp);
undoins(p,amnt);
p->b->chnged=1;
}

P *binsm(p,blk,amnt)
P *p;
char *blk;
int amnt;
{
long nlines=mcnt(blk,'\n',amnt);
H *h=0;
int hdramnt;
if(p->hdr!=p->b->eof->hdr && amnt<=GGAPSZ(p->hdr))
 {
 h=p->hdr;
 hdramnt=amnt;
 ginsm(p->hdr,p->ptr,p->ofst,blk,amnt);
 }
else if(p->hdr==p->b->eof->hdr &&
        p->b->bof->hdr != p->b->eof->hdr &&
        amnt<=GGAPSZ(p->hdr->link.prev))
 {
 pprev(p); 
 ginsm(p->hdr,p->ptr,p->ofst,blk,amnt);
 }
else
 {
 H *a=bldchn(p->b,blk,amnt);
 inschn(p,a);
 }
fixup1(p,(long)amnt,nlines,h,hdramnt);
return p;
}

P *binsc(p,c)
P *p;
char c;
{
return binsm(p,&c,1);
}

P *binss(p,s)
P *p;
char *s;
{
return binsm(p,s,zlen(s));
}

P *binsb(p,from,to)
P *p, *from, *to;
{
P *dp=pdup(p);
P *sp=pdup(from);
int amnt;
if(p->b==from->b && p->byte>=from->byte && p->byte<to->byte)
 {
 P *mid=pdup(dp);
 while(mid->byte-sp->byte>=1024)
  {
  brmem(sp,buffer,1024);
  binsm(dp,buffer,1024);
  pfwrdn(sp,(long)1024), sp->byte+=1024;
  pfwrdn(dp,(long)1024), dp->byte+=1024;
  }
 if(amnt=mid->byte-sp->byte)
  {
  brmem(sp,buffer,amnt);
  binsm(dp,buffer,amnt);
  pfwrdn(sp,(long)amnt), sp->byte+=amnt;
  pfwrdn(dp,(long)amnt), dp->byte+=amnt;
  }
 prm(mid);
 pset(sp,dp);
 pset(dp,to);
 }
while(to->byte-sp->byte>=1024)
 {
 brmem(sp,buffer,1024);
 binsm(dp,buffer,1024);
 pfwrdn(sp,(long)1024), sp->byte+=1024;
 pfwrdn(dp,(long)1024), dp->byte+=1024;
 }
if(amnt=to->byte-sp->byte)
 {
 brmem(sp,buffer,amnt);
 binsm(dp,buffer,amnt);
 }
prm(dp);
prm(sp);
return p;
}

static int bkread(fi,buff,size)
char *buff;
int size;
{
int a,b;
if(!size) return -1;
for(a=b=0;(a<size) && ((b=read(fi,buff+a,size-a))>0);a+=b);
return (b<0) ? -1 : a;
}

static H *rdchn(b,fi,linesp,totala,max)
B *b;
long *linesp;
long *totala;
long *max;
{
H anchor, *l;
long lines=0, total=0;
char *ptr;
int amnt;
long vseg;
char *seg;
izque(H,link,&anchor);
while((amnt=bkread(fi,seg=vlock(b->text,vseg=salloc(b)),*max>=SEGSIZ?SEGSIZ:(int)*max))>0)
 {
 total+=amnt;
 *max-=amnt;
 lines+=mcnt(seg,'\n',amnt);
 vchanged(seg); vunlock(seg);
 l=halloc();
 l->seg=vseg;
 l->hole=amnt;
 l->ehole=SEGSIZ;
 enqueb(H,link,&anchor,l);
 }
sfree(b,vseg);
vunlock(seg);
if(!total) return 0;
*linesp=lines;
*totala=total;
l=anchor.link.next;
deque(H,link,&anchor);
return l;
}

int binsfd(p,fd,max)
P *p;
long max;
{
long nlines;
long amnt;
int flg;
H *a;
if(a=rdchn(p->b,fd,&nlines,&amnt,&max))
 {
 inschn(p,a);
 fixup1(p,amnt,nlines,NULL,0);
 return 0;
 }
return -2;
}

int binsf(p,s)
P *p;
char *s;
{
FILE *fi;
int flg;
long skip,amnt;
s=parsens(s,&skip,&amnt);
if(s[0]=='!')
 {
 nescape(maint->t);
 ttclsn();
 fi=popen(s+1,"r");
 }
else if(!zcmp(s,"-")) fi=stdin;
else fi=fopen(s,"r");
if(!fi)
 {
 if(s[0]=='!') ttopnn(), nreturn(maint->t);
 if(errno==ENOENT) return -1;
 else return -4;
 }
if(skip)
 {
 if(lseek(fileno(fi),skip,0)<0)
  {
  int r;
  while(skip>1024)
   {
   r=read(fileno(fi),buffer,1024);
   if(r!= -1) skip-=r;
   else { flg= -3; goto err; }
   }
  while(skip)
   {
   r=read(fileno(fi),buffer,(int)skip);
   if(r!= -1) skip-=r;
   else { flg= -3; goto err; }
   }
  }
 }
flg=binsfd(p,fileno(fi),amnt);
err:;
if(s[0]=='!') pclose(fi);
else if(zcmp(s,"-")) fclose(fi);
if(s[0]=='!') ttopnn(), nreturn(maint->t);
vsrm(s);
return flg;
}

int bload(b,s)
B *b;
char *s;
{
int rtval;
long skip,amnt;
inundo=1;
rtval=binsf(b->bof,s);
inundo=0;
b->name=zdup(s);
if(rtval)
 {
 b->backup=1;
 b->chnged=0;
 return rtval;
 }
b->chnged=0;
b->backup=0;
s=parsens(s,&skip,&amnt);
if(!zcmp(s,"-")) b->backup=1, b->chnged=1;
else if(s[0]=='!') b->backup=1;
else if(skip || amnt!=MAXLONG) b->backup=1;
vsrm(s);
return rtval;
}

/* View chain */

void check(b)
B *b;
{
H *h;
for(h=b->bof->hdr->link.next;h!=b->bof->hdr;h=h->link.next)
 {
 printf("\r%8.8X: prev=%X next=%X seq=%lX hole=%d ehole=%d\r\n",
        h,h->link.prev,h->link.next,h->seg,h->hole,h->ehole);
 }
printf("\r%8.8X: prev=%X next=%X seq=%lX hole=%d ehole=%d\r\n",
       h,h->link.prev,h->link.next,h->seg,h->hole,h->ehole);
}

/* View pointers */

void checkp(b)
B *b;
{
P *p;
printf("\rPointers\r\n");
 
for(p=b->bof->link.next;p!=b->bof;p=p->link.next)
 {
 if(p==b->bof) printf("\rBof: ");
 else if(p==b->eof) printf("\rEof: ");
 else printf("\r");
  printf("Byte=%ld Hdr=%X Ofst=%d Line=%ld Col=%ld Lbyte=%ld\r\n",
	 p->byte,p->hdr,p->ofst,p->line,p->col,p->lbyte);
 }
if(p==b->bof) printf("\rBof: ");
else if(p==b->eof) printf("\rEof: ");
else printf("\r");
printf("Byte=%ld Hdr=%X Ofst=%d Line=%ld Col=%ld Lbyte=%ld\r\n",p->byte,p->hdr,p->ofst,p->line,p->col,p->lbyte);
}

/* Refigure column numbers */

void refigure()
{
B *b;
P *p;
for(b=bufs.link.next;b!=&bufs;b=b->link.next)
 {
 p=b->bof; do
  pfcol(p);
  while(p=p->link.next, p!=b->bof);
 }
}

/* Save edit buffers when editor dies */

void ttsig(sig)
{
long tim=time(0);
B *b;
FILE *f=fopen("DEADJOE","a");
fprintf(f,"\n*** Modified files in JOE when it aborted on %s",ctime(&tim));
if(sig) fprintf(f,"*** JOE was aborted by signal %d\n",sig);
else fprintf(f,"*** JOE was aborted because the terminal closed\n");
fflush(f);
for(b=bufs.link.next;b!=&bufs;b=b->link.next)
 if(b->chnged)
  {
  if(b->name) fprintf(f,"\n*** File \'%s\'\n",b->name);
  else fprintf(f,"\n*** File \'(Unnamed)\'\n",b->name);
   fflush(f);
  bsavefd(b->bof,fileno(f),b->eof->byte);
  }
_exit(1);
}
