/* Fast space-efficient heap manager
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

#include "blocks.h"
#include "random.h"
#include "heap.h"

#define MAXSIZE (MAXINT-16)

/* A free block of memory */

typedef struct freeblck FREE;
struct freeblck
 {
 int size;		/* Includes self, +1 for allocated blocks */
 			/* Equals '1' for end of heap or segment */
 FREE *next;		/* Data part of allocated blocks begins here */
 FREE *prev;
 };

static int reent=0;	/* Set if freeit is reentering itself */
void *sbrk();		/* Operating system memory allocator */
void (*mtrap)()=0;	/* Memory overflow handler */
static trapflag=0;	/* Set while handling an overflow condition */

static char *heapend=0;	/* Top of heap */

/* A skiplist entry */

struct skipnode
 {
 int key;
 int nptrs;
 FREE *value;
 struct skipnode *ptrs[1];
 };

/* Max number of pointers in a skiplist node */
#define SKIPDPTH 16

static struct skipnode *freeones[SKIPDPTH+1];	/* Free lists of skip nodes */
static struct skipnode *top=0;			/* Skip list root node */
static struct skipnode nil;			/* Ending skiplist node */
static struct skipnode *update[SKIPDPTH];	/* Record of search path */

/* Memory allocator for skip-list nodes */

static char *big=0;
static int bigx=0;

/* Allocation amount for skiplist data */
#define SKIPCHNK 256

/* Key is for recomputing skip-list search after a reentrant malloc */

static struct skipnode *skipa(x,key)
int x, key;
{
struct skipnode *t;
int amnt;
if(freeones[x])
 {
 t=freeones[x];
 freeones[x]=(struct skipnode *)t->value;
 return t;
 }
amnt=sizeof(struct skipnode)+sizeof(struct skipnode *)*(x-1);
if(bigx+amnt>SKIPCHNK || !big)
 {
 reent=1;
 big=(char *)malloc(SKIPCHNK);
 if(key)
  {
  struct skipnode *t=top;
  int y;
  for(y=top->nptrs;--y>=0;)
   {
   while(t->ptrs[y]->key<key) t=t->ptrs[y];
   update[y]=t;
   }
  }
 reent=0;
 bigx=0;
 }
t=(struct skipnode *)(big+bigx);
bigx+=amnt;
return t;
}

/* Free a skip node */

static void freea(t)
struct skipnode *t;
{
t->value=(FREE *)freeones[t->nptrs];
freeones[t->nptrs]=t;
}

/* Insert a free block into the free-list */

static void freeit(b)
FREE *b;
{
struct skipnode *t=top;
int x;
b=normalize(b);
for(x=top->nptrs;--x>=0;)
 {
 while(t->ptrs[x]->key<b->size) t=t->ptrs[x];
 update[x]=t;
 }
t=t->ptrs[0];
if(t->key==b->size)
 {
 b->next=t->value;
 b->prev=0;
 t->value->prev=b;
 t->value=b;
 }
else
 {
 int level;
 for(level=1;random()<0x29000000;++level);
 if(level>15) level=15;
 b->size+=1;
 t=(struct skipnode *)skipa(level,b->size-1);
 b->size-=1;
 while(level>top->nptrs) update[top->nptrs++]=top;
 t->nptrs=level;
 for(x=0;x!=level;++x) t->ptrs[x]=update[x]->ptrs[x], update[x]->ptrs[x]=t;
 t->key=b->size;
 t->value=b;
 b->next=0; b->prev=0;
 }
}

/* Remove an element from the free list */

static void unfreeit(b)
FREE *b;
{
struct skipnode *t;
int y;
if(b->prev)
 {
 if(b->prev->next=b->next) b->next->prev=b->prev;
 return;
 }
t=top;
for(y=top->nptrs;--y>=0;)
 {
 while(t->ptrs[y]->key<b->size) t=t->ptrs[y];
 update[y]=t;
 }
t=t->ptrs[0];
if(b->next) b->next->prev=0, t->value=b->next;
else
 {
 int x;
 for(x=0;x!=top->nptrs && update[x]->ptrs[x]==t;++x)
  update[x]->ptrs[x]=t->ptrs[x];
 while(top->nptrs && top->ptrs[top->nptrs-1]==&nil) --top->nptrs;
 freea(t);
 }
}

void *malloc(size)
{
int x;
int flg=0, y;
FREE *b;
struct skipnode *t;
if(!top)
 {
 nil.key=MAXINT;
 nil.nptrs=0;
 nil.ptrs[0]= &nil;
 top= &nil;
 top=(struct skipnode *)skipa(16,0);
 top->key=0;
 top->nptrs=0;
 for(x=0;x!=16;++x) top->ptrs[x]= &nil;
 }
size+=sizeof(int);
if(size<sizeof(FREE)) size=sizeof(FREE);
size=(size+sizeof(int)-1)&~(sizeof(int)-1);
loop:
b=0;
t=top;
for(y=top->nptrs;--y>=0;)
 {
 while(t->ptrs[y]->key<size) t=t->ptrs[y];
 update[y]=t;
 }
t=t->ptrs[0];
if(t->key!= MAXINT)
 {
 b=t->value;
 if(b->next) b->next->prev=0, t->value=b->next;
 else
  {
  for(x=0;x!=top->nptrs && update[x]->ptrs[x]==t;++x)
   update[x]->ptrs[x]=t->ptrs[x];
  while(top->nptrs && top->ptrs[top->nptrs-1]==&nil) --top->nptrs;
  freea(t);
  }
 }
if(b)
 {
 while(!(1&*(int *)((char *)b+b->size)))
  {
  FREE *c=(FREE *)((char *)b+b->size);
  if((long)b->size+(long)c->size>=MAXSIZE) break;
  b->size+=c->size;
  unfreeit(c);
  }
 if(normalize((char *)b+b->size)+sizeof(int)==heapend)
  {
  sbrk(-b->size);
  heapend=(char *)b;
  b->size=1;
  goto loop;
  }
 if((b->size-size)>=(size>>2) && b->size-size>=sizeof(FREE) && !reent)
  {
  FREE *c=(FREE *)((char *)b+size);
  c->size=b->size-size;
  b->size=size|1;
  freeit(c);
  }
 else b->size|=1;
 if(flg) trapflag=0;
 return (void *)((char *)b+sizeof(int));
 }
b=(FREE *)sbrk(size+sizeof(int));
if(!b || b==((FREE *)(long)-1))
 {
 if(mtrap && !trapflag)
  {
  trapflag=1;
  flg=1;
  mtrap(size);
  goto loop;
  }
 write(2,"\rOut of memory\r\n",16);
 _exit(1);
 }
b=normalize(b);
if(normalize(heapend)==(char *)b)
 {
 b=(FREE *)(heapend-sizeof(int));
 size+=sizeof(int);
 }
heapend=normalize((char *)b+size)+sizeof(int);
*((int *)heapend-1)=1;
b->size=size|1;
if(flg) trapflag=0;
return (void *)((char *)b+sizeof(int));
}

void *memalign(align,size)
int align,size;
{
FREE *b;
char *blk;
int off;
size+=sizeof(int);
if(size<sizeof(FREE)) size=sizeof(FREE);
size=(size+sizeof(int)-1)&~(sizeof(int)-1);
align=(align+sizeof(int)-1)&~(sizeof(int)-1);
if(!align) align=sizeof(int);
if(size>=align) blk=(char *)malloc(size+align+sizeof(FREE));
else blk=(char *)malloc(align*2+sizeof(FREE));
off=physical(blk)%align;
if(off)
 {
 b=(FREE *)(blk-sizeof(int));
 if(align-off>=sizeof(FREE)) blk+=align-off;
 else blk+=2*align-off;
 *((int *)blk-1)=b->size-(blk-sizeof(int)-(char *)b);
 b->size-= *((int *)blk-1);
 freeit(b);
 }
b=normalize((FREE *)(blk-sizeof(int)));
if((b->size-1-size)>=(size>>2) && b->size-1-size>=sizeof(FREE))
 {
 FREE *c=(FREE *)((char *)b+size);
 c->size=b->size-1-size;
 b->size=size+1;
 freeit(c);
 }
return (char *)b+sizeof(int);
}

void *realloc(blk,size)
void *blk;
int size;
{
FREE *b;
int osize;
if(!blk) return malloc(size);
b=(FREE *)((char *)blk-sizeof(int));
osize=b->size-1-sizeof(int);
size+=sizeof(int);
if(size<sizeof(FREE)) size=sizeof(FREE);
size=(size+sizeof(int)-1)&~(sizeof(int)-1);
while(!(1&*(int *)((char *)b+b->size-1)))
 {
 FREE *c=(FREE *)((char *)b+b->size-1);
 if((long)b->size+(long)c->size>=MAXSIZE) break;
 b->size+=c->size;
 unfreeit(c);
 }
if(size<=b->size-1)
 {
 if(b->size-1-size>size && b->size-1-size>=sizeof(FREE))
  {
  FREE *c=(FREE *)((char *)b+size);
  c->size=b->size-size-1;
  b->size=size+1;
  freeit(c);
  }
 return blk;
 }
else
 {
 char *n=(char *)malloc(size);
 FREE *c=(FREE *)(n-sizeof(int));
 if(normalize((char *)c+c->size-1)+sizeof(int)==heapend &&
    (FREE *)normalize(((char *)b+b->size-1))==c)
  {
  sbrk(-(c->size-1+b->size-1-size));
  b->size=size+1;
  heapend=normalize((char *)b+b->size-1)+sizeof(int);
  *((int *)heapend-1)=1;
  return blk;
  }
 mcpy(n,blk,osize);
 free(blk);
 return (void *)n;
 }
}

void free(blk)
void *blk;
{
FREE *b;
if(!blk) return;
b=(FREE *)((char *)blk-sizeof(int));
b->size-=1;
while(!(1&*(int *)((char *)b+b->size)))
 {
 FREE *c=(FREE *)((char *)b+b->size);
 if((long)b->size+(long)c->size>=MAXSIZE) break;
 b->size+=c->size;
 unfreeit(c);
 }
if(normalize((char *)b+b->size)+sizeof(int)==heapend)
 {
 heapend=(char *)b+sizeof(int);
 sbrk(-b->size);
 b->size=1;
 }
else freeit(b);
}

void *calloc(a,b)
int a,b;
{
return (void *)mset((char *)malloc(a*b),0,a*b);
}
