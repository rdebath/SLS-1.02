/* User edit functions
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
#include "config.h"
#include "tty.h"
#include "b.h"
#include "w.h"
#include "termcap.h"
#include "vfile.h"
#include "toomany.h"
#include "scrn.h"
#include "vs.h"
#include "bw.h"
#include "pw.h"
#include "tw.h"
#include "zstr.h"
#include "main.h"
#include "edfuncs.h"

B *filehist=0;
B *filthist=0;
B *linehist=0;

char *msgs[]=
{
"Error writing file",
"Error opening file",
"Error seeking file",
"Error reading file",
"New File"
};

/****************/
/* Window stuff */
/****************/

void uprevw(w)
W *w;
{
wprev(w->t);
}

void unextw(w)
W *w;
{
wnext(w->t);
}

void ugroww(w)
W *w;
{
wgrow(w);
}

void ushrnk(w)
W *w;
{
wshrink(w);
}

void uexpld(w)
W *w;
{
if(w->t->h-w->t->wind==getgrouph(w)) wshowall(w->t);
else wshowone(w);
}

/***************/
/* Block stuff */
/***************/

void umarkb(w)
W *w;
{
BW *bw=(BW *)w->object;
pdupown(bw->cursor,&w->t->markb);
updall();
}

void umarkk(w)
W *w;
{
BW *bw=(BW *)w->object;
pdupown(bw->cursor,&w->t->markk);
updall();
}

void ublkdel(w)
W *w;
{
BW *bw=(BW *)w->object;
if(w->t->markb && w->t->markb->b==bw->b &&
   (!w->t->markk || w->t->markk->b!=bw->b))
 if(bw->cursor->byte<w->t->markb->byte)
  w->t->markk=w->t->markb, w->t->markk->owner=&w->t->markk,
  pdupown(bw->cursor,&w->t->markb);
 else if(bw->cursor->byte>w->t->markb->byte) pdupown(bw->cursor,&w->t->markk);
 else prm(w->t->markb), prm(w->t->markk);
if(w->t->markb && w->t->markk)
 if(w->t->markb->b==w->t->markk->b)
  {
  bdel(w->t->markb,w->t->markk);
  prm(w->t->markb);
  prm(w->t->markk);
  return;
  }
msgnw(w,"No block");
}

void ublkmove(w)
W *w;
{
BW *bw=(BW *)w->object;
long size;
if(w->t->markb && w->t->markk && w->t->markb->b==w->t->markk->b &&
   (size=w->t->markk->byte-w->t->markb->byte)>0 &&
   (bw->cursor->b!=w->t->markk->b ||
    bw->cursor->byte>w->t->markk->byte || bw->cursor->byte<w->t->markb->byte))
 {
 binsb(bw->cursor,w->t->markb,w->t->markk);
 ublkdel(w);
 umarkb(w);
 umarkk(w);
 pfwrd(w->t->markk,size);
 updall();
 return;
 }
msgnw(w,"No block");
}


void ublkcpy(w)
W *w;
{
BW *bw=(BW *)w->object;
long size;
if(w->t->markb && w->t->markb->b==bw->b &&
   (!w->t->markk || w->t->markk->b!=bw->b))
 if(bw->cursor->byte<w->t->markb->byte)
  w->t->markk=w->t->markb, w->t->markk->owner=&w->t->markk,
  pdupown(bw->cursor,&w->t->markb);
 else if(bw->cursor->byte>w->t->markb->byte) pdupown(bw->cursor,&w->t->markk);
 else prm(w->t->markb), prm(w->t->markk);
if(w->t->markb && w->t->markk && w->t->markb->b==w->t->markk->b &&
   (size=w->t->markk->byte-w->t->markb->byte)>0)
 {
 binsb(bw->cursor,w->t->markb,w->t->markk);
 umarkb(w);
 umarkk(w);
 pfwrd(w->t->markk,size);
 updall();
 return;
 }
msgnw(w,"No block");
}

void ushell(w)
W *w;
{
nescape(w->t->t);
ttsusp();
nreturn(w->t->t);
}

void dowrite(w,s)
W *w;
char *s;
{
long size;
int fl;
if(w->t->markb && w->t->markk && w->t->markb->b==w->t->markk->b &&
   (size=w->t->markk->byte-w->t->markb->byte)>0)
 {
 if(fl=bsave(w->t->markb,s,size)) msgnw(w,msgs[5+fl]);
 }
else msgnw(w,"No block");
vsrm(s);
}

void ublksave(w)
W *w;
{
BW *bw=(BW *)w->object;
if(w->t->markb && w->t->markb->b==bw->b &&
   (!w->t->markk || w->t->markk->b!=bw->b))
 if(bw->cursor->byte<w->t->markb->byte)
  w->t->markk=w->t->markb, w->t->markk->owner=&w->t->markk,
  pdupown(bw->cursor,&w->t->markb);
 else if(bw->cursor->byte>w->t->markb->byte) pdupown(bw->cursor,&w->t->markk);
 else prm(w->t->markb), prm(w->t->markk);
if(w->t->markb && w->t->markk && w->t->markb->b==w->t->markk->b &&
   (w->t->markk->byte-w->t->markb->byte)>0)
 {
 wmkfpw(w,"Name of file to write (^C to abort): ",&filehist,dowrite,"Names");
 return;
 }
msgnw(w,"No block");
}

long pindent();

void setindent(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p, *q;
long indent;
if(pblank(bw->cursor)) return;

p=pdup(bw->cursor);
q=pdup(p);
indent=pindent(p);

do
 if(!pprevl(p)) goto done;
 else pboln(p);
 while(pindent(p)>=indent && !pblank(p));
pnextl(p);
done:
pboln(p);
if(w->t->markb) prm(w->t->markb);
w->t->markb=p; p->owner=&w->t->markb;

do
 if(!pnextl(q)) break;
 while(pindent(q)>=indent && !pblank(q));
pfcol(q);

if(w->t->markk) prm(w->t->markk);
w->t->markk=q; q->owner=&w->t->markk;

updall();
}

void urindent(w)
W *w;
{
BW *bw=(BW *)w->object;
if(w->t->markb && w->t->markb->b==bw->b &&
   (!w->t->markk || w->t->markk->b!=bw->b))
 if(bw->cursor->byte<w->t->markb->byte)
  w->t->markk=w->t->markb, w->t->markk->owner=&w->t->markk,
  pdupown(bw->cursor,&w->t->markb);
 else if(bw->cursor->byte>w->t->markb->byte) pdupown(bw->cursor,&w->t->markk);
 else prm(w->t->markb), prm(w->t->markk);
if(!w->t->markb || !w->t->markk || w->t->markb->b!=w->t->markk->b ||
   bw->cursor->byte<w->t->markb->byte || bw->cursor->byte>w->t->markk->byte)
 {
 setindent(w);
 }
else
 {
 P *p=pdup(w->t->markb);
 while(p->byte<w->t->markk->byte)
  {
  pbol(p);
  binsc(p,' ');
  pnextl(p);
  }
 prm(p);
 }
}

void ulindent(w)
W *w;
{
BW *bw=(BW *)w->object;
if(w->t->markb && w->t->markb->b==bw->b &&
   (!w->t->markk || w->t->markk->b!=bw->b))
 if(bw->cursor->byte<w->t->markb->byte)
  w->t->markk=w->t->markb, w->t->markk->owner=&w->t->markk,
  pdupown(bw->cursor,&w->t->markb);
 else if(bw->cursor->byte>w->t->markb->byte) pdupown(bw->cursor,&w->t->markk);
 else prm(w->t->markb), prm(w->t->markk);
if(!w->t->markb || !w->t->markk || w->t->markb->b!=w->t->markk->b ||
   bw->cursor->byte<w->t->markb->byte || bw->cursor->byte>w->t->markk->byte)
 {
 setindent(w);
 }
else
 {
 P *p=pdup(w->t->markb);
 pbol(p);
 while(p->byte<w->t->markk->byte)
  {
  if(!pindent(p)) { prm(p); return; }
  pnextl(p);
  }
 pset(p,w->t->markb);
 pbol(p);
 while(p->byte<w->t->markk->byte)
  {
  P *q=pdup(p);
  pgetc(q);
  bdel(p,q);
  prm(q);
  pnextl(p);
  }
 prm(p);
 }
}

static void dofilt(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
int fr[2];
int fw[2];
char c;
pipe(fr);
pipe(fw);
nescape(w->t->t);
ttclsn();
if(!fork())
 {
 signrm();
 close(0);
 close(1);
 dup(fw[0]);
 dup(fr[1]);
 close(fw[0]);
 close(fr[1]);
 close(fw[1]);
 close(fr[0]);
 execl("/bin/sh","/bin/sh","-c",s,NULL);
 _exit(0);
 }
close(fr[1]);
close(fw[0]);
if(fork())
 {
 long szz;
 close(fw[1]);
 bdel(w->t->markb,w->t->markk);
 szz=w->t->markk->b->eof->byte;
 binsfd(w->t->markk,fr[0],MAXLONG);
 pfwrd(w->t->markk,w->t->markk->b->eof->byte-szz);
 close(fr[0]);
 wait(0);
 wait(0);
 }
else
 {
 bsavefd(w->t->markb,fw[1],w->t->markk->byte-w->t->markb->byte);
 close(fw[1]);
 _exit(0);
 }
vsrm(s);
ttopnn();
nreturn(w->t->t);
bw->cursor->xcol=bw->cursor->col;
}

void ufilt(w)
W *w;
{
BW *bw=(BW *)w->object;
if(w->t->markb && w->t->markb->b==bw->b &&
   (!w->t->markk || w->t->markk->b!=bw->b))
 if(bw->cursor->byte<w->t->markb->byte)
  w->t->markk=w->t->markb, w->t->markk->owner=&w->t->markk,
  pdupown(bw->cursor,&w->t->markb);
 else if(bw->cursor->byte>w->t->markb->byte) pdupown(bw->cursor,&w->t->markk);
 else prm(w->t->markb), prm(w->t->markk);
if(w->t->markb && w->t->markk && w->t->markb->b==w->t->markk->b &&
   (w->t->markk->byte-w->t->markb->byte)>0)
 {
 wmkpw(w,"Command to filter block through (^C to abort): ",&filthist,dofilt,NULL);
 return;
 }
msgnw(w,"No block");
}

/****************************/
/* File loading and storing */
/****************************/

static int backup(w)
W *w;
{
BW *bw=(BW *)w->object;
if(!bw->b->backup)
 {
 char *s=0;
 /* Create command string */
 s=vsncpy(s,0,sc("/bin/cp "));
 s=vsncpy(s,sLEN(s),sz(bw->b->name));
 s=vsadd(s,' ');
 s=vsncpy(s,sLEN(s),sz(bw->b->name));
 s=vsncpy(s,sLEN(s),sc("~ 2>/dev/null"));

 if(system(s))
  {
  msgnw(w,"Couldn't make backup file... file not saved");
  vsrm(s);
  return 1;
  }
 else
  {
  bw->b->backup=1;
  vsrm(s);
  return 0;
  }
 }
else return 0;
}

static int dosave(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
FILE *f;
int fl;
if(backup(w)) { vsrm(s); return 1; }
if(fl=bsave(bw->b->bof,s,bw->b->eof->byte))
 {
 msgnw(w,msgs[fl+5]);
 vsrm(s);
 return 1;
 }
else
 {
 bw->b->chnged=0;
 vsrm(s);
 return 0;
 }
}

void usave(w)
W *w;
{
BW *bw=(BW *)w->object;
W *pw=wmkfpw(w,"Name of file to save (^C to abort): ",&filehist,dosave,"Names");
if(bw->b->name)
 {
 BW *pbw=(BW *)pw->object;
 binss(pbw->cursor,bw->b->name);
 pset(pbw->cursor,pbw->b->eof);
 }
}

static void doedit(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
void *object=bw->object;
B *b=bfind(s);
if(!b)
 {
 b=bmk(ctab);
 if(!zlen(s))
  msgnwt(w,"New file ");
 else
  {
  int fl;
  if(fl=bload(b,s)) msgnwt(w,msgs[fl+5]);
  }
 }
bwrm(bw);
w->object=(void *)(bw=bwmk(w->t,b,w->x,w->y+1,w->w,w->h-1));
wredraw(w);
setoptions(bw,s);
bw->object=object;
vsrm(s);
}

void uedit(w)
W *w;
{
BW *bw=(BW *)w->object;
if(bw->b->count==1 && bw->b->chnged)
 {
 int c=query(w,"Loose changes to this file (y,n)? ");
 if(c!='y' && c!='Y') return;
 }
wmkfpw(w,"Name of file to edit (^C to abort): ",&filehist,doedit,"Names");
}

static void doinsf(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
int fl;
if(fl=binsf(bw->cursor,s)) msgnw(w,msgs[fl+5]);
vsrm(s);
bw->cursor->xcol=bw->cursor->col;
}

void uinsf(w)
W *w;
{
BW *bw=(BW *)w->object;
wmkfpw(w,"Name of file to insert (^C to abort): ",&filehist,doinsf,"Names");
}

extern char *exmsg;

static void doex(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
bw->b->name=zdup(s); 
if(dosave(w,s)) { free(bw->b->name); bw->b->name=0; return; }
exmsg=vsncpy(NULL,0,sc("File "));
exmsg=vsncpy(exmsg,sLEN(exmsg),sz(bw->b->name));
exmsg=vsncpy(exmsg,sLEN(exmsg),sc(" saved."));
wabort(w);
}

void uexsve(w)
W *w;
{
BW *bw=(BW *)w->object;
if(!bw->b->chnged)
 {
 exmsg=vsncpy(NULL,0,sc("File "));
 exmsg=vsncpy(exmsg,sLEN(exmsg),sz(bw->b->name));
 exmsg=vsncpy(exmsg,sLEN(exmsg),sc(" not changed so no updated needed."));
 wabort(w);
 return;
 }
if(bw->b->name)
 {
 if(dosave(w,vsncpy(NULL,0,sz(bw->b->name)))) return;
 exmsg=vsncpy(NULL,0,sc("File "));
 exmsg=vsncpy(exmsg,sLEN(exmsg),sz(bw->b->name));
 exmsg=vsncpy(exmsg,sLEN(exmsg),sc(" saved."));
 wabort(w);
 } 
else wmkfpw(w,"Name of file to save (^C to abort): ",&filehist,doex,"Names");
}

/*************/
/* Goto line */
/*************/

static void doline(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
long num=0;
sscanf(s,"%ld",&num);
if(num>=1) pline(bw->cursor,num-1), bw->cursor->xcol=bw->cursor->col;
else msgnw(w,"Invalid line number");
vsrm(s);
}

void uline(w)
W *w;
{
wmkpw(w,"Goto line (^C to abort): ",&linehist,doline,NULL);
}

/************************/
/* Basic edit functions */
/************************/

void uquote(w)
W *w;
{
BW *bw=(BW *)w->object;
int c=queryn(w,"Ctrl-");
if((c>=0x40 && c<=0x5F) || (c>='a' && c<='z')) c&=0x1F;
if(c=='?') c=127;
utype(w,c);
}

void uquote8(w)
W *w;
{
BW *bw=(BW *)w->object;
int c=queryn(w,"Meta-");
if(c=='`')
 {
 c=queryn(w,"Meta-Ctrl-");
 if((c>=0x40 && c<=0x5F) || (c>='a' && c<='z')) c&=0x1F;
 if(c=='?') c=127;
 }
c|=128;
utype(w,c);
}

void uretyp(w)
W *w;
{
BW *bw=(BW *)w->object;
nredraw(w->t->t);
}

P *pboi(p)
P *p;
{
pbol(p);
while(cwhite(brc(p))) pgetc(p);
return p;
}

int pisedge(p)
P *p;
{
P *q;
int c;
if(pisbol(p)) return 1;
if(piseol(p)) return 1;
q=pdup(p);
pboi(q);
if(q->byte==p->byte) goto yes;
if(cwhite(c=brc(p)))
 {
 pset(q,p); if(cwhite(prgetc(q))) goto no;
 if(c=='\t') goto yes;
 pset(q,p); pgetc(q);
 if(pgetc(q)==' ') goto yes;
 goto no;
 }
else
 {
 pset(q,p); c=prgetc(q);
 if(c=='\t') goto yes;
 if(c!=' ') goto no;
 if(prgetc(q)==' ') goto yes;
 goto no;
 }

yes: prm(q); return 1;
no:  prm(q); return 0;
}

void upedge(w)
W *w;
{
BW *bw=(BW *)w->object;
prgetc(bw->cursor);
while(!pisedge(bw->cursor)) prgetc(bw->cursor);
}

void unedge(w)
W *w;
{
BW *bw=(BW *)w->object;
pgetc(bw->cursor);
while(!pisedge(bw->cursor)) pgetc(bw->cursor);
}

void ubol(w)
W *w;
{
BW *bw=(BW *)w->object;
pbol(bw->cursor);
}

void ueol(w)
W *w;
{
BW *bw=(BW *)w->object;
peol(bw->cursor);
}

void ubof(w)
W *w;
{
BW *bw=(BW *)w->object;
pbof(bw->cursor);
}

void ueof(w)
W *w;
{
BW *bw=(BW *)w->object;
peof(bw->cursor);
}

void ultarw(w)
W *w;
{
BW *bw=(BW *)w->object;
prgetc(bw->cursor);
}


void urtarw(w)
W *w;
{
BW *bw=(BW *)w->object;
pgetc(bw->cursor);
}

void uprvwrd(w)
W *w;
{
BW *bw=(BW *)w->object;
int c, d;

/* Move to end of previous word or edge */
lp:
d=' ';
while(c=prgetc(bw->cursor),
      c!= MAXINT && !cword(c) && (!cwhitel(c) || cwhitel(d)))
 d=c; 
if(c==' ')
 {
 d=prgetc(bw->cursor); if(d!=MAXINT) pgetc(bw->cursor);
 if(!cwhitel(d)) { pgetc(bw->cursor); goto lp; }
 }
if(c!= MAXINT) pgetc(bw->cursor);


/* Move to beginning of current word */
while(cword(c=prgetc(bw->cursor)));
if(c!= MAXINT) pgetc(bw->cursor);
}

void unxtwrd(w)
W *w;
{
BW *bw=(BW *)w->object;
int c, d;
/* Move to start of next word or edge */
lp:
d=' ';
while(c=brc(bw->cursor),
      c!= MAXINT && !cword(c) && (!cwhitel(c) || cwhitel(d)))
 d=pgetc(bw->cursor);
if(c==' ')
 {
 pgetc(bw->cursor); d=brc(bw->cursor); prgetc(bw->cursor);
 if(!cwhitel(d)) goto lp;
 }

/* Move to end of current word */
while(c=brc(bw->cursor), cword(c)) pgetc(bw->cursor);
}

void utomatch(w)
W *w;
{
BW *bw=(BW *)w->object;
int c, f, dir, cnt, d;
P *p;
c=brc(bw->cursor);
f= MAXINT; dir=1;
if(c=='(') f=')';
if(c=='[') f=']';
if(c=='{') f='}';
if(c=='`') f='\'';
if(c=='<') f='>';
if(c==')') f='(', dir= -1;
if(c==']') f='[', dir= -1;
if(c=='}') f='{', dir= -1;
if(c=='\'') f='`', dir= -1;
if(c=='>') f='<', dir= -1;
if(f== MAXINT) return;
if(dir==1)
 {
 p=pdup(bw->cursor);
 cnt=0;
 pgetc(p);
 while(d=pgetc(p), d!= MAXINT)
  if(d==c) ++cnt;
  else if(d==f) if(!cnt--) break;
 if(d!= MAXINT)
  {
  prgetc(p);
  pset(bw->cursor,p);
  }
 prm(p);
 }
else
 {
 p=pdup(bw->cursor);
 cnt=0;
 while(d=prgetc(p), d!= MAXINT)
  if(d==c) ++cnt;
  else if(d==f) if(!cnt--) break;
 if(d!= MAXINT) pset(bw->cursor,p);
 prm(p);
 }
}

void uuparw(w)
W *w;
{
BW *bw=(BW *)w->object;
long col=bw->cursor->xcol;
pprevl(bw->cursor);
pboln(bw->cursor);
pcol(bw->cursor,bw->cursor->xcol=col);
}

void udnarw(w)
W *w;
{
BW *bw=(BW *)w->object;
long col=bw->cursor->xcol;
pnextl(bw->cursor);
pcol(bw->cursor,bw->cursor->xcol=col);
}

void scrup(w,n,flg)
W *w;
{
BW *bw=(BW *)w->object;
int scrollamnt=0;
int cursoramnt=0;
int x;
long col=bw->cursor->xcol;
if(bw->top->line>=n) scrollamnt=cursoramnt=n;
else if(bw->top->line) scrollamnt=cursoramnt=bw->top->line;
else
 if(flg) cursoramnt=bw->cursor->line;
 else if(bw->cursor->line>=n) cursoramnt=n;

for(x=0;x!=scrollamnt;++x) pprevl(bw->top);
pboln(bw->top);
for(x=0;x!=cursoramnt;++x) pprevl(bw->cursor);
pboln(bw->cursor); pcol(bw->cursor,bw->cursor->xcol=col);
if(scrollamnt && scrollamnt<bw->h)
 {
 nscrldn(w->t->t,bw->y,bw->y+bw->h,scrollamnt);
 scrldn(w->t->t->updtab,bw->y,bw->y+bw->h,scrollamnt);
 }
else if(scrollamnt)
 {
 scrldn(w->t->t->updtab,bw->y,bw->y+bw->h,bw->h);
 }
}

void scrdn(w,n,flg)
W *w;
{
BW *bw=(BW *)w->object;
int scrollamnt=0;
int cursoramnt=0;
int x;
long col=bw->cursor->xcol;

if(bw->top->b->eof->line<bw->top->line+bw->h)
 {
 cursoramnt=bw->top->b->eof->line-bw->cursor->line;
 if(!flg && cursoramnt>n) cursoramnt=n;
 }
else if(bw->top->b->eof->line-(bw->top->line+bw->h)>=n)
 cursoramnt=scrollamnt=n;
else
 cursoramnt=scrollamnt=bw->top->b->eof->line-(bw->top->line+bw->h)+1;

for(x=0;x!=scrollamnt;++x) pnextl(bw->top);
for(x=0;x!=cursoramnt;++x) pnextl(bw->cursor);
pcol(bw->cursor,bw->cursor->xcol=col);
if(scrollamnt && scrollamnt<bw->h)
 {
 int x;
 nscrlup(w->t->t,bw->y,bw->y+bw->h,scrollamnt);
 scrlup(w->t->t->updtab,bw->y,bw->y+bw->h,scrollamnt);
 }
else if(scrollamnt) scrlup(w->t->t->updtab,bw->y,bw->y+bw->h,bw->h);
}

int pgamnt= -1;

void upgup(w)
W *w;
{
BW *bw=(BW *)w->object;
if(pgamnt== -1) scrup(w,bw->h/2+(bw->h&1),1);
else if(pgamnt<bw->h) scrup(w,bw->h-pgamnt,1);
else scrup(w,1,1);
}

void upgdn(w)
W *w;
{
BW *bw=(BW *)w->object;
if(pgamnt== -1) scrdn(w,bw->h/2+(bw->h&1),1);
else if(pgamnt<bw->h) scrdn(w,bw->h-pgamnt,1);
else scrdn(w,1,1);
}

void uupslide(w)
W *w;
{
scrup(w,1,0);
}

void udnslide(w)
W *w;
{
scrdn(w,1,0);
}

void udelch(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
p=pdup(bw->cursor);
pgetc(p);
bdel(bw->cursor,p);
prm(p);
}

void ubacks(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
int c;
if(bw->overtype==1)
 {
 ultarw(w);
 return;
 }
if(bw->overtype==2)
 {
 c=prgetc(bw->cursor);
 if(piseol(bw->cursor) && cwhite(c)) pgetc(bw->cursor);
 else return;
 }
p=pdup(bw->cursor);
if((c=prgetc(bw->cursor))!= MAXINT)
 bdel(bw->cursor,p);
prm(p);
}

void udelw(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
int c;
p=pdup(bw->cursor);
c=brc(p);
if(cword(c))
 while(c=brc(p), cword(c)) pgetc(p);
else if(cwhitel(c))
 while(c=brc(p), cwhitel(c)) pgetc(p);
else pgetc(p);
bdel(bw->cursor,p);
prm(p);
}

void ubackw(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
int c;
p=pdup(bw->cursor);
c=prgetc(bw->cursor);
if(cword(c))
 {
 while(c=prgetc(bw->cursor), cword(c));
 if(c!= MAXINT) pgetc(bw->cursor);
 }
else if(cwhitel(c))
 {
 while(c=prgetc(bw->cursor), cwhitel(c));
 if(c!= MAXINT) pgetc(bw->cursor);
 }
bdel(bw->cursor,p);
prm(p);
}

void udelel(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
p=pdup(bw->cursor);
peol(p);
if(bw->cursor->byte==p->byte)
 {
 prm(p);
 udelch(w);
 }
else
 {
 bdel(bw->cursor,p);
 prm(p);
 }
}

void udelbl(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
p=pdup(bw->cursor);
pbol(p);
bdel(p,bw->cursor);
prm(p);
}

void udelln(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
pbol(bw->cursor);
p=pdup(bw->cursor);
pnextl(p);
bdel(bw->cursor,p);
prm(p);
}

void ucenter(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p=bw->cursor, *q;
long endcol, begcol, x;
int c;

peol(p);
while(!pisbol(p) && cwhite(c=prgetc(p)));
if(pisbol(p)) return;
pgetc(p); endcol=p->col;

pbol(p);
while(!piseol(p) && cwhite(c=pgetc(p)));
if(piseol(p)) return;
prgetc(p); begcol=p->col;

if(endcol-begcol>bw->rmargin+bw->lmargin) return;

q=pdup(p); pbol(q); bdel(q,p); prm(q);

for(x=0;x!=(bw->lmargin+bw->rmargin)/2-(endcol-begcol)/2;++x) binsc(p,' ');

if(!pnextl(p))
 {
 binsc(p,'\n');
 pgetc(p);
 }
}

/* Paragraph stuff */

/* Determine if line pointer is on is blank */

int pblank(p)
P *p;
{
P *q=pdup(p);
int rtval;
pbol(q);
while(cwhite(brc(q))) pgetc(q);
rtval=piseol(q);
prm(q);
return rtval;
}

/* Determine indentation level of line pointer is on */

long pindent(p)
P *p;
{
P *q=pdup(p);
long col;
pbol(q);
while(cwhite(brc(q))) pgetc(q);
col=q->col;
prm(q);
return col;
}

/* Move pointer to beginning of paragraph */

P *pbop(p)
P *p;
{
long indent;
pbol(p);
indent=pindent(p);
while(!pisbof(p))
 {
 long ind;
 pprevl(p); pboln(p);
 ind=pindent(p);
 if(pblank(p) || ind<indent)
  {
  pnextl(p);
  break;
  }
 if(ind>indent) break;
 }
return p;
}

/* Move pointer to end of paragraph */

P *peop(p)
P *p;
{
long indent;
pbol(p);
indent=pindent(p);
while(!piseof(p))
 {
 long ind;
 pnextl(p);
 ind=pindent(p);
 if(ind>indent || pblank(p)) break;
 if(ind<indent) indent=ind;
 }
if(piseof(p))  peol(p);
return p;
}

/* Wrap word */

void wrapword(p,indent)
P *p;
long indent;
{
int c;
while(!pisbol(p) && !cwhite(c=prgetc(p)));
if(!pisbol(p))
 {
 pgetc(p);
 binsc(p,'\n');
 pgetc(p);
 if(indent) while(indent--) binsc(p,' ');
 }
peol(p);
}

/* Reformat paragraph */

void uformat(w)
W *w;
{
BW *bw=(BW *)w->object;
long indent;
char *buf, *b;
int len;
long curoff;
int c;
P *p, *q;
p=pdup(bw->cursor); pbol(p);

if(pblank(p))
 {
 prm(p);
 return;
 }

pbop(p);
curoff=bw->cursor->byte-p->byte;
peop(bw->cursor);

if(bw->cursor->lbyte) binsc(bw->cursor,'\n'), pgetc(bw->cursor);

indent=pindent(p);
q=pdup(p); pnextl(q);
if(q->line!=bw->cursor->line) indent=pindent(q);
prm(q);
if(bw->lmargin>indent) indent=bw->lmargin;

buf=(char *)malloc(len=(bw->cursor->byte-p->byte));
brmem(p,buf,len);
bdel(p,bw->cursor);
prm(p);

/* text is in buffer.  insert it at cursor */

/* Do first line */
b=buf;
p=pdup(bw->cursor);

/* This improves speed when formatting a paragraph at end of file */
if(piseof(p))
 {
 binsc(bw->cursor,'\n');
 pgetc(bw->cursor);
 }

while(len--)
 {
 if(b-buf==curoff) pset(bw->cursor,p);
 c= *b++;
 if(c=='\n') { ++len; --b; break; }
 if(cwhite(c))
  {
  char *r=b;
  int rlen=len;
  int z;
  while(rlen--)
   {
   z=*r++;
   if(z=='\n') break;
   if(!cwhite(z)) goto ok;
   }
  ++len; --b; break;
  ok:;
  }
 binsc(p,c); pgetc(p);
 if(p->col>bw->rmargin && !cwhite(c))
  {
  wrapword(p,indent);
  break;
  }
 }

while(len>0)
 if(cwhitel(*b))
  {
  if(b[-1]=='.' || b[-1]=='?' || b[-1]=='!') binsc(p,' '), pgetc(p);
  binsc(p,' '); pgetc(p);
  while(len && cwhitel(*b))
   {
   if(b-buf==curoff) pset(bw->cursor,p);
   ++b, --len;
   }
  }
 else
  {
  if(b-buf==curoff) pset(bw->cursor,p);
  binsc(p,*b++); --len; pgetc(p);
  if(p->col>bw->rmargin) wrapword(p,indent);
  }

binsc(p,'\n');
prm(p);
free(buf);
}

void uinsc(w)
W *w;
{
BW *bw=(BW *)w->object;
binsc(bw->cursor,' ');
}

void utype(w,c)
W *w;
int c;
{
BW *bw=(BW *)w->object;
P *p;
if(pblank(bw->cursor))
 while(bw->cursor->col<bw->lmargin) binsc(bw->cursor,' '), pgetc(bw->cursor);
binsc(bw->cursor,c);
pgetc(bw->cursor);
if(bw->wordwrap && bw->cursor->col>bw->rmargin && !cwhite(c))
 wrapword(bw->cursor,bw->lmargin);
if(bw->overtype==1) udelch(w);
else if(bw->overtype==2 && !piseol(bw->cursor))
 {
 int d=brc(bw->cursor);
 if(c=='\t' && d=='\t' || c!='\t' && d!='\t') udelch(w);
 }
}

void urtn(w)
W *w;
{
BW *bw=(BW *)w->object;
P *p;
int c;
if(bw->overtype==2)
 {
 peol(bw->cursor);
 if(!piseof(bw->cursor))
  {
  pgetc(bw->cursor);
  return;
  }
 }
p=pdup(bw->cursor);
binsc(bw->cursor,'\n');
pgetc(bw->cursor);
if(bw->autoindent)
 {
 pbol(p);
 while(cwhite(c=pgetc(p))) binsc(bw->cursor,c), pgetc(bw->cursor);
 }
prm(p);
if(bw->overtype==1) udelch(w);
}

void uopen(w)
W *w;
{
BW *bw=(BW *)w->object;
P *q=pdup(bw->cursor);
urtn(w);
pset(bw->cursor,q);
prm(q);
}

/* Mode commands */

void uiindent(w)
W *w;
{
BW *bw=(BW *)w->object;
bw->autoindent= !bw->autoindent;
if(bw->autoindent) msgnw(w,"Autoindent enabled");
else msgnw(w,"Autoindent disabled");
}

void uiwrap(w)
W *w;
{
BW *bw=(BW *)w->object;
bw->wordwrap= !bw->wordwrap;
if(bw->wordwrap) msgnw(w,"Word wrap enabled");
else msgnw(w,"Word wrap disabled");
}

void uitype(w)
W *w;
{
BW *bw=(BW *)w->object;
if(++bw->overtype==3) bw->overtype=0;
if(bw->overtype==0) msgnw(w,"Insert mode");
else if(bw->overtype==2) msgnw(w,"Text file overtype mode");
else msgnw(w,"Binary file overtype mode (preserves size of file)");
}

void uimid(w)
W *w;
{
mid= !mid;
if(mid) msgnw(w,"Cursor will be recentered after a scroll");
else msgnw(w,"Cursor will not be recentered after a scroll");
}

void uiforce(w)
W *w;
{
force= !force;
if(force) msgnw(w,"Last line forced to have LF when file saved");
else msgnw(w,"Last line not forced to have LF");
}

void uictrl(w)
W *w;
{
dspattr= !dspattr;
if(dspattr) msgnw(w,"Underline & Inverse for Ctrl & Meta chars");
else msgnw(w,"^ & M- for Ctrl & Meta chars");
refigure();
updall();
}

void uiasis(w)
W *w;
{
dspasis= !dspasis;
if(dspasis) msgnw(w,"Characters above 127 displayed as-is");
else msgnw(w,"Characters above 127 remapped");
refigure();
updall();
}

void uistacol(w)
W *w;
{
stacol= !stacol;
}

void uistarow(w)
W *w;
{
starow= !starow;
}

static void dolmar(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
long v=bw->lmargin;
sscanf(s,"%ld",&v);
vsrm(s);
bw->lmargin=v;
}

void uilmargin(w)
W *w;
{
BW *bw=(BW *)w->object;
char buf[30];
sprintf(buf,"Left margin (%ld): ",bw->lmargin);
wmkpw(w,buf,NULL,dolmar,NULL);
}

static void dormar(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
long v=bw->rmargin;
sscanf(s,"%ld",&v);
vsrm(s);
bw->rmargin=v;
}

void uirmargin(w)
W *w;
{
BW *bw=(BW *)w->object;
char buf[30];
sprintf(buf,"Right margin (%ld): ",bw->rmargin);
wmkpw(w,buf,NULL,dormar,NULL);
}

static void dopgamnt(w,s)
W *w;
char *s;
{
BW *bw=(BW *)w->object;
long v=pgamnt;
sscanf(s,"%ld",&v);
vsrm(s);
if(v<-1) v= -1;
pgamnt=v;
}

void uipgamnt(w)
W *w;
{
char buf[80];
sprintf(buf,"Lines to keep for pgup/pgdn or -1 for 1/2 (%ld): ",pgamnt);
wmkpw(w,buf,NULL,dopgamnt,NULL);
}

/* Argument setting */

void uarg(w,c)
W *w;
{
char buf[30];
if(c>='1' && c<='9') w->t->arg=(c&0xF);
else w->t->arg=0;
sprintf(buf,"%d",w->t->arg); msgnw(w,buf);
while(c=edgetc(), c>='0' && c<='9')
 {
 w->t->arg=w->t->arg*10+(c&0xf);
 sprintf(buf,"%d",w->t->arg); msgnw(w,buf);
 }
if(c==3) w->t->arg=0;
else eungetc(c);
}
