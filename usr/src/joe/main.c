/* Editor startup and main edit loop
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
#include "vfile.h"
#include "blocks.h"
#include "tty.h"
#include "toomany.h"
#include "termcap.h"
#include "scrn.h"
#include "b.h"
#include "bw.h"
#include "tw.h"
#include "w.h"
#include "kbd.h"
#include "zstr.h"
#include "macro.h"
#include "tab.h"
#include "pw.h"
#include "edfuncs.h"
#include "poshist.h"
#include "pattern.h"
#include "help.h"
#include "vs.h"
#include "main.h"

/* Message to display when exiting the editor */

int help=0;

char *exmsg=0;

/* Main screen */

SCREEN *maint;

/* Command table */

#define EMID 1
#define ECHKXCOL 2
#define EFIXXCOL 4
#define EMINOR 8
#define EPOS 16
#define EMOVE 32

int typen;

static CMD cmds[]=
{
  { "abort", TYPETW, uaborttw },
  { "aborthelp", TYPEHELP, uhabort },
  { "abortpw", TYPEPW, uabortpw },
  { "aborttab", TYPETAB, tuabort },
  { "arg", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uarg },
  { "backs", TYPETW+TYPEPW+ECHKXCOL+EFIXXCOL+EMINOR, ubacks },
  { "backstab", TYPETAB, tbacks },
  { "backw", TYPETW+TYPEPW+ECHKXCOL+EFIXXCOL, ubackw },
  { "blkcpy", TYPETW+TYPEPW+0, ublkcpy },
  { "blkdel", TYPETW+TYPEPW+0, ublkdel },
  { "blkmove", TYPETW+TYPEPW+0, ublkmove },
  { "blksave", TYPETW+TYPEPW+0, ublksave },
  { "bof", TYPETW+TYPEPW+EMOVE+EFIXXCOL, ubof },
  { "bofhelp", TYPEHELP, uhbof },
  { "boftab", TYPETAB, tbof },
  { "bol", TYPETW+TYPEPW+EFIXXCOL, ubol },
  { "bolhelp", TYPEHELP, uhbol },
  { "boltab", TYPETAB, tbol },
  { "center", TYPETW+TYPEPW+EFIXXCOL, ucenter },
  { "check", TYPETW+TYPEPW+0, ucheck },
  { "checkp", TYPETW+TYPEPW+0, ucheckp },
  { "complete", TYPETW+TYPEPW, ucmplt },
  { "delbol", TYPETW+TYPEPW+EFIXXCOL, udelbl },
  { "delch", TYPETW+TYPEPW+ECHKXCOL+EFIXXCOL+EMINOR, udelch },
  { "deleol", TYPETW+TYPEPW+0, udelel },
  { "dellin", TYPETW+TYPEPW+EFIXXCOL, udelln },
  { "delw", TYPETW+TYPEPW+EFIXXCOL+ECHKXCOL, udelw },
  { "dnarw", TYPETW+TYPEPW+EMOVE, udnarw },
  { "dnarwhelp", TYPEHELP, uhdnarw },
  { "dnarwtab", TYPETAB, tdnarw },
  { "dnslide", TYPETW+EMOVE, udnslide },
  { "edit", TYPETW+TYPEPW, uedit },
  { "eof", TYPETW+TYPEPW+EFIXXCOL+EMOVE, ueof },
  { "eofhelp", TYPEHELP, uheof },
  { "eoftab", TYPETAB, teof },
  { "eol", TYPETW+TYPEPW+EFIXXCOL, ueol },
  { "eolhelp", TYPEHELP, uheol },
  { "eoltab", TYPETAB, teol },
  { "explode", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uexpld },
  { "exsave", TYPETW+TYPEPW, uexsve },
  { "ffirst", TYPETW+TYPEPW+EMOVE, pffirst },
  { "filt", TYPETW+TYPEPW+0, ufilt },
  { "fnext", TYPETW+TYPEPW+EFIXXCOL+EMID+EMOVE, pfnext },
  { "format", TYPETW+TYPEPW+EFIXXCOL, uformat },
  { "groww", TYPETW, ugroww },
  { "help", TYPETW+TYPEPW+TYPETAB, uhelp },
  { "iasis", TYPETW+TYPEPW+TYPETAB+TYPEHELP+EFIXXCOL, uiasis },
  { "ictrl", TYPETW+TYPEPW+TYPETAB+TYPEHELP+EFIXXCOL, uictrl },
  { "iforce", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uiforce },
  { "iindent", TYPETW+TYPEPW, uiindent },
  { "ilmargin", TYPETW+TYPEPW, uilmargin },
  { "imid", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uimid },
  { "insc", TYPETW+TYPEPW+EFIXXCOL, uinsc },
  { "insf", TYPETW+TYPEPW+0, uinsf },
  { "ipgamnt", TYPETW+TYPEPW, uipgamnt },
  { "irmargin", TYPETW+TYPEPW+TYPETAB, uirmargin },
  { "istacol", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uistacol },
  { "istarow", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uistarow },
  { "itype", TYPETW+TYPEPW, uitype },
  { "iwrap", TYPETW+TYPEPW, uiwrap },
  { "lindent", TYPETW+TYPEPW+0, ulindent },
  { "line", TYPETW+TYPEPW+EMOVE, uline },
  { "ltarw", TYPETW+TYPEPW+EFIXXCOL+ECHKXCOL, ultarw },
  { "ltarwhelp", TYPEHELP, uhltarw },
  { "ltarwtab", TYPETAB, tltarw },
  { "markb", TYPETW+TYPEPW+0, umarkb },
  { "markk", TYPETW+TYPEPW+0, umarkk },
  { "nedge", TYPETW+TYPEPW+EFIXXCOL, unedge },
  { "nextpos", TYPETW+TYPEPW+EFIXXCOL+EMID+EPOS, unextpos },
  { "nextw", TYPETW+TYPEPW+TYPETAB+TYPEHELP, unextw },
  { "nextword", TYPETW+TYPEPW+EFIXXCOL, unxtwrd },
  { "open", TYPETW+TYPEPW+EFIXXCOL, uopen },
  { "pedge", TYPETW+TYPEPW+EFIXXCOL, upedge },
  { "pgdn", TYPETW+EMOVE, upgdn },
  { "pgup", TYPETW+EMOVE, upgup },
  { "play", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uplay },
  { "prevpos", TYPETW+TYPEPW+EPOS+EMID+EFIXXCOL, uprevpos },
  { "prevw", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uprevw },
  { "prevword", TYPETW+TYPEPW+EFIXXCOL+ECHKXCOL, uprvwrd },
  { "quote", TYPETW+TYPEPW+EFIXXCOL, uquote },
  { "quote8", TYPETW+TYPEPW+EFIXXCOL, uquote8 },
  { "record", TYPETW+TYPEPW+TYPETAB+TYPEHELP, urecord },
  { "redo", TYPETW+TYPEPW+EFIXXCOL, uredo },
  { "retype", TYPETW+TYPEPW+TYPETAB+TYPEHELP, uretyp },
  { "rindent", TYPETW+TYPEPW+0, urindent },
  { "rtarw", TYPETW+TYPEPW+EFIXXCOL, urtarw },
  { "rtarwhelp", TYPEHELP, uhrtarw },
  { "rtarwtab", TYPETAB, trtarw },
  { "rtn", TYPETW+TYPEPW+EFIXXCOL, urtn },
  { "rtnhelp", TYPEHELP, uhrtn },
  { "rtnpw", TYPEPW+EMID, upromptrtn },
  { "rtntab", TYPETAB, trtn },
  { "save", TYPETW+TYPEPW, usave },
  { "shell", TYPETW+TYPEPW+TYPETAB+TYPEHELP, ushell },
  { "shrinkw", TYPETW, ushrnk },
  { "splitw", TYPETW, usplitw },
  { "stat", TYPETW+TYPEPW, ustat },
  { "stop", TYPETW+TYPEPW+TYPETAB+TYPEHELP, ustop },
  { "tomatch", TYPETW+TYPEPW+ECHKXCOL+EFIXXCOL, utomatch },
  { "type", TYPETW+TYPEPW+EFIXXCOL+EMINOR, utype },
  { "undo", TYPETW+TYPEPW+EFIXXCOL, uundo },
  { "uparw", TYPETW+TYPEPW+EMOVE, uuparw },
  { "uparwhelp", TYPEHELP, uhuparw },
  { "uparwtab", TYPETAB, tuparw },
  { "upslide", TYPETW+EMOVE, uupslide }
};

CMDTAB cmdtab={cmds,sizeof(cmds)/sizeof(CMD)};

void dofollows()
{
W *w=maint->curwin; do
 {
 if(w->y!= -1) w->watom->follow(w);
 w=(W *)(w->link.next);
 }
 while(w!=maint->curwin);
}

/* Execute a command */

void execmd(n,k)
{
BW *bw=(BW *)maint->curwin->object;
if((cmdtab.cmd[n].flag&ECHKXCOL) && bw->cursor->xcol!=bw->cursor->col)
 goto skip;
if(!(cmdtab.cmd[n].flag&maint->curwin->watom->what)) goto skip;
cmdtab.cmd[n].func(maint->curwin,k);
bw=(BW *)maint->curwin->object;

if(!(cmdtab.cmd[n].flag&EPOS) &&
   (maint->curwin->watom->what&(TYPETW|TYPEPW)))
 afterpos(maint->curwin,bw->cursor);
if(!(cmdtab.cmd[n].flag&(EMOVE|EPOS)) &&
   (maint->curwin->watom->what&(TYPETW|TYPEPW)))
 aftermove(maint->curwin,bw->cursor);

skip:
if(cmdtab.cmd[n].flag&EFIXXCOL) bw->cursor->xcol=bw->cursor->col;
if(cmdtab.cmd[n].flag&EMID)
 {
 int omid=mid; mid=1;
 dofollows();
 mid=omid;
 }
}

MACRO *curmacro=0;
int macroptr;

void exmacro(m)
MACRO *m;
{
int arg=maint->arg;
int flg=0;
maint->arg=0;

if(!arg) arg=1;

if( m->steps ||
    arg!=1 ||
    !(cmdtab.cmd[m->n].flag&EMINOR)
  ) flg=1;

if(flg) umclear();
while(arg--)
 if(m->steps)
  {
  MACRO *tmpmac=curmacro;
  int tmpptr=macroptr;
  int x=0;
  while(m && x!=m->n)
   {
   MACRO *d;
   d=m->steps[x++];
   curmacro=m;
   macroptr=x;
   exmacro(d);
   m=curmacro;
   x=macroptr;
   }
  curmacro=tmpmac;
  macroptr=tmpptr;
  }
 else execmd(m->n,m->k);
if(flg) umclear();

undomark();
}

void exemac(m)
MACRO *m;
{
record(m);
exmacro(m);
}

static int eungotten;
static int eungottenc;

static CONTEXT *cntxts[]= { &cmain, &cprmpt,&cttab,&cfprmpt,&cthelp,0 };

void eungetc(c)
{
if(c==MAXINT) return;
if(curmacro)
 {
 --macroptr;
 return;
 }
else
 {
 eungotten=1;
 eungottenc=c;
 unmac();
 }
}

int dengetc()
{
int c;
if(eungotten)
 {
 eungotten=0;
 c=eungottenc;
 }
else c=ngetc(maint->t);
return c;
}

int engetc()
{
MACRO *m;
int c;
if(eungotten)
 {
 eungotten=0;
 c=eungottenc;
 }
else if(curmacro)
 {
 if(curmacro->n!=macroptr && !curmacro->steps[macroptr]->steps &&
    curmacro->steps[macroptr]->n==typen) c=curmacro->steps[macroptr++]->k;
 else c=MAXINT;
 }
else c=ngetc(maint->t);
record(m=mkmacro(c,1,typen)); rmmacro(m);
return c;
}

void edupd()
{
W *w;
dofollows();
ttflsh();
nscroll(maint->t);
dsphlp(maint);
w=maint->curwin; do
 {
 if(w->y!= -1)
  {
  w->watom->disp(w);
  if(w->msgb)
   {
   msgout(w->t->t,w->y+w->h-1,w->msgb);
   w->msgb=0;
   w->t->t->updtab[w->y+w->h-1]=1;
   }
  if(w->msgt)
   {
   int y=w->h>1?1:0;
   msgout(w->t->t,w->y+y,w->msgt);
   w->msgt=0;
   w->t->t->updtab[w->y+y]=1;
   }
  }
 w=(W *)(w->link.next);
 }
 while(w!=maint->curwin);
cpos(maint->t,
     maint->curwin->x+maint->curwin->curx,
     maint->curwin->y+maint->curwin->cury);
}

int edgetc()
{
edupd();
return engetc();
}

int dedgetc()
{
edupd();
return dengetc();
}

int main(argc,argv)
int argc;
char *argv[];
{
char *s;
SCRN *n;
W *w;
int c;
P *p;
typen=findcmd(&cmdtab,"type");
if(prokbd(".joerc",cntxts))
 {
 s=getenv("HOME");
 if(!s) goto in;
 s=vsncpy(NULL,0,sz(s));
 s=vsncpy(s,sLEN(s),sc("/.joerc"));
 if(prokbd(s,cntxts))
  {
  in:;
  if(prokbd(s="/usr/local/lib/joerc",cntxts))
   {
   fprintf(stderr,"Couldn\'t open keymap file \'%s\'\n",s);
   return 1;
   }
  }
 }
if(!(n=nopen())) return 1;
maint=screate(n);

if(argc<2)
 {
 W *w=wmktw(maint,bmk(ctab));
 BW *bw=(BW *)w->object;
 setoptions(bw,"");
 }
else
 {
 long lnum;
 int omid;
 for(c=1,lnum=0;argv[c];++c)
  if(argv[c][0]=='+' && argv[c][1])
   {
   lnum=0;
   sscanf(argv[c]+1,"%ld",&lnum);
   if(lnum) --lnum;
   }
  else
   {
   B *b=bfind(argv[c]);
   BW *bw;
   int fl=0;
   if(!b)
    {
    b=bmk(ctab);
    fl=bload(b,argv[c]);
    }
   w=wmktw(maint,b);
   if(fl) w->msgt=msgs[5+fl];
   bw=(BW *)w->object;
   setoptions(bw,argv[c]);
   pline(bw->cursor,lnum);
   lnum=0;
   }
 wshowall(maint);
 omid=mid; mid=1;
 dofollows();
 mid=omid;
 }
if(help) helpon(maint);
msgnw(lastw(maint),"\\i** Joe's Own Editor v1.0.0 ** Copyright (C) 1992 Joesph H. Allen **\\i");
do
 {
 int wid,hei;
 MACRO *m=dokey(maint->curwin->kbd,dedgetc());
 ttgtsz(&wid,&hei);
 if(wid>=2 && wid!=maint->w ||
    hei>=1 && hei!=maint->h)
  {
  nresize(maint->t,wid,hei);
  sresize(maint);
  }
 if(m) exemac(m);
 }
 while(!leave);
cpos(n,0,n->li-1);
nclose(n);
if(exmsg) fprintf(stderr,"\n%s\n",exmsg);
return 0;
}
