/* Keyboard handler
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
#include "heap.h"
#include "zstr.h"
#include "va.h"

/* For help text loading in init. file */
#include "help.h"

/* For special key sequence table */
#include "scrn.h"

/* For option settings */
#include "bw.h"

#include "macro.h"
#include "kbd.h"

char **help_names;
struct help **help_structs;
struct help *first_help;

/* Create a KBD */

KBD *mkkbd(context)
CONTEXT *context;
{
KBD *kbd=(KBD *)malloc(sizeof(KBD));
kbd->topmap=context->kmap;
kbd->curmap=context->kmap;
return kbd;
}

/* Eliminate a KBD */

void rmkbd(k)
KBD *k;
{
free(k);
}

/* Lookup key in keyboard table */

static int findkey(kmap,c)
KMAP *kmap;
{
int x,y,z;
x=0; y=kmap->len; z= -1;
if(y)
 while(z!=(x+y)/2)
  {
  z=(x+y)/2;
  if((kmap->keys[z].k&KEYMASK)==c) return z;
  else if((kmap->keys[z].k&KEYMASK)>c) y=z;
  else x=z;
  }
return y;
}

/* Process next key for KBD */

MACRO *dokey(kbd,k)
KBD *kbd;
{
int n=findkey(kbd->curmap,k);
if(n==kbd->curmap->len || (kbd->curmap->keys[n].k&KEYMASK)!=k)
 kbd->curmap=kbd->topmap;
else if(kbd->curmap->keys[n].k&KEYSUB)
 kbd->curmap=kbd->curmap->keys[n].value.submap;
else
 {
 MACRO *macro=kbd->curmap->keys[n].value.macro;
 kbd->curmap=kbd->topmap;
 return macro;
 }
return 0;
}

/* Return command table index for given command name */

int findcmd(cmdtab,s)
CMDTAB *cmdtab;
char *s;
{
int x,y,z;
x=0; y=cmdtab->len; z= -1;
while(z!=(x+y)/2)
 {
 z=(x+y)/2;
 switch(zcmp(s,cmdtab->cmd[z].name))
  {
 case  1: x=z; break;
 case -1: y=z; break;
 case  0: return z;
  }
 }
return -1;
}

/* Return key code for key name */

static int keyval(s)
char *s;
{
int z1;
for(z1=0;z1!=NKEYS;++z1) if(!zcmp(s,seqs[z1].name))
 return seqs[z1].code;
if(s[0]=='^')
 if(s[1]=='?') return 127;
 else return s[1]&0x1f;
else if(!zcmp(s,"SP")) return ' ';
else return s[0];
}

/* Add a key to a keymap */

static void addkey(kmap,n,k,v)
KMAP *kmap;
MACRO *v;
{
if(kmap->len==kmap->size)
 kmap->keys=(KEY *)realloc(kmap->keys,sizeof(KEY)*(kmap->size+=64));
mbkwd(kmap->keys+n+1,kmap->keys+n,(kmap->len++-n)*sizeof(KEY));
kmap->keys[n].k=k;
kmap->keys[n].value.macro=v;
}

/* Eliminate a keymap */

static void rmkmap(kmap)
KMAP *kmap;
{
int x;
if(!kmap) return;
for(x=0;x!=kmap->len;++x)
 if(kmap->keys[x].k&KEYSUB) rmkmap(kmap->keys[x].value.submap);
 else rmmacro(kmap->keys[x].value.macro);
free(kmap->keys);
free(kmap);
}

OPTIONS *options=0;
extern int mid, dspasis, dspctrl, force, help, pgamnt, starow, stacol;

void setoptions(bw,name)
BW *bw;
char *name;
{
OPTIONS *o;
for(o=options;o;o=o->next)
 if(rmatch(o->name,name))
  {
  bw->overtype=o->overtype;
  bw->lmargin=o->lmargin;
  bw->rmargin=o->rmargin;
  bw->autoindent=o->autoindent;
  bw->wordwrap=o->wordwrap;
  break;
  }
}

/* Process initialization file */

int prokbd(name,cntxts)
char *name;
CONTEXT **cntxts;
{
CONTEXT *context=0;		/* Current context */
KMAP *kmap;			/* Current keymap */
char buf[256];			/* Input buffer */
FILE *fd=fopen(name,"r");	/* File */
MACRO *macro=0;
struct help *tmp;
int nhelp=0;
int line=0;			/* Line number */
int err=0;			/* Set if there was any errors */
int x,y,n,z,c,d;

first_help=NULL;
help_names=vatrunc(NULL,0);

if(!fd) return -1;

fprintf(stderr,"Processing keymap file \'%s\'...",name); fflush(stdout);

while(++line, fgets(buf,256,fd))
 {
 /* Set file-dependant options */
 if(buf[0]=='*')
  {
  OPTIONS *n=(OPTIONS *)malloc(sizeof(OPTIONS));
  for(x=0;buf[x] && buf[x]!='\n' && buf[x]!=' ' && buf[x]!='\t';++x);
  buf[x]=0;
  n->lmargin=0;
  n->rmargin=76;
  n->overtype=0;
  n->autoindent=0;
  n->wordwrap=0;
  n->next=options;
  options=n;
  n->name=zdup(buf);
  continue;
  }
 
 if(buf[0]=='-')
  {
  int v;
  for(x=0;buf[x] && buf[x]!='\n' && buf[x]!=' ' && buf[x]!='\t';++x);
  c=buf[x]; buf[x]=0;
  if(!zcmp(buf+1,"mid")) mid=1;
  else if(!zcmp(buf+1,"asis")) dspasis=1;
  else if(!zcmp(buf+1,"stacol")) stacol=1;
  else if(!zcmp(buf+1,"starow")) starow=1;
  else if(!zcmp(buf+1,"ctrl")) dspattr=1;
  else if(!zcmp(buf+1,"force")) force=1;
  else if(!zcmp(buf+1,"help")) help=1;
  else if(!zcmp(buf+1,"pg") && c) sscanf(buf+x+1,"%d",&pgamnt);
  else
   if(options)
    if(!zcmp(buf+1,"wordwrap")) options->wordwrap=1;
    else if(!zcmp(buf+1,"autoindent")) options->autoindent=1;
    else if(!zcmp(buf+1,"typewriter")) options->overtype=2;
    else if(!zcmp(buf+1,"overwrite")) options->overtype=1;
    else if(!zcmp(buf+1,"lmargin") && c) sscanf(buf+x+1,"%ld",&options->lmargin);
    else if(!zcmp(buf+1,"rmargin") && c) sscanf(buf+x+1,"%ld",&options->rmargin);
    else fprintf(stderr,"\n%s %d: Unknown option",name,line);
   else fprintf(stderr,"\n%s %d: No pattern selected for option",name,line);
  continue;
  }

 /* Process help text */
 if(buf[0]=='{')
  {
  int bfl;
  tmp=(struct help *) malloc(sizeof(struct help));
  nhelp++;
  tmp->next=first_help;
  first_help=tmp;
  tmp->name=vsncpy(NULL,0,sz(buf+1)-1);
  help_names=vaadd(help_names,tmp->name);
  tmp->hlptxt=0;
  tmp->hlpsiz=0;
  tmp->hlpbsz=0;
  tmp->hlplns=0;
  up:
  if(++line, !fgets(buf,256,fd))
   {
   err=1;
   fprintf(stderr,
           "\n%s %d: End of keymap file occured before end of help text",
           name,line);
   break;
   }
  if(buf[0]=='}')
   {
   if(!hlptxt)
    hlptxt=tmp->hlptxt,
    hlpsiz=tmp->hlpsiz,
    hlpbsz=tmp->hlpbsz,
    hlplns=tmp->hlplns;
   continue;
   }
  bfl=zlen(buf);
  if(tmp->hlpsiz+bfl>tmp->hlpbsz)
   {
   if(tmp->hlptxt) tmp->hlptxt=(char *)realloc(tmp->hlptxt,tmp->hlpbsz+bfl+1024);
   else tmp->hlptxt=(char *)malloc(bfl+1024), tmp->hlptxt[0]=0;
   tmp->hlpbsz+=bfl+1024;
   }
  zcpy(tmp->hlptxt+tmp->hlpsiz,buf);
  tmp->hlpsiz+=bfl;
  ++tmp->hlplns;
  goto up;
  }
 
 /* Get context name */
 if(buf[0]==':')
  {
  for(x=1;buf[x] && buf[x]!=' ' && buf[x]!='\t' && buf[x]!='\n';++x);
  buf[x]=0;
  if(x==1) continue;
  for(x=0,context=0;cntxts[x];++x)
   if(!zcmp(buf+1,cntxts[x]->name))
    {
    context=cntxts[x];
    break;
    }
  if(!context) fprintf(stderr,"\n%s %d: Unknown context",name,line), err=1;
  continue;
  }
 
 /* Process Macro */
 x=0;
 macro=0;
 macroloop:
 if(buf[x]=='\"')
  {
  ++x;
  while(buf[x] && buf[x]!='\"')
   {
   if(buf[x]=='\\' && buf[x+1])
    {
    ++x;
    switch(buf[x])
     {
    case 'n': buf[x]=10; break;
    case 'r': buf[x]=13; break;
    case 'b': buf[x]=8; break;
    case 'f': buf[x]=12; break;
    case 'a': buf[x]=7; break;
    case 't': buf[x]=9; break;
    case 'x':
     c=0;
     if(buf[x+1]>='0' && buf[x+1]<='9') c=c*16+buf[++x]-'0';
     else if(buf[x+1]>='a' && buf[x+1]<='f' ||
             buf[x+1]>='A' && buf[x+1]<='F') c=c*16+(buf[++x]&0xF)+9;
     if(buf[x+1]>='0' && buf[x+1]<='9') c=c*16+buf[++x]-'0';
     else if(buf[x+1]>='a' && buf[x+1]<='f' ||
             buf[x+1]>='A' && buf[x+1]<='F') c=c*16+(buf[++x]&0xF)+9;
     buf[x]=c;
     break;
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    case '8': case '9':
     c=buf[x]-'0';
     if(buf[x+1]>='0' && buf[x+1]<='7') c=c*8+buf[++x]-'0';
     if(buf[x+1]>='0' && buf[x+1]<='7') c=c*8+buf[++x]-'0';
     buf[x]=c;
     break;
     }
    }
   if(macro)
    {
    if(!macro->steps)
     {
     MACRO *m=macro;
     macro=mkmacro(0,1,0);
     addmacro(macro,m);
     }
    addmacro(macro,mkmacro(buf[x],1,findcmd(&cmdtab,"type")));
    }
   else macro=mkmacro(buf[x],1,findcmd(&cmdtab,"type"));
   ++x;
   }
  if(buf[x]=='\"') ++x;
  }
 else
  {
  for(y=x;
      buf[y] && buf[y]!=',' && buf[y]!=' ' && buf[y]!='\t' && buf[y]!='\n';
      ++y);
  if(y!=x)
   {
   z=buf[y]; buf[y]=0;
   n=findcmd(&cmdtab,buf+x);
   if(n== -1)
    {
    fprintf(stderr,"\n%s %d: Key function \'%s\' not found",name,line,buf);
    err=1;
    continue;
    }
   else if(macro)
    {
    if(!macro->steps)
     {
     MACRO *m=macro;
     macro=mkmacro(0,1,0);
     addmacro(macro,m);
     }
    addmacro(macro,mkmacro(-1,1,n));
    }
   else macro=mkmacro(-1,1,n);
   buf[x=y]=z;
   }
  }
 if(buf[x]==',')
  {
  ++x;
  goto macroloop;
  }

 if(!macro) continue;

 if(!context)
  {
  err=1;
  fprintf(stderr,"\n%s %d: No context selected for key",name,line);
  continue;
  }

 /* Process key sequence */
 kmap=0;
 n= -1;
 while(buf[x]==' ' || buf[x]=='\t') ++x;
 while(1)
  {
  int qw,zz;
  if(buf[x]==' ') ++x;
  if(!buf[x] || buf[x]=='\n' || buf[x]==' ' || buf[x]=='\t') break;
  /* Got Next key */
  for(zz=x;buf[zz]!=' ' && buf[zz] && buf[zz]!='\t' && buf[zz]!='\n';++zz);
  qw=buf[zz]; buf[zz]=0;
  d=c=keyval(buf+x);
  buf[zz]=qw; x=zz;

  if(buf[x]==' ') ++x;
  if(buf[x]=='T' && buf[x+1]=='O')
   {
   x+=2;
   if(buf[x]==' ') ++x;
   if(buf[x] && buf[x]!='\n' && buf[x]!=' ' && buf[x]!='\t')
    {
    for(zz=x;buf[zz]!=' ' && buf[zz] && buf[zz]!='\t' && buf[zz]!='\n';++zz);
    qw=buf[zz]; buf[zz]=0;
    d=keyval(buf+x);
    buf[zz]=qw; x=zz;
    }
   }
  if(d<c) d=c;

  /* Add it as if it were a submap */
  if(!kmap)
   {
   if(!(kmap=context->kmap))
    {
    kmap=(KMAP *)malloc(sizeof(KMAP));
    kmap->keys=(KEY *)malloc((kmap->size=128)*sizeof(KEY));
    kmap->len=0;
    context->kmap=kmap;
    }
   }
  else
   if(kmap->keys[n].k&KEYSUB) kmap=kmap->keys[n].value.submap;
   else
    {
    kmap->keys[n].value.submap=(KMAP *)malloc(sizeof(KMAP));
    kmap->keys[n].k|=KEYSUB;
    kmap=kmap->keys[n].value.submap;
    kmap->keys=(KEY *)malloc((kmap->size=128)*sizeof(KEY));
    kmap->len=0;
    }
  n=findkey(kmap,c);
  if(n==kmap->len || (kmap->keys[n].k&KEYMASK)!=c) addkey(kmap,n,c,NULL);
  }
 while(c<=d)
  {
  n=findkey(kmap,c);
  if(n==kmap->len || (kmap->keys[n].k&KEYMASK)!=c)
   addkey(kmap,n,c,c==d?macstk(macro,c):dupmacro(macstk(macro,c)));
  else
   {
   if(kmap->keys[n].k&KEYSUB)
    rmkmap(kmap->keys[n].value.submap),
    kmap->keys[n].k&=~KEYSUB;
   else
    rmmacro(kmap->keys[n].value.macro);
   kmap->keys[n].value.macro=(c==d?macstk(macro,c):dupmacro(macstk(macro,c)));
   }
  ++c;
  }
 }
fclose(fd);
if(err) fprintf(stderr,"\ndone\n");
else fprintf(stderr,"done\n");
if (nhelp)
 {
  help_structs=(struct help **) malloc(sizeof(struct help *)*nhelp);
  tmp=first_help;
  while(nhelp--)
   {
    help_structs[nhelp]=tmp;
    tmp=tmp->next;
   }
 }
return 0;
}

struct help *get_help(char *name)
 {
  struct help *tmp;
  for(tmp=first_help;tmp && strcmp(tmp->name,name);tmp=tmp->next);
  return tmp;
 }
 