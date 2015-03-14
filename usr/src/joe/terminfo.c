/* TERMINFO database interface
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

#include "termcap.h"
#include "vs.h"

/* Get terminfo entry */

CAP *getcap(name,baud,out,outptr)
char *name;
int baud;
void (*out)();
void *outptr;
{
CAP *cap;
if(NULL==name && NULL==(name=getenv("TERM"))) return NULL;
cap=(CAP *)malloc(sizeof(CAP));
cap->out=out;
cap->outptr=outptr;
cap->tbuf=(char *)malloc(4096);
cap->abuf=(char *)malloc(4096);
cap->abufp=cap->abuf;
cap->baud=baud;
cap->div=100000/baud;
ospeed=baud;
if(tgetent(cap->tbuf,name)!=1)
 {
 free(cap->tbuf);
 free(cap->abuf);
 return NULL;
 }
cap->pad=getstr(cap,"pc");
if(NULL!=cap->pad) PC=cap->pad[0];
else PC=0;
BC=0; UP=0;
return cap;
}

/* Get string capability */
/* Warning, repeated calls to this will eventually use up all of cap->abuf */

char *getstr(cap,name)
CAP *cap;
char *name;
{
return tgetstr(name,&cap->abufp);
}

/* Get flag capability */

int getflag(cap,name)
CAP *cap;
char *name;
{
return tgetflag(name);
}

/* Get numeric capability */

int getnum(cap,name)
CAP *cap;
char *name;
{
return tgetnum(name);
}

/* Eliminate a CAP */

void rmcap(cap)
CAP *cap;
{
free(cap->tbuf);
free(cap->abuf);
free(cap); 
}

/* Execute a string capability */

static CAP *outcap;

static int outout(c)
{
outcap->out(outcap->outptr,c);
}

void texec(cap,str,l,a0,a1,a2,a3)
CAP *cap;
char *str;
int l,a0,a1,a2,a3;
{
char *a;
outcap=cap;
a=tgoto(str,a1,a0);
tputs(a,l,outout);
}

static int total;

static void cst()
{
++total;
}

int tcost(cap,s,l,a0,a1,a2,a3)
CAP *cap;
char *s;
int l,a0,a1,a2,a3;
{
void (*out)()=cap->out;
if(NULL==s) return 10000;
total=0;
cap->out=cst;
texec(cap,s,l,a0,a1,a2,a3);
cap->out=out;
return total;
}

static char *ssp;
static void cpl(ptr,c)
char *ptr;
char c;
{
vsadd(ssp,c);
}

char *tcompile(cap,s,a0,a1,a2,a3)
CAP *cap;
char *s;
int a0,a1,a2,a3;
{
void (*out)()=cap->out;
int div=cap->div;
if(NULL==s) return NULL;
cap->out=cpl; cap->div=10000;
ssp=vsmk(10);
texec(cap,s,0,a0,a1,a2,a3);
cap->out=out; cap->div=div;
return ssp;
}
