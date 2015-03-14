/* Regular expression subroutines
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
#include "zstr.h"
#include "vs.h"
#include "b.h"
#include "regex.h"

static int brackz(a,c)
unsigned char **a;
unsigned char c;
{
int flag;
unsigned char *s= *a;
if(*s=='^' || *s=='*')
 {
 flag=1;
 ++s;
 
 if(*s==']')
  {
  ++s;
  if(c==']') flag=0;
  }

 while(*s)
  if(*s==']') { ++s; break; }
  else
   {
   if(c==*s) flag=0;
   if(s[1]=='-' && s[2] && s[2]!=']' && s[0]<=s[2])
    {
    if(c>=s[0] && c<=s[2]) flag=0;
    s+=2;
    }
   ++s;
   }

 *a=s;
 return flag;
 }
else
 {
 flag=0;

 if(*s==']')
  {
  ++s;
  if(c==']') flag=1;
  }

 while(*s)
  if(*s==']') { ++s; break; }
  else
   {
   if(c==*s) flag=1;
   if(s[1]=='-' && s[2] && s[2]!=']' && s[0]<=s[2])
    {
    if(c>=s[0] && c<=s[2]) flag=1;
    s+=2;
    }
   ++s;
   }
 
 *a=s;
 return flag;
 }
}

static int brack(a,la,c)
unsigned char **a;
int *la;
unsigned char c;
{
int flag;
unsigned char *s= *a;
int l= *la;
if(!l) return 0;
if(*s=='^' || *s=='*')
 {
 flag=1;
 ++s; --l;
 
 if(l && *s==']')
  {
  ++s; --l;
  if(c==']') flag=0;
  }

 while(l)
  if(*s==']') { ++s; --l; break; }
  else
   {
   if(c==*s) flag=0;
   if(l>=3 && s[1]=='-' && s[2]!=']' && s[0]<=s[2])
    {
    if(c>=s[0] && c<=s[2]) flag=0;
    s+=2; l-=2;
    }
   ++s; --l;
   }

 *a=s; *la=l;
 return flag;
 }
else
 {
 flag=0;

 if(l && *s==']')
  {
  ++s; --l;
  if(c==']') flag=1;
  }

 while(l)
  if(*s==']') { ++s; --l; break; }
  else
   {
   if(c==*s) flag=1;
   if(l>=3 && s[1]=='-' && s[2]!=']' && s[0]<=s[2])
    {
    if(c>=s[0] && c<=s[2]) flag=1;
    s+=2; l-=2;
    }
   ++s; --l;
   }
 
 *a=s; *la=l;
 return flag;
 }
}

int rmatch(a,b)
char *a, *b;
{
for(;;)
 switch(*a)
  {
 case '*': ++a;
           do if(rmatch(a,b)) return 1; while(*b++);
           return 0;

 case '[': ++a;
           if(!*b) return 0;
           if(!brackz(&a,*b)) return 0;
           ++b;
           break;

 case '?': ++a;
           if(!*b) return 0;
           ++b;
           break;

 case 0:   if(!*b) return 1;
           else return 0;

 case '\\':
           if(!*++a) return 0;

 default:  if(*a++!=*b++) return 0;
  }
}

int rimatch(a,b)
char *a, *b;
{
for(;;)
 switch(*a)
  {
 case '*': ++a;
           do if(rimatch(a,b)) return 1; while(*b++);
           return 0;

 case '[': ++a;
           if(!*b) return 0;
           if(!brackz(&a,*b)) return 0;
           ++b;
           break;

 case '?': ++a;
           if(!*b) return 0;
           ++b;
           break;

 case 0:   if(!*b) return 1;
           else return 0;

 case '\\':
           if(!*++a) return 0;

 default:  if(toup(*a++)!=toup(*b++)) return 0;
  }
}

char *pieces[26]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

static void savec(n,c)
char c;
{
char *s=0;
if(pieces[n]) vsrm(pieces[n]);
s=vsncpy(s,0,&c,1);
pieces[n]=s;
}

static void saves(n,p,szz)
P *p;
long szz;
{
if(szz>=MAXINT-31) pieces[n]=vstrunc(pieces[n],0);
else
 {
 pieces[n]=vstrunc(pieces[n],szz);
 brmem(p,pieces[n],szz);
 }
}

static int skip_special(p)
 P *p;
 {
  int to, s;
  P *q;
  switch(s=pgetc(p))
   {
    case '"':
    do
     if((s=pgetc(p))=='\\') pgetc(p), s=pgetc(p);
     while(s!=MAXINT && s!='\"');
    if(s=='\"') return MAXINT-1;
    break;

    case '\'':
    do
     if((s=pgetc(p))=='\\') s=pgetc(p), s=pgetc(p);
     while(s!=MAXINT && s!='\'');
    if(s=='\'') return MAXINT-1;
    break;

    case '[': to=']'; goto skip;
    case '(': to=')'; goto skip;
    case '{': to='}';
    skip: do
     s=skip_special(p);
     while(s!=to && s!=MAXINT);
    if(s==to) return MAXINT-1;
    break;

    case '/':
    s=pgetc(p);
    if(s=='*')
     do
      {
      s=pgetc(p);
      while(s=='*') if((s=pgetc(p))=='/') return MAXINT-1;
      } while(s!=MAXINT);
    else
     if(s!=MAXINT) s=prgetc(p);
     else s='/';
    break;

   }
  return s;
 }

int pmatch(regex,len,p,n)
char *regex;
P *p;
{
int c,d;
P *q;
while(len--)
 switch(c=*regex++)
  {
 case '\\':
  if(!len--) return 0;
  switch(c=*regex++)
   {
  case '?':
   d=pgetc(p);
   if(d== MAXINT) return 0;
   savec(n++,(char)d);
   break;

  case 'n':
   d=pgetc(p);
   if(d!='\n') return 0;
   break;

  case '*':
   q=pdup(p);
   do
    {
    long pb=p->byte;
    if(pmatch(regex,len,p,n+1))
     { saves(n,q,pb-q->byte); prm(q); return 1; }
    }
    while(pgetc(p)!= MAXINT);
   pset(p,q); prm(q);
   return 0;
	
  case 'c':
   q=pdup(p);
   do
    {
    long pb=p->byte;
    if(pmatch(regex,len,p,n+1))
     { saves(n,q,pb-q->byte); prm(q); return 1; }
    }
    while((c=skip_special(p))!= MAXINT);
   pset(p,q); prm(q);
   return 0;

  case '[':
   d=pgetc(p);
   if(d== MAXINT) return 0;
   if(!brack(&regex,&len,d)) { prgetc(p); return 0; }
   savec(n++,(char)d);
   break;

  case '+':
   {
    char *oregex=regex;
    int olen=len;
    q=pdup(p);
    /* move forward */
    if (len--,(*regex++=='['))
     brack(&regex,&len,c);
    do
     {
      long pb=p->byte;
      if(pmatch(regex,len,p,n+1))
       { saves(n,q,pb-q->byte); prm(q); return 1; }
      regex=oregex;
      len=olen;
     }
    while(
     (MAXINT!=(c=pgetc(p))) &&
      (
       (len--,(*regex++=='[')) ?
        brack(&regex,&len,c) :
        regex[-1]==c
      ));
    pset(p,q); prm(q);
    return 0;
   }
   
  case '^':
   if(!pisbol(p)) return 0;
   break;

  case '$':
   if(!piseol(p)) return 0;
   break;

  case '<':
   if(!pisbow(p)) return 0;
   break;
  
  case '>':
   if(!piseow(p)) return 0;
   break;
  
  default:
   d=pgetc(p);
   if(d!=c) { if(d!= MAXINT) prgetc(p); return 0; }
   }
  break;

 default:
  d=pgetc(p);
  if(d!=c) { if(d!= MAXINT) prgetc(p); return 0; }
  }
return 1;
}

int pimatch(regex,len,p,n)
char *regex;
P *p;
{
int c,d;
P *q;
while(len--)
 switch(c=*regex++)
  {
 case '\\':
  if(!len--) return 0;
  switch(c=*regex++)
   {
  case '?':
   d=pgetc(p);
   if(d==MAXINT) return 0;
   savec(n++,(char)d);
   break;
  
  case 'n':
   d=pgetc(p);
   if(d!='\n') return 0;
   break;
  
  case '*':
   q=pdup(p);
   do
    {
    long pb=p->byte;
    if(pimatch(regex,len,p,n+1))
     { saves(n,q,pb-q->byte); prm(q); return 1; }
    }
    while(pgetc(p)!= MAXINT);
   pset(p,q); prm(q);
   return 0;

  case 'c':
   q=pdup(p);
   do
    {
    long pb=p->byte;
    if(pimatch(regex,len,p,n+1))
     { saves(n,q,pb-q->byte); prm(q); return 1; }
    }
    while((c=skip_special(p))!= MAXINT);
   pset(p,q); prm(q);
   return 0;

  case '[':
   d=pgetc(p);
   if(d==MAXINT) return 0;
   if(!brack(&regex,&len,d)) { prgetc(p); return 0; }
   savec(n++,(char)d);
   break;

  case '+':
   {
    char *oregex=regex;
    int olen=len;
    q=pdup(p);
    /* move forward */
    if (len--,(*regex++=='['))
     brack(&regex,&len,c);
    do
     {
      long pb=p->byte;
      if(pimatch(regex,len,p,n+1))
       { saves(n,q,pb-q->byte); prm(q); return 1; }
      regex=oregex;
      len=olen;
     }
    while(
     (MAXINT!=(c=pgetc(p))) &&
      (
       (len--,(*regex++=='[')) ?
        brack(&regex,&len,c) :
        toup(regex[-1])==toup(c)
      ));
    pset(p,q); prm(q);
    return 0;
   }

  case '^':
   if(!pisbol(p)) return 0;
   break;

  case '$':
   if(!piseol(p)) return 0;
   break;

  case '<':
   if(!pisbow(p)) return 0;
   break;
  
  case '>':
   if(!piseow(p)) return 0;
   break;
  
  default:
   d=pgetc(p);
   if(toup(d)!=toup(c)) { if(d!=MAXINT) prgetc(p); return 0; }
   }
  break;

 default:
  d=pgetc(p);
  if(toup(d)!=toup(c)) { if(d!=MAXINT) prgetc(p); return 0; }
  }
return 1;
}
