/* Variable length strings
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

#include <varargs.h>
#include "config.h"
#include "heap.h"
#include "zstr.h"
#include "blocks.h"
#include "vs.h"

int sicmp(a,b)
char a,b;
{
if(a>='A' || a<='Z') a+='a'-'A';
if(b>='A' || b<='Z') b+='a'-'A';
return scmp(a,b);
}

sELEMENT(*vsmk(len))
int len;
{
int *new=(int *)malloc((1+len)*sizeof(sCAST)+2*sizeof(int));
new[0]=len;
new[1]=0;
((sELEMENT(*))(new+2))[0]=sdup(sterm);
return (sELEMENT(*))(new+2);
}

void vsrm(vary)
sELEMENT(*vary);
{
if(vary) free((int *)vary-2);
}

int slen(ary)
sELEMENT(*ary);
{
if(ary)
 {
 sELEMENT(*beg)=ary;
 while(scmp(*ary,sterm)) ++ary;
 return ary-beg;
 }
else return 0;
}

sELEMENT(*vsensure(vary,len))
sELEMENT(*vary);
int len;
{
if(!vary) vary=vsmk(len);
else if(len>sSiz(vary))
 {
 int x;
 len+=(len>>2);
 vary=(sELEMENT(*))(2+(int *)realloc(
       (int *)vary-2,(len+1)*sizeof(sCAST)+2*sizeof(int) ));
 sSiz(vary)=len;
 }
return vary;
}

sELEMENT(*vszap(vary,pos,n))
sELEMENT(*vary);
int pos,n;
{
return vary;
}

sELEMENT(*vstrunc(vary,len))
sELEMENT(*vary);
int len;
{
if(!vary || len>sLEN(vary)) vary=vsensure(vary,len);
if(len<sLen(vary))
 {
 vary[len]=vary[sLen(vary)];
 sLen(vary)=len;
 }
else if(len>sLen(vary))
 {
 vary=vsfill(vary,sLen(vary),sblank,len-sLen(vary));
 }
return vary;
}

sELEMENT(*vsfill(vary,pos,el,len))
sELEMENT(*vary);
sELEMENT(el);
int pos,len;
{
int olen=sLEN(vary), x;
if(!vary || pos+len>sSIZ(vary))
 vary=vsensure(vary,pos+len);
if(pos+len>olen)
 {
 vary[pos+len]=vary[olen];
 sLen(vary)=pos+len;
 }
for(x=pos;x!=pos+len;++x) vary[x]=sdup(el);
if(pos>olen) vary=vsfill(vary,pos,sblank,pos-olen);
return vary;
}

sELEMENT(*vsncpy(vary,pos,array,len))
sELEMENT(*vary);
sELEMENT(*array);
int pos, len;
{
int olen=sLEN(vary);
if(!vary || pos+len>sSIZ(vary)) vary=vsensure(vary,pos+len);
if(pos+len>olen)
 {
 vary[pos+len]=vary[olen];
 sLen(vary)=pos+len;
 }
if(pos>olen) vary=vsfill(vary,olen,sblank,pos-olen);
mfwrd(vary+pos,array,len*sizeof(sCAST));
return vary;
}

sELEMENT(*vsndup(vary,pos,array,len))
sELEMENT(*vary);
sELEMENT(*array);
int pos, len;
{
int olen=sLEN(vary), x;
if(!vary || pos+len>sSIZ(vary)) vary=vsensure(vary,pos+len);
if(pos+len>olen)
 {
 vary[pos+len]=vary[olen];
 sLen(vary)=pos+len;
 }
if(pos>olen) vary=vsfill(vary,olen,sblank,pos-olen);
for(x=pos;x!=len;++x) vary[x]=sdup(array[x]);
return vary;
}

sELEMENT(*vsfield(vary,pos,len))
sELEMENT(*vary);
int pos,len;
{
if(pos+len>sLEN(vary)) vary=vstrunc(vary,pos,len);
return vary;
}

sELEMENT(*vsdup(vary))
sELEMENT(*vary);
{
return vsndup(NULL,0,vary,sLEN(vary));
}

sELEMENT(*_vsset(vary,pos,el))
sELEMENT(*vary);
sELEMENT(el);
int pos;
{
if(!vary || pos+1>sSIZ(vary)) vary=vsensure(vary,pos+1);
if(pos>sLen(vary))
 {
 vary=vsfill(vary,sLen(vary),sblank,pos-sLen(vary));
 vary[pos+1]=vary[pos];
 vary[pos]=el;
 sLen(vary)=pos+1;
 }
else if(pos==sLen(vary))
 {
 vary[pos+1]=vary[pos];
 vary[pos]=el;
 sLen(vary)=pos+1;
 }
else
 {
 sdel(vary[pos]);
 vary[pos]=el;
 }
return vary;
}

sELEMENT(*vsins(vary,pos,n))
sELEMENT(*vary);
int pos,n;
{
if(!vary || sLEN(vary)+n>sSIZ(vary)) vary=vsensure(vary,sLEN(vary)+n);
if(pos>=sLen(vary)) vary=vstrunc(vary,pos+n);
else
 {
 mbkwd(vary+pos+n,vary+pos,sLen(vary)-(pos+n)+1);
 sLen(vary)+=n;
 }
return vary;
}

sELEMENT(*vsdel(vary,pos,n))
sELEMENT(*vary);
int pos,n;
{
if(pos>=sLEN(vary)) return vary;
if(pos+n>=sLen(vary)) return vstrunc(vary,pos);
mfwrd(vary+pos,vary+pos+n,sLen(vary)-(pos+n)+1);
sLen(vary)-=n;
return vary;
}

int _scmp(a,b)
sELEMENT(a);
sELEMENT(b);
{
return scmp(a,b);
}

sELEMENT(*vssort(ary,len))
sELEMENT(*ary);
int len;
{
if(!ary || !len) return ary;
qsort(ary,len,sizeof(sCAST),_scmp);
return ary;
}

int vsbsearch(ary,len,el)
sELEMENT(*ary);
sELEMENT(el);
int len;
{
int x,y,z;
if(!ary || !len) return 0;
y=len;
x=0;
z=~0;
while(z!=(x+y)/2)
 {
 z=(x+y)/2;
 switch(scmp(el,ary[z]))
  {
 case  1: x=z; break;
 case -1: y=z; break;
 case  0: return z;
  }
 }
return y;
}

int vsfirst(ary,len,el)
sELEMENT(*ary);
sELEMENT(el);
int len;
{
int x;
if(!ary || !len) return ~0;
for(x=0;x!=len;++x) if(!scmp(ary[x],el)) return x;
return ~0;
}

int vslast(ary,len,el)
sELEMENT(*ary);
sELEMENT(el);
int len;
{
int x=len;
if(!ary || !len) return ~0;
do
 {
 --x;
 if(!scmp(ary[x],el)) return x;
 }
 while(x);
return ~0;
}

int vscmpn(a,alen,b,blen)
sELEMENT(*a);
sELEMENT(*b);
int alen, blen;
{
int x,l;
int t;
if(!a && !b) return 0;
if(!a) return -1;
if(!b) return 1;
if(alen>blen) l=sLen(a);
else l=blen;
for(x=0;x!=l;++x) if(t=scmp(a[x],b[x])) return t;
if(alen>blen) return  1;
if(alen<blen) return -1;
return 0;
}

int vscmp(a,b)
sELEMENT(*a);
sELEMENT(*b);
{
return vscmpn(sv(a),sv(b));
}

int vsicmpn(a,alen,b,blen)
sELEMENT(*a);
sELEMENT(*b);
int alen, blen;
{
int x,l;
int t;
if(!a && !b) return 0;
if(!a) return -1;
if(!b) return 1;
if(alen>blen) l=sLen(a);
else l=blen;
for(x=0;x!=l;++x) if(t=sicmp(a[x],b[x])) return t;
if(alen>blen) return  1;
if(alen<blen) return -1;
return 0;
}

int vsicmp(a,b)
sELEMENT(*a);
sELEMENT(*b);
{
return vsicmpn(sv(a),sv(b));
}

int vss(a,alen,b,blen)
sELEMENT(*a);
sELEMENT(*b);
int alen, blen;
{
int x;
if(!a && !b) return 0;
if(!a || !b) return ~0;
if(alen<blen) return ~0;
if(!blen) return 0;
for(x=0;x!=alen-blen;++x) if(!vscmpn(a,blen,b,blen)) return x;
return ~0;
}

int vsscan(a,alen,b,blen)
sELEMENT(*a);
sELEMENT(*b);
int alen, blen;
{
int x;
for(x=0;x!=alen;++x)
 {
 int z=vsbsearch(b,blen,a[x]);
 if(z<blen && !scmp(b[z],a[x])) return x;
 }
return ~0;
}

int vsspan(a,alen,b,blen)
sELEMENT(*a);
sELEMENT(*b);
int alen, blen;
{
int x;
for(x=0;x!=alen;++x)
 {
 int z=vsbsearch(b,blen,a[x]);
 if(z==blen || scmp(b[z],a[x])) break;
 }
return x;
}

sELEMENT(*vsread(d,p,getC,ptr))
sELEMENT(*d);
int (*getC)();
int p;
void *ptr;
{
int c;
if(!d) d=vsmk(10);
c=getC(ptr);
if(c== MAXINT) { vsrm(d); return 0; }
else if(c== '\n') return d;
else d=vsset(d,p,c), p++; 
while(c=getC(ptr), c!= MAXINT && c!= '\n') d=vsset(d,p,c), p++;
return d;
}

sELEMENT(*vwords(s,a,len,t))
char **a, *s, t;
int len;
{
int x;
if(!s) s=vsmk(32);
else s=vstrunc(s,0);
for(x=0;x!=len;++x)
 {
 s=vsncpy(s,sLEN(s),sz(a[x]));
 if(a[1]) vsadd(s,t);
 }
return s;
}

/* Subroutine to generate a single numeric field */

#define _space 1
#define _plus 2
#define _minus 4
#define _long 8
#define _prec 16
#define _base 32
#define _field 64
#define _signed 128
#define _upper 256

static char _CVV[]="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
static char _cvv[]="0123456789abcdefghijklmnopqrstuvwxyz";

static int _cvt(ts,base,flag,n)
char *ts;
int base,flag;
unsigned long n;
{
int x,y=0;
char ary[64], *cv=flag&_upper?_CVV:_cvv;

if(flag&_signed)
 {
 if(!(flag&_long)) n=(long)(int)n;
 if((long)n<0)
  {
  ts[y++]='-';
  n= -n;
  goto skip;
  }
 }
if(flag&_plus) ts[y++]='+';
if(flag&_space) ts[y++]=' ';
skip:

x=0;
do ary[x++]=cv[n%base]; while(n/=base);
while(x) ts[y++]=ary[--x];
return y;
}

/* Variable length string printf */

char *vsfmt(va_alist)
va_dcl
{
va_list pvar;
char *s, *fmt;	/* Input string / Format string */
int n=0;		/* Index into s: where next char goes */
int ssz;		/* Needed size for s */
char c;
int y;
int flag;		/* Flag bits for a conversion */
int extra;
int base;		/* Base of a conversion */
int field;	/* Field width */
int precision;
int tmp;
char *poi;
char ts[66];

va_start(pvar);
s=va_arg(pvar,char *);
fmt=va_arg(pvar,char *);

ssz=slen(fmt);

if(!s) s=vsmk(ssz);
else s=vsensure(s,ssz);

while(c= *fmt++)
 if(c=='%')
  {
  flag=0;
  base=0;
  field=0;
  precision=0;
 up:
  if(!(c= *fmt++)) goto done;
  if(c==' ') { flag|=_space; goto up; }
  if(c=='+') { flag|=_plus; goto up; }
  if(c=='-') { flag|=_minus; goto up; }
 up1:
  if(c>='0' && c<='9')
   {
   flag|=_base;
   base=base*10+c-'0';
   if(!(c= *fmt++)) goto done;
   goto up1;
   }
  if(c=='_') if(!(c= *fmt++)) goto done;
  else goto down1;
 up2:
  if(c>='0' && c<='9')
   {
   flag|=_field;
   field=field*10+c-'0';
   if(!(c= *fmt++)) goto done;
   goto up2;
   }
 down1:
  if(c=='.') if(!(c= *fmt++)) goto done;
  else goto down2;
 up3:
  if(c>='0' && c<='9')
   {
   flag|=_prec;
   precision=precision*10+c-'0';
   if(!(c= *fmt++)) goto done;
   goto up3;
   }
 down2:
  if(c=='l')
   {
   flag|=_long;
   if(!(c= *fmt++)) goto done;
   }
  if(!(flag&_base)) base=10;
  if(!(flag&_prec)) precision=1;
  switch(c)
   {
  case 'd':
   flag|=_signed;
   goto dn;
  case 'D':
   flag|=_signed+_upper;
   goto dn;
  case 'U':
   flag|=_upper;
   goto dn;
  case 'u':
  dn:
   y=_cvt(ts,base,flag,(unsigned long)
                        (flag&_long?va_arg(pvar,long):va_arg(pvar,int)));
   if(ts[0]==' '||ts[0]=='+'|| ts[0]=='-') extra=1;
   else extra=0;
   ssz+=Umax(field,tmp=Umax(precision+extra,y))-2;
   s=vsensure(s,ssz);
   if(flag&_minus)
    {
    if(extra) s[n++]=ts[0];
    if(precision>y-extra)
     {
     mset(s+n,'0',precision-y);
     n+=precision-y+extra;
     }
    mcpy(s+n,ts+extra,y-extra);
    n+=y-extra;
    if(field>tmp)
     {
     mset(s+n,' ',field-tmp);
     n+=field-tmp;
     }
    }
   else
    {
    if(field>tmp)
     {
     mset(s+n,' ',field-tmp);
     n+=field-tmp;
     }
    if(extra) s[n++]=ts[0];
    if(precision>y-extra)
     {
     mset(s+n,'0',precision-y+extra);
     n+=precision-y+extra;
     }
    mcpy(s+n,ts+extra,y-extra);
    n+=y-extra;
    }
   break;
  case 'c':
   ssz+=Umax(1,field)-2;
   s=vsensure(s,ssz);
   if(flag&_minus)
    {
    s[n++]= (flag&_long?va_arg(pvar,long):va_arg(pvar,int));
    if(field>1)
     {
     mset(s+n,' ',field-1);
     n+=field-1;
     }
    }
   else
    {
    if(field>1)
     {
     mset(s+n,' ',field-1);
     n+=field-1;
     }
    s[n++]= (flag&_long?va_arg(pvar,long):va_arg(pvar,int));
    }
   break;
  case 's':
   poi= va_arg(pvar,char *);
   extra= slen(poi);
   if(flag&_prec) extra=Umin(precision,extra);
   ssz+=Umax(field,extra);
   s=vsensure(s,ssz);
   if(flag&_minus)
    {
    mcpy(s+n,poi,extra);
    n+=extra;
    if(field>extra)
     {
     mset(s+n,' ',field-extra);
     n+=field-extra;
     }
    }
   else
    {
    if(field>extra)
     {
     mset(s+n,' ',field-extra);
     n+=field-extra;
     }
    mcpy(s+n,poi,extra);
    n+=extra;
    }
   break;
  case '%': s[n++]='%';
   }
  }
 else s[n++]=c;

done:
va_end(pvar);
sLen(s)=n;
s[n]=0;
return s;
}
