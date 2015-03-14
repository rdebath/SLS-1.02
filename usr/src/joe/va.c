/* Variable length array of strings
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
#include "heap.h"
#include "zstr.h"
#include "va.h"

aELEMENT(*vamk(len))
int len;
{
int *new=(int *)malloc((1+len)*sizeof(aCAST)+2*sizeof(int));
new[0]=len;
new[1]=0;
((aELEMENT(*))(new+2))[0]=aterm;
return (aELEMENT(*))(new+2);
}

void varm(vary)
aELEMENT(*vary);
{
if(vary)
 {
 vazap(vary,0,aLen(vary));
 free((int *)vary-2);
 }
}

int alen(ary)
aELEMENT(*ary);
{
if(ary)
 {
 aELEMENT(*beg)=ary;
 while(acmp(*ary,aterm)) ++ary;
 return ary-beg;
 }
else return 0;
}

aELEMENT(*vaensure(vary,len))
aELEMENT(*vary);
int len;
{
if(!vary) vary=vamk(len);
else if(len>aSiz(vary))
 {
 int x;
 len+=(len>>2);
 vary=(aELEMENT(*))(2+(int *)realloc(
       (int *)vary-2,(len+1)*sizeof(aCAST)+2*sizeof(int) ));
 aSiz(vary)=len;
 }
return vary;
}

aELEMENT(*vazap(vary,pos,n))
aELEMENT(*vary);
int pos,n;
{
if(vary)
 {
 int x;
 if(pos<aLen(vary))
  if(pos+n<=aLen(vary))
   for(x=pos;x!=pos+n;++x) adel(vary[x]);
  else
   for(x=pos;x!=aLen(vary);++x) adel(vary[x]);
 }
return vary;
}

aELEMENT(*vatrunc(vary,len))
aELEMENT(*vary);
int len;
{
if(!vary || len>aLEN(vary)) vary=vaensure(vary,len);
if(len<aLen(vary))
 {
 vary=vazap(vary,len,aLen(vary)-len);
 vary[len]=vary[aLen(vary)];
 aLen(vary)=len;
 }
else if(len>aLen(vary))
 {
 vary=vafill(vary,aLen(vary),ablank,len-aLen(vary));
 }
return vary;
}

aELEMENT(*vafill(vary,pos,el,len))
aELEMENT(*vary);
aELEMENT(el);
int pos,len;
{
int olen=aLEN(vary), x;
if(!vary || pos+len>aSIZ(vary))
 vary=vaensure(vary,pos+len);
if(pos+len>olen)
 {
 vary[pos+len]=vary[olen];
 aLen(vary)=pos+len;
 }
for(x=pos;x!=pos+len;++x) vary[x]=adup(el);
if(pos>olen) vary=vafill(vary,pos,ablank,pos-olen);
return vary;
}

aELEMENT(*vancpy(vary,pos,array,len))
aELEMENT(*vary);
aELEMENT(*array);
int pos, len;
{
int olen=aLEN(vary);
if(!vary || pos+len>aSIZ(vary)) vary=vaensure(vary,pos+len);
if(pos+len>olen)
 {
 vary[pos+len]=vary[olen];
 aLen(vary)=pos+len;
 }
if(pos>olen) vary=vafill(vary,olen,ablank,pos-olen);
mfwrd(vary+pos,array,len*sizeof(aCAST));
return vary;
}

aELEMENT(*vandup(vary,pos,array,len))
aELEMENT(*vary);
aELEMENT(*array);
int pos, len;
{
int olen=aLEN(vary), x;
if(!vary || pos+len>aSIZ(vary)) vary=vaensure(vary,pos+len);
if(pos+len>olen)
 {
 vary[pos+len]=vary[olen];
 aLen(vary)=pos+len;
 }
if(pos>olen) vary=vafill(vary,olen,ablank,pos-olen);
for(x=pos;x!=len;++x) vary[x]=adup(array[x]);
return vary;
}

aELEMENT(*vafield(vary,pos,len))
aELEMENT(*vary);
int pos,len;
{
if(pos+len>aLEN(vary)) vary=vatrunc(vary,pos,len);
return vary;
}

aELEMENT(*vadup(vary))
aELEMENT(*vary);
{
return vandup(NULL,0,vary,aLEN(vary));
}

aELEMENT(*_vaset(vary,pos,el))
aELEMENT(*vary);
aELEMENT(el);
int pos;
{
if(!vary || pos+1>aSIZ(vary)) vary=vaensure(vary,pos+1);
if(pos>aLen(vary))
 {
 vary=vafill(vary,aLen(vary),ablank,pos-aLen(vary));
 vary[pos+1]=vary[pos];
 vary[pos]=el;
 aLen(vary)=pos+1;
 }
else if(pos==aLen(vary))
 {
 vary[pos+1]=vary[pos];
 vary[pos]=el;
 aLen(vary)=pos+1;
 }
else
 {
 adel(vary[pos]);
 vary[pos]=el;
 }
return vary;
}

aELEMENT(*vains(vary,pos,n))
aELEMENT(*vary);
int pos,n;
{
if(!vary || aLEN(vary)+n>aSIZ(vary)) vary=vaensure(vary,aLEN(vary)+n);
if(pos>=aLen(vary)) vary=vatrunc(vary,pos+n);
else
 {
 mbkwd(vary+pos+n,vary+pos,aLen(vary)-(pos+n)+1);
 aLen(vary)+=n;
 }
return vary;
}

aELEMENT(*vadel(vary,pos,n))
aELEMENT(*vary);
int pos,n;
{
if(pos>=aLEN(vary)) return vary;
if(pos+n>=aLen(vary)) return vatrunc(vary,pos);
mfwrd(vary+pos,vary+pos+n,aLen(vary)-(pos+n)+1);
aLen(vary)-=n;
return vary;
}

int _acmp(a,b)
aELEMENT(*a);
aELEMENT(*b);
{
return acmp(*a,*b);
}

aELEMENT(*vasort(ary,len))
aELEMENT(*ary);
int len;
{
if(!ary || !len) return ary;
qsort(ary,len,sizeof(aCAST),_acmp);
return ary;
}

int vabsearch(ary,len,el)
aELEMENT(*ary);
aELEMENT(el);
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
 switch(acmp(el,ary[z]))
  {
 case  1: x=z; break;
 case -1: y=z; break;
 case  0: return z;
  }
 }
return y;
}

int vafirst(ary,len,el)
aELEMENT(*ary);
aELEMENT(el);
int len;
{
int x;
if(!ary || !len) return ~0;
for(x=0;x!=len;++x) if(!acmp(ary[x],el)) return x;
return ~0;
}

int valast(ary,len,el)
aELEMENT(*ary);
aELEMENT(el);
int len;
{
int x=len;
if(!ary || !len) return ~0;
do
 {
 --x;
 if(!acmp(ary[x],el)) return x;
 }
 while(x);
return ~0;
}

int vacmpn(a,alen,b,blen)
aELEMENT(*a);
aELEMENT(*b);
int alen, blen;
{
int x,l;
int t;
if(!a && !b) return 0;
if(!a) return -1;
if(!b) return 1;
if(alen>blen) l=aLen(a);
else l=blen;
for(x=0;x!=l;++x) if(t=acmp(a[x],b[x])) return t;
if(alen>blen) return  1;
if(alen<blen) return -1;
return 0;
}

int vacmp(a,b)
aELEMENT(*a);
aELEMENT(*b);
{
return vacmpn(av(a),av(b));
}

int vaa(a,alen,b,blen)
aELEMENT(*a);
aELEMENT(*b);
int alen, blen;
{
int x;
if(!a && !b) return 0;
if(!a || !b) return ~0;
if(alen<blen) return ~0;
if(!blen) return 0;
for(x=0;x!=alen-blen;++x) if(!vacmpn(a,blen,b,blen)) return x;
return ~0;
}

int vascan(a,alen,b,blen)
aELEMENT(*a);
aELEMENT(*b);
int alen, blen;
{
int x;
for(x=0;x!=alen;++x)
 {
 int z=vabsearch(b,blen,a[x]);
 if(z<blen && !acmp(b[z],a[x])) return x;
 }
return ~0;
}

int vaspan(a,alen,b,blen)
aELEMENT(*a);
aELEMENT(*b);
int alen, blen;
{
int x;
for(x=0;x!=alen;++x)
 {
 int z=vabsearch(b,blen,a[x]);
 if(z==blen || acmp(b[z],a[x])) break;
 }
return x;
}

aELEMENT(*vawords(a,s,len,sep,seplen))
aELEMENT(*a);
char *s, *sep;
int len, seplen;
{
int x;
if(!a) a=vamk(10);
else a=vatrunc(a,0);
loop:
x=vsspan(s,len,sep,seplen);
s+=x; len-=x;
if(len)
 {
 x=vsscan(s,len,sep,seplen);
 if(x!= ~0)
  {
  a=vaadd(a,vsncpy(vsmk(x),0,s,x));
  s+=x; len-=x;
  if(len) goto loop;
  }
 else a=vaadd(a,vsncpy(vsmk(len),0,s,len));
 }
return a;
}

aELEMENT(*vaiwords(a,s,len,inc,inclen))
aELEMENT(*a);
char *s, *inc;
int len, inclen;
{
int x;
if(!a) a=vamk(10);
else a=vatrunc(a,0);
loop:
x=vsscan(s,len,inc,inclen);
s+=x; len-=x;
if(len)
 {
 x=vsspan(s,len,inc,inclen);
 if(x!= ~0)
  {
  a=vaadd(a,vsncpy(vsmk(x),0,s,x));
  s+=x; len-=x;
  if(len) goto loop;
  }
 else a=vaadd(a,vsncpy(vsmk(len),0,s,len));
 }
return a;
}
