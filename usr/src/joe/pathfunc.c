/* Directory and path functions
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
#include "vs.h"
#include "tty.h"
#include "pathfunc.h"

char *abspth(path)
char *path;
{
char *s=0;
int x=0;
int y;
if(path[0]=='/')
 {
 s=vsadd(s,'/');
 while(path[x]=='/') x++;
 y=1;
 }
else
 {
 if(!(s=pwd())) return 0;
 s=vsncpy(NULL,0,sz(s));
 if(s[1]) s=vsadd(s,'/');
 y=sLEN(s);
 }
while(path[x])
 {
 if(path[x]=='.' && (path[x+1]==0 || path[x+1]=='/'))
  {
  x++;
  while(path[x]=='/') x++;
  continue;
  }
 if(path[x]=='.' && path[x+1]=='.' &&
    (path[x+2]==0 || path[x+2]=='/'))
  {
  x+=2;
  while(path[x]=='/') x++;
  if(y!=1)
   {
   --y;
   while(s[y-1]!='/') --y;
   }
  continue;
  }
 do
  s=vsset(s,y,path[x]), ++y, ++x;
  while(path[x] && path[x]!='/');
 s=vsset(s,y,'/'), ++y;
 while(path[x]=='/') x++;
 }
if(y!=1 && s[y-1]=='/') --y;
s=vstrunc(s,y);
return s;
}

char *namprt(path)
char *path;
{
char *z=path+slen(path);
while(z!=path)
 if(z[-1]=='/') break;
 else --z;
return vsncpy(NULL,0,sz(z));
}

char *dirprt(path)
char *path;
{
char *z=path+slen(path);
while(z!=path)
 if(*(z-1)=='/') break;
 else --z;
return vsncpy(NULL,0,path,z-path);
}

char *begprt(path)
char *path;
{
char *z=path+slen(path);
while(z!=path && z[-1]=='/') --z;
if(z==path) return vsncpy(NULL,0,sz(path));
else
 {
 while(z!=path)
  if(z[-1]=='/') break;
  else --z;
 return vsncpy(NULL,0,path,z-path);
 }
}

char *endprt(path)
char *path;
{
char *z=path+slen(path);
while(z!=path && z[-1]=='/') --z;
if(z==path) return vsncpy(NULL,0,sc(""));
else
 {
 while(z!=path)
  if(z[-1]=='/') break;
  else --z;
 return vsncpy(NULL,0,sz(z));
 }
}

int mkpath(path)
char *path;
{
char *s;
if(path[0]=='/')
 {
 if(chddir("/")) return 1;
 s=path;
 goto in;
 }
while(path[0])
 {
 int c;
 while(*s && *s!='/') s++;
 c= *s; *s=0;
 if(chddir(path))
  {
  if(mkdir(path,0777)) return 1;
  if(chddir(path)) return 1;
  }
 *s=c;
 in:
 while(*s=='/') s++;
 path=s;
 }
return 0;
}

/* Create a temporary file */

char *mktmp(where)
char *where;
{
static int seq=0;
char *name=0;
int fd;
loop:
name=vsfmt(name,"%sJ%d%d.tmp",where,seq++%1000,(int)time(NULL)%1000);
if((fd=open(name,0))!= -1)
 {
 close(fd);
 goto loop;
 }
if((fd=creat(name,0777))== -1) return 0;
if(close(fd)) return 0;
return name;
}
