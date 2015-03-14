/* TURBO-C directory interface
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
#include "heap.h"
#include "zstr.h"
#include "blocks.h"
#include "dir.h"

int findfirst();
int findnext();

struct ffblk
 {
 char ff_reserved[21];
 char ff_attrib;
 int ff_ftime;
 int ff_fdate;
 long ff_fsize;
 char ff_name[13];
 };

struct duh
 {
 struct ffblk ffblk;
 int first;
 };

void *opendir(name)
char *name;
{
struct duh *duh=(struct duh *)malloc(sizeof(struct duh));
duh->first=findfirst("*.*",&duh->ffblk,0);
return duh;
}

void closedir(f)
struct duh *f;
{
free(f);
}

struct direct *readdir(f)
struct duh *f;
{
static struct direct direct;
while(f->first!= -1)
 if(!f->first)
  {
  zcpy(direct.d_name,f->ffblk.ff_name);
  f->first=1;
  return &direct;
  }
 else f->first=findnext(&f->ffblk);
return 0;
}
