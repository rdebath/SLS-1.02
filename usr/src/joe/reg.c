/* Generate list of matching directory entries
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
#include <sys/types.h>
#include <sys/dir.h>
#include "config.h"
#include "vs.h"
#include "va.h"
#include "regex.h"

char **rexpnd(path,word)
char *path, *word;
{
void *dir;
char **lst=0;
struct direct *de;
if(path && path[0]) dir=opendir(path);
else dir=opendir(".");
if(!dir) return 0;
while(de=readdir(dir))
 if(zcmp(".",de->d_name))
  if(rmatch(word,de->d_name))
   lst=vaadd(lst,vsncpy(NULL,0,de->d_name,slen(de->d_name)));
closedir(dir);
return lst;
}
