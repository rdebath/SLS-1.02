/* Oh no! Too many files!
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

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "config.h"
#include "vs.h"
#include "heap.h"
#include "toomany.h"

static int nopen=0;
static File openfiles={{&openfiles,&openfiles}};

static void toomany()
{
File *f;
for(f=openfiles.link.prev;f!=&openfiles;f=f->link.prev)
 if(f->fd!= -1)
  {
  close(f->fd);
  f->fd= -1;
  --nopen;
  return;
  }
}

static void reopen(file)
File *file;
{
if(nopen==Fmaxopen) toomany();
if(file->writeable) file->fd=open(file->name,O_RDWR /* | O_BINARY */);
else file->fd=open(file->name,O_RDONLY /* |O_BINARY */);
if(file->fd== -1)
 {
 /* Uh... */
 }
++nopen;
lseek(file->fd,file->pos,0);
}

File *Fopen(name)
char *name;
{
File *file;
int fd;
int writeable;
struct stat buf;
if(nopen==Fmaxopen) toomany();
fd=open(name,O_RDWR /*|O_BINARY*/);
if(fd== -1) fd=open(name,O_RDONLY/*|O_BINARY*/), writeable=0;
else writeable=1;
if(fd== -1) return NULL;
++nopen;
fstat(fd,&buf);
file=(File *)malloc(sizeof(File));
file->fd= fd;
file->writeable= writeable;
file->name=vsncpy(NULL,0,sz(name));
file->size=buf.st_size;
file->pos=0;
file->inode=buf.st_ino;
file->dev=buf.st_dev;
enquef(File,link,&openfiles,file);
return file;
}

void Fclose(file)
File *file;
{
if(file->fd!= -1) close(file->fd), --nopen;
deque(File,link,file);
free(file);
}

int Fread(file,buf,size)
File *file;
char *buf;
{
int amnt;
promote(File,link,&openfiles,file);
if(file->fd== -1) reopen(file);
amnt=read(file->fd,buf,size);
if(amnt>0) file->pos+=amnt;
return amnt;
}

int Fwrite(file,buf,size)
File *file;
char *buf;
{
int amnt;
if(!file->writeable) return -1;
promote(File,link,&openfiles,file);
if(file->fd== -1) reopen(file);
amnt=write(file->fd,buf,size);
if(amnt>0)
 {
 file->pos+=amnt;
 if(file->pos>file->size) file->size=file->pos;
 }
return amnt;
}

int Fseek(file,pos)
File *file;
long pos;
{
promote(File,link,&openfiles,file);
if(file->fd== -1) reopen(file);
file->pos=lseek(file->fd,pos,0);
if(file->pos<0) return -1;
return 0;
}
