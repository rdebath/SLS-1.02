/* Too many files!
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

#ifndef _Itoomany
#define _Itoomany 1

#include "config.h"
#include "queue.h"

typedef struct File File;

#define Fmaxopen 16

struct File
 {
 LINK(File) link;		/* Linked list of open files */
 int fd;			/* The file or -1 if closed */
 char *name;			/* Name of the file */
 int writeable;			/* Set if it's ok to write to the file */
 long size;			/* Current size of file */
 long pos;			/* Current file pointer */
 int inode;			/* Inode of file */
 int dev;			/* Device file is on */
 };

File *Fopen();
int Fread();
int Fwrite();
int Fseek();
void Fclose();

#endif
