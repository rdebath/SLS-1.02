/* Edit buffer window generation
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

#ifndef _Ibw
#define _Ibw 1

#include "config.h"
#include "b.h"
#include "w.h"

extern int dspattr;
extern int dspasis;

typedef struct bw BW;

struct bw
 {
 B *b;
 P *top;
 P *cursor;
 long offset;
 SCREEN *t;
 int h,w,x,y;
 
 long lmargin;
 long rmargin;
 int autoindent;
 int wordwrap;
 int overtype;
 
 void *object;
 };

extern char *ctab[];
extern int mid;
void bwfllw();
void bwins();
void bwdel();
void bwgen();
BW *bwmk();
void bwmove();
void bwresz();
void bwrm();
void ustat();

#endif
