/* Text editing windows
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

#ifndef _Itw
#define _Itw 1

#include "config.h"
#include "kbd.h"
#include "main.h"

typedef struct tw TW;

extern int starow, stacol;

struct tw
 {
 /* Status line info */
 char *stanam;
 char *stalin;
 int stamod;
 int stahlp;
 struct recmac *starec;
 int starow;
 int stacol;
 int staupd;			/* Set if status line should get updated */
 };

#define TYPETW 0x100

extern CONTEXT cmain;

/* W *wmktw(SCREEN *t,B *b)
 */
W *wmktw();

void uaborttw();
void usplitw();
void ucheck();
void ucheckp();

#endif
