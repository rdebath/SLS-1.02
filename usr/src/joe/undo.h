/* UNDO system
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

#ifndef _Iundo
#define _Iundo 1

#include "queue.h"

/* Number of undo records to keep */

#define UNDOKEEP 100

typedef struct undorec UNDOREC;
typedef struct undo UNDO;

extern int inundo;

/* An undo record */
struct undorec
 {
 LINK(UNDOREC) link;		/* Doubly linked list of undo records */
 int first;			/* Set if this is the first undo rec */
 int min;			/* Set if this is from a minor change */
 long where;			/* Buffer position this are from */
 int del;			/* Set for delete, Clr for insert */
 int len;			/* No. chars in this rec */
 UNDOREC *unit;			/* Last/First rec in unit */
 char *data;			/* The chars */
 };

/* An undo point */
struct undo
 {
 LINK(UNDO) link;		/* Doubly linked list of undo points */
 UNDOREC recs;			/* Undo records */
 int nrecs;			/* Number of undo records */
 UNDOREC *ptr;			/* Undo/Redo location */
 UNDOREC *first;		/* First of unit */
 UNDOREC *last;			/* Last of unit */
 };

void umclear();			/* Prevent combinding of undo recs */
void undomark();		/* Everything from prev. mark is a unit */
void undoend();
void undomk();
void undorm();
void uundo();
void uredo();
void undoins();
void undodel();

#endif
