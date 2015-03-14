/* Buffer management
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


#ifndef _Ib
#define _Ib 1

#include "config.h"
#include "queue.h"
#include "vfile.h"
#include "undo.h"

typedef struct header H;
typedef struct point P;
typedef struct buffer B;

#define HDRSIZ 16		/* Text segment header size */
#define SEGSIZ 1024		/* Text segment size */

#define TABSIZ 8		/* Tab width... this should be a variable */

/* A text segment header */

struct header
 {
 LINK(H) link;			/* Doubly-linked list of headers */
 long seg;			/* Virtual address of text segment */
 unsigned short hole;		/* Offset to hole */
 unsigned short ehole;		/* Offset to text following hole */
 };

/* A buffer */

extern int force;
extern char *stdio;		/* If file name is set to this, use stdio */

struct buffer
 {
 LINK(B) link;			/* Doubly-linked list of buffers */
 VFILE *text;			/* Software virtual memory for this buffer */
 long fretxt;			/* Linked list of free text */
 P *bof;			/* Address of BOF pointer */
 P *eof;			/* Address of EOF pointer */
 char *name;			/* File name/0 for unnamed/stdio */
 int count;			/* Reference count */
 int chnged;			/* Set if changed since last save */
 int backup;			/* Set if backup file needn't be written */
 char **ctab;			/* Character table to use with this buffer */
 UNDO *undo;			/* Undo storage */
 };

/* A pointer */

struct point
 {
 LINK(P) link;			/* Doubly-linked list of pointers */

 /* Physical section of pointer
  * These must be absolutely correct for the pointer to actually be pointing
  * anywhere */
 B *b;				/* Buffer this pointer is in */
 int ofst;			/* Byte offset into current segment */
 char *ptr;			/* Address of current segment */
 H *hdr;			/* Address of segment's header */

 /* Logical section
  * These give more information about the pointer's location.  Sometimes
  * one or more of these may not be correctly updated */
 long byte;			/* Byte offset from beginning of file */
 long line;			/* Line number */
 long col;			/* Column number */
 long xcol;			/* Extended column number */
 long lbyte;			/* Byte number */
 
 P **owner;			/* Owner of this pointer */
 };

/*******************************/
/* Buffer management functions */
/*******************************/

/* B *bmk(char **ctab);
 * Create a new empty buffer.  Uses given character set table.
 * The following variables get these initializations:
 *     name            NULL
 *     reference count 1
 *     backup flag     1
 *     changed flag    0
 */
B *bmk();

/* B *bfind(char *name);
 * Find file.  Returns with buffer of already loaded file or 0.  If the
 * file is found, the buffer's reference count is incremented.
 */
B *bfind();

/* void brm(B *b);
 * Decrement a buffer's reference count.  If it reaches zero, eliminate the
 * buffer.
 */
void brm();

/********************************/
/* Pointer management functions */
/********************************/

/* P *pdup(P *p);
 * Duplicate a pointer.  The new pointer's owner is set to NULL.
 */
P *pdup();

/* P *pdupown(P *p,P **o);
 * Duplicate pointer and set ownership of pointer
 */
P *pdupown();

/* P *pset(P *p,P *n);
 * Force a pointer p to point to the same place as another pointer n.
 * Caution: this assumes that the original pointer p is pointing to a
 * valid locked text segment.  If it doesn't, you must clear p->ptr
 * before calling this function.
 */
P *pset();

/* P *peof(P *p);
 * Force a pointer to point to the end of file
 */
P *peof();

/* P *pbof(P *p);
 * Force a pointer to point to the beginning of file
 */
P *pbof();

/* void prm(P *p);
 * Remove a pointer.  Clears the pointer's owner address.
 */
void prm();

/* int pisbof(P *p);
 * Return true if pointer is at beginning of file
 * This can be used on pointers with incorrect logical sections
 */
int pisbof();

/* int piseof(P *p);
 * Return true if pointer is at end of file
 * Ok to use if the pointer's logical section is incorrect or if the buffer's
 * end of file pointer is incorrect.
 */
int piseof();

/* int pisbol(P *p);
 * Return true if pointer is at beginning of line.
 * Ok to use if the pointer's logical section is incorrect.
 */
int pisbol();

/* int piseol(P *p);
 * Return true if pointer is at end of line
 * This can be used if the pointer's logical section is incorrect.  It can
 * also be used if the buffer's end of file pointer is incorrect.
 */
int piseol();

/* int pisbow(P *p);
 * Return true if pointer is at beginning of a word
 */
int pisbow();

/* int piseow(P *p);
 * Return true if pointer is at end of word
 */
int piseow();

/* int pnext(P *p);
 * Advance pointer to beginning of next segment.  Only updates physical part
 * of pointer.  Returns false if the pointer was at or reached end of buffer.
 */
int pnext();

/* int pprev(P *p);
 * Set pointer to after end of previous segment.  Only updates physical part of
 * pointer.  Returns false if pointer was at or reached beginning of buffer.
 * Warning: this function sets the pointer to an invalid place.  Only use this
 * if p->ofst will be subsequently decremented before any characters are read.
 */
int pprev();

/* int pgetcn(P *p);
 * Get character at pointer and advance pointer.  Only updates physical
 * section of pointer.  Workds ok if buffer's end of file pointer is
 * incorrect.  Returns -1 if pointer was at end of buffer.
 */
int pgetcn();

/* int pgetc(P *p);
 * Get character at pointer and advance pointer.  Returns -1 if pointer
 * was at end of file.  Works ok if buffer's end of file pointer is incorrect.
 */
int pgetc();

/* P *pfwrdn(P *p,long n);
 * Advance pointer by n characters.  Only the physical section of the pointer
 * is updated.  Works ok if buffer's end of buffer pointer is incorrect.
 */
P *pfwrdn();

/* P *pfwrd(P *p,long n);
 * Advance pointer by n characters.  If the pointer goes past the end of the
 * buffer, the pointer is set to the end of the buffer and 0 is returned
 * instead of the pointer.
 */
P *pfwrd();

/* int prgetcn(P *p);
 * Move pointer back one and return character that position.  Returns -1
 * if pointer was at beginning of file.  Only updates physical part of
 * pointer.
 */
int prgetcn();

/* int prgetc(P *p);
 * Move pointer back one and return character at that position.  Returns -1
 * if pointer was at beginning of file.
 */
int prgetc();

/* P *pbkwdn(P *p,long n);
 * Move pointer back n characters.  Returns 0 on attempt to go before
 * beginning of buffer.  Only updates physical section of pointer.
 */
P *pbkwdn();

/* P *pbkwdf(P *p,long n);
 * Move pointer back n characters.  Returns 0 on attempt to go before
 * beginning of buffer.  Only updates line number part of logical section
 * of pointer.
 */
P *pbkwdf();

/* P *pfcol(P *p);
 * Determine and set column number of line byte number parts of logical
 * section of p.  Works ok if end of file pointer is incorrect.
 */
P *pfcol();

/* P *pbkwd(P *p,long n);
 * Move pointer back n characters.  Returns 0 if attempt to move before
 * beginning of buffer.
 */
P *pbkwd();

/* P *pbol(P *p);
 * Set pointer to beginning of line.  'lbyte' of logical part of pointer
 * must be correct.
 */
P *pbol();

/* P *pboln(P *p);
 * Set pointer to beginning of line.  Works ok if logical part of pointer
 * is not correct.
 */
P *pboln();

/* P *peol(P *p);
 * Set pointer to end of line.  Works ok if end of buffer pointer is
 * incorrect.
 */
P *peol();

/* P *pnextl(P *p);
 * Set pointer to beginning of next line or end of last line.  Returns 0
 * for end of last line.  Column number section of logical part of pointer
 * is not set.
 */
P *pnextl();

/* P *pprevl(P *p);
 * Set pointer to end of previous line or beginning of first line.  Returns
 * 0 if we got to the beginning part of the first line.  Column number
 * section of logicl part of pointer is not set.
 */
P *pprevl();

/* P *pline(P *p,long line);
 * Goto beginning of given line number.  Line number part of logical section
 * of pointer (and eof pointer) must be correct.
 */
P *pline();

/* P *pcol(P *p,long col);
 * Goto given column number.  The original column number (and lbyte) need
 * not be correct.
 */
P *pcol();

/* P *pfindrn(P *p,char *s,int len);
 * Search reverse for string
 */
P *pfindrn();

/* P *pfindrni(P *p,char *s,int len);
 * Search reverse for string case insensitive
 */
P *pfindrni();

/* P *pfindfn(P *p,char *s,int len);
 * Find first occurance of string s/len beginning at p.  If found, returns
 * p pointing to beginning of string.  If not found, returns 0.
 */
P *pfindfn();

/* P *pfindfni(P *p,char *s,int len);
 * Case insensitive version of above
 */
P *pfindfni();

/* int brc(P *p);
 * Read character at a pointer or return -1 if pointer was at end of the buffer
 */
int brc();

/* char *brmem(P *p,char *blk,int size);
 * Read characters into a memory block
 */
char *brmem();

/* char *brs(P *p,int size);
 * Read a zero terminated string into an malloc block and return the malloc
 * block
 */
char *brs();

/* char *brvs(P *p,int size);
 * Read a variable length string
 */
char *brvs();

/* char *parsens(char *s,long skip,long amnt);
 * Parse file name.
 */
char *parsens();

/* int bsavefd(P *p,int fd,long size);
 * Write 'size' bytes beginning at 'p' to 'fd'.
 */
int bsavefd();

/* int bsave(P *p,char *name,long size);
 * Save characters into a file
 */
int bsave();

/* P *bdel(P *from,P *to);
 * Delete characters from a buffer
 */

P *bdel();

/* P *binsc(P *p,char c);
 * Insert a character into a buffer
 */
P *binsc();

/* P *binsm(P *p,char *blk,int size);
 * Insert a memory block into a buffer
 */
P *binsm();

/* P *binss(P *p,char *s);
 * Insert zero terminated string into a buffer
 */
P *binss();

/* P *binsb(P *p,P *from,P *to);
 * Insert characters from another buffer
 */
P *binsb();

/* int binsfd(P *p,int fd,long max);
 * Insert a file into a buffer.  No more than 'max' bytes are loaded from
 * the file.
 */
int binsfd();

/* int binsf(P *p,char *name);
 * Insert a file into a buffer
 */
int binsf();

/* int bload(B *b,char *s);
 * Load a file into a buffer
 */
int bload();

void refigure();

#endif
