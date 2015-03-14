/* Keyboard handler
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

#ifndef _Ikbd
#define _Ikbd 1

#include "config.h"
#include "macro.h"

typedef struct key KEY;
typedef struct kmap KMAP;
typedef struct cmd CMD;
typedef struct context CONTEXT;
typedef struct kbd KBD;


typedef struct options OPTIONS;
struct options
 {
 OPTIONS *next;
 char *name;
 int overtype;
 long lmargin;
 long rmargin;
 int autoindent;
 int wordwrap;
 };

void setoptions();

/* A key to macro binding in a keymap */

struct key
 {
 int k;			/* Key value */
 union
  {
  MACRO *macro;		/* Macro or */
  KMAP *submap;		/* Submap address (for prefix keys) */
  } value;
 };

/* A map of keycode to command/sub-map bindings */

struct kmap
 {
 int len;		/* Number of KEY entries */
 int size;		/* Malloc size of block */
 KEY *keys;		/* KEYs.  Sorted. */
 };

/* Masks & bits for 'k' entry in KEY */

#define KEYMASK 0x7fff	/* Mask to get key value */
#define KEYSUB 0x8000	/* Bit set for prefix key */

/* Command entry */

struct cmd
 {
 char *name;		/* Command name */
 int flag;		/* Execution flags */
 void (*func)();	/* Function bound to name */
 };

/* A Context (a set of bindings) */

struct context
 {
 char *name;		/* Name of this context */
 KMAP *kmap;		/* Keymap for this context */
 };

/* Command table */

struct cmdtab
 {
 CMD *cmd;		/* The entries themselves (sorted by name) */
 int len;		/* Number of entries */
 };

typedef struct cmdtab CMDTAB;
/* The command table */

extern CMDTAB cmdtab;

/* A keyboard handler */

struct kbd
 {
 KMAP *curmap;		/* Current keymap */
 KMAP *topmap;		/* Top-level keymap */
 };

/* KBD *mkkbd(CONTEXT *context);
   Create a keyboard handler for the given context.
*/
KBD *mkkbd();

/* void rmkbd(KBD *);
 *
 * Eliminate a keyboard handler
 */
void rmkbd();

/* MACRO *dokey(KBD *kbd,char k);
   Handle a key for a KBD:

     Returns 0 for invalid or prefix keys

     Returns a macro address for completed key-sequences
*/
MACRO *dokey();

/* int prokbd(char *name,CONTEXT **cmds);  Process a keymap set-up file into
   the list of contexts.  Returns 0 for success or -1 for error
*/
int prokbd();

/* int findcmd(CMDTAB *cmdtab,char *s);
 * Return command table index for the named command
 */
int findcmd();
struct help *get_help(char *name);

#endif
