#ifndef UTILSH
#define UTILSH

/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
/*  t. lord	Sun Aug  9 20:37:34 1992	*/

#include <stdio.h>
#include "funcdef.h"
#include "global.h"

extern char *argv_name;
extern int __make_backups;
extern int __backup_by_copying;

#ifdef __STDC__
extern VOIDSTAR ck_malloc (size_t);
extern VOIDSTAR ck_calloc (size_t);
extern VOIDSTAR ck_realloc (void *,size_t);
extern char * ck_savestr (char *);
#define ck_free free
#define ck_remalloc(OLD, SIZE) \
  ((OLD) ? ck_realloc ((OLD), (SIZE)) : ck_malloc (SIZE))

extern void get_usr_stats (int, char **);
extern void set_usr_stats (int, char **);

extern char *char_to_string (int);
extern int string_to_char (char **);
extern FILE *xopen_with_backup (const char *,const char *);
extern int xclose (FILE *);
extern char *err_msg (void);
extern char *mk_sprintf (char *, ...);

extern void init_mem (void);
extern void init_eval (void);
extern void init_refs (void);
extern void init_cells (void);
extern VOIDSTAR init_stack (void);

struct rng;
extern void panic_read_file (FILE *,int);
extern void panic_write_file (FILE *,struct rng *);
extern  int panic_set_options (int, char *);
extern void panic_show_options (void);

extern VOIDSTAR pop_stack (VOIDSTAR);
extern void push_stack (VOIDSTAR, VOIDSTAR);
extern void flush_stack (VOIDSTAR);
#else
extern VOIDSTAR ck_malloc ();
extern VOIDSTAR ck_calloc ();
extern VOIDSTAR ck_realloc ();
extern char * ck_savestr ();
#define ck_free free
#define ck_remalloc(OLD, SIZE) \
  ((OLD) ? ck_realloc ((OLD), (SIZE)) : ck_malloc (SIZE))

extern void get_usr_stats ();
extern void set_usr_stats ();

extern char *char_to_string ();
extern int string_to_char ();
extern FILE *xopen_with_backup ();
extern int xclose ();
extern char *err_msg ();
extern char *mk_sprintf ();

extern void init_mem ();
extern void init_eval ();
extern void init_refs ();
extern void init_cells ();
extern VOIDSTAR init_stack ();

extern void panic_read_file ();
extern void panic_write_file ();
extern  int panic_set_options ();
extern void panic_show_options ();

extern VOIDSTAR pop_stack ();
extern void push_stack ();
extern void flush_stack ();
#endif

#endif

