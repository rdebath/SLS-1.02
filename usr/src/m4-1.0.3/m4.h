/* -*- c -*-
 * GNU m4 -- A simple macro processor
 * Copyright (C) 1989-1992 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef __STDC__
#define const
#define Args(x) ()
#else
#define Args(x) x
#endif

#ifndef __GNUC__
#define volatile
#else
#define volatile __volatile__
#endif

#include <stdio.h>
#include <ctype.h>
#include <varargs.h>

#include "obstack.h"

#if defined (HAVE_STRING_H) || defined (STDC_HEADERS)
#include <string.h>
#ifndef index
#define	index(s, c)	strchr ((s), (c))
#endif
#ifndef rindex
#define	rindex(s, c)	strrchr ((s), (c))
#endif

#ifdef NEED_MEMORY_H
#include <memory.h>
#endif /* NEED_MEMORY_H */

/* This is for obstack code -- should live in obstack.h.  */
#define bcopy(s,d,n)	memcpy ((d), (s), (n))
#else /* not HAVE_STRING_H and not STDC_HEADERS */
#include <strings.h>

extern void bcopy ();
#endif /* not HAVE_STRING_H and not STDC_HEADERS */

#ifdef STDC_HEADERS
#include <stdlib.h>
#else /* STDC_HEADERS */
extern void *malloc ();
extern void *realloc ();
extern char *getenv ();
#endif /* STDC_HEADERS */

extern char *mktemp ();



#define obstack_chunk_alloc	xmalloc
#define obstack_chunk_free	xfree

/* If FALSE is defined, we presume TRUE is defined too.  In this case,
   merely typedef boolean as being int.  Or else, define these all.  */
#ifndef FALSE
/* Do not use `enum boolean': this tag is used in SVR4 <sys/types.h>.  */
typedef enum { FALSE = 0, TRUE = 1 } boolean;
#else
typedef int boolean;
#endif

#include <errno.h>
#ifndef errno
extern int errno;
#endif

extern int sys_nerr;
extern char *sys_errlist[];

#define syserr() ((errno > 0 && errno < sys_nerr) ? sys_errlist[errno] : "Unknown error")


/* Miscellaneous, that must come first.  */
typedef void builtin_func ();
typedef struct token_data token_data;


/* File: m4.c  --- global definitions.  */

/* Option flags.  */
extern int sync_output;			/* -s */
extern int debug_level;			/* -d */
extern int hash_table_size;		/* -H */
extern int ndiversion;			/* -N */
extern int no_gnu_extensions;		/* -g */
extern int max_debug_argument_length;	/* -l */
extern int suppress_warnings;		/* -Q */

/* Error handling.  */
extern void warning ();		/* varargs */
extern void error ();		/* varargs */
extern volatile void fatal ();	/* varargs */
extern volatile void internal_error ();	/* varargs */

/* Memory allocation.  */
extern void *xmalloc Args ((unsigned int));
extern void *xrealloc Args ((void *, unsigned int));
extern void xfree Args ((void *));
extern char *xstrdup Args ((const char *));


/* File: debug.c  --- debugging and tracing function.  */

/* debug_level is a bitmask of these.  */
enum debug_info
{
  DEBUG_TRACE_ARGS = 0x01,	/* a: show arglist in trace output */
  DEBUG_TRACE_EXPANSION = 0x02,	/* e: show expansion in trace output */
  DEBUG_TRACE_QUOTE = 0x04,	/* q: quote args and expansion in trace output */
  DEBUG_TRACE_ALL = 0x08,	/* t: trace all macros -- overrides trace{on,off} */
  DEBUG_TRACE_LINE = 0x10,	/* l: add line numbers to trace output */
  DEBUG_TRACE_FILE  = 0x20,	/* f: add file name to trace output */
  DEBUG_TRACE_PATH = 0x40,	/* p: trace path search of include files */
  DEBUG_TRACE_CALL = 0x80,	/* c: show macro call before args collection */
  DEBUG_TRACE_INPUT = 0x100,	/* i: trace changes of input files */
  DEBUG_TRACE_CALLID = 0x200,	/* x: add call id to trace output */

  DEBUG_TRACE_VERBOSE = 0x3ff,	/* V: very verbose --  print everything */
  DEBUG_TRACE_DEFAULT = 0x07	/* default flags -- equiv: aeq */
};

extern void debug_init Args ((void));
extern int debug_decode Args ((char *));
extern boolean debug_set_output Args ((char *));
extern void debug_print ();	/* varargs */
extern void debug_message ();	/* varargs */

extern void trace_prepre Args ((char *, int));
extern void trace_pre Args ((char *, int, int, token_data **));
extern void trace_post Args ((char *, int, int, token_data **, char *));


/* File: input.c  --- lexical definitions.  */

/* Various different token types.  */
enum token_type
{
  TOKEN_EOF,			/* end of file */
  TOKEN_STRING,			/* a quoted string */
  TOKEN_WORD,			/* an identifier */
  TOKEN_SIMPLE,			/* a single character */
  TOKEN_MACDEF			/* a macros definition (see "defn") */
};

/* The amount of data for a token, a macro argument, and a macro definition.
   */
enum token_data_type
{
  TOKEN_VOID,
  TOKEN_TEXT,
  TOKEN_FUNC
};

struct token_data
{
  enum token_data_type type;
  union
    {
      struct
	{
	  char *text;
	}
      u_t;
      struct
	{
	  builtin_func *func;
	  boolean traced;
	}
      u_f;
    }
  u;
};

#define TOKEN_DATA_TYPE(td)		((td)->type)
#define TOKEN_DATA_TEXT(td)		((td)->u.u_t.text)
#define TOKEN_DATA_FUNC(td)		((td)->u.u_f.func)
#define TOKEN_DATA_FUNC_TRACED(td) 	((td)->u.u_f.traced)

typedef enum token_type token_type;
typedef enum token_data_type token_data_type;

extern void input_init Args ((void));
extern int peek_input Args ((void));
extern token_type next_token Args ((token_data *));
extern void skip_line Args ((void));

/* push back input */
extern void push_file Args ((FILE *, const char *));
extern void push_macro Args ((builtin_func *, boolean));

extern struct obstack *push_string_init Args ((void));
extern char *push_string_finish Args ((void));

extern void push_wrapup Args ((char *));
extern boolean pop_wrapup Args ((void));

/* current input file, and line */
extern char *current_file;
extern int current_line;

/* left and right quote, begin and end comment */
extern char *bcomm, *ecomm;
extern char *lquote, *rquote;
/* lenght of quote strings */
extern int len_lquote, len_rquote;

#define DEF_LQUOTE "`"
#define DEF_RQUOTE "\'"
#define DEF_BCOMM "#"
#define DEF_ECOMM "\n"

extern void set_quotes Args ((char *, char *));
extern void set_comment Args ((char *, char *));


/* File: output.c --- output functions.  */
extern int output_current_line;

#define NDIVERSIONS	10	/* default, overridden by -Nnum */

extern void output_init Args ((void));
extern void shipout_text Args ((struct obstack *, char *));
extern void make_diversion Args ((int));
extern void insert_diversion Args ((int));
extern void insert_file Args ((FILE *));


/* File symtab.c  --- symbol table definitions.  */

/* Operation modes for lookup_symbol ().  */
enum symbol_lookup
{
  SYMBOL_LOOKUP,
  SYMBOL_INSERT,
  SYMBOL_DELETE,
  SYMBOL_PUSHDEF,
  SYMBOL_POPDEF
};

/* Symbol table entry.  */
struct symbol
{
  struct symbol *next;
  boolean traced;
  boolean shadowed;
  boolean macro_args;

  char *name;
  token_data data;
};

#define SYMBOL_NEXT(s)		((s)->next)
#define SYMBOL_TRACED(s)	((s)->traced)
#define SYMBOL_SHADOWED(s)	((s)->shadowed)
#define SYMBOL_MACRO_ARGS(s)	((s)->macro_args)
#define SYMBOL_NAME(s)		((s)->name)
#define SYMBOL_TYPE(s)		(TOKEN_DATA_TYPE (&(s)->data))
#define SYMBOL_TEXT(s)		(TOKEN_DATA_TEXT (&(s)->data))
#define SYMBOL_FUNC(s)		(TOKEN_DATA_FUNC (&(s)->data))

typedef enum symbol_lookup symbol_lookup;
typedef struct symbol symbol;
typedef void hack_symbol ();

#define HASHMAX 509		/* default, overridden by -Hsize */

extern symbol **symtab;

extern void symtab_init Args ((void));
extern symbol *lookup_symbol Args ((const char *, symbol_lookup));

extern void hack_all_symbols Args ((hack_symbol *, char *));


/* File: macro.c  --- macro expansion.  */

extern void expand_input Args ((void));
extern void call_macro Args ((symbol *, int, token_data **, struct obstack *));


/* File: builtin.c  --- builtins.  */

struct builtin
{
  char *name;
  boolean gnu_extension;
  boolean groks_macro_args;
  builtin_func *func;
};

struct predefined
{
  const char *unix_name;
  const char *gnu_name;
  const char *func;
};

typedef struct builtin builtin;
typedef struct predefined predefined;

extern void builtin_init Args ((void));
extern void define_user_macro Args ((const char *, const char *, symbol_lookup));
extern void undivert_all Args ((void));
extern void expand_user_macro Args ((struct obstack *, symbol *, int, token_data **));

extern const builtin *find_builtin_by_addr Args ((builtin_func *));


/* File: path.c  --- path search for include files.  */

extern void include_init Args ((void));
extern void include_env_init Args ((void));
extern void add_include_directory Args ((char *));
extern FILE *path_search Args ((const char *));


/* File: eval.c  --- expression evaluation.  */

extern boolean evaluate Args ((char *, int *));


/* File: format.c  --- printf like formatting.  */

extern void format Args ((struct obstack *, int, token_data **));


/* Debug stuff.  */

#ifdef DEBUG
#define DEBUG_INPUT
#define DEBUG_MACRO
#define DEBUG_SYM
#define DEBUG_INCL
#endif
