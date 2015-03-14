/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "funcdef.h"
#include <stdio.h>

#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"

#include "sysdef.h"
#include "global.h"
#include "node.h"
#include "eval.h"
#include "math.h"
#include "hash.h"
#include "ref.h"


#ifdef __STDC__
extern int yyparse (void);
#else
extern int yyparse ();
#endif

extern struct function busi_funs[];
extern struct function string_funs[];
extern struct function cells_funs[];
extern char *instr;
extern int parse_error;
extern struct node *parse_return;
extern void sort ();


#ifdef __STDC__
static void add_backpatch (unsigned, unsigned);
#else
static void add_backpatch ();
#endif

struct backpatch
  {
    unsigned from, to;
  };

static struct backpatch *patches;
static int patches_allocated;
static int patches_used;
static void *fn_stack;
static void *str_stack;
struct obstack tmp_mem;
void *tmp_mem_start;


#define V (void(*)())

/* These have to go in some file or other, so it is stuck in here (for now).
 */
struct function the_funs[] =
{
  {0, X_A0, "", 0, "<END>"},
  {0, X_A0, "", 0, "<DUMMY1>"},

  {C_IF | R | INF (1), X_A1 | X_J, "D", 0, "?"},
  {C_IF | R | INF (1), X_A1 | X_JL, "D", 0, "?"},
  {C_IF, X_A1 | X_J, "D", 0, "if"},
  {C_IF, X_A1 | X_JL, "D", 0, "if"},
  {C_ANDOR, X_A1 | X_J, "D", 0, "and"},
/* { C_ANDOR|L|INF(3), X_A1, "DD", 0,		"&" }, */
  {C_ANDOR, X_A1 | X_JL, "D", 0, "and"},
/* { C_ANDOR|L|INF(3), X_A1, "DD", 0,		"&" }, */
  {C_ANDOR, X_A1 | X_J, "D", 0, "or"},
/* { C_ANDOR|L|INF(2), X_A1, "DD", 0,		"|" }, */
  {C_ANDOR, X_A1 | X_JL, "D", 0, "or"},
/* { C_ANDOR|L|INF(2), X_A1, "DD", 0,		"|" }, */
  {C_STR, X_A0 | X_J, "", 0, "\"%s\""},
  {C_STR, X_A0 | X_JL, "", 0, "\"%s\""},

  {C_CELL, X_A0, "", 0, "$%s$%u"},
  {C_CELL, X_A0, "", 0, "$%s%u"},
  {C_CELL, X_A0, "", 0, "%s$%u"},
  {C_CELL, X_A0, "", 0, "%s%u"},
  {C_RANGE, X_A0, "", 0, "$%s$%u:$%s$%u"},
  {C_RANGE, X_A0, "", 0, "$%s%u:$%s$%u"},
  {C_RANGE, X_A0, "", 0, "$%s$%u:$%s%u"},
  {C_RANGE, X_A0, "", 0, "$%s%u:$%s%u"},
  {C_RANGE, X_A0, "", 0, "%s$%u:$%s$%u"},
  {C_RANGE, X_A0, "", 0, "%s%u:$%s$%u"},
  {C_RANGE, X_A0, "", 0, "%s$%u:$%s%u"},
  {C_RANGE, X_A0, "", 0, "%s%u:$%s%u"},
  {C_RANGE, X_A0, "", 0, "$%s$%u:%s$%u"},
  {C_RANGE, X_A0, "", 0, "$%s%u:%s$%u"},
  {C_RANGE, X_A0, "", 0, "$%s$%u:%s%u"},
  {C_RANGE, X_A0, "", 0, "$%s%u:%s%u"},
  {C_RANGE, X_A0, "", 0, "%s$%u:%s$%u"},
  {C_RANGE, X_A0, "", 0, "%s%u:%s$%u"},
  {C_RANGE, X_A0, "", 0, "%s$%u:%s%u"},
  {C_RANGE, X_A0, "", 0, "%s%u:%s%u"},

  {C_CONST, X_A0, "", 0, tname},
  {C_CONST, X_A0, "", 0, fname},

  {C_CONST, X_A0, "", 0, iname},
  {C_CONST, X_A0, "", 0, mname},
  {C_CONST, X_A0, "", 0, nname},
  {C_ERR, X_A0 | X_J, "", 0, "%s"},
  {C_FLT, X_A0, "", 0, "%.15g"},
  {C_INT, X_A0, "", 0, "%ld"},

  {C_VAR, X_A0, "", 0, "%s"},

  {C_UNA, X_A1, "F", 0, "-"},
  {C_UNA, X_A1, "B", 0, "!"},

  {C_INF | L | INF (6), X_A2, "NN", 0, "-"},
  {C_INF | L | INF (7), X_A2, "NN", 0, "/"},
  {C_INF | L | INF (7), X_A2, "NN", 0, "%"},
  {C_INF | L | INF (7), X_A2, "NN", 0, "*"},
  {C_INF | L | INF (6), X_A2, "NN", 0, "+"},
  {C_INF | L | INF (2), X_A2, "SS", 0, "&"},
  {C_INF | N | INF (4), X_A2, "AA", 0, "="},
  {C_INF | N | INF (5), X_A2, "AA", 0, ">="},
  {C_INF | N | INF (5), X_A2, "AA", 0, ">"},
  {C_INF | N | INF (5), X_A2, "AA", 0, "<"},
  {C_INF | N | INF (5), X_A2, "AA", 0, "<="},
  {C_INF | N | INF (4), X_A2, "AA", 0, "!="},
  {C_INF | R | INF (8), X_A2, "FF", V pow, "^"},

  {C_FN0, X_A0, "", 0, "pi"},
  {C_FN0X, X_A0, "", 0, "row"},
  {C_FN0X, X_A0, "", 0, "col"},
  {C_FN0 | C_T, X_A0, "", 0, "now"},

  {C_FN1, X_A1, "F", V fabs, "abs"},
  {C_FN1, X_A1, "F", V acos, "acos"},
  {C_FN1, X_A1, "F", V asin, "asin"},
  {C_FN1, X_A1, "F", V atan, "atan"},
  {C_FN1, X_A1, "F", V ceil, "ceil"},
  {C_FN1, X_A1, "F", V to_int, "int"},
  {C_FN1, X_A1, "F", V floor, "floor"},
  {C_FN1, X_A1, "F", V cos, "cos"},
  {C_FN1, X_A1, "F", V dtr, "dtr"},
  {C_FN1, X_A1, "F", V exp, "exp"},
  {C_FN1, X_A1, "F", V log, "log"},
  {C_FN1, X_A1, "F", V log10, "log10"},
  {C_FN1, X_A1, "F", V rtd, "rtd"},
  {C_FN1, X_A1, "F", V sin, "sin"},
  {C_FN1, X_A1, "F", V sqrt, "sqrt"},
  {C_FN1, X_A1, "F", V tan, "tan"},
  {C_FN1, X_A1, "I", 0, "ctime"},
  {C_FN1, X_A1, "A", 0, "negate"},
  {C_FN1, X_A1, "A", 0, "not"},
  {C_FN1, X_A1, "A", 0, "iserr"},
  {C_FN1, X_A1, "A", 0, "isnum"},

  {C_FN1 | C_T, X_A1, "I", 0, "rnd"},
  {C_FN1, X_A1, "R", 0, "rows"},
  {C_FN1, X_A1, "R", 0, "cols"},
  {C_FN2, X_A2, "FF", V atan2, "atan2"},
  {C_FN2, X_A2, "FF", V hypot, "hypot"},
  {C_FN2, X_A2, "FI", 0, "fixed"},
  {C_FN2, X_A2, "AA", 0, "iferr"},
  {C_FN2, X_A2, "RI", 0, "index"},
  {C_FN3, X_A3, "RII", 0, "index"},
  {C_FNN, X_AN, "IAAA", 0, "oneof"},

  {C_FNN, X_AN, "SIIA", 0, "file"},
  {C_FNN, X_AN, "EEEE", 0, "sum"},
  {C_FNN, X_AN, "EEEE", 0, "prod"},
  {C_FNN, X_AN, "EEEE", 0, "avg"},
  {C_FNN, X_AN, "EEEE", 0, "std"},
  {C_FNN, X_AN, "EEEE", 0, "max"},
  {C_FNN, X_AN, "EEEE", 0, "min"},
  {C_FNN, X_AN, "EEEE", 0, "count"},
  {C_FNN, X_AN, "EEEE", 0, "var"},

};

#ifdef USE_DLD
int n_usr_funs;
struct function **usr_funs;
int *usr_n_funs;
#else
int n_usr_funs = 3;
static struct function *__usr_funs[] =
{
  busi_funs,
  string_funs,
  cells_funs,
};
static int __usr_n_funs[] =
{
  18, 11, 10
};

struct function **usr_funs = __usr_funs;
int *usr_n_funs = __usr_n_funs;
#endif

/* ... A whole huge empty space, then ... */
struct function skip_funs[] =
{
  {C_SKIP, X_A0 | X_J, "", 0, "<Skip %u>"},
  {C_SKIP, X_A0 | X_JL, "", 0, "<SkipL %u>"},
};

/* The memory allocated here is used for several things, but byte_compile
   is a small file, so it might as well be here */
void
init_mem ()
{
  int n;

  parse_hash = hash_new ();
  hash_insert (parse_hash, the_funs[F_IF].fn_str, &the_funs[F_IF]);
  hash_insert (parse_hash, the_funs[AND].fn_str, &the_funs[AND]);
  hash_insert (parse_hash, the_funs[OR].fn_str, &the_funs[OR]);
  for (n = F_PI; n < USR1; n++)
    hash_insert (parse_hash, the_funs[n].fn_str, &the_funs[n]);
#ifndef USE_DLD
  for (n = 0; n < n_usr_funs; n++)
    {
      int nn;

      for (nn = 0; usr_funs[n][nn].fn_str; nn++)
	hash_insert (parse_hash, usr_funs[n][nn].fn_str, &usr_funs[n][nn]);
#ifdef TEST
      if (usr_n_funs[n] != nn)
	{
	  fprintf (stderr, "Usr_n_funs[%d]%d!=%d", n, usr_n_funs[n], nn);
	  usr_n_funs[n] = nn;

	}
#endif
    }
#endif
  fn_stack = init_stack ();
  str_stack = init_stack ();
  obstack_begin (&tmp_mem, 400);
  tmp_mem_start = obstack_alloc (&tmp_mem, 0);
}

#ifdef USE_DLD
void
add_usr_funs (new_funs)
     struct function *new_funs;
{
  int n;

  n_usr_funs++;
  usr_funs = usr_funs ? ck_realloc (usr_funs, n_usr_funs * sizeof (struct function *)) : ck_malloc (sizeof (struct function *));
  usr_n_funs = usr_n_funs ? ck_realloc (usr_n_funs, n_usr_funs * sizeof (int)) : ck_malloc (sizeof (int));

  usr_funs[n_usr_funs - 1] = new_funs;
  for (n = 0; new_funs[n].fn_str; n++)
    hash_insert (parse_hash, new_funs[n].fn_str, &new_funs[n]);
  usr_n_funs[n_usr_funs - 1] = n;
}

#endif

/* Stash away a backpatch for future editing. */
static void
add_backpatch (from, to)
     unsigned from;
     unsigned to;
{
  if (!patches)
    {
      patches_allocated = 5;
      patches = (struct backpatch *) ck_malloc (patches_allocated * sizeof (struct backpatch));
      patches_used = 0;
    }
  if (patches_allocated == patches_used)
    {
      patches_allocated *= 2;
      patches = (struct backpatch *) ck_realloc (patches, patches_allocated * sizeof (struct backpatch));
    }
  patches[patches_used].from = from;
  patches[patches_used].to = to;
  patches_used++;
}

static int
cmp_patch (n1, n2)
     int n1;
     int n2;
{
  int ret;

  ret = (patches[n1].from == patches[n2].from) ? patches[n1].to - patches[n2].to : patches[n1].from - patches[n2].from;
  return ret;
}

static void
swp_patch (n1, n2)
     int n1;
     int n2;
{
  struct backpatch tmp;

  tmp = patches[n1];
  patches[n1] = patches[n2];
  patches[n2] = tmp;
}

static void
rot_patch (n1, n2)
     int n1;
     int n2;
{
  struct backpatch tmp;
  tmp = patches[n2];
  while (n2 > n1)
    {
      patches[n2] = patches[n2 - 1];
      --n2;
    }
  patches[n2] = tmp;
}


/* This takes an ascii string and returns a pointer to the byte-compiled
   result.  It calls yyparse() to do the actual parsing.  This is complicated
   only because yyparse returns a parse tree which needs to be turned into
   postfix compiled bytes.  This is further complicated by the presence of
   forward branches in the byte-compiled code.  That's what the backpatch
   stuff is for.

   It'd be nice if oneof() could compile into
   arg1
   ONEOF n_possibilities
   JUMP poss1
   JUMP poss2
   JUMP poss3
   ...
   JUMP error
   {poss 1}
   JUMP end
   {poss 2}
   JUMP end
   ...
   end: {rest of expression}
   instead of the simplistic (and slow-to-execute) version currently used

   It'd also be nice if byte-compiled expressions could have *BIG*
   subexpressions, instead of silently failing as they do now.  Error checking
   and a way to encode longer branches would be a *good* idea.
 */
unsigned char *
parse_and_compile (string)
     char *string;
{
  struct node *new_node;
  struct node *node;
  const struct function *f;
  unsigned char *ret;
  int n;
  unsigned buf_siz;
  int need_relax;
  int byte;

  instr = string;
  parse_error = 0;
  patches_used = 0;
  if (yyparse () || parse_error)
    {
      ret = ck_malloc (strlen (string) + 5);
      ret[0] = CONST_ERR;
      ret[1] = 2;
      ret[2] = parse_error;
      ret[3] = ENDCOMP;
      strcpy ((char *) &ret[4], string);
      (void) obstack_free (&tmp_mem, tmp_mem_start);
      return ret;
    }

  node = parse_return;
  if (!node)
    return 0;

loop:
  if (node->comp_value < USR1)
    {
      f = &the_funs[node->comp_value];
    }
  else if (node->comp_value < SKIP)
    {
      n = node->sub_value;
      f = &usr_funs[node->comp_value - USR1][n];
    }
  else
    {
      f = &skip_funs[node->comp_value - SKIP];
    }
  byte = node->comp_value;

#ifdef TEST
  if (!f)
    panic ("f is zero in byte_compile!");
#endif
  switch (GET_COMP (f->fn_comptype))
    {
    case C_IF:
      /* if compiles to
		   test-code IF amt-to-skip-on-false true-code SKIP
 		      amt-to-skip-on-true false-code */
      if (node->n_x.v_subs[0])
	{
	  if (node->n_x.v_subs[0]->n_x.v_subs[0])
	    {
	      /* Put out the test-code */
	      push_stack (fn_stack, node);
	      new_node = node->n_x.v_subs[0]->n_x.v_subs[0];
	      node->n_x.v_subs[0]->n_x.v_subs[0] = 0;
	      node = new_node;
	      goto loop;
	    }
	  /* Put out IF, null-byte to backpatch */
	  (void) obstack_1grow (&tmp_mem, byte);
	  node->add_byte = obstack_object_size (&tmp_mem);
	  (void) obstack_1grow (&tmp_mem, 0);

	  /* put out true-code */
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[0]->n_x.v_subs[1];
	  node->n_x.v_subs[0] = 0;
	  node = new_node;
	  goto loop;
	}
      if (node->n_x.v_subs[1])
	{

	  (void) obstack_1grow (&tmp_mem, SKIP);
	  (void) obstack_1grow (&tmp_mem, 0);
	  add_backpatch (node->add_byte, obstack_object_size (&tmp_mem));
	  node->add_byte = obstack_object_size (&tmp_mem) - 1;

	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[1];
	  node->n_x.v_subs[1] = 0;
	  node = new_node;
	  goto loop;
	}
      add_backpatch (node->add_byte, obstack_object_size (&tmp_mem));
      break;

    case C_ANDOR:
      if (node->n_x.v_subs[0])
	{
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[0];
	  node->n_x.v_subs[0] = 0;
	  node = new_node;
	  goto loop;
	}
      if (node->n_x.v_subs[1])
	{
	  (void) obstack_1grow (&tmp_mem, byte);
	  node->add_byte = obstack_object_size (&tmp_mem);
	  (void) obstack_1grow (&tmp_mem, 0);	/* for backpatching */
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[1];
	  node->n_x.v_subs[1] = 0;
	  node = new_node;
	  goto loop;
	}
      add_backpatch (node->add_byte, obstack_object_size (&tmp_mem));
      break;

    case C_ERR:
      (void) obstack_1grow (&tmp_mem, byte);
      node->add_byte = obstack_object_size (&tmp_mem);
      (void) obstack_1grow (&tmp_mem, 0);
      (void) obstack_1grow (&tmp_mem, node->n_x.v_int);
      node->n_x.v_string = ename[node->n_x.v_int];
      push_stack (str_stack, node);
      break;

    case C_FLT:
      (void) obstack_1grow (&tmp_mem, byte);
      (void) obstack_grow (&tmp_mem, &(node->n_x.v_float), sizeof (double));
      break;

    case C_INT:
      (void) obstack_1grow (&tmp_mem, byte);
      (void) obstack_grow (&tmp_mem, &(node->n_x.v_int), sizeof (long));
      break;

    case C_STR:
      (void) obstack_1grow (&tmp_mem, byte);
      node->add_byte = obstack_object_size (&tmp_mem);
      (void) obstack_1grow (&tmp_mem, 0);
      push_stack (str_stack, node);
      break;

    case C_VAR:
      add_ref_to (obstack_object_size (&tmp_mem));
      add_var_ref (node->n_x.v_var);
      (void) obstack_1grow (&tmp_mem, byte);
      (void) obstack_grow (&tmp_mem, &(node->n_x.v_var), sizeof (struct var *));
      break;

    case C_CELL:
      add_ref_to (obstack_object_size (&tmp_mem));
      add_ref (node->n_x.v_rng.lr, node->n_x.v_rng.lc);
      (void) obstack_1grow (&tmp_mem, byte);
#if BITS_PER_CELLREF==16
      (void) obstack_1grow (&tmp_mem, node->n_x.v_rng.lr >> 8);
      (void) obstack_1grow (&tmp_mem, node->n_x.v_rng.lr);
      (void) obstack_1grow (&tmp_mem, node->n_x.v_rng.lc >> 8);
      (void) obstack_1grow (&tmp_mem, node->n_x.v_rng.lc);
#else
#if BITS_PER_CELLREF==8
      (void) obstack_1grow (&tmp_mem, node->n_x.v_rng.lr);
      (void) obstack_1grow (&tmp_mem, node->n_x.v_rng.lc);
#else
      Insert appropriate code here
#endif
#endif
        break;

    case C_RANGE:
      add_ref_to (obstack_object_size (&tmp_mem));
      add_range_ref (&(node->n_x.v_rng));
      (void) obstack_1grow (&tmp_mem, byte);
      (void) obstack_grow (&tmp_mem, &(node->n_x.v_rng), sizeof (struct rng));
      break;

    case C_FN0X:
      add_ref_to (obstack_object_size (&tmp_mem));
      /* FALLTHROUGH */
    case C_FN0:
    case C_CONST:
    add_byte:
      if (f->fn_comptype & C_T)
	add_timer_ref (obstack_object_size (&tmp_mem));
      (void) obstack_1grow (&tmp_mem, byte);
      if (byte >= USR1 && byte < SKIP)
	(void) obstack_1grow (&tmp_mem, (int) node->sub_value);
      break;

    case C_FN1:
    case C_UNA:
      if (node->n_x.v_subs[0])
	{
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[0];
	  node->n_x.v_subs[0] = 0;
	  node = new_node;
	  goto loop;
	}
      goto add_byte;

    case C_FN2:
    case C_INF:
      if (node->n_x.v_subs[0])
	{
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[0];
	  node->n_x.v_subs[0] = 0;
	  node = new_node;
	  goto loop;
	}
      if (node->n_x.v_subs[1])
	{
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[1];
	  node->n_x.v_subs[1] = 0;
	  node = new_node;
	  goto loop;
	}
      goto add_byte;

    case C_FN3:
      if (node->n_x.v_subs[0])
	{
	  if (node->n_x.v_subs[0]->n_x.v_subs[0])
	    {
	      push_stack (fn_stack, node);
	      new_node = node->n_x.v_subs[0]->n_x.v_subs[0];
	      node->n_x.v_subs[0]->n_x.v_subs[0] = 0;
	      node = new_node;
	      goto loop;
	    }
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[0]->n_x.v_subs[1];
	  node->n_x.v_subs[0] = 0;
	  node = new_node;
	  goto loop;
	}
      if (node->n_x.v_subs[1])
	{
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[1];
	  node->n_x.v_subs[1] = 0;
	  node = new_node;
	  goto loop;
	}
      goto add_byte;

    case C_FN4:
      if (node->n_x.v_subs[0])
	{
	  if (node->n_x.v_subs[0]->n_x.v_subs[0])
	    {
	      push_stack (fn_stack, node);
	      new_node = node->n_x.v_subs[0]->n_x.v_subs[0];
	      node->n_x.v_subs[0]->n_x.v_subs[0] = 0;
	      node = new_node;
	      goto loop;
	    }
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[0]->n_x.v_subs[1];
	  node->n_x.v_subs[0] = 0;
	  node = new_node;
	  goto loop;
	}
      if (node->n_x.v_subs[1])
	{
	  if (node->n_x.v_subs[1]->n_x.v_subs[0])
	    {
	      push_stack (fn_stack, node);
	      new_node = node->n_x.v_subs[1]->n_x.v_subs[0];
	      node->n_x.v_subs[1]->n_x.v_subs[0] = 0;
	      node = new_node;
	      goto loop;
	    }
	  push_stack (fn_stack, node);
	  new_node = node->n_x.v_subs[1]->n_x.v_subs[1];
	  node->n_x.v_subs[1] = 0;
	  node = new_node;
	  goto loop;
	}
      goto add_byte;

    case C_FNN:
      if (node->n_x.v_subs[1])
	{
	  if (node->add_byte == 0)
	    for (new_node = node; new_node->n_x.v_subs[1]; new_node = new_node->n_x.v_subs[1])
	      node->add_byte++;
	  for (new_node = node; new_node->n_x.v_subs[1]->n_x.v_subs[1]; new_node = new_node->n_x.v_subs[1])
	    ;
	  push_stack (fn_stack, node);
	  node = new_node->n_x.v_subs[1]->n_x.v_subs[0];
	  new_node->n_x.v_subs[1] = 0;
	  goto loop;
	}
      (void) obstack_1grow (&tmp_mem, byte);
      if (byte >= USR1 && byte < SKIP)
	(void) obstack_1grow (&tmp_mem, (int) node->sub_value);
      (void) obstack_1grow (&tmp_mem, node->add_byte);
      break;

    default:
      panic ("Bad comptype %d", f->fn_comptype);
    }
  node = (struct node *) pop_stack (fn_stack);
  if (node)
    goto loop;

  (void) obstack_1grow (&tmp_mem, 0);

  while (node = pop_stack (str_stack))
    {
      add_backpatch (node->add_byte, obstack_object_size (&tmp_mem));
      (void) obstack_grow (&tmp_mem, node->n_x.v_string, strlen (node->n_x.v_string) + 1);
    }

  buf_siz = obstack_object_size (&tmp_mem);
  ret = (unsigned char *) ck_malloc (buf_siz);
  bcopy (obstack_finish (&tmp_mem), ret, buf_siz);

  need_relax = 0;
  for (n = 0; n < patches_used; n++)
    {
      long offset;

      offset = (patches[n].to - patches[n].from) - 1;
      if (offset < 0 || offset > 255)
	need_relax++;
      else
	ret[patches[n].from] = offset;
    }
  if (need_relax)
    {
      int n_lo;
      long offset;
      int start;

      /* ... Sort the patches list ... */
      sort (patches_used, cmp_patch, swp_patch, rot_patch);

      while (need_relax)
	{
	  ret = ck_realloc (ret, buf_siz + need_relax);
	  for (n_lo = 0; n_lo < patches_used; n_lo++)
	    {
	      offset = (patches[n_lo].to - patches[n_lo].from) - 1;
	      if (offset < 0 || offset > 255 - need_relax)
		break;
	    }

	  /* n_lo points to the first jump that may need to be relaxed */
	  for (n = n_lo; n < patches_used; n++)
	    {
	      offset = (patches[n].to - patches[n].from) - 1;
	      if (offset < 0 || offset > 255)
		{
		  int nn;

		  start = patches[n].from;

		  ret[start - 1]++;	/* Translate insn to LONG */
		  ret[start] = offset;
		  bcopy (&ret[start + 1], &ret[start + 2], buf_siz - start);
		  ret[start + 1] = offset >> 8;
		  need_relax--;
		  buf_siz++;
		  for (nn = 0; nn < patches_used; nn++)
		    {
		      if (patches[nn].from > start)
			patches[nn].from++;
		      if (patches[nn].to > start)
			patches[nn].to++;
		      if (patches[nn].from < start && patches[nn].to > start && ret[patches[nn].from]++ == 255)
			{
			  if (ret[patches[nn].from - 1] & 01)
			    ret[patches[nn].from + 1]++;
			  else
			    need_relax++;
			}
		    }
		}
	    }
	}
    }

  (void) obstack_free (&tmp_mem, tmp_mem_start);

  patches_used = 0;

  return ret;
}

/* Back when strings stored a char*, they needed to be freed when a
   byte-compiled expression was freed.  Now that they're appended to the end,
   they don't need to be specially freed anymore.
 */
void
byte_free (form)
     unsigned char *form;
{
  /* no longer needed
	unsigned char *f;

	for(f=form;*f;f++) {
		switch(*f) {
		case IF:
		case F_IF:
		case SKIP:
		case AND:
		case OR:
		case CONST_STR:
			f++;
			break;
		case CONST_INT:
			f+=sizeof(long);
			break;
		case CONST_FLT:
			f+=sizeof(double);
			break;
		case VAR:
			f+=sizeof(struct var *);
			break;
		case R_CELL:
		case R_CELL|ROWREL:
		case R_CELL|COLREL:
		case R_CELL|ROWREL|COLREL:
			f+=EXP_ADD;
			break;
		case RANGE:
		case RANGE|LRREL:
		case RANGE|LRREL|LCREL:
		case RANGE|LRREL|LCREL|HCREL:
		case RANGE|LRREL|HCREL:
		case RANGE|LRREL|HRREL:
		case RANGE|LRREL|HRREL|LCREL:
		case RANGE|LRREL|HRREL|LCREL|HCREL:
		case RANGE|LRREL|HRREL|HCREL:
		case RANGE|HRREL:
		case RANGE|HRREL|LCREL:
		case RANGE|HRREL|LCREL|HCREL:
		case RANGE|HRREL|HCREL:
		case RANGE|LCREL:
		case RANGE|LCREL|HCREL:
		case RANGE|HCREL:
			f+=EXP_ADD_RNG;
			break;
		case F_PRINTF:
		case F_CONCAT:
		case F_ONEOF:
		case F_STRSTR:
		case F_EDIT:
		case AREA_SUM:
		case AREA_PROD:
		case AREA_AVG:
		case AREA_STD:
		case AREA_MAX:
		case AREA_MIN:
		case AREA_CNT:
		case AREA_VAR:
			f++;
			break;
		default:
			break;
		}
	} */
  free (form);
}

/* This tries to tell if a byte-compiled expression is a constant.  If it
   is a constant, we can free it, and never try to recompute its value.
   This returns non-zero if the expression is constant.*/
int
is_constant (bytes)
     unsigned char *bytes;
{
  /* It's constant, but it's already been dealt with.
	   Pretend it isn't. */
  if (!bytes)
    return 0;

  switch (bytes[0])
    {
    case CONST_ERR:
      return (bytes[3] == 0 && !strcmp ((char *) bytes + 4, ename[bytes[2]]));
    case CONST_INT:
      return bytes[sizeof (long) + 1] == ENDCOMP;
    case CONST_FLT:
      return bytes[sizeof (double) + 1] == ENDCOMP;
    case CONST_STR:
      return bytes[2] == ENDCOMP;
    case F_TRUE:
    case F_FALSE:
    case CONST_INF:
    case CONST_NINF:
    case CONST_NAN:
      return bytes[1] == ENDCOMP;
    default:
      return 0;
    }
}
