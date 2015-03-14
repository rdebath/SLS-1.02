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

#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "ref.h"
#include "window.h"

#define ROW_BUF 3
#define COL_BUF 2
#define MAX MAX_ROW
#define MIN MIN_ROW
static struct obstack find_stack;

#if 1
#define malloc_chain_check(x)
#endif

#ifdef __GNUC__
#define inline __inline__
#else
#define inline
#endif

#ifdef TEST_ME
typedef unsigned char CELLREF;
typedef unsigned int size_t;
#define ck_malloc malloc
extern void *malloc ();
#define MIN 1
#define MAX 65535
#endif

struct list
  {
    CELLREF lo, hi;
    struct list *next;
    char mem[1];
  };

struct find
  {
    struct find *next;
    CELLREF lo, hi, cur;
    struct list **start;
    struct list *curptr;
    CELLREF left;
    void *ret;
    char fini;
    int ele;
  };

struct find *finds;

static inline void
flush (ptr)
     struct list *ptr;
{
  struct list *nxt;

  while (ptr)
    {
      nxt = ptr->next;
      free (ptr);
      ptr = nxt;
    }
}

static inline void
resync (tofree, new, ele)
     struct list *tofree;
     struct list *new;
     int ele;
{
  extern struct cell *my_cell;
  struct find *findp;

  if (ele == sizeof (struct cell) && my_cell && (char *) my_cell >= tofree->mem && (char *) my_cell <= tofree->mem + ele * (1 + tofree->hi - tofree->lo))
      my_cell = (struct cell *) (new->mem + ele * (cur_row - new->lo));
  for (findp = finds; findp; findp = findp->next)
    {
      if (tofree == findp->curptr)
	{
	  CELLREF hi;
	  findp->curptr = new;
	  findp->ret = new->mem + (findp->cur - new->lo) * ele;
	  hi = (findp->hi < new->hi ? findp->hi : new->hi);
	  if (findp->cur < hi)
	    findp->left = hi - findp->cur;
	}
    }
  free (tofree);
}

static inline void *
find (pos, ptr, ele)
     CELLREF pos;
     struct list *ptr;
     int ele;
{
  for (; ptr; ptr = ptr->next)
    {
      if (ptr->lo > pos)
	break;
      if (ptr->hi >= pos)
	return ptr->mem + (pos - ptr->lo) * ele;
    }
  return 0;
}

static inline void *
make (pos, prevp, ele, buf)
     CELLREF pos;
     struct list **prevp;
     int ele;
     int buf;
{
  CELLREF lo, hi;
  size_t size;
  struct list *ptr;

  while (*prevp && (*prevp)->next && (*prevp)->next->lo < pos)
    prevp = &((*prevp)->next);

  /* Was it easy? */
  if (*prevp && (*prevp)->lo <= pos && (*prevp)->hi >= pos)
    return (*prevp)->mem + (pos - (*prevp)->lo) * ele;

  lo = (pos < MIN + buf) ? MIN : pos - buf;
  hi = (pos > MAX - buf) ? MAX : pos + buf;

  if (!*prevp
      || ((*prevp)->hi < lo - 1
	  && (!(*prevp)->next
	      || (*prevp)->next->lo - 1 > hi)))
    {
      /* Allocate a whole new structure */
      size = (1 + hi - lo) * ele;
      ptr = ck_malloc (sizeof (struct list) + size);
      ptr->lo = lo;
      ptr->hi = hi;
      if (*prevp && (*prevp)->hi < lo)
	{
	  ptr->next = (*prevp)->next;
	  (*prevp)->next = ptr;
	}
      else
	{
	  ptr->next = *prevp;
	  *prevp = ptr;
	}
      bzero (ptr->mem, size);
      malloc_chain_check (1);
    }
  else if ((*prevp)->lo > lo)
    {
      /* Stretch one down a bit to fit */
      hi = (*prevp)->hi;
      size = (1 + hi - lo) * ele;
      ptr = ck_malloc (sizeof (struct list) + size);
      ptr->lo = lo;
      ptr->hi = hi;
      ptr->next = (*prevp)->next;
      bcopy ((*prevp)->mem, ptr->mem + ((*prevp)->lo - ptr->lo) * ele, (1 + (*prevp)->hi - (*prevp)->lo) * ele);
      bzero (ptr->mem, ((*prevp)->lo - ptr->lo) * ele);
      resync (*prevp, ptr, ele);
      *prevp = ptr;
      malloc_chain_check (1);
    }
  else if ((*prevp)->hi < hi && (*prevp)->next && (*prevp)->next->lo <= hi)
    {
      /* Merge this one and the one after it */
      size = (1 + (*prevp)->next->hi - (*prevp)->lo) * ele;
      ptr = ck_malloc (sizeof (struct list) + size);
      ptr->lo = (*prevp)->lo;
      ptr->hi = (*prevp)->next->hi;
      ptr->next = (*prevp)->next->next;
      bcopy ((*prevp)->mem, ptr->mem, (1 + (*prevp)->hi - (*prevp)->lo) * ele);
      bzero (ptr->mem + (1 + (*prevp)->hi - ptr->lo) * ele, ((*prevp)->next->lo - (*prevp)->hi) * ele);
      bcopy ((*prevp)->next->mem,
	     ptr->mem + ((*prevp)->next->lo - ptr->lo) * ele,
	     (1 + (*prevp)->next->hi - (*prevp)->next->lo) * ele);
      resync ((*prevp)->next, ptr, ele);
      resync (*prevp, ptr, ele);
      *prevp = ptr;
      malloc_chain_check (1);
    }
  else if ((*prevp)->hi < hi)
    {
      /* stretch this one up a bit */
      size = (1 + hi - (*prevp)->lo) * ele;
      ptr = ck_malloc (sizeof (struct list) + size);
      ptr->lo = (*prevp)->lo;
      ptr->hi = hi;
      ptr->next = (*prevp)->next;
      bcopy ((*prevp)->mem, ptr->mem, (1 + (*prevp)->hi - (*prevp)->lo) * ele);
      bzero (ptr->mem + (1 + (*prevp)->hi - ptr->lo) * ele, (hi - (*prevp)->hi) * ele);
      resync (*prevp, ptr, ele);
      *prevp = ptr;
      malloc_chain_check (1);
    }
  else
    ptr = *prevp;
#ifdef TEST
  if (ptr->lo > pos || ptr->hi < pos)
    panic ("Make at %u not in %u %u", pos, ptr->lo, ptr->hi);
#endif

  return ptr->mem + (pos - ptr->lo) * ele;
}

static inline void *
find_rng (start, lo, hi, ele)
     struct list **start;
     CELLREF lo;
     CELLREF hi;
     int ele;
{
  struct list *ptr;
  struct find *f;

  f = (struct find *)obstack_alloc (&find_stack, sizeof (struct find));
  f->lo = lo;
  f->hi = hi;
  f->ele = ele;
  f->start = start;
  for (ptr = *start; ptr; ptr = ptr->next)
    if (ptr->hi >= lo)
      break;
  if (ptr && ptr->lo <= hi)
    {
      f->cur = (ptr->lo > lo ? ptr->lo : lo);
      f->curptr = ptr;
      f->ret = ptr->mem + (f->cur - ptr->lo) * ele;
      f->left = 1 + (f->hi < ptr->hi ? f->hi : ptr->hi) - f->cur;
      f->fini = 0;
    }
  else
    f->fini = 1;
  f->next = finds;
  finds = f;
  return f;
}

static inline void *
make_rng (start, lo, hi, ele, buf)
     struct list **start;
     CELLREF lo;
     CELLREF hi;
     int ele;
     int buf;
{
  struct list **prevp;
  struct list *ptr;
  size_t size;
  struct find *f;

  f = (struct find *)obstack_alloc (&find_stack, sizeof (struct find));
  f->lo = f->cur = lo;
  f->hi = hi;
  f->left = 1 + hi - lo;
  f->fini = 0;
  f->ele = ele;
  f->start = start;

  lo = lo <= MIN + buf ? MIN : lo - buf;
  hi = hi >= MAX - buf ? MAX : hi + buf;

  for (prevp = start; *prevp && (*prevp)->hi < lo - 1; prevp = &((*prevp)->next))
    ;
  ptr = *prevp;
  if (!*prevp || (*prevp)->lo - 1 > hi)
    {
      /* Allocate the whole thing */
      size = (1 + hi - lo) * ele;
      ptr = ck_malloc (sizeof (struct list) + size);
      ptr->lo = lo;
      ptr->hi = hi;
      ptr->next = *prevp;
      bzero (ptr->mem, size);
      if (*prevp && (*prevp)->hi < lo)
	{
	  ptr->next = (*prevp)->next;
	  (*prevp)->next = ptr;
	}
      else
	{
	  ptr->next = *prevp;
	  *prevp = ptr;
	}
      *prevp = ptr;
      malloc_chain_check (1);
    }
  else
    {
      if ((*prevp)->lo > lo)
	{
	  /* Stretch this one down a bit */
	  size = (1 + (*prevp)->hi - lo) * ele;
	  ptr = ck_malloc (sizeof (struct list) + size);
	  ptr->lo = lo;
	  ptr->hi = (*prevp)->hi;
	  ptr->next = (*prevp)->next;
	  bcopy ((*prevp)->mem,
		 ptr->mem + ((*prevp)->lo - ptr->lo) * ele,
		 (1 + (*prevp)->hi - (*prevp)->lo) * ele);
	  bzero (ptr->mem, ((*prevp)->lo - lo) * ele);
	  resync (*prevp, ptr, ele);
	  *prevp = ptr;
	  malloc_chain_check (1);
	}
      while ((*prevp)->hi < hi && (*prevp)->next && (*prevp)->next->lo <= hi)
	{
	  /* Merge this one and the one after it */
	  /* Repeat as needed */
	  size = (1 + (*prevp)->next->hi - (*prevp)->lo) * ele;
	  ptr = ck_malloc (sizeof (struct list) + size);
	  ptr->lo = (*prevp)->lo;
	  ptr->hi = (*prevp)->next->hi;
	  ptr->next = (*prevp)->next->next;
	  bcopy ((*prevp)->mem, ptr->mem, (1 + (*prevp)->hi - (*prevp)->lo) * ele);
	  bzero (ptr->mem + (1 + (*prevp)->hi - ptr->lo) * ele, ((*prevp)->next->lo - (*prevp)->hi) * ele);
	  bcopy ((*prevp)->next->mem,
		 ptr->mem + ((*prevp)->next->lo - ptr->lo) * ele,
		 (1 + (*prevp)->next->hi - (*prevp)->next->lo) * ele);
	  resync ((*prevp)->next, ptr, ele);
	  resync (*prevp, ptr, ele);
	  *prevp = ptr;
	  malloc_chain_check (1);
	}
      if ((*prevp)->hi < hi)
	{
	  /* stretch this one up a bit */
	  size = (1 + hi - (*prevp)->lo) * ele;
	  ptr = ck_malloc (sizeof (struct list) + size);
	  ptr->lo = (*prevp)->lo;
	  ptr->hi = hi;
	  ptr->next = (*prevp)->next;
	  bcopy ((*prevp)->mem, ptr->mem, (1 + (*prevp)->hi - (*prevp)->lo) * ele);
	  bzero (ptr->mem + (1 + (*prevp)->hi - ptr->lo) * ele, (hi - (*prevp)->hi) * ele);
	  resync (*prevp, ptr, ele);
	  *prevp = ptr;
	  malloc_chain_check (1);
	}
    }
#ifdef TEST
  if (ptr->lo > f->lo || ptr->hi < f->hi)
    panic ("Vector of %u-%u not big enough for %u-%u", (*prevp)->lo, (*prevp)->hi, f->lo, f->hi);
#endif
  f->curptr = ptr;
  f->ret = ptr->mem + (f->cur - ptr->lo) * ele;
  f->next = finds;
  finds = f;
  return f;
}

static inline void *
next_rng (f, posp)
     struct find *f;
     CELLREF *posp;
{
  void *ret;
  struct find *next;

  if (!f)
    return 0;
  if (!f->fini)
    {
      if (f->left)
	{
	  --(f->left);
	fini:
	  if (posp)
	    *posp = f->cur;
	  f->cur++;
	  ret = f->ret;
	  f->ret = (char *) (f->ret) + f->ele;
	  return ret;
	}
      if (f->curptr->hi < f->hi)
	{
	  f->curptr = f->curptr->next;
	  if (f->curptr && f->curptr->lo <= f->hi)
	    {
	      f->ret = f->curptr->mem;
	      f->left = (f->hi < f->curptr->hi ? f->hi : f->curptr->hi) - f->curptr->lo;
	      f->cur = f->curptr->lo;
	      goto fini;
	    }
	}
    }
  next = f->next;
  obstack_free (&find_stack, f);
  finds = next;
  return 0;
}


struct cf
  {
    struct cf *next;
    struct find *rows, *cols;
    int make;
  };

static struct cf *fp;
static struct list *the_cols;

extern unsigned short default_width;
static struct find *w_find;

extern unsigned short default_height;
static struct find *h_find;

static struct list *wids, *hgts;

void 
init_cells ()
{
  obstack_begin (&find_stack, sizeof (struct find) * 15);
  the_cols = 0;
  wids = 0;
  hgts = 0;
}

void 
flush_everything ()
{
  struct list *ptr, *nxt;
  int n;

  flush_variables ();
  for (ptr = the_cols; ptr; ptr = nxt)
    {
      nxt = ptr->next;
      for (n = 0; n <= ptr->hi - ptr->lo; n++)
	flush (*(struct list **) (ptr->mem + (n * sizeof (struct list *))));
      free (ptr);
    }
  the_cols = 0;
  flush (wids);
  wids = 0;
  flush (hgts);
  hgts = 0;
  flush_fonts ();
}

#if __STDC__
struct cell *
find_cell (CELLREF row, CELLREF col)
#else
struct cell *
find_cell (row, col)
     CELLREF row;
     CELLREF col;
#endif
{
  void **v;

  v = find (col, the_cols, sizeof (void *));
  return v ? find (row, *v, sizeof (struct cell)) : 0;
}

#if __STDC__
struct cell *
find_or_make_cell (CELLREF row, CELLREF col)
#else
struct cell *
find_or_make_cell (row, col)
     CELLREF row;
     CELLREF col;
#endif
{
  struct list **v;

  v = make (col, &the_cols, sizeof (struct list *), COL_BUF);
  return make (row, v, sizeof (struct cell), ROW_BUF);
}

void
find_cells_in_range (r)
     struct rng *r;
{
  struct cf *new;
  struct list **firstcol;

  new = (struct cf *)obstack_alloc (&find_stack, sizeof (struct cf));
  new->make = 0;
  new->next = fp;
  fp = new;
  new->rows = find_rng (&the_cols, r->lc, r->hc, sizeof (void *));
  firstcol = next_rng (new->rows, 0);
  if (firstcol)
    new->cols = find_rng (firstcol, r->lr, r->hr, sizeof (struct cell));
  else
    new->cols = 0;
}

void
make_cells_in_range (r)
     struct rng *r;
{
  struct cf *new;
  struct list **firstcol;

  new = (struct cf *)obstack_alloc (&find_stack, sizeof (struct cf));
  new->make = 1;
  new->next = fp;
  fp = new;
  new->rows = make_rng (&the_cols, r->lc, r->hc, sizeof (void *), ROW_BUF);
  firstcol = next_rng (new->rows, 0);
  new->cols = make_rng (firstcol, r->lr, r->hr, sizeof (struct cell), COL_BUF);
}

struct cell *
next_cell_in_range ()
{
  struct cell *ret;
  void *new_row;

  for (;;)
    {
      if (ret = next_rng (fp->cols, 0))
	return ret;
      new_row = next_rng (fp->rows, 0);
      if (!new_row)
	{
	  struct cf *old;

	  old = fp->next;
	  obstack_free (&find_stack, fp);
	  fp = old;
	  return 0;
	}
      fp->cols = fp->make ? make_rng (new_row, fp->cols->lo, fp->cols->hi, sizeof (struct cell), ROW_BUF)
      : find_rng (new_row, fp->cols->lo, fp->cols->hi, sizeof (struct cell));
    }
}

struct cell *
next_row_col_in_range (rowp, colp)
     CELLREF *rowp;
     CELLREF *colp;
{
  struct cell *ret;
  struct list **new_row;

  for (;;)
    {
      if (ret = next_rng (fp->cols, rowp))
	{
	  *colp = fp->rows->cur - 1;
	  return ret;
	}
      new_row = next_rng (fp->rows, colp);
      if (!new_row)
	{
	  struct cf *old;

	  old = fp->next;
	  obstack_free (&find_stack, fp);
	  fp = old;
	  return 0;
	}
      fp->cols = fp->make ? make_rng (new_row, fp->cols->lo, fp->cols->hi, sizeof (struct cell), ROW_BUF)
      : find_rng (new_row, fp->cols->lo, fp->cols->hi, sizeof (struct cell));
    }
}

void
no_more_cells ()
{
  struct cf *old;

  old = fp->next;
  obstack_free (&find_stack, fp);
  fp = old;
}

CELLREF
max_row (col)
     CELLREF col;
{
  struct list **ptr;

  ptr = find (col, the_cols, sizeof (void *));
  if (!ptr || !*ptr)
    return MIN;
  while ((*ptr)->next)
    ptr = &((*ptr)->next);
  return (*ptr)->hi;
}

CELLREF
max_col (row)
     CELLREF row;
{
  struct list *ptr;

  if (!the_cols)
    return MIN;
  for (ptr = the_cols; ptr->next; ptr = ptr->next)
    ;
  return ptr->hi;
}

CELLREF 
highest_row ()
{
  void *f;
  struct list **ptr;
  CELLREF hi = MIN;

  f = find_rng (&the_cols, MIN, MAX, sizeof (void *));
  while (ptr = next_rng (f, 0))
    {
      if (*ptr)
	{
	  while ((*ptr)->next)
	    ptr = &((*ptr)->next);
	  if ((*ptr)->hi > hi)
	    hi = (*ptr)->hi;
	}
    }
  return hi;
}


CELLREF 
highest_col ()
{
  struct list *ptr;

  if (!the_cols)
    return MIN;
  for (ptr = the_cols; ptr->next; ptr = ptr->next)
    ;
  return ptr->hi;
}


/* Routines for dealing with the widths of columns. . . */

#ifdef __STDC__
unsigned short 
get_width (CELLREF col)
#else
unsigned short 
get_width (col)
     CELLREF col;
#endif
{
  unsigned short *ptr;

  ptr = find (col, wids, sizeof (unsigned short));
  if (!ptr || !*ptr)
    return default_width;
  return (*ptr) - 1;
}


#ifdef __STDC__
unsigned short 
get_nodef_width (CELLREF col)
#else
unsigned short 
get_nodef_width (col)
     CELLREF col;
#endif
{
  unsigned short *ptr;

  ptr = find (col, wids, sizeof (unsigned short));
  return ptr ? *ptr : 0;
}

void 
set_width (col, wid)
     CELLREF col;
     unsigned short wid;
{
  unsigned short *ptr;

  ptr = make (col, &wids, sizeof (unsigned short), COL_BUF);
  *ptr = wid;
}

void 
find_widths (lo, hi)
     CELLREF lo;
     CELLREF hi;
{
  w_find = find_rng (&wids, lo, hi, sizeof (unsigned short));
}

unsigned short 
next_width (posp)
     CELLREF *posp;
{
  unsigned short *ptr;

  do
    ptr = next_rng (w_find, posp);
  while (ptr && !*ptr);
  return ptr ? *ptr : 0;
}

static void
do_shift (over, lo, hi, start, buf)
     int over;
     CELLREF lo;
     CELLREF hi;
     struct list **start;
     int buf;
{
  CELLREF pos;
  unsigned short w;
  unsigned short *ptr;
  int inc;
  struct list *p;

  if (!*start)
    return;
  for (p = *start; p->next; p = p->next)
    ;
  if (hi > p->hi)
    hi = p->hi;

  if (over > 0)
    {
      pos = hi;
      hi = lo;
      lo = pos;
      inc = -1;
    }
  else
    inc = 1;

  for (pos = lo;; pos += inc)
    {
      ptr = find (pos, *start, sizeof (unsigned short));
      w = ptr ? *ptr : 0;
      ptr = w ? make (pos + over, start, sizeof (unsigned short), buf) :
        find (pos + over, *start, sizeof (unsigned short));
      if (w || (ptr && *ptr))
	*ptr = w;
      if (pos == hi)
	break;
    }
  for (pos = hi + over;;)
    {
      pos += inc;
      ptr = find (pos, *start, sizeof (unsigned short));
      if (ptr)
	*ptr = 0;
      if (pos == hi)
	break;
    }
}

void 
shift_widths (over, lo, hi)
     int over;
     CELLREF lo;
     CELLREF hi;
{
  do_shift (over, lo, hi, &wids, COL_BUF);
}


/* Routines for dealing with the height of rows */
#ifdef __STDC__
unsigned short 
get_height (CELLREF row)
#else
unsigned short 
get_height (row)
     CELLREF row;
#endif
{
  unsigned short *ptr;

  ptr = find (row, hgts, sizeof (unsigned short));
  if (!ptr || !*ptr)
    return default_height;
  return *ptr - 1;
}

#ifdef __STDC__
unsigned short 
get_nodef_height (CELLREF row)
#else
unsigned short 
get_nodef_height (row)
     CELLREF row;
#endif
{
  unsigned short *ptr;

  ptr = find (row, hgts, sizeof (unsigned short));
  return ptr ? *ptr : 0;
}

#ifdef __STDC__
void 
set_height (CELLREF row, unsigned short hgt)
#else
void 
set_height (row, hgt)
     CELLREF row;
     unsigned short hgt;
#endif
{
  unsigned short *ptr;

  ptr = make (row, &hgts, sizeof (unsigned short), ROW_BUF);
  *ptr = hgt;
}

int height_scale = 1;
int width_scale = 1;

int 
get_scaled_height (r)
     CELLREF r;
{
  return get_height (r) * height_scale;
}

int 
get_scaled_width (c)
     CELLREF c;
{
  return get_width (c) * width_scale;
}


#ifdef __STDC__
void 
find_heights (CELLREF lo, CELLREF hi)
#else
void 
find_heights (lo, hi)
     CELLREF lo;
     CELLREF hi;
#endif
{
  h_find = find_rng (&hgts, lo, hi, sizeof (unsigned short));
}

#ifdef __STDC__
unsigned short 
next_height (CELLREF *posp)
#else
unsigned short 
next_height (posp)
     CELLREF *posp;
#endif
{
  unsigned short *ptr;

  do
    ptr = next_rng (h_find, posp);
  while (ptr && !*ptr);
  return ptr ? *ptr : 0;
}

void 
shift_heights (dn, lo, hi)
     int dn;
     CELLREF lo;
     CELLREF hi;
{
  do_shift (dn, lo, hi, &hgts, ROW_BUF);
}

#ifdef TEST
extern char *bname[];

extern void dbg_print_ref_fm ();
extern void dbg_print_ref_to ();
extern void dbg_print_formula ();

void
dbg_print_cell (cp)
     CELL *cp;
{
  char *ptr1, *ptr2;
  char tmpbuf[30];

  switch (GET_TYP (cp))
    {
    case 0:
      ptr1 = "(null)";
      ptr2 = "";
      break;
    case TYP_FLT:
      sprintf (tmpbuf, "Float: %.16g", cp->cell_flt);
      ptr1 = tmpbuf;
      ptr2 = "";
      break;
    case TYP_INT:
      sprintf (tmpbuf, "Int: %ld", cp->cell_int);
      ptr1 = tmpbuf;
      ptr2 = "";
      break;
    case TYP_ERR:
      sprintf (tmpbuf, "Error: %d: ", cp->cell_err);
      ptr1 = tmpbuf;
      ptr2 = ename[cp->cell_err];
      break;
    case TYP_BOL:
      sprintf (tmpbuf, "Bool: %d: ", cp->cell_bol);
      ptr1 = tmpbuf;
      ptr2 = bname[cp->cell_bol];
      break;
    case TYP_STR:
      sprintf (tmpbuf, "String: %p: ", cp->cell_str);
      ptr1 = tmpbuf;
      ptr2 = cp->cell_str;
      break;
    default:
      sprintf (tmpbuf, "Unknown: %d", GET_TYP (cp));
      ptr1 = tmpbuf;
      ptr2 = "";
      break;
    }
  io_text_line ("    Cell %p:  flg %#lx  fm %p  to %p  fa %p  cy %d  val %s%s",
		cp, cp->cell_flags, cp->cell_refs_from, cp->cell_refs_to,
		cp->cell_formula, cp->cell_cycle, ptr1, ptr2);
  dbg_print_ref_fm (cp->cell_refs_from);
  dbg_print_ref_to (cp->cell_refs_to);
  dbg_print_formula (cp->cell_formula);
}

#ifdef __STDC__
void 
dbg_print_list (struct list *ptr, int ele, char *txt, void (*prsub) (void *))
#else
void 
dbg_print_list (ptr, ele, txt, prsub)
     struct list *ptr;
     int ele;
     char *txt;
     void (*prsub) ();
#endif
{
  CELLREF pos;

  while (ptr)
    {
      io_text_line ("%s %p: lo %u  hi %u  nxt %p  mem %p", txt, ptr, ptr->lo, ptr->hi, ptr->next, &(ptr->mem[0]));
      pos = ptr->lo;
      for (;;)
	{
	  (*prsub) (ptr->mem + ele * (pos - ptr->lo));
	  if (pos == ptr->hi)
	    break;
	  pos++;
	}
      ptr = ptr->next;
    }
}

static void 
dbg_pr_row (p)
     VOIDSTAR p;
{
  io_text_line ("  %p", p);
}

void 
dbg_print_rows (pos)
     CELLREF pos;
{
  dbg_print_list (the_cols, sizeof (struct list *), "row", dbg_pr_row);
}

static void 
dbg_pr_all (p)
     VOIDSTAR p;
{
  struct list **ptr;

  io_text_line ("  %p", p);
  ptr = p;
  dbg_print_list (*ptr, sizeof (struct cell), "  col",
		    (void (*)(void *)) dbg_print_cell);
}

void 
dbg_print_array ()
{
  dbg_print_list (the_cols, sizeof (struct list *), "row", dbg_pr_all);
}

void 
dbg_print_cols (pos)
     CELLREF pos;
{
  /* struct list **ptr;

	ptr=find(...);
	if(...); */
}

#endif

#ifdef TEST_ME
main ()
{
  char buf[100];
  static void *vec[10];
  static siz[10];
  char *ret;
  int n;
  int t;
  int hi, lo;
  CELLREF pos;
  int z;


  while (printf ("-->"), gets (buf))
    {
      n = buf[1] - '0';
      switch (buf[0])
	{
	case 'i':
	  if (sscanf (&buf[2], "%d %d", &siz[n], &t) < 1)
	    {
	      printf ("No size?\n");
	      break;
	    }
	  vec[n] = list_init (siz[n], t);
	  printf ("vec %d init'd to %lx with siz %u and buf %d\n", n, vec[n], siz[n], t);
	  break;

	case 'f':
	  ret = find (vec[n], atoi (&buf[2]));
	  if (ret)
	    {
	      printf ("Found at %lx  ", ret);
	      for (t = 0; t < siz[n]; t++)
		printf ("%x ", ret[t]);
	      printf ("\n");
	    }
	  else
	    printf ("Not found\n");
	  break;

	case 'F':
	  if (sscanf (&buf[2], "%d %d", &lo, &hi) != 2)
	    {
	      printf ("Faild to scan\n");
	      break;
	    }
	  find_rng (vec[n], lo, hi);
	  while (ret = next_rng (&pos))
	    {
	      printf ("Found %u at %lx  ", pos, ret);
	      for (t = 0; t < siz[n]; t++)
		printf ("%x ", ret[t]);
	      printf ("\n");
	    }
	  break;

	case 'm':
	  ret = make (vec[n], atoi (&buf[2]));
	  if (ret)
	    {
	      z = atoi (&buf[4]);
	      printf ("Made at %lx  ", ret);
	      for (t = 0; t < siz[n]; t++)
		{
		  printf ("%x(%x) ", ret[t], z);
		  ret[t] = z;
		}
	      printf ("\n");
	    }
	  else
	    printf ("Failed!!\n");
	  break;

	case 'M':
	  z = 0;
	  if (sscanf (&buf[2], "%d %d %d", &lo, &hi, &z) < 2)
	    {
	      printf ("Scan failed\n");
	      break;
	    }
	  make_rng (vec[n], lo, hi);
	  while (ret = next_rng (&pos))
	    {
	      printf ("Found %u at %lx  ", pos, ret);
	      for (t = 0; t < siz[n]; t++)
		{
		  printf ("%x(%x) ", ret[t], z);
		  ret[t] = z;
		}
	      if (z)
		z++;
	      printf ("\n");
	    }
	  break;

	case 'q':
	  exit (0);

	default:
	  printf ("Unknown command!\n");
	}
    }
}

#endif
