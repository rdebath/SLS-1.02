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


#include <ctype.h>
#include <errno.h>
#include "font.h"
#include "math.h"
#include "window.h"
#include "io-abstract.h"

static struct font_memo default_cell_font =
{
  "*times-medium-r-*",
  "Times-Roman",
  1.0,
  0
};

struct font_memo *font_list = 0;

void 
init_fonts ()
{
}

char *default_x_name = "*times-medium-r-*";
char *default_ps_name = "Times-Roman";

struct font_names
{
  struct font_names * next;
  char * name;
  char * definition;
  struct font_memo * font;
};

static struct font_names * font_names;

void
define_font (description)
     char * description;
{
  char * p;
  char * p2;
  char oleo_name[1000];
  struct font_names * fn;

  for (p = description; isspace (*p); ++p);
  for (p2 = oleo_name; *p && (*p != ',') && !isspace (*p); *p2++ = *p++);
  *p2 = '\0';
  if (oleo_name[0] == '\0')
    return;
  
  for (fn = font_names; fn; fn = fn->next)
    if (!strcmp(fn->name, oleo_name))
      {
	free (fn->definition);
	fn->definition = 0;
      }
  if (!fn)
    {
      fn = (struct font_names *)ck_malloc (sizeof (*fn));
      fn->name = strdup (oleo_name);
      fn->next = font_names;
      font_names = fn;
    }
  fn->definition = strdup (p);
  fn->font = intern_font (fn->definition);
}


/* Returns NULL if the font isn't returned by any of the font_names. */

static char * 
nice_name (font)
     struct font_memo * font;
{
  struct font_names * fn;
  if (!font)
    return "def";
  for (fn = font_names; fn; fn = fn->next)
    if (fn->font == font)
      return fn->name;
  return 0;
}

static int
says_default (str)
     char * str;
{
  char * key = "ault";
  if (strincmp(str, "def", 3))
    return 0;
  str += 3;
  while (*str && *key)
    if (tolower(*str) != *key)
      return 0;
  while (isspace(*str))
    ++str;
  return !*str;
}

struct font_memo *
intern_font (fullname)
     char *fullname;
{
  struct font_memo *it = font_list;
  char name[1000];
  char psname[1000];
  char retrybuf[1000];
  char *p;
  char *p2;
  double scale;

 retry:
  for (p = fullname; isspace (*p); ++p);
  if (says_default (p))		/* The specification "default" is different */
    return 0;			/* from "default,".  The output of the former */
				/* changes when set_x_default_font is changed, */
				/* the latter is fixed. */
  for (p2 = name; *p && (*p != ',') && !isspace (*p); *p2++ = *p++);
  *p2 = '\0';
  if (name[0] == '\0' || says_default(name))
    strcpy (name, default_x_name);

  /* The X name might really have been a `defined font name'.
   * In that case, we splice the definition in in place of the name
   * and start over again.
   */
  {
    struct font_names * fn;
    for (fn = font_names; fn; fn = fn->next)
      if (!strcmp (name, fn->name))
	{
	  strcpy (retrybuf, fn->definition);
	  strcat (retrybuf, p);
	  fullname = retrybuf;
	  goto retry;
	}
  }

  while (isspace (*p))
    ++p;
  if (*p == ',')
    ++p;
  while (isspace (*p))
    ++p;
  for (p2 = psname; *p && (*p != ',') && !isspace (*p); *p2++ = *p++);
  *p2 = '\0';
  if (psname[0] == '\0' || says_default(name))
    strcpy (psname, default_ps_name);
  while (isspace (*p))
    ++p;
  if (*p == ',')
    ++p;
  while (isspace (*p))
    ++p;
  if (isdigit (*p))
    {
      errno = 0;
      scale = atof (p);
      if (errno)
	scale = 1.;
    }
  else
    scale = 1.;

  while (it)
    if (   scale == it->scale
	&& !strcmp (name, it->name)
	&& !strcmp (psname, it->psname))
      break;
    else
      it = it->next;
  if (!it)
    {
      struct font_memo *f =
	(struct font_memo *) ck_malloc (sizeof (*f));
      f->scale = scale;
      f->name = strdup (name);
      f->psname = strdup (psname);
      f->next = font_list;
      font_list = f;
      it = f;
    }
  return it;
}


/* This fills a line with the name of a font, using the defined font_names 
 * if any apply. 
 */
void
set_line_to_nice_font_name (line, font)
     struct line * line;
     struct font_memo * font;
{
  char * nice = nice_name (font);
  if (nice)
    set_line (line, nice);
  else
    sprint_line (line, "%s, %s, %f", font->name, font->psname, font->scale);
}

void 
set_area_font (rng, font)
     struct rng *rng;
     struct font_memo *font;
{
  CELL * cp;
  make_cells_in_range (rng);
  cp = next_cell_in_range ();
  while (cp)
    {
      cp->cell_font = font;
      cp = next_cell_in_range ();
    }
  io_redo_region (rng);
}

void 
flush_fonts ()
{
  CELL * cp;
  struct rng rng;
  rng.lr = MIN_ROW;
  rng.hr = MAX_ROW;
  rng.lc = MIN_COL;
  rng.hc = MAX_COL;
  find_cells_in_range (&rng);
  cp = next_cell_in_range ();
  while (cp)
    {
      cp->cell_font = 0;
      cp = next_cell_in_range ();
    }
}

struct font_memo * 
default_font ()
{
  return &default_cell_font;
}

void
set_x_default_font (str)
     char * str;
{
  default_cell_font.name = strdup(str);
  io_repaint ();
}

void
set_ps_font_cmd (name)
     char * name;
{
  /* A deliberate core leak so we can init with a constant. */
  default_cell_font.psname = strdup (name);
}
