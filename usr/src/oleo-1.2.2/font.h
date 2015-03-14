#ifndef FONTH
#define FONTH

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
/*  t. lord	Sat Sep 12 13:46:16 1992	*/
#include "global.h"
#include "line.h"

struct font_memo
{
  char *name;			/* X11 font name */
  char *psname;
  double scale;			/* ratio of def pt. size to this */
  struct font_memo *next;
  int id_memo;			/* This is used by oleofile. */
};

extern struct font_memo *font_list;

/*
 * Font spec syntax:
 * X name(w/wild cards), postscript name, size
 *
 * The x and postscript names may be blank. The size may be omitted
 * altogether (it defaults to 1).
 *
 * Define font takes an additional name:
 *      oleo-name, xname, psname, size
 *
 * Intern also understands just 
 *	oleo-name
 * for previously defined names. 
 */

#ifdef __STDC__
extern void init_fonts (void);
extern void flush_fonts (void);
extern struct font_memo *intern_font (char *spec);
extern void define_font (char * spec);
extern void set_area_font (struct rng *, struct font_memo *);
extern struct font_memo *default_font (void);
extern void set_ps_font_cmd (char *);
extern void set_line_to_nice_font_name (struct line *, struct font_memo *);
#ifdef HAVE_X11_X_H
extern void set_x_default_font (char *);
#endif
#else
extern void init_fonts ();
extern void flush_fonts ();
extern struct font_memo *intern_font ();
extern void define_font ();
extern void set_area_font ();
extern struct font_memo *default_font ();
extern void query_fonts ();
extern void set_ps_font_cmd ();
extern void set_line_to_nice_font_name ();
#ifdef HAVE_X11_X_H
extern void set_x_default_font ();
#endif
#endif

#endif
