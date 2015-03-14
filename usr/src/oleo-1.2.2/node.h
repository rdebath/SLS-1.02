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

struct node
  {
    unsigned char comp_value;
    unsigned char sub_value;	/* Used only for USR functions */
    unsigned short add_byte;
    union
      {
	double v_float;
	char *v_string;
	long v_int;
	struct node *v_subs[2];
	struct rng v_rng;
	VOIDSTAR v_var;
      } n_x;
  };
