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


#include "funcdef.h"
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>

#undef NULL

#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "io-edit.h"

struct cmd_func edit_funcs[] =
{
  {"toggle-over-write", 0, TOPLN, 0},
  {"beginning-of-line", 0, TOPLN, 0},
  {"end-of-line", 0, TOPLN, 0},
  {"backward-char", "n", TOPLN, 0},
  {"backward-word", "n", TOPLN, 0},
  {"delete-backward-char", "n", TOPLN, 0},
  {"backward-delete-word", "n", TOPLN, 0},
  {"delete-to-start", 0, TOPLN, 0},
  {"forward-char", "n", TOPLN, 0},
  {"forward-word", "n", TOPLN, 0},
  {"delete-char", "n", TOPLN, 0},
  {"delete-word", "n", TOPLN, 0},
  {"kill-line", 0, TOPLN, 0},
  {"finish-line", 0, TOPLN, 0},
  {"insert-cell-expression", 0, TOPLN, 0},
  {"insert-cell-value", 0, TOPLN, 0},
  {"insert-rel-ref", 0, TOPLN, 0},
  {"insert-abs-ref", 0, TOPLN, 0},
  {"insert-character", "T", TOPLN, 0},
  {0, 0, 0, 0}
};
