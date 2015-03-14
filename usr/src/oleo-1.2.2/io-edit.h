#ifndef IO_EDITH
#define IO_EDITH

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

#include "cmd.h"

/* These must agree with the order of functions in edit_funcs.  */
#define L_TOGGLE_OVER	 0
#define L_BEG_LINE	 1
#define L_END_LINE	 2
#define L_BK_CHR	 3
#define L_BK_WORD	 4
#define L_BK_DEL_CHR	 5
#define L_BK_DEL_WORD	 6
#define L_BK_DEL_END	 7
#define L_FW_CHR	 8
#define L_FW_WORD	 9
#define L_FW_DEL_CHR	10
#define L_FW_DEL_WORD	11
#define L_FW_DEL_END	12
#define L_FINISH	13
#define L_INS_EXPR	14
#define L_INS_VAL	15
#define L_INS_REL	16
#define L_INS_ABS	17
#define L_INS_CHR	18

extern struct cmd_func edit_funcs[];

#endif
