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

/* These are all the possible error codes that eval_expression() can return. */
#define ERR_CMD		1
#define BAD_INPUT	2
#define NON_NUMBER	3
#define NON_STRING	4
#define NON_BOOL	5
#define NON_RANGE	6
#define OUT_OF_RANGE	7
#define NO_VALUES	8
#define DIV_ZERO	9
#define BAD_NAME	10
#define NOT_AVAL	11
#define PARSE_ERR	12
#define NO_OPEN		13
#define NO_CLOSE	14
#define NO_QUOTE	15
#define BAD_CHAR	16
#define BAD_FUNC	17
