#ifndef EVALH
#define EVALH

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


#include "cell.h"


/* Despite the name, this file contains the #defines for all the byte-compiled
   byte values.
 */

/* 0 - 5  Control stuff */
#define ENDCOMP		0	/* End of compiled code */

/* These are followed by a jump-code, or a two-byte jump-code */

#define IF		2	/* Followed by jump-code */
#define IF_L		3
#define F_IF		4	/* Like if, but decompiles differently */
#define F_IF_L		5
#define AND		6	/* Followed by jump-code */
#define AND_L		7	/* Followed by jump-code */
#define OR		8	/* Followed by jump-code */
#define OR_L		9

#define CONST_STR	10
#define CONST_STR_L	11

/* 12 - 15 Cell references */
#define R_CELL		12
#define ROWREL		1
#define COLREL		2

/* 16 - 31 Range references */
#define RANGE		16
#define LRREL		1
#define HRREL		2
#define LCREL		4
#define HCREL		8

/* 32 - 130  Various Constants */
#define F_TRUE		32
#define F_FALSE		33
#define CONST_INF	34
#define CONST_NINF	35
#define CONST_NAN	36
#define CONST_ERR	37	/* Followed by error code and text(?) */
#define CONST_FLT	38	/* Followed by double */
#define CONST_INT	39	/* Followed by long */

/* 131	Variable reference */
#define VAR		40	/* Followed by struct var * */

/* Unary Functions */
#define NEGATE		41
#define NOT		42

/* Binary Functions */
#define DIFF		43
#define DIV		44
#define MOD		45
#define PROD		46
#define SUM		47
#define CONCAT		48
#define EQUAL		49
#define GREATEQ		50
#define GREATER		51
#define LESS		52
#define LESSEQ		53
#define NOTEQUAL	54
#define POW		55

#define F_PI		56
#define F_ROW		57
#define F_COL		58
#define F_NOW		59

/* 40 - 65 one-operand functions */
#define F_ABS		60
#define F_ACOS		61
#define F_ASIN		62
#define F_ATAN		63
#define F_CEIL		64
#define F_INT		65
#define F_FLOOR		66
#define F_COS		67
#define F_DTR		68
#define F_EXP		69
#define F_LOG		70
#define F_LOG10		71
#define F_RTD		72
#define F_SIN		73
#define F_SQRT		74
#define F_TAN		75
#define F_CTIME		76
#define F_NEG		77
#define F_NOT		78
#define F_ISERR		79
#define F_ISNUM		80
#define F_RND		81
#define F_ROWS		82
#define F_COLS		83

/* 75 - 98 Two-operand functions */
#define F_ATAN2		84
#define F_HYPOT		85
#define F_FIXED		86
#define F_IFERR		87

#define F_INDEX		88

/* 100 - 106 Three input functions */
#define F_INDEX2	89

/* 110 - 114 N-input functions */
#define F_ONEOF		90

#define F_FILE		91

/* 115 - 122 area functions */
#define AREA_SUM	92
#define AREA_PROD	93
#define AREA_AVG	94
#define AREA_STD	95
#define AREA_MAX	96
#define AREA_MIN	97
#define AREA_CNT	98
#define AREA_VAR	99

#define USR1		100	/* User defined function */
/* Followed by function # */

#define SKIP		254
#define SKIP_L		255

struct function
  {
    /* See C_mumble below.  This is used when byte-compiling,
 		   and decompiling */
    /* The infix information is *not* used for parsing, although
		   it should be. . . */
    short fn_comptype;

    /* See X_mumble below.  This encodes the number of arguments
		   this function takes.  It is used by the expression
		   evaluator and the parser. */
    char fn_argn;

    /* This is used by the expression evaluator to convert the
 		   args to the fun into the appropriate type
 		   (And by the parser to see if the function will accept
		   regions as arguments) */
    char fn_argt[5];

    /* This function is called by the expression evaluator
 		   when the (spreadsheet) function is executed */
    void (*fn_fun) ();

    /* This is the function's name.  It gets used for compiling
		   decompiling, and parsing. . . */
    char *fn_str;
  };

extern struct function the_funs[];
extern int n_usr_funs;
extern struct function **usr_funs;
extern int *usr_n_funs;
extern struct function skip_funs[];

/* Magic numbers for byte-compiling and decompiling expressions */
/* These need only be distinct (and have x|C_T be distinct too!) */
#define GET_COMP(x) ((x)&0x1f)
#define C_IF	 0x1
#define C_ANDOR  0x2
#define C_ERR	 0x3
#define C_FLT	 0x4
#define C_INT	 0x5
#define C_STR	 0x6
#define C_VAR	 0x7
#define C_CELL	 0x8
#define C_RANGE	 0x9
#define C_FN0X	 0xA
#define C_FN0	 0xB
#define C_FN1	 0xC
#define C_FN2	 0xD
#define C_FN3	 0xE
#define C_FN4	 0xF
#define C_FNN	0x10
#define C_INF	0x11
#define C_UNA	0x12
#define C_CONST	0x13
#define C_SKIP  0x14

#define C_T	 0x20

#define INF(x)	(x<<8)
#define ASO	0xC0
#define R	0x40
#define L	0x80
#define N	0xC0

#define ISINFIX(x)	((x)&ASO)
#define GET_IN(x)	((x)>>8)
#define GET_ASO(x)	(((x)&ASO)==R ? -1 : (((x)&ASO)==L ? 1 : 0))

#define X_ARGS	0x07
#define X_A0	0
#define X_A1	1
#define X_A2	2
#define X_A3	3
#define X_A4	4
#define X_AN	5

#define X_J	0x08
#define X_JL	0x10

#ifdef __STDC__
extern void update_cell (CELL *);
extern double rtd (double);
extern double dtr (double);
extern double to_int (double);
#else
extern void update_cell ();
extern double rtd ();
extern double dtr ();
extern double to_int ();
#endif

#endif
