
/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations. Selling this software for profit without
 * the permission of the Board of Regents of the University
 * of Wisconsin is strictly forbidden. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */

/*	SC	A Table Calculator
 *		Common definitions
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *		R. Bond  12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 6.1 $
 *
 */

/* 7-21-91  B. Backman  Added minimum window size definition to this file */


#include <stdio.h>

#define MAXROWS 200
#define MAXCOLS 40
#define RESCOL 5  /* columns reserved for row numbers */
#define RESROW 4  /* rows reserved for prompt, error, and column numbers */
#define DEFWIDTH 10 /* Default column width and precision */
#define DEFPREC   2
/*#define error move(1,0),clrtoeol(), (void) printw */
  /* error is now defined in scXstuff.h */

/*the following give the minimum size of the main window, in text coordinates*/ 
#define MIN_COLS 80
#define MIN_ROWS 25

/* X stuff definitions: */
#define SC_FONT "fixed"
#define SC_TITLE "XSpread V7.0"

struct ent_ptr {
    int vf;
    struct ent *vp;
};

struct range_s {
	struct ent_ptr left, right;
};

/*
 * If you want to save room, make row and col below into unsigned
 * chars and make sure MAXROWS and MAXCOLS above are both less
 * than 256.  (128 if your compiler doesn't support unsigned char).
 *
 * Some not too obvious things about the flags:
 *    is_valid means there is a valid number in v.
 *    label set means it points to a valid constant string.
 *    is_strexpr set means expr yields a string expression.
 *    If is_strexpr is not set, and expr points to an expression tree, the
 *        expression yields a numeric expression.
 *    So, either v or label can be set to a constant. 
 *        Either (but not both at the same time) can be set from an expression.
 */

#define VALID_CELL(p, r, c) ((p = tbl[r][c])&&((p->flags&is_valid)||p->label))

struct ent {
    double v;
    char *label;
    struct enode *expr;
    short flags;
    short row, col;
    struct ent *next;
};

struct range {
    struct ent_ptr r_left, r_right;
    char *r_name;
    struct range *r_next, *r_prev;
    int r_is_range;
};

#define FIX_ROW 1
#define FIX_COL 2

struct enode {
    int op;
    union {
	double k;
	struct ent_ptr v;
	struct range_s r;
	char *s;
	struct {
	    struct enode *left, *right;
	} o;
    } e;
};

/* op values */
#define O_VAR 'v'
#define O_CONST 'k'
#define O_SCONST '$'
#define REDUCE 0200	/* Or'ed into OP if operand is a range */

#define OP_BASE 256
#define ACOS OP_BASE + 0
#define ASIN OP_BASE + 1
#define ATAN OP_BASE + 2
#define CEIL OP_BASE + 3
#define COS OP_BASE + 4
#define EXP OP_BASE + 5 
#define FABS OP_BASE + 6 
#define FLOOR OP_BASE + 7
#define HYPOT OP_BASE + 8
#define LOG OP_BASE + 9
#define LOG10 OP_BASE + 10
#define POW OP_BASE + 11
#define SIN OP_BASE + 12
#define SQRT OP_BASE + 13
#define TAN OP_BASE + 14
#define DTR OP_BASE + 15
#define RTD OP_BASE + 16
#define MIN OP_BASE + 17
#define MAX OP_BASE + 18
#define RND OP_BASE + 19
#define HOUR OP_BASE + 20
#define MINUTE OP_BASE + 21
#define SECOND OP_BASE + 22
#define MONTH OP_BASE + 23
#define DAY OP_BASE + 24
#define YEAR OP_BASE + 25
#define NOW OP_BASE + 26
#define DATE OP_BASE + 27
#define FMT OP_BASE + 28
#define SUBSTR OP_BASE + 29
#define STON OP_BASE + 30
#define EQS OP_BASE + 31
#define EXT OP_BASE + 32
#define ELIST OP_BASE + 33	/* List of expressions */
#define LMAX  OP_BASE + 34
#define LMIN  OP_BASE + 35
#define NVAL OP_BASE + 36
#define SVAL OP_BASE + 37
#define PV OP_BASE + 38
#define FV OP_BASE + 39
#define PMT OP_BASE + 40
#define STINDEX OP_BASE + 41
#define LOOKUP OP_BASE + 42
#define ATAN2 OP_BASE + 43
#define INDEX OP_BASE + 44

/* flag values */
#define is_valid     0001
#define is_changed   0002
#define is_strexpr   0004
#define is_leftflush 0010
#define is_deleted   0020

#define ctl(c) (c&037)
#define ESC 033
#define DEL 0177

#define BYCOLS 1
#define BYROWS 2
#define BYGRAPH 4		/* Future */

#define	TBL	1		/* tblprint style output for 'tbl' */
#define	LATEX	2		/* tblprint style output for 'LaTeX' */
#define	TEX	3		/* tblprint style output for 'TeX' */

/* Types for etype() */

#define NUM	1
#define STR	2

extern struct ent *tbl[MAXROWS][MAXCOLS];

extern char curfile[];
extern int strow, stcol;
extern int currow, curcol;
extern int savedrow, savedcol;
extern int FullUpdate;
extern int maxrow, maxcol;
extern int fwidth[MAXCOLS];
extern int precision[MAXCOLS];
extern char col_hidden[MAXCOLS];
extern char row_hidden[MAXROWS];
extern char line[1000];
extern char stringbuf[1024];
extern int linelim;
extern int changed;
extern struct ent *to_fix;
extern int showsc, showsr;
extern struct enode *new();
extern struct enode *new_const();
extern struct enode *new_var();
extern struct enode *new_str();
extern struct enode *new_range();
extern struct ent *lookat();
extern struct enode *copye();
extern char *coltoa();
extern FILE *f ;
extern struct range *find_range();
extern char *v_name();
extern char *r_name();
extern double eval();
extern char *seval();
extern int modflg;
extern int Crypt;
extern char *mdir;
extern char *xmalloc();
extern int xfree();
extern double prescale;
extern int extfunc;
extern int propagation;
extern int calc_order;
extern int autocalc;
extern int numeric;
extern int showcell;
extern int showtop;
extern int loading;
extern int tbl_style;
extern char *progname;
extern int cx, cy ;
extern int charwidth, charheight ;
extern char *string ;

extern unsigned int menu();
extern char *get_str();
extern void show_top_line();
extern void Main_Menu();
extern void Range_Menu();
extern void Row_Col_Menu();
extern void Option_Menu();
extern void File_Menu();
/* extern void xplot(); */

/* extern void Graph_Menu(); */
 
#if BSD42 || SYSIII

#ifndef cbreak
#define	cbreak		crmode
#define	nocbreak	nocrmode
#endif

#endif

