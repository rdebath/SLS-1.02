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

#include <math.h>
#include <ctype.h>
#include <stdio.h>

#ifdef __TURBOC__
#define SMALLEVAL
#endif

#include "funcdef.h"

#if defined(HAVE_RINT)
#ifdef __STDC__
extern double rint (double);
extern long random (void);
#else
extern double rint ();
extern long random ();
#endif
#else
#define rint(x) (((x)<0) ? ceil((x)-.5) : floor((x)+.5))
#endif

#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"

#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "eval.h"
#include "errors.h"


extern int n_usr_funs;




double to_int ();
static int deal_area ();
static void add_int ();
static void add_flt ();
#ifndef __TURBOC__
RETSIGTYPE math_sig ();
#endif

#ifdef __STDC__
int fls (long);
#else
int fls ();
#endif
#ifdef SMALLEVAL
int __to_flt (struct value *);
int __to_int (struct value *);
int __to_num (struct value *);
int __to_str (struct value *);
int __to_bol (struct value *);
int __to_rng (struct value *);
#endif



struct value
  {
    int type;
    union vals x;
  };

#define Float	x.c_d
#define String	x.c_s
#define Int	x.c_l
#define Value	x.c_i
#define Rng	x.c_r

#define PI (3.14159265358979326848)

static struct value *stack;
static int stackmax;
static int curstack;

unsigned short current_cycle;

CELLREF cur_row;
CELLREF cur_col;

static double exp10_arr[] =
{
  1E0, 1E1, 1E2, 1E3, 1E4,
  1E5, 1E6, 1E7, 1E8, 1E9,
  1E10, 1E11, 1E12, 1E13, 1E14,
  1E15, 1E16, 1E17, 1E18, 1E19,
  1E20, 1E21, 1E22, 1E23, 1E24,
  1E25, 1E26, 1E27, 1E28, 1E29
};

/* Various math conversions with error checking */
#define I_ADD(i1,i2) {	itmp=(i1)+(i2);					\
			if((i1>0)==(i2>0) && (itmp>0)!=(i1>0)) {	\
				p->Float=(double)(i1)+(double)(i2);	\
				p->type=TYP_FLT;			\
			} else						\
				p->Int=itmp;	}

#define I_SUB(i1,i2) {	itmp=(i1)-(i2);					\
			if(((i1)<0)==((i2)>0) && ((itmp)>0)!=((i2)<0)) {\
				p->Float=(double)(i1)-(double)(i2);	\
				p->type=TYP_FLT;			\
			} else						\
				p->Int=itmp;	}

#define I_DIV(i1,i2) {	ftmp=(double)(i1)/(double)(i2);			\
			/* ... */;					\
			p->Float=ftmp;					\
			p->type=TYP_FLT;	}

#define I_MOD(i1,i2) {itmp=(i1)%(i2);/* ... */;p->Int=itmp;}

#define I_MUL(i1,i2) {	if(fls(i1)+fls(i2)>32) {			\
				p->Float=(double)(i1)*(double)(i2);	\
				p->type=TYP_FLT;			\
			} else						\
				p->Int=(i1)*(i2);	}

#define F_ADD(f1,f2) {	ftmp=(f1)+(f2);/* ... */;p->Float=ftmp;	}

#define F_SUB(f1,f2) {	ftmp=(f1)-(f2);/* ... */;p->Float=ftmp;}

#define F_DIV(f1,f2) {	ftmp=(f1)/(f2);/* ... */;p->Float=ftmp;}

#define F_MOD(f1,f2) {	itmp=(long)(f1)%(long)(f2);/* ... */;p->Int=itmp;p->type=TYP_INT;}

#define F_MUL(f1,f2) {	ftmp=(f1)*(f2);/* ... */;p->Float=ftmp;}

double ftmp;
long itmp;
int overflow;

/* You may ask:  Why not jsut put the value in stack[0] and goto break_out
   The answer is that ERROR is a valid input type for several operators, so
   we want to work if we're feeding an error into one of these operators. . .
 */
#define ERROR(cause)		\
	{			\
		p->type=TYP_ERR;\
		p->Value=cause; \
		goto next_byte; \
	}

#ifdef SMALLEVAL

#define TO_FLT(val)			\
	if((tmp=__to_flt(val))!=0)	\
		ERROR(tmp);

#define TO_INT(val)			\
	if((tmp=__to_int(val))!=0)	\
		ERROR(tmp);

#define TO_NUM(val)			\
	if((tmp=__to_num(val))!=0)	\
		ERROR(tmp);

#define TO_STR(val)			\
	if((tmp=__to_str(val))!=0)	\
		ERROR(tmp);

#define TO_BOL(val)			\
	if((tmp=__to_bol(val))!=0)	\
		ERROR(tmp);

#define TO_RNG(val)			\
	if((tmp=__to_rng(val))!=0)	\
		ERROR(tmp);

#else
#define TO_FLT(val)	\
	if((val)->type==TYP_FLT) \
		; \
	else if((val)->type==TYP_INT) { \
		(val)->type=TYP_FLT; \
		(val)->Float=(double)(val)->Int; \
	} else if((val)->type==TYP_STR) { \
		(val)->type=TYP_FLT; \
		strptr=(val)->String; \
		(val)->Float=astof(&strptr); \
		if(*strptr) \
			ERROR(NON_NUMBER); \
	} else if((val)->type==TYP_ERR) {\
		ERROR((val)->Value); \
	} else if((val)->type==0) { \
		(val)->type=TYP_FLT; \
		(val)->Float=0.0; \
	} else \
		ERROR(NON_NUMBER);

#define TO_INT(val)	\
	if((val)->type==TYP_INT) \
		; \
	else if((val)->type==TYP_FLT) { \
		(val)->type=TYP_INT; \
		(val)->Int=(long)(val)->Float; \
	} else if((val)->type==TYP_STR) { \
		(val)->type=TYP_INT; \
		strptr=(val)->String; \
		(val)->Int=astol(&strptr); \
		if(*strptr) \
			ERROR(NON_NUMBER); \
	} else if((val)->type==TYP_ERR) {\
		ERROR((val)->Value); \
	} else if((val)->type==0) { \
		(val)->type=TYP_INT; \
		(val)->Int=0; \
	} else \
		ERROR(NON_NUMBER);

#define TO_NUM(val)	\
	if((val)->type==TYP_INT || (val)->type==TYP_FLT) \
		; \
	else if((val)->type==TYP_STR) { \
		(val)->type=TYP_FLT; \
		strptr=(val)->String; \
		(val)->Float=astof(&strptr); \
		if(*strptr) \
			ERROR(NON_NUMBER); \
	} else if((val)->type==TYP_ERR) {\
		ERROR((val)->Value); \
	} else if((val)->type==0) { \
		(val)->type=TYP_INT; \
		(val)->Int=0; \
	} else \
		ERROR(NON_NUMBER);

#define TO_STR(val)	\
	if((val)->type==TYP_STR)	\
		;	\
	else if((val)->type==TYP_INT) {	\
		char *s;	\
		(val)->type=TYP_STR;	\
		s=obstack_alloc(&tmp_mem,30); \
		sprintf(s,"%ld",(val)->Int); \
		(val)->String=s;	\
	} else if((val)->type==TYP_FLT) {		\
		char *s;				\
		s=flt_to_str((val)->Float);		\
		(void)obstack_grow(&tmp_mem,s,strlen(s)+1); \
		(val)->String=obstack_finish(&tmp_mem);	\
		(val)->type=TYP_STR;			\
	} else if((val)->type==TYP_ERR) {		\
		ERROR((val)->Value);	\
	} else if((val)->type==0) {	\
		(val)->type=TYP_STR;	\
		(val)->String=obstack_alloc(&tmp_mem,1); \
		(val)->String[0]='\0'; \
	} else \
		ERROR(NON_STRING);

#define TO_BOL(val)	\
	if((val)->type==TYP_BOL)	\
		;	\
	else if((val)->type==TYP_ERR) {	\
		ERROR((val)->Value);	\
	} else	\
		ERROR(NON_BOOL);


#define TO_RNG(val) \
	if((val)->type==TYP_RNG) \
		; \
	else if((val)->type==TYP_ERR) {\
		ERROR((val)->Value); \
	} else \
		ERROR(NON_RANGE);

#endif

#define TO_ANY(val) \
	if((val)->type==TYP_RNG) \
		ERROR(BAD_INPUT); \

#define PUSH_ANY(cp)				\
	if(!cp || !GET_TYP(cp)) {		\
		p->type=0;			\
		p->Int=0;			\
	} else {				\
		p->type=GET_TYP(cp);		\
		p->x=cp->c_z;			\
	}

void
init_eval ()
{
  stack = (struct value *) ck_malloc (20 * sizeof (struct value));
  stackmax = 20;
  curstack = 0;
  current_cycle++;
#ifndef __TURBOC__
  (void) signal (SIGFPE, math_sig);
#endif
}

/* This huge function takes a byte-compiled expression and executes it. */
struct value *
eval_expression (expr)
     unsigned char *expr;
{
  unsigned char byte;
  unsigned numarg;
  unsigned jumpto;
  struct function *f;
  struct value *p;
  char *strptr;
  int tmp;

  CELLREF lrow, hrow, crow;
  CELLREF lcol, hcol, ccol;

  struct cell *cell_ptr;

  if (!expr)
    return 0;
#ifdef TEST
  jumpto = 0;
  numarg = 0;
  p = 0;
#endif
  curstack = 0;
  while ((byte = *expr++) != ENDCOMP)
    {
      if (byte < USR1)
	f = &the_funs[byte];
      else if (byte < SKIP)
	{
#ifdef TEST
	  if (byte - USR1 >= n_usr_funs)
	    panic ("Only have %d usr-function slots, but found byte for slot %d", n_usr_funs, 1 + byte - USR1);
#endif
	  tmp = *expr++;
	  f = &usr_funs[byte - USR1][tmp];
	}
      else
	f = &skip_funs[byte - SKIP];

      if (f->fn_argn & X_J)
	jumpto = *expr++;
      else if (f->fn_argn & X_JL)
	{
	  jumpto = expr[0] + ((unsigned) (expr[1]) << 8);
	  expr += 2;
	}

      switch (f->fn_argn & X_ARGS)
	{
	  /* A0 is special, since it makes the stack grow, while
			   all the others make the stack the same size or
			   less. . . */
	case X_A0:
	  numarg = 0;
	  if (curstack == stackmax)
	    {
	      stackmax *= 2;
	      stack = (struct value *) ck_realloc (stack, sizeof (struct value) * stackmax);
	    }
	  p = &stack[curstack];
	  curstack++;
	  break;

	case X_A1:
	  numarg = 1;
	  break;

	case X_A2:
	  numarg = 2;
	  break;
	case X_A3:
	  numarg = 3;
	  break;
	case X_A4:
	  numarg = 4;
	  break;
	case X_AN:
	  numarg = *expr++;
	  break;
#ifdef TEST
	default:
	  panic ("Unknown arg_num %d", f->fn_argn);
	  numarg = 0;
	  p = 0;
#endif
	}
      if (numarg > 0)
	{
	  int xt;

#ifdef TEST
	  if (curstack < numarg)
	    panic ("Only %u values on stack, not %u", curstack, numarg);
#endif
	  p = &stack[curstack - numarg];
	  curstack -= (numarg - 1);
	  for (xt = 0; xt < numarg; xt++)
	    {
	      switch (f->fn_argt[xt <= 3 ? xt : 3])
		{
		  /* A is for anything */
		  /* Any non-range value */
		case 'A':
		  TO_ANY (p + xt);
		  break;
		  /* B is for boolean */
		case 'B':
		  TO_BOL (p + xt);
		  break;
		  /* D is for Don't check */
		case 'D':
		  break;
		  /* E is for Everything */
		case 'E':
		  break;
		  /* F is for Float */
		case 'F':
		  TO_FLT (p + xt);
		  break;
		  /* I is for Int */
		case 'I':
		  TO_INT (p + xt);
		  break;
		  /* N is for Number (int or float) */
		case 'N':
		  TO_NUM (p + xt);
		  break;
		  /* R is for Range */
		case 'R':
		  TO_RNG (p + xt);
		  break;
		  /* S is for String */
		case 'S':
		  TO_STR (p + xt);
		  break;
#ifdef TEST
		default:
		  io_error_msg ("YIKE!  Unknown argtype for Fun %u  arg #%u", byte, xt);
		  break;
#endif
		}
	    }
	}

      switch (byte)
	{
	case IF_L:
	case F_IF_L:
	case IF:
	case F_IF:
	  if (p->type != TYP_BOL)
	    {
	      if (p->type != TYP_ERR)
		{
		  p->type = TYP_ERR;
		  p->Value = NON_BOOL;
		}
	      expr += jumpto;
	      if (expr[-2] != SKIP)
		jumpto = expr[-1] + (((unsigned) expr[-2]) << 8);
	      else
		jumpto = expr[-1];
	      expr += jumpto;	/* Skip both branches of the if */

	    }
	  else if (p->Value == 0)
	    {
	      expr += jumpto;
	      --curstack;
	    }
	  else
	    --curstack;
	  break;

	case SKIP_L:
	case SKIP:
	  --curstack;
	  expr += jumpto;
	  break;

	case AND_L:
	case AND:
	  if (p->type == TYP_ERR)
	    expr += jumpto;
	  else if (p->type != TYP_BOL)
	    {
	      p->type = TYP_ERR;
	      p->Value = NON_BOOL;
	      expr += jumpto;
	    }
	  else if (p->Value == 0)
	    expr += jumpto;
	  else
	    --curstack;
	  break;

	case OR_L:
	case OR:
	  if (p->type == TYP_ERR)
	    expr += jumpto;
	  else if (p->type != TYP_BOL)
	    {
	      p->type = TYP_ERR;
	      p->Value = NON_BOOL;
	      expr += jumpto;
	    }
	  else if (p->Value)
	    expr += jumpto;
	  else
	    --curstack;
	  break;

	case CONST_FLT:
	  p->type = TYP_FLT;
	  bcopy ((VOIDSTAR) expr, (VOIDSTAR) (&(p->Float)), sizeof (double));
	  expr += sizeof (double);
	  break;

	case CONST_INT:
	  p->type = TYP_INT;
	  bcopy ((VOIDSTAR) expr, (VOIDSTAR) (&(p->Int)), sizeof (long));
	  expr += sizeof (long);
	  break;

	case CONST_STR:
	case CONST_STR_L:
	  p->type = TYP_STR;
	  p->String = (char *) expr + jumpto;
	  break;

	case CONST_ERR:
	  p->type = TYP_ERR;
	  p->Value = *expr++;
	  /* expr+=sizeof(char *); */
	  break;

	case CONST_INF:
	case CONST_NINF:
	case CONST_NAN:
	  p->type = TYP_FLT;
	  p->Float = (byte == CONST_INF) ? __plinf : ((byte == CONST_NINF) ? __neinf : __nan);
	  break;

	case VAR:
	  {
	    struct var *varp;

	    bcopy ((VOIDSTAR) expr, (VOIDSTAR) (&varp), sizeof (struct var *));
	    expr += sizeof (struct var *);
	    switch (varp->var_flags)
	      {
	      case VAR_UNDEF:
		p->type = TYP_ERR;
		p->Value = BAD_NAME;
		break;

	      case VAR_CELL:
		cell_ptr = find_cell (varp->v_rng.lr, varp->v_rng.lc);
		PUSH_ANY (cell_ptr);
		break;

	      case VAR_RANGE:
		if (varp->v_rng.lr == varp->v_rng.hr && varp->v_rng.lc == varp->v_rng.hc)
		  {
		    cell_ptr = find_cell (varp->v_rng.lr, varp->v_rng.lc);
		    PUSH_ANY (cell_ptr);
		  }
		else
		  {
		    p->type = TYP_RNG;
		    p->Rng = varp->v_rng;
		  }
		break;
#ifdef TEST
	      default:
		panic ("Unknown var type %d", varp->var_flags);
#endif
	      }
	  }
	  break;

	  /* Cell refs */
	case R_CELL:
	case R_CELL | COLREL:
	case R_CELL | ROWREL:
	case R_CELL | ROWREL | COLREL:
	  {
	    CELLREF torow, tocol;

	    torow = GET_ROW (expr);
	    tocol = GET_COL (expr);
	    expr += EXP_ADD;
#ifdef DONTDEF
	    if (byte & ROWREL)
	      torow = (short) torow + cur_row;
	    if (byte & COLREL)
	      tocol = (short) tocol + cur_col;
#endif
#ifdef DONTDEF
	    if (torow < MIN_ROW || torow > MAX_ROW || tocol < MIN_COL || tocol > MAX_COL)
	      {
		p->type = TYP_ERR;
		p->Value = OUT_OF_RANGE;
	      }
	    else
	      {
#endif
		cell_ptr = find_cell ((CELLREF) torow, (CELLREF) tocol);
		PUSH_ANY (cell_ptr);
#ifdef DONTDEF
	      }
#endif
	  }
	  break;

	case RANGE:
	case RANGE | LRREL:
	case RANGE | LRREL | LCREL:
	case RANGE | LRREL | LCREL | HCREL:
	case RANGE | LRREL | HCREL:
	case RANGE | LRREL | HRREL:
	case RANGE | LRREL | HRREL | LCREL:
	case RANGE | LRREL | HRREL | LCREL | HCREL:
	case RANGE | LRREL | HRREL | HCREL:
	case RANGE | HRREL:
	case RANGE | HRREL | LCREL:
	case RANGE | HRREL | LCREL | HCREL:
	case RANGE | HRREL | HCREL:
	case RANGE | LCREL:
	case RANGE | LCREL | HCREL:
	case RANGE | HCREL:
	  p->type = TYP_RNG;
	  GET_RNG (expr, &(p->Rng));
	  expr += EXP_ADD_RNG;
	  break;

	case F_TRUE:
	case F_FALSE:
	  p->type = TYP_BOL;
	  p->Value = (byte == F_TRUE);
	  break;

	case F_PI:
	  p->type = TYP_FLT;
	  p->Float = PI;
	  break;

	case F_ROW:
	case F_COL:
	  p->type = TYP_INT;
	  p->Int = ((byte == F_ROW) ? cur_row : cur_col);
	  break;

	case F_NOW:
	  p->type = TYP_INT;
	  p->Int = time ((VOIDSTAR) 0);
	  break;

	  /* Single operand instrs */
	case F_ABS:
	case F_ACOS:
	case F_ASIN:
	case F_ATAN:
	case F_CEIL:
	case F_COS:
	case F_DTR:
	case F_EXP:
	case F_FLOOR:
	case F_INT:
	case F_LOG:
	case F_LOG10:
	case F_RTD:
	case F_SIN:
	case F_SQRT:
	case F_TAN:
	  {
#ifdef __STDC__
	    double (*funp1) (double);
	    funp1 = (double (*)(double)) (f->fn_fun);
#else
	    double (*funp1) ();
	    funp1 = (double (*)()) (f->fn_fun);
#endif

	    p->Float = (*funp1) (p->Float);
	    if (p->Float != p->Float)
	      ERROR (OUT_OF_RANGE);
	  }
	  break;

	case F_CTIME:
	  p->type = TYP_STR;
	  strptr = ctime ((time_t*) &p->Int);
	  p->String = obstack_alloc (&tmp_mem, 25);
	  strncpy (p->String, strptr, 24);
	  p->String[24] = '\0';
	  break;

	case NEGATE:
	case F_NEG:
	  if (p->type == TYP_ERR)
	    break;
	  if (p->type == TYP_INT)
	    p->Int = -(p->Int);
	  else if (p->type == TYP_FLT)
	    p->Float = -(p->Float);
	  else
	    ERROR (NON_NUMBER);
	  break;

	case F_RND:
	  p->Int = (random () % (p->Int)) + 1;
	  break;

	case NOT:
	case F_NOT:
	  p->Value = !(p->Value);
	  break;

	case F_ISERR:
	  p->Value = (p->type == TYP_ERR);
	  p->type = TYP_BOL;
	  break;

	case F_ISNUM:
	  if (p->type == TYP_FLT || p->type == TYP_INT)
	    p->Value = 1;
	  else if (p->type == TYP_STR)
	    {
	      strptr = p->String;
	      (void) astof (&strptr);
	      p->Value = (*strptr == '\0');
	    }
	  else
	    p->Value = 0;
	  p->type = TYP_BOL;
	  break;

	case F_ROWS:
	case F_COLS:
	  p->type = TYP_INT;
	  p->Int = 1 + (byte == F_ROWS ? (p->Rng.hr - p->Rng.lr) : (p->Rng.hc - p->Rng.lc));
	  break;

	  /* Two operand cmds */
	case F_ATAN2:
	case F_HYPOT:
	case POW:
	  {
#ifdef __STDC__
	    double (*funp2) (double, double);
	    funp2 = (double (*)(double, double)) (f->fn_fun);
#else
	    double (*funp2) ();
	    funp2 = (double (*)()) (f->fn_fun);
#endif

	    p->Float = (*funp2) (p->Float, (p + 1)->Float);
	    if (p->Float != p->Float)
	      ERROR (OUT_OF_RANGE);
	  }
	  break;

	case DIFF:
	case DIV:
	case MOD:
	case PROD:
	case SUM:

	  if (p->type != (p + 1)->type)
	    {
	      if (p->type == TYP_INT)
		{
		  p->type = TYP_FLT;
		  p->Float = (double) p->Int;
		}
	      if ((p + 1)->type == TYP_INT)
		{
		  (p + 1)->type = TYP_FLT;
		  (p + 1)->Float = (double) ((p + 1)->Int);
		}
	    }
	  if (p->type == TYP_INT)
	    {
	      switch (byte)
		{
		case DIFF:
		  I_SUB (p->Int, (p + 1)->Int);
		  break;
		case DIV:
		  if ((p + 1)->Int == 0)
		    ERROR (DIV_ZERO);
		  I_DIV (p->Int, (p + 1)->Int);
		  break;
		case MOD:
		  if ((p + 1)->Int == 0)
		    ERROR (DIV_ZERO);
		  I_MOD (p->Int, (p + 1)->Int);
		  break;
		case PROD:
		  I_MUL (p->Int, (p + 1)->Int);
		  break;
		case SUM:
		  I_ADD (p->Int, (p + 1)->Int);
		  break;
#ifdef TEST
		default:
		  panic ("Evaluator confused by byte-value %d", byte);
#endif
		}
	    }
	  else
	    {
	      switch (byte)
		{
		case DIFF:
		  F_SUB (p->Float, (p + 1)->Float);
		  break;
		case DIV:
		  if ((p + 1)->Float == 0)
		    ERROR (DIV_ZERO);
		  F_DIV (p->Float, (p + 1)->Float);
		  break;
		case MOD:
		  if ((p + 1)->Float == 0)
		    ERROR (DIV_ZERO);
		  F_MOD (p->Float, (p + 1)->Float);
		  break;
		case PROD:
		  F_MUL (p->Float, (p + 1)->Float);
		  break;
		case SUM:
		  F_ADD (p->Float, (p + 1)->Float);
		  break;
#ifdef TEST
		default:
		  panic ("Unknown operation %d", byte);
#endif
		}
	    }
	  if (overflow)
	    ERROR (OUT_OF_RANGE);
	  break;

	case EQUAL:
	case NOTEQUAL:

	case GREATEQ:
	case GREATER:
	case LESS:
	case LESSEQ:
	  if (p->type == TYP_ERR)
	    break;
	  if ((p + 1)->type == TYP_ERR)
	    ERROR ((p + 1)->Value);

	  if (p->type == TYP_BOL || (p + 1)->type == TYP_BOL)
	    {
	      if (p->type != (p + 1)->type || (byte != EQUAL && byte != NOTEQUAL))
		ERROR (BAD_INPUT);
	      if (byte == EQUAL)
		p->Value = p->Value == (p + 1)->Value;
	      else
		p->Value = p->Value != (p + 1)->Value;
	      break;
	    }
	  if (p->type != (p + 1)->type)
	    {
	      if (p->type == 0)
		{
		  if ((p + 1)->type == TYP_STR)
		    {
		      p->type = TYP_STR;
		      p->String = "";
		    }
		  else if ((p + 1)->type == TYP_INT)
		    {
		      p->type = TYP_INT;
		      p->Int = 0;
		    }
		  else
		    {
		      p->type = TYP_FLT;
		      p->Float = 0.0;
		    }
		}
	      else if ((p + 1)->type == 0)
		{
		  if (p->type == TYP_STR)
		    {
		      (p + 1)->type = TYP_STR;
		      (p + 1)->String = "";
		    }
		  else if (p->type == TYP_INT)
		    {
		      (p + 1)->type = TYP_INT;
		      (p + 1)->Int = 0;
		    }
		  else
		    {
		      (p + 1)->type = TYP_FLT;
		      (p + 1)->Float = 0.0;
		    }
		}
	      else if (p->type == TYP_STR)
		{
		  strptr = p->String;
		  if ((p + 1)->type == TYP_INT)
		    {
		      p->type = TYP_INT;
		      p->Int = astol (&strptr);
		    }
		  else
		    {
		      p->type = TYP_FLT;
		      p->Float = astof (&strptr);
		    }
		  if (*strptr)
		    {
		      p->type = TYP_BOL;
		      p->Value = (byte == NOTEQUAL);
		      break;
		    }
		}
	      else if ((p + 1)->type == TYP_STR)
		{
		  strptr = (p + 1)->String;
		  if (p->type == TYP_INT)
		    (p + 1)->Int = astol (&strptr);
		  else
		    (p + 1)->Float = astof (&strptr);
		  if (*strptr)
		    {
		      p->type = TYP_BOL;
		      p->Value = (byte == NOTEQUAL);
		      break;
		    }

		  /* If we get here, one is INT, and the other
				   is FLT  Make them both FLT */
		}
	      else if (p->type == TYP_INT)
		{
		  p->type = TYP_FLT;
		  p->Float = (double) p->Int;
		}
	      else
		(p + 1)->Float = (double) (p + 1)->Int;
	    }
	  if (p->type == TYP_STR)
	    tmp = strcmp (p->String, (p + 1)->String);
	  else if (p->type == TYP_FLT)
	    tmp = (p->Float < (p + 1)->Float) ? -1 : ((p->Float > (p + 1)->Float) ? 1 : 0);
	  else if (p->type == TYP_INT)
	    tmp = (p->Int < (p + 1)->Int ? -1 : ((p->Int > (p + 1)->Int) ? 1 : 0));
	  else if (p->type == 0)
	    tmp = 0;
	  else
	    {
	      tmp = 0;
	      panic ("Bad type value %d", p->type);
	    }
	  p->type = TYP_BOL;
	  if (tmp < 0)
	    p->Value = (byte == NOTEQUAL || byte == LESS || byte == LESSEQ);
	  else if (tmp == 0)
	    p->Value = (byte == EQUAL || byte == GREATEQ || byte == LESSEQ);
	  else
	    p->Value = (byte == NOTEQUAL || byte == GREATER || byte == GREATEQ);
	  break;

	case F_FIXED:
	  tmp = (p + 1)->Int;
	  if (tmp < -29 || tmp > 29)
	    ERROR (OUT_OF_RANGE);
	  if (tmp < 0)
	    p->Float = rint ((p->Float) / exp10_arr[-tmp]) * exp10_arr[-tmp];
	  else
	    p->Float = rint ((p->Float) * exp10_arr[tmp]) / exp10_arr[tmp];
	  break;

	case F_IFERR:
	  if (p->type == TYP_ERR)
	    *p = *(p + 1);
	  break;

	case F_INDEX:
	  tmp = (p + 1)->Int - 1;
	  if (tmp < 0)
	    ERROR (OUT_OF_RANGE);
	  lrow = p->Rng.lr;
	  lcol = p->Rng.lc;
	  hrow = p->Rng.hr;
	  hcol = p->Rng.hc;
	  if (lrow != hrow && lcol != hcol)
	    {
	      int dex;

	      dex = 1 + hrow - lrow;
	      if (tmp >= dex * (1 + hcol - lcol))
		ERROR (OUT_OF_RANGE);
	      crow = tmp % dex;
	      ccol = tmp / dex;
	      lrow += crow;
	      lcol += ccol;
	    }
	  else if (lrow != hrow)
	    {
	      if (tmp > (hrow - lrow))
		ERROR (OUT_OF_RANGE);
	      lrow += tmp;
	    }
	  else
	    {
	      if (tmp > (hcol - lcol))
		ERROR (OUT_OF_RANGE);
	      lcol += tmp;
	    }
	  cell_ptr = find_cell (lrow, lcol);
	  PUSH_ANY (cell_ptr);
	  break;

	case F_INDEX2:
	  crow = (p + 1)->Int - 1;
	  ccol = (p + 2)->Int - 1;
	  lrow = p->Rng.lr;
	  lcol = p->Rng.lc;
	  hrow = p->Rng.hr;
	  hcol = p->Rng.hc;
	  /* This generates warnings if CELLREF is an unsigned type. 
 	   * Getting rid of the warning is more trouble than 
 	   * it's worth.
 	   */
	  if (crow < 0 || ccol < 0 || crow > (hrow - lrow) ||
	      ccol > (hcol - lcol)) 
	    ERROR (OUT_OF_RANGE);
	  cell_ptr = find_cell (lrow + crow, lcol + ccol);
	  PUSH_ANY (cell_ptr);
	  break;

	  /* case F_PRINTF:
			panic("no printf yet");
			break; */

	case CONCAT:
	  strptr = (char *) obstack_alloc (&tmp_mem, strlen (p->String) + strlen ((p + 1)->String) + 1);
	  strcpy (strptr, p->String);
	  strcat (strptr, (p + 1)->String);
	  p->String = strptr;
	  break;

	case F_ONEOF:
	  if (numarg < 2)
	    ERROR (NO_VALUES);
	  --numarg;
	  tmp = p->Int;
	  if (tmp < 1 || tmp > numarg)
	    ERROR (OUT_OF_RANGE);
	  /* Can never happen? */
	  TO_ANY (p + tmp);
	  p[0] = p[tmp];
	  break;

	case F_FILE:
	  {
	    FILE *fp;
	    char buf[128];
	    int num;

	    if (numarg < 1)
	      ERROR (NO_VALUES);
	    fp = fopen (p->String, "r");
	    if (!fp)
	      ERROR (BAD_INPUT);
	    switch (numarg)
	      {
	      case 2:
		fseek (fp, (p + 1)->Int, 0);
		/* Fallthrough */

	      case 1:
		while ((num = fread (buf, sizeof (char), sizeof (buf), fp)) > 0)
		    (void) obstack_grow (&tmp_mem, buf, num);
		break;

	      case 3:
		fseek (fp, (p + 1)->Int, 0);
		for (;;)
		  {
		    num = ((p + 2)->Int < sizeof (buf)) ? (p + 2)->Int : sizeof (buf);
		    (p + 2)->Int -= num;
		    num = fread (buf, sizeof (char), num, fp);
		    (void) obstack_grow (&tmp_mem, buf, num);
		    if (num == 0 || (p + 2)->Int == 0)
		      break;
		  }
		break;

	      default:
		ERROR (BAD_INPUT);
	      }
	    fclose (fp);
	    (void) obstack_1grow (&tmp_mem, 0);
	    p->String = obstack_finish (&tmp_mem);
	    break;
	  }

	case AREA_SUM:
	case AREA_PROD:
	case AREA_AVG:
	case AREA_STD:
	case AREA_MAX:
	case AREA_MIN:
	case AREA_CNT:
	case AREA_VAR:
	  tmp = deal_area (byte, numarg, p);
	  if (tmp)
	    ERROR (tmp);
	  break;

	  /* This is now a fallthrough for all the USRmumble codes */
	case USR1:
	default:
	  if ((f->fn_argn & X_ARGS) == X_AN)
	    {
#ifdef __STDC__
	      void (*funp) (int, struct value *);
	      funp = (void (*)(int, struct value *)) f->fn_fun;
#else
	      void (*funp) ();
	      funp = (void (*)()) f->fn_fun;
#endif
	      (*funp) (numarg, p);
	    }
	  else
	    {
#ifdef __STDC__
	      void (*funp) (struct value *);
	      funp = (void (*)(struct value *)) f->fn_fun;
#else
	      void (*funp) ();
	      funp = (void (*)()) f->fn_fun;
#endif
	      (*funp) (p);
	    }
	  break;

	  /* #ifdef TEST
		default:
			panic("Unknown byte-value %d",byte);
			break;
#endif */
	}
      /* Goto next-byte is the equiv of a multi-level break, which
		   C doesn't allow. */
    next_byte:
      ;
    }
#ifdef TEST
  if (curstack != 1)
    io_error_msg ("%d values on stack", curstack);
#endif
  return stack;
}

/* These helper functions were split out so that eval_expression would compile
   under Turbo C 2.0 on my PC.
 */


static int cnt_flt;
static int cnt_int;

static long int_tmp;
static double flt_tmp;

static long sqr_int_tmp;	/* for AREA_STD */
static double sqr_flt_tmp;

static unsigned char area_cmd;

static int
deal_area (cmd, num_args, p)
     unsigned char cmd;
     unsigned char num_args;
     struct value *p;
{
  double flt_cnt_flt;
  CELL *cell_ptr;
  char *strptr;

  area_cmd = cmd;
  cnt_flt = 0;
  cnt_int = 0;
  for (; num_args--;)
    {
      switch (p[num_args].type)
	{
	case TYP_INT:
	  add_int (p[num_args].Int);
	  break;

	case TYP_FLT:
	  add_flt (p[num_args].Float);
	  break;

	case TYP_STR:
	  strptr = p[num_args].String;
	  flt_cnt_flt = astof (&strptr);
	  if (*strptr)
	    return NON_NUMBER;
	  add_flt (flt_cnt_flt);
	  break;

	case TYP_RNG:
	  find_cells_in_range (&(p[num_args].Rng));
	  while (cell_ptr = next_cell_in_range ())
	    {
	      if (GET_TYP (cell_ptr) == TYP_FLT)
		add_flt (cell_ptr->cell_flt);
	      else if (GET_TYP (cell_ptr) == TYP_INT)
		add_int (cell_ptr->cell_int);
	      else if (GET_TYP (cell_ptr) == TYP_STR)
		{
		  strptr = cell_ptr->cell_str;
		  flt_cnt_flt = astof (&strptr);
		  if (!*strptr)
		    add_flt (flt_cnt_flt);
		}
	    }
	  break;

	case 0:
	  break;

	case TYP_ERR:
	  return p[num_args].Value;

	default:
	  return NON_NUMBER;
	}
    }
  if (!cnt_flt && !cnt_int && area_cmd != AREA_CNT)
    return NO_VALUES;

  switch (area_cmd)
    {
    case AREA_SUM:
      if (cnt_flt && cnt_int)
	{
	  flt_tmp += (double) int_tmp;
	  cnt_int = 0;
	}
      break;
    case AREA_PROD:
      if (cnt_flt && cnt_int)
	{
	  flt_tmp *= (double) int_tmp;
	  cnt_int = 0;
	}
      break;
    case AREA_AVG:
      if (cnt_flt && cnt_int)
	{
	  flt_tmp += (double) int_tmp;
	  flt_tmp /= (double) ((cnt_flt + cnt_int));
	  cnt_int = 0;
	}
      else if (cnt_flt)
	flt_tmp /= (double) cnt_flt;
      else
	{
	  flt_tmp = (double) int_tmp / (double) cnt_int;
	  cnt_int = 0;
	}
      break;
    case AREA_STD:
      if (cnt_int && cnt_flt)
	{
	  flt_tmp += (double) int_tmp;
	  sqr_flt_tmp += (double) sqr_int_tmp;
	  cnt_flt += cnt_int;
	  cnt_int = 0;
	}
      else if (cnt_int)
	{
	  flt_tmp = (double) int_tmp;
	  sqr_flt_tmp = (double) sqr_int_tmp;
	  cnt_flt = cnt_int;
	  cnt_int = 0;
	}
      flt_cnt_flt = (double) cnt_flt;
      flt_tmp = sqrt (((flt_cnt_flt * sqr_flt_tmp) -
		       (flt_tmp * flt_tmp)) /
		      (flt_cnt_flt * (flt_cnt_flt - 1)));
      break;
    case AREA_VAR:
      if (cnt_int && cnt_flt)
	{
	  flt_tmp += (double) int_tmp;
	  sqr_flt_tmp += (double) sqr_int_tmp;
	  cnt_flt += cnt_int;
	  cnt_int = 0;
	}
      else if (cnt_int)
	{
	  flt_tmp = (double) int_tmp;
	  sqr_flt_tmp = (double) sqr_int_tmp;
	  cnt_flt = cnt_int;
	  cnt_int = 0;
	}
      flt_cnt_flt = (double) cnt_flt;
      flt_tmp = ((flt_cnt_flt * sqr_flt_tmp) -
		 (flt_tmp * flt_tmp)) /
	(flt_cnt_flt * flt_cnt_flt);
      break;

    case AREA_MAX:
      if (cnt_flt && cnt_int && flt_tmp > (double) int_tmp)
	cnt_int = 0;
      break;

    case AREA_MIN:
      if (cnt_flt && cnt_int && flt_tmp < (double) int_tmp)
	cnt_int = 0;
      break;

    case AREA_CNT:
      int_tmp = cnt_int + cnt_flt;
      cnt_int = 1;
      break;

#ifdef TEST
    default:
      panic ("Unknown AREA command %d", area_cmd);
#endif
    }
  if (cnt_int)
    {
      p->type = TYP_INT;
      p->Int = int_tmp;
    }
  else
    {
      p->type = TYP_FLT;
      p->Float = flt_tmp;
    }
  return 0;
}

static void
add_flt (value)
     double value;
{
  if (cnt_flt++ == 0)
    {
      flt_tmp = value;
      sqr_flt_tmp = value * value;
      return;
    }

  switch (area_cmd)
    {
    case AREA_STD:
    case AREA_VAR:
      sqr_flt_tmp += value * value;
      /* Fall through */
    case AREA_SUM:
    case AREA_AVG:
      flt_tmp += value;
      return;
    case AREA_PROD:
      flt_tmp *= value;
      return;
    case AREA_MAX:
      if (flt_tmp < value)
	flt_tmp = value;
      return;
    case AREA_MIN:
      if (flt_tmp > value)
	flt_tmp = value;
      return;
    case AREA_CNT:
      return;
#ifdef TEST
    default:
      panic ("Unknown area command %d in add_flt(%g)", area_cmd, value);
#endif
    }
}

static void
add_int (value)
     long value;
{
  if (cnt_int++ == 0)
    {
      int_tmp = value;
      sqr_int_tmp = value * value;
      return;
    }

  switch (area_cmd)
    {
    case AREA_STD:
    case AREA_VAR:
      sqr_int_tmp += value * value;
      /* Fall through */
    case AREA_SUM:
    case AREA_AVG:
      int_tmp += value;
      return;
    case AREA_PROD:
      int_tmp *= value;
      return;
    case AREA_MAX:
      if (int_tmp < value)
	int_tmp = value;
      return;
    case AREA_MIN:
      if (int_tmp > value)
	int_tmp = value;
      return;
    case AREA_CNT:
      return;
#ifdef TEST
    default:
      panic ("Unknown Area command %d in add_int(%ld)", area_cmd, value);
#endif
    }
}

#ifdef __STDC__
double
dtr (double x)
#else
double
dtr (x)
     double x;
#endif
{
  return x * (PI / (double) 180.0);
}

#ifdef __STDC__
double
rtd (double x)
#else
double
rtd (x)
     double x;
#endif
{
  return x * (180.0 / (double) PI);
}

#ifdef __STDC__
double
to_int (double x)
#else
double
to_int (x)
     double x;
#endif
{
  return (x < 0 ? ceil (x) : floor (x));
}

/* Various methods of dealing with arithmatic overflow.  They don't work well.
   Someone should really convince this thing to properly deal with it.
 */
#ifdef __TURBOC__
int
matherr (exc)
     struct exception *exc;
{
  stack[curstack].type = TYP_ERR;
  stack[curstack].Value = BAD_INPUT;
  write (2, "MATHERR\n", 8);
  return 1;
}

#endif

#ifndef __TURBOC__
RETSIGTYPE
math_sig (sig)
     int sig;
{
  stack[curstack].type = TYP_ERR;
  stack[curstack].Value = BAD_INPUT;
}

#endif

/* Here's the entry point for this module. */
void
update_cell (cell)
     CELL *cell;
{
  struct value *new;
  int new_val;

#ifdef TEST
  extern int debug;

  if (cell->cell_cycle == current_cycle && (debug & 01))
    {
      io_error_msg ("Cycle detected at %s (%d)", cell_name (cur_row, cur_col), current_cycle);
      /* return; */
    }
  else if (debug & 02)
    io_error_msg ("Update %s", cell_name (cur_row, cur_col));
#endif
  new = eval_expression (cell->cell_formula);
  if (!new)
    {
      push_refs (cell->cell_refs_from);
      return;
    }
  cell->cell_cycle = current_cycle;

  if (new->type != GET_TYP (cell))
    {
      if (GET_TYP (cell) == TYP_STR)
	free (cell->cell_str);
      SET_TYP (cell, new->type);
      new_val = 1;
      if (new->type == TYP_STR)
	new->String = strdup (new->String);
    }
  else
    switch (new->type)
      {
      case 0:
	new_val = 0;
	break;
      case TYP_FLT:
	new_val = new->Float != cell->cell_flt;
	break;
      case TYP_INT:
	new_val = new->Int != cell->cell_int;
	break;
      case TYP_STR:
	new_val = strcmp (new->String, cell->cell_str);
	if (new_val)
	  {
	    free (cell->cell_str);
	    new->String = strdup (new->String);
	  }
	break;
      case TYP_BOL:
	new_val = new->Value != cell->cell_bol;
	break;
      case TYP_ERR:
	new_val = new->Value != cell->cell_err;
	break;
#ifdef TEST
      default:
	new_val = 0;
	panic ("Unknown type %d in update_cell", new->type);
#endif
      }
  if (new_val)
    {
      cell->c_z = new->x;
      push_refs (cell->cell_refs_from);
    }
  (void) obstack_free (&tmp_mem, tmp_mem_start);
}

int
fls (num)
     long num;
{
  int ret = 1;

  if (!num)
    return 0;
  if (num < 0)
    num = -num;
  if (num & 0xffff0000)
    {
      ret += 16;
      num = (num >> 16) & 0xffff;
    }
  if (num & 0xff00)
    {
      ret += 8;
      num >>= 8;
    }
  if (num & 0xf0)
    {
      ret += 4;
      num >>= 4;
    }
  if (num & 0x0c)
    {
      ret += 2;
      num >>= 2;
    }
  if (num & 2)
    ret++;
  return ret;
}

#ifdef SMALLEVAL
int
__to_flt (p)
     struct value *p;
{
  char *strptr;

  switch (p->type)
    {
    case 0:
      p->type = TYP_FLT;
      p->Float = 0;
      /* Fallthrough */
    case TYP_FLT:
      return 0;
    case TYP_INT:
      p->Float = (double) p->Int;
      p->type = TYP_FLT;
      return 0;
    case TYP_STR:
      p->type = TYP_FLT;
      strptr = p->String;
      p->Float = astof (&strptr);
      if (*strptr)
	return NON_NUMBER;
      return 0;
    case TYP_ERR:
      return p->Value;
    default:
      return NON_NUMBER;
    }
}

int
__to_int (p)
     struct value *p;
{
  char *strptr;

  switch (p->type)
    {
    case 0:
      p->type = TYP_INT;
      p->Int = 0;
    case TYP_INT:
      return 0;
    case TYP_FLT:
      p->type = TYP_INT;
      p->Int = (long) p->Float;
      return 0;
    case TYP_STR:
      p->type = TYP_INT;
      strptr = p->String;
      p->Int = astol (&strptr);
      if (*strptr)
	return NON_NUMBER;
      return 0;
    case TYP_ERR:
      return p->Value;
    default:
      return NON_NUMBER;
    }
}

int
__to_num (p)
     struct value *p;
{
  char *strptr;

  switch (p->type)
    {
    case 0:
      p->type = TYP_INT;
      p->Int = 0;
      return 0;
    case TYP_FLT:
    case TYP_INT:
      return 0;
    case TYP_STR:
      p->type = TYP_FLT;
      strptr = p->String;
      p->Float = astof (&strptr);
      if (*strptr)
	return NON_NUMBER;
      return 0;
    case TYP_ERR:
      return p->Value;
    default:
      return NON_NUMBER;
    }
}

int
__to_str (p)
     struct value *p;
{
  char *strptr;

  switch (p->type)
    {
    case 0:
      p->type = TYP_STR;
      p->String = obstack_alloc (&tmp_mem, 1);
      p->String[0] = '\0';
      return 0;

    case TYP_STR:
      return 0;

    case TYP_INT:
      p->type = TYP_STR;
      strptr = obstack_alloc (&tmp_mem, 30);
      sprintf (strptr, "%ld", p->Int);
      p->String = strptr;
      return 0;

    case TYP_FLT:
      p->type = TYP_STR;
      strptr = flt_to_str (p->Float);
      (void) obstack_grow (&tmp_mem, strptr, strlen (strptr) + 1);
      p->String = obstack_finish (&tmp_mem);
      return 0;

    case TYP_ERR:
      return p->Value;

    default:
      return NON_STRING;
    }
}

int
__to_bol (p)
     struct value *p;
{
  switch (p->type)
    {
    case TYP_BOL:
      return 0;
    case TYP_ERR:
      return p->Value;
    default:
      return NON_BOOL;
    }
}

int
__to_rng (p)
     struct value *p;
{
  switch (p->type)
    {
    case TYP_RNG:
      return 0;
    case TYP_ERR:
      return p->Value;
    default:
      return NON_BOOL;
    }
}

#endif
