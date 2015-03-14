/*---------------------------------------------------------------------------+
 |  reg_add_sub.c                                                            |
 |                                                                           |
 | Functions to add or subtract two registers and put the result in a third. |
 |                                                                           |
 | Copyright (C) 1992,1993                                                   |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail apm233m@vaxc.cc.monash.edu.au    |
 |                                                                           |
 |                                                                           |
 +---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------+
 | For each function, the destination may be any FPU_REG, including one of   |
 | the source FPU_REGs.                                                      |
 +---------------------------------------------------------------------------*/

#include "exception.h"
#include "reg_constant.h"
#include "fpu_emu.h"
#include "control_w.h"
#include "fpu_system.h"


void reg_add(FPU_REG *a, FPU_REG *b, FPU_REG *dest, int control_w)
{
  int diff;
  
  if ( !(a->tag | b->tag) )
    {
      /* Both registers are valid */
      if (!(a->sign ^ b->sign))
	{
	  /* signs are the same */
	  reg_u_add(a, b, dest, control_w);
	  dest->sign = a->sign;
	  return;
	}
      
      /* The signs are different, so do a subtraction */
      diff = a->exp - b->exp;
      if (!diff)
	{
	  diff = a->sigh - b->sigh;  /* Works only if ms bits are identical */
	  if (!diff)
	    {
	      diff = a->sigl > b->sigl;
	      if (!diff)
		diff = -(a->sigl < b->sigl);
	    }
	}
      
      if (diff > 0)
	{
	  reg_u_sub(a, b, dest, control_w);
	  dest->sign = a->sign;
	}
      else if ( diff == 0 )
	{
	  reg_move(&CONST_Z, dest);
	  /* sign depends upon rounding mode */
	  dest->sign = ((control_w & CW_RC) != RC_DOWN)
	    ? SIGN_POS : SIGN_NEG;
	}
      else
	{
	  reg_u_sub(b, a, dest, control_w);
	  dest->sign = b->sign;
	}
      return;
    }
  else
    {
      if ( (a->tag == TW_NaN) || (b->tag == TW_NaN) )
	{ real_2op_NaN(a, b, dest); return; }
      else if (a->tag == TW_Zero)
	{
	  if (b->tag == TW_Zero)
	    {
	      char different_signs = a->sign ^ b->sign;
	      /* Both are zero, result will be zero. */
	      reg_move(a, dest);
	      if (different_signs)
		{
		  /* Signs are different. */
		  /* Sign of answer depends upon rounding mode. */
		  dest->sign = ((control_w & CW_RC) != RC_DOWN)
		    ? SIGN_POS : SIGN_NEG;
		}
	    }
	  else
	    reg_move(b, dest);
	  return;
	}
      else if (b->tag == TW_Zero)
	{ reg_move(a, dest); return; }
      else if (a->tag == TW_Infinity)
	{
	  if (b->tag != TW_Infinity)
	    { reg_move(a, dest); return; }
	  /* They are both + or - infinity */
	  if (a->sign == b->sign)
	    { reg_move(a, dest); return; }
	  reg_move(&CONST_QNaN, dest);	/* inf - inf is undefined. */
	  return;
	}
      else if (b->tag == TW_Infinity)
	{ reg_move(b, dest); return; }
    }
#ifdef PARANOID
  EXCEPTION(EX_INTERNAL|0x101);
#endif
}


/* Subtract b from a.  (a-b) -> dest */
void reg_sub(FPU_REG *a, FPU_REG *b, FPU_REG *dest, int control_w)
{
  int diff;

  if ( !(a->tag | b->tag) )
    {
      /* Both registers are valid */
      diff = a->exp - b->exp;
      if (!diff)
	{
	  diff = a->sigh - b->sigh;  /* Works only if ms bits are identical */
	  if (!diff)
	    {
	      diff = a->sigl > b->sigl;
	      if (!diff)
		diff = -(a->sigl < b->sigl);
	    }
	}
      
      switch (a->sign*2 + b->sign)
	{
	case 0: /* P - P */
	case 3: /* N - N */
	  if (diff > 0)
	    {
	      reg_u_sub(a, b, dest, control_w);
	      dest->sign = a->sign;
	    }
	  else if ( diff == 0 )
	    {
	      reg_move(&CONST_Z, dest);
	      /* sign depends upon rounding mode */
	      dest->sign = ((control_w & CW_RC) != RC_DOWN)
		? SIGN_POS : SIGN_NEG;
	    }
	  else
	    {
	      reg_u_sub(b, a, dest, control_w);
	      dest->sign = a->sign ^ SIGN_POS^SIGN_NEG;
	    }
	  return;
	case 1: /* P - N */
	  reg_u_add(a, b, dest, control_w);
	  dest->sign = SIGN_POS;
	  return;
	case 2: /* N - P */
	  reg_u_add(a, b, dest, control_w);
	  dest->sign = SIGN_NEG;
	  return;
	}
    }
  else
    {
      if ( (a->tag == TW_NaN) || (b->tag == TW_NaN) )
	{ real_2op_NaN(a, b, dest); return; }
      else if (b->tag == TW_Zero)
	{ 
	  if (a->tag == TW_Zero)
	    {
	      char same_signs = !(a->sign ^ b->sign);
	      /* Both are zero, result will be zero. */
	      reg_move(a, dest); /* Answer for different signs. */
	      if (same_signs)
		{
		  /* Sign depends upon rounding mode */
		  dest->sign = ((control_w & CW_RC) != RC_DOWN)
		    ? SIGN_POS : SIGN_NEG;
		}
	    }
	  else
	    reg_move(a, dest);
	  return;
	}
      else if (a->tag == TW_Zero)
	{
	  reg_move(b, dest);
	  dest->sign ^= SIGN_POS^SIGN_NEG;
	  return;
	}
      else if (a->tag == TW_Infinity)
	{
	  if (b->tag != TW_Infinity)
	    { reg_move(a, dest); return; }
	  if (a->sign == b->sign)
	    { reg_move(&CONST_QNaN, dest); return; }
	  reg_move(a, dest);
	  return;
	}
      else if (b->tag == TW_Infinity)
	{
	  reg_move(b, dest);
	  dest->sign ^= SIGN_POS^SIGN_NEG;
	  return;
	}
    }
#ifdef PARANOID
  EXCEPTION(EX_INTERNAL|0x110);
#endif
}

