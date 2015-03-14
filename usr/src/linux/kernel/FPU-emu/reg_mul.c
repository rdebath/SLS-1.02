/*---------------------------------------------------------------------------+
 |  reg_mul.c                                                                |
 |                                                                           |
 | Multiply one FPU_REG by another, put the result in a destination FPU_REG. |
 |                                                                           |
 | Copyright (C) 1992,1993                                                   |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail apm233m@vaxc.cc.monash.edu.au    |
 |                                                                           |
 |                                                                           |
 +---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------+
 | The destination may be any FPU_REG, including one of the source FPU_REGs. |
 +---------------------------------------------------------------------------*/

#include "exception.h"
#include "reg_constant.h"
#include "fpu_emu.h"
#include "fpu_system.h"


/* This routine must be called with non-empty source registers */
void reg_mul(FPU_REG *a, FPU_REG *b, FPU_REG *dest, unsigned int control_w)
{
  if (!(a->tag | b->tag))
    {
      /* This should be the most common case */
      dest->sign = (a->sign ^ b->sign);
      reg_u_mul(a, b, dest, control_w);
      dest->exp += - EXP_BIAS + 1;
/*      dest->tag = TW_Valid; ****** */
      if ( dest->exp <= EXP_UNDER )
	{ arith_underflow(FPU_st0_ptr); }
      else if ( dest->exp >= EXP_OVER )
	{ arith_overflow(FPU_st0_ptr); }
      return;
    }
  else if ((a->tag <= TW_Zero) && (b->tag <= TW_Zero))
    {
      /* Must have either both arguments == zero, or
	 one valid and the other zero.
	 The result is therefore zero. */
      reg_move(&CONST_Z, dest);
    }
  else if ((a->tag <= TW_Denormal) && (b->tag <= TW_Denormal))
    {
      /* One or both arguments are de-normalized */
      /* Internal de-normalized numbers are not supported yet */
      EXCEPTION(EX_INTERNAL|0x105);
      reg_move(&CONST_Z, dest);
    }
  else
    {
      /* Must have infinities, NaNs, etc */
      if ( (a->tag == TW_NaN) || (b->tag == TW_NaN) )
	{ real_2op_NaN(a, b, dest); return; }
      else if (a->tag == TW_Infinity)
	{
	  if (b->tag == TW_Zero)
	    { arith_invalid(dest); return; }
	  else
	    {
	      reg_move(a, dest);
	      dest->sign = a->sign == b->sign ? SIGN_POS : SIGN_NEG;
	    }
	}
      else if (b->tag == TW_Infinity)
	{
	  if (a->tag == TW_Zero)
	    { arith_invalid(dest); return; }
	  else
	    {
	      reg_move(b, dest);
	      dest->sign = a->sign == b->sign ? SIGN_POS : SIGN_NEG;
	    }
	}
#ifdef PARANOID
      else
	{
	  EXCEPTION(EX_INTERNAL|0x102);
	}
#endif PARANOID
      dest->sign = (a->sign ^ b->sign);
    }
}
