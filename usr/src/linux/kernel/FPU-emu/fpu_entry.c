/*---------------------------------------------------------------------------+
 |  fpu_entry.c                                                              |
 |                                                                           |
 | The entry function for wm-FPU-emu                                         |
 |                                                                           |
 | Copyright (C) 1992,1993                                                   |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail apm233m@vaxc.cc.monash.edu.au    |
 |                                                                           |
 | See the files "README" and "COPYING" for further copyright and warranty   |
 | information.                                                              |
 |                                                                           |
 +---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------+
 | Note:                                                                     |
 |    The file contains code which accesses user memory.                     |
 |    Emulator static data may change when user memory is accessed, due to   |
 |    other processes using the emulator while swapping is in progress.      |
 +---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------+
 | math_emulate() is the sole entry point for wm-FPU-emu                     |
 +---------------------------------------------------------------------------*/

#include <linux/config.h>

#ifdef CONFIG_MATH_EMULATION

#include <linux/signal.h>

#include "fpu_system.h"
#include "fpu_emu.h"
#include "exception.h"
#include "control_w.h"

#include <asm/segment.h>


#define __BAD__ Un_impl   /* Not implemented */

#ifndef NO_UNDOC_CODE    /* Un-documented FPU op-codes supported by default. */

/* WARNING: These codes are not documented by Intel in their 80486 manual
   and may not work on FPU clones or later Intel FPUs. */

/* Changes to support the un-doc codes provided by Linus Torvalds. */

#define _d9_d8_ fstp_i    /* unofficial code (19) */
#define _dc_d0_ fcom_st   /* unofficial code (14) */
#define _dc_d8_ fcompst   /* unofficial code (1c) */
#define _dd_c8_ fxch_i    /* unofficial code (0d) */
#define _de_d0_ fcompst   /* unofficial code (16) */
#define _df_c0_ ffreep    /* unofficial code (07) ffree + pop */
#define _df_c8_ fxch_i    /* unofficial code (0f) */
#define _df_d0_ fstp_i    /* unofficial code (17) */
#define _df_d8_ fstp_i    /* unofficial code (1f) */

static FUNC st_instr_table[64] = {
  fadd__,   fld_i_,  __BAD__, __BAD__, fadd_i,  ffree_,  faddp_,  _df_c0_,
  fmul__,   fxch_i,  __BAD__, __BAD__, fmul_i,  _dd_c8_, fmulp_,  _df_c8_,
  fcom_st,  fp_nop,  __BAD__, __BAD__, _dc_d0_, fst_i_,  _de_d0_, _df_d0_,
  fcompst,  _d9_d8_, __BAD__, __BAD__, _dc_d8_, fstp_i,  fcompp,  _df_d8_,
  fsub__,   fp_etc,  __BAD__, finit_,  fsubri,  fucom_,  fsubrp,  fstsw_,
  fsubr_,   fconst,  fucompp, __BAD__, fsub_i,  fucomp,  fsubp_,  __BAD__,
  fdiv__,   trig_a,  __BAD__, __BAD__, fdivri,  __BAD__, fdivrp,  __BAD__,
  fdivr_,   trig_b,  __BAD__, __BAD__, fdiv_i,  __BAD__, fdivp_,  __BAD__,
};

#else     /* Support only documented FPU op-codes */

static FUNC st_instr_table[64] = {
  fadd__,   fld_i_,  __BAD__, __BAD__, fadd_i,  ffree_,  faddp_,  __BAD__,
  fmul__,   fxch_i,  __BAD__, __BAD__, fmul_i,  __BAD__, fmulp_,  __BAD__,
  fcom_st,  fp_nop,  __BAD__, __BAD__, __BAD__, fst_i_,  __BAD__, __BAD__,
  fcompst,  __BAD__, __BAD__, __BAD__, __BAD__, fstp_i,  fcompp,  __BAD__,
  fsub__,   fp_etc,  __BAD__, finit_,  fsubri,  fucom_,  fsubrp,  fstsw_,
  fsubr_,   fconst,  fucompp, __BAD__, fsub_i,  fucomp,  fsubp_,  __BAD__,
  fdiv__,   trig_a,  __BAD__, __BAD__, fdivri,  __BAD__, fdivrp,  __BAD__,
  fdivr_,   trig_b,  __BAD__, __BAD__, fdiv_i,  __BAD__, fdivp_,  __BAD__,
};

#endif NO_UNDOC_CODE


#define _NONE_ 0   /* Take no special action */
#define _REG0_ 1   /* Need to check for not empty st(0) */
#define _REGI_ 2   /* Need to check for not empty st(0) and st(rm) */
#define _REGi_ 0   /* Uses st(rm) */
#define _PUSH_ 3   /* Need to check for space to push onto stack */
#define _null_ 4   /* Function illegal or not implemented */

#ifndef NO_UNDOC_CODE

/* Un-documented FPU op-codes supported by default. (see above) */

static unsigned char type_table[64] = {
  _REGI_, _NONE_, _null_, _null_, _REGI_, _REGi_, _REGI_, _REGi_,
  _REGI_, _REGI_, _null_, _null_, _REGI_, _REGI_, _REGI_, _REGI_,
  _REGI_, _NONE_, _null_, _null_, _REGI_, _REG0_, _REGI_, _REG0_,
  _REGI_, _REG0_, _null_, _null_, _REGI_, _REG0_, _REGI_, _REG0_,
  _REGI_, _NONE_, _null_, _NONE_, _REGI_, _REGI_, _REGI_, _NONE_,
  _REGI_, _NONE_, _REGI_, _null_, _REGI_, _REGI_, _REGI_, _null_,
  _REGI_, _NONE_, _null_, _null_, _REGI_, _null_, _REGI_, _null_,
  _REGI_, _NONE_, _null_, _null_, _REGI_, _null_, _REGI_, _null_
};

#else     /* Support only documented FPU op-codes */

static unsigned char type_table[64] = {
  _REGI_, _NONE_, _null_, _null_, _REGI_, _REGi_, _REGI_, _null_,
  _REGI_, _REGI_, _null_, _null_, _REGI_, _null_, _REGI_, _null_,
  _REGI_, _NONE_, _null_, _null_, _null_, _REG0_, _null_, _null_,
  _REGI_, _null_, _null_, _null_, _null_, _REG0_, _REGI_, _null_,
  _REGI_, _NONE_, _null_, _NONE_, _REGI_, _REGI_, _REGI_, _NONE_,
  _REGI_, _NONE_, _REGI_, _null_, _REGI_, _REGI_, _REGI_, _null_,
  _REGI_, _NONE_, _null_, _null_, _REGI_, _null_, _REGI_, _null_,
  _REGI_, _NONE_, _null_, _null_, _REGI_, _null_, _REGI_, _null_
};

#endif NO_UNDOC_CODE


/* Be careful when using any of these global variables...
   they might change if swapping is triggered */
unsigned char  FPU_rm;
char	       FPU_st0_tag;
FPU_REG       *FPU_st0_ptr;

#ifdef PARANOID
char emulating=0;
#endif PARANOID

#define bswapw(x) __asm__("xchgb %%al,%%ah":"=a" (x):"0" ((short)x))


void math_emulate(long arg)
{
  unsigned char  FPU_modrm;
  unsigned short code;

#ifdef PARANOID
  if ( emulating )
    {
      printk("ERROR: wm-FPU-emu is not RE-ENTRANT!\n");
    }
  RE_ENTRANT_CHECK_ON
#endif PARANOID

  if (!current->used_math)
    {
      finit();
      current->used_math = 1;
      control_word = 0x037f;
      status_word = 0x0000;
    }

  FPU_info = (struct info *) &arg;

  /* We cannot handle emulation in v86-mode */
  if (FPU_EFLAGS & 0x00020000)
    {
      FPU_ORIG_EIP = FPU_EIP;
      math_abort(FPU_info,SIGILL);
    }

  /* 0x000f means user code space */
  if (FPU_CS != 0x000f)
    {
      printk("math_emulate: %04x:%08x\n",FPU_CS,FPU_EIP);
      panic("Math emulation needed in kernel");
    }

  FPU_lookahead = 1;
  if (current->flags & PF_PTRACED)
  	FPU_lookahead = 0;
do_another:

  FPU_entry_eip = FPU_ORIG_EIP = FPU_EIP;

  RE_ENTRANT_CHECK_OFF
  code = get_fs_word((unsigned short *) FPU_EIP);
  RE_ENTRANT_CHECK_ON

  if ( (code & 0xff) == 0x66 )
    {
      FPU_EIP++;
      RE_ENTRANT_CHECK_OFF
      code = get_fs_word((unsigned short *) FPU_EIP);
      RE_ENTRANT_CHECK_ON
    }
  FPU_EIP += 2;

  FPU_modrm = code >> 8;
  FPU_rm = FPU_modrm & 7;

  if ( FPU_modrm < 0300 )
    {
      /* All of these instructions use the mod/rm byte to get a data address */
      get_address(FPU_modrm);
      if ( !(code & 1) )
	{
	  switch ( (code >> 1) & 3 )
	    {
	    case 0:
	      reg_load_single();
	      break;
	    case 1:
	      reg_load_int32();
	      break;
	    case 2:
	      reg_load_double();
	      break;
	    case 3:
	      reg_load_int16();
	      break;
	    }

	  /* No more access to user memory, it is safe
	     to use static data now */
	  FPU_st0_ptr = &st(0);
	  FPU_st0_tag = FPU_st0_ptr->tag;
	  if ( NOT_EMPTY_0 )
	    {
	      switch ( (FPU_modrm >> 3) & 7 )
		{
		case 0:         /* fadd */
		  reg_add(FPU_st0_ptr, &FPU_loaded_data, FPU_st0_ptr,
			  control_word);
		  break;
		case 1:         /* fmul */
		  reg_mul(FPU_st0_ptr, &FPU_loaded_data, FPU_st0_ptr,
			  control_word);
		  break;
		case 2:         /* fcom */
		  compare_st_data();
		  break;
		case 3:         /* fcomp */
		  compare_st_data();
		  pop();
		  break;
		case 4:         /* fsub */
		  reg_sub(FPU_st0_ptr, &FPU_loaded_data, FPU_st0_ptr,
			  control_word);
		  break;
		case 5:         /* fsubr */
		  reg_sub(&FPU_loaded_data, FPU_st0_ptr, FPU_st0_ptr,
			  control_word);
		  break;
		case 6:         /* fdiv */
		  reg_div(FPU_st0_ptr, &FPU_loaded_data, FPU_st0_ptr,
			  control_word);
		  break;
		case 7:         /* fdivr */
		  reg_div(&FPU_loaded_data, FPU_st0_ptr, FPU_st0_ptr,
			  control_word);
		  break;
		}
	    }
	  else
	    stack_underflow();
	}
      else
	{
	  load_store_instr(((FPU_modrm & 0x38) | (code & 6)) >> 1);
	}

      data_operand_offset = (unsigned long)FPU_data_address;
    }
  else
    {
      /* None of these instructions access user memory */
      unsigned char instr_index = (FPU_modrm & 0x38) | (code & 7);
      FPU_st0_ptr = &st(0);
      FPU_st0_tag = FPU_st0_ptr->tag;
      switch ( type_table[(int) instr_index] )
	{
	case _NONE_:
	  break;
	case _REG0_:
	  if ( !NOT_EMPTY_0 )
	    {
	      stack_underflow();
	      goto instruction_done;
	    }
	  break;
	case _REGI_:
	  if ( !NOT_EMPTY_0 || !NOT_EMPTY(FPU_rm) )
	    {
	      stack_underflow();
	      goto instruction_done;
	    }
	  break;
	case _PUSH_:     /* Only used by the fld st(i) instruction */
	  break;
	case _null_:
	  Un_impl();
	  goto instruction_done;
	default:
	  EXCEPTION(EX_INTERNAL|0x111);
	  goto instruction_done;
	}
      (*st_instr_table[(int) instr_index])();
    }

instruction_done:

  ip_offset = FPU_entry_eip;
  bswapw(code);
  *(1 + (unsigned short *)&cs_selector) = code & 0x7ff;

  if (FPU_lookahead && !need_resched)
    {
      unsigned char next;
skip_fwait:
      RE_ENTRANT_CHECK_OFF
      next = get_fs_byte((unsigned char *) FPU_EIP);
      RE_ENTRANT_CHECK_ON
test_for_fp:
      if ( (next & 0xf8) == 0xd8 )
	{
	  goto do_another;
	}
      if ( next == 0x9b )  /* fwait */
	{ FPU_EIP++; goto skip_fwait; }
      if ( next == 0x66 )  /* size prefix */
	{
	  RE_ENTRANT_CHECK_OFF
	  next = get_fs_byte((unsigned char *) (FPU_EIP+1));
	  RE_ENTRANT_CHECK_ON
	  if ( (next & 0xf8) == 0xd8 )
	    goto test_for_fp;
	}
    }

  RE_ENTRANT_CHECK_OFF
}


void __math_abort(struct info * info, unsigned int signal)
{
	FPU_EIP = FPU_ORIG_EIP;
	send_sig(signal,current,1);
	RE_ENTRANT_CHECK_OFF
	__asm__("movl %0,%%esp ; ret"::"g" (((long) info)-4));
#ifdef PARANOID
      printk("ERROR: wm-FPU-emu math_abort failed!\n");
#endif PARANOID
}

#else /* no math emulation */

#include <linux/signal.h>
#include <linux/sched.h>

void math_emulate(long arg)
{
  printk("math-meulation not enabled and no coprocessor found.\n");
  printk("killing %s.\n",current->comm);
  send_sig(SIGFPE,current,1);
  schedule();
}

#endif /* CONFIG_MATH_EMULATION */
