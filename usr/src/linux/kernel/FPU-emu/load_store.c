/*---------------------------------------------------------------------------+
 |  load_store.c                                                             |
 |                                                                           |
 | This file contains most of the code to interpret the FPU instructions     |
 | which load and store from user memory.                                    |
 |                                                                           |
 | Copyright (C) 1992,1993                                                   |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail apm233m@vaxc.cc.monash.edu.au    |
 |                                                                           |
 |                                                                           |
 +---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------+
 | Note:                                                                     |
 |    The file contains code which accesses user memory.                     |
 |    Emulator static data may change when user memory is accessed, due to   |
 |    other processes using the emulator while swapping is in progress.      |
 +---------------------------------------------------------------------------*/

#include <asm/segment.h>

#include "fpu_system.h"
#include "exception.h"
#include "fpu_emu.h"
#include "status_w.h"


#define _NONE_ 0   /* FPU_st0_ptr etc not needed */
#define _REG0_ 1   /* Will be storing st(0) */
#define _PUSH_ 3   /* Need to check for space to push onto stack */
#define _null_ 4   /* Function illegal or not implemented */

#define pop_0()	{ pop_ptr->tag = TW_Empty; top++; }


static unsigned char type_table[32] = {
  _PUSH_, _PUSH_, _PUSH_, _PUSH_,
  _null_, _null_, _null_, _null_,
  _REG0_, _REG0_, _REG0_, _REG0_,
  _REG0_, _REG0_, _REG0_, _REG0_,
  _NONE_, _null_, _NONE_, _PUSH_,
  _NONE_, _PUSH_, _null_, _PUSH_,
  _NONE_, _null_, _NONE_, _REG0_,
  _NONE_, _REG0_, _NONE_, _REG0_
  };

void load_store_instr(char type)
{
  FPU_REG *pop_ptr;  /* We need a version of FPU_st0_ptr which won't change. */

  pop_ptr = NULL;    /* Initialized just to stop compiler warnings. */
  switch ( type_table[(int) (unsigned) type] )
    {
    case _NONE_:
      break;
    case _REG0_:
      pop_ptr = &st(0);       /* Some of these instructions pop after
				 storing */

      FPU_st0_ptr = pop_ptr;      /* Set the global variables. */
      FPU_st0_tag = FPU_st0_ptr->tag;
      break;
    case _PUSH_:
      {
	pop_ptr = &st(-1);
	if ( pop_ptr->tag != TW_Empty )
	  { stack_overflow(); return; }
	top--;
      }
      break;
    case _null_:
      return Un_impl();
#ifdef PARANOID
    default:
      return EXCEPTION(EX_INTERNAL);
#endif PARANOID
    }

switch ( type )
  {
  case 000:       /* fld m32real */
    reg_load_single();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 001:      /* fild m32int */
    reg_load_int32();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 002:      /* fld m64real */
    reg_load_double();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 003:      /* fild m16int */
    reg_load_int16();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 010:      /* fst m32real */
    reg_store_single();
    break;
  case 011:      /* fist m32int */
    reg_store_int32();
    break;
  case 012:     /* fst m64real */
    reg_store_double();
    break;
  case 013:     /* fist m16int */
    reg_store_int16();
    break;
  case 014:     /* fstp m32real */
    if ( reg_store_single() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  case 015:     /* fistp m32int */
    if ( reg_store_int32() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  case 016:     /* fstp m64real */
    if ( reg_store_double() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  case 017:     /* fistp m16int */
    if ( reg_store_int16() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  case 020:     /* fldenv  m14/28byte */
    fldenv();
    break;
  case 022:     /* frstor m94/108byte */
    frstor();
    break;
  case 023:     /* fbld m80dec */
    reg_load_bcd();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 024:     /* fldcw */
    RE_ENTRANT_CHECK_OFF
    control_word = get_fs_word((unsigned short *) FPU_data_address);
    RE_ENTRANT_CHECK_ON
#ifdef NO_UNDERFLOW_TRAP
    if ( !(control_word & EX_Underflow) )
      {
	control_word |= EX_Underflow;
      }
#endif
    FPU_data_address = (void *)data_operand_offset; /* We want no net effect */
    FPU_entry_eip = ip_offset;               /* We want no net effect */
    break;
  case 025:      /* fld m80real */
    reg_load_extended();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 027:      /* fild m64int */
    reg_load_int64();
    reg_move(&FPU_loaded_data, pop_ptr);
    break;
  case 030:     /* fstenv  m14/28byte */
    fstenv();
    FPU_data_address = (void *)data_operand_offset; /* We want no net effect */
    FPU_entry_eip = ip_offset;               /* We want no net effect */
    break;
  case 032:      /* fsave */
    fsave();
    FPU_data_address = (void *)data_operand_offset; /* We want no net effect */
    FPU_entry_eip = ip_offset;               /* We want no net effect */
    break;
  case 033:      /* fbstp m80dec */
    if ( reg_store_bcd() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  case 034:      /* fstcw m16int */
    RE_ENTRANT_CHECK_OFF
    verify_area(VERIFY_WRITE,FPU_data_address,2);
    put_fs_word(control_word, (short *) FPU_data_address);
    RE_ENTRANT_CHECK_ON
    FPU_data_address = (void *)data_operand_offset; /* We want no net effect */
    FPU_entry_eip = ip_offset;               /* We want no net effect */
    break;
  case 035:      /* fstp m80real */
    if ( reg_store_extended() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  case 036:      /* fstsw m2byte */
    status_word &= ~SW_TOP;
    status_word |= (top&7) << SW_TOPS;
    RE_ENTRANT_CHECK_OFF
    verify_area(VERIFY_WRITE,FPU_data_address,2);
    put_fs_word(status_word,(short *) FPU_data_address);
    RE_ENTRANT_CHECK_ON
    FPU_data_address = (void *)data_operand_offset; /* We want no net effect */
    FPU_entry_eip = ip_offset;               /* We want no net effect */
    break;
  case 037:      /* fistp m64int */
    if ( reg_store_int64() )
      pop_0();  /* pop only if the number was actually stored
		 (see the 80486 manual p16-28) */
    break;
  }
}
