	.file	"reg_div.S"
/*---------------------------------------------------------------------------+
 |  reg_div.S                                                                |
 |                                                                           |
 | Divide one FPU_REG by another and put the result in a destination FPU_REG.|
 |                                                                           |
 | Copyright (C) 1992,1993                                                   |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail apm233m@vaxc.cc.monash.edu.au    |
 |                                                                           |
 | Call from C as:                                                           |
 |   void reg_div(FPU_REG *a, FPU_REG *b, FPU_REG *dest,                     |
 |                                    unsigned int control_word)             |
 |                                                                           |
 +---------------------------------------------------------------------------*/

#include "exception.h"
#include "fpu_asm.h"

.text
	.align 2

.globl	_reg_div
_reg_div:
	pushl	%ebp
	movl	%esp,%ebp

	pushl	%esi
	pushl	%edi
	pushl	%ebx

	movl	PARAM1,%esi
	movl	PARAM2,%ebx
	movl	PARAM3,%edi

	movb	TAG(%esi),%al
	orb	TAG(%ebx),%al

	jne	L_div_special		// Not (both numbers TW_Valid)


// Both arguments are TW_Valid
	movl	EXP(%esi),%edx
	movl	EXP(%ebx),%eax
	subl	%eax,%edx
	addl	EXP_BIAS,%edx
	movl	%edx,EXP(%edi)

	movb	TW_Valid,TAG(%edi)

	movb	SIGN(%esi),%cl
	cmpb	%cl,SIGN(%ebx)
	setne	(%edi)	      // Set the sign, requires SIGN_NEG=1, SIGN_POS=0

//	add	$SIGL_OFFSET,%ebx
//	add	$SIGL_OFFSET,%esi

	jmp	_divide_kernel


/*-----------------------------------------------------------------------*/
L_div_special:
	cmpb	TW_NaN,TAG(%esi)	// A NaN with anything to give NaN
	je	L_arg1_NaN

	cmpb	TW_NaN,TAG(%ebx)	// A NaN with anything to give NaN
	jne	L_no_NaN_arg

// Operations on NaNs
L_arg1_NaN:
L_arg2_NaN:
	pushl	%edi
	pushl	%ebx
	pushl	%esi
	call	_real_2op_NaN
	jmp	LDiv_exit

// Invalid operations
L_zero_zero:
L_inf_inf:
	pushl	%esi
	call	_arith_invalid
	jmp	LDiv_exit

L_no_NaN_arg:
	cmpb	TW_Infinity,TAG(%esi)
	jne	L_arg1_not_inf

	cmpb	TW_Infinity,TAG(%ebx)
	je	L_inf_inf		// invalid operation

	// Note that p16-9 says that infinity/0 returns infinity
	jmp	L_copy_arg1		// Answer is Inf

L_arg1_not_inf:
	cmpb	TW_Zero,TAG(%ebx)	// Priority to div-by-zero error
	jne	L_arg2_not_zero

	cmpb	TW_Zero,TAG(%esi)
	je	L_zero_zero		// invalid operation

// Division by zero error
	pushl	%esi
	movb	SIGN(%esi),%al
	xorb	SIGN(%ebx),%al
	pushl	%eax			// lower 8 bits have the sign
	call	_divide_by_zero
	jmp	LDiv_exit

L_arg2_not_zero:
	cmpb	TW_Infinity,TAG(%ebx)
	jne	L_arg2_not_inf

	jmp	L_return_zero		// Answer is zero

L_arg2_not_inf:
	cmpb	TW_Zero,TAG(%esi)
	jne	L_unknown_tags

L_copy_arg1:
	movb	TAG(%esi),%ax
	movb	%ax,TAG(%edi)
	movl	EXP(%esi),%eax
	movl	%eax,EXP(%edi)
	movl	SIGL(%esi),%eax
	movl	%eax,SIGL(%edi)
	movl	SIGH(%esi),%eax
	movl	%eax,SIGH(%edi)

	movb	SIGN(%esi),%cl
	cmpb	%cl,SIGN(%ebx)
	jne	LDiv_negative_result

	movb	SIGN_POS,SIGN(%edi)
	jmp	LDiv_exit

LDiv_set_result_sign:
	movb	SIGN(%esi),%cl
	cmpb	%cl,SIGN(%edi)
	jne	LDiv_negative_result

	movb	SIGN_POS,SIGN(%ebx)
	jmp	LDiv_exit

	.align 2,0x90
LDiv_negative_result:
	movb	SIGN_NEG,SIGN(%edi)

LDiv_exit:
	leal	-12(%ebp),%esp

	popl	%ebx
	popl	%edi
	popl	%esi
	leave
	ret


L_return_zero:
	movb	TW_Zero,TAG(%edi)
	jmp	LDiv_set_result_sign

L_unknown_tags:
	push	EX_INTERNAL | 0x208
	call	EXCEPTION

	// Generate a NaN for unknown tags
	movl	_CONST_QNaN,%eax
	movl	%eax,(%edi)
	movl	_CONST_QNaN+4,%eax
	movl	%eax,SIGL(%edi)
	movl	_CONST_QNaN+8,%eax
	movl	%eax,SIGH(%edi)
	jmp	LDiv_exit
