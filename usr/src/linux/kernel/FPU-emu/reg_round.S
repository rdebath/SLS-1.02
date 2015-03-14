	.file "reg_round.S"
/*---------------------------------------------------------------------------+
 |  reg_round.S                                                              |
 |                                                                           |
 | Rounding/truncation/etc for FPU basic arithmetic functions.               |
 |                                                                           |
 | Copyright (C) 1993                                                        |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail apm233m@vaxc.cc.monash.edu.au    |
 |                                                                           |
 | Not callable from C.                                                      |
 | Must be entered by a jmp intruction.                                      |
 |                                                                           |
 +---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------+
 | Two entry points.                                                         |
 |                                                                           |
 | Needed by both entry points:                                              |
 |  %eax:%ebx  64 bit significand                                            |
 |  %edx       32 bit extension of the significand                           |
 |  %edi       pointer to an FPU_REG for the result to be stored             |
 |  stack      calling function must have set up a C stack frame and         |
 |             pushed %esi, %edi, and %ebx                                   |
 |                                                                           |
 | Needed just for the FPU_round_sqrt entry point:                           |
 |  %cx  A control word in the same format as the FPU control word.          |
 | Otherwise, PARAM4 must give such a value.                                 |
 |                                                                           |
 |                                                                           |
 | The significand and its extension are assumed to be exact in the          |
 | following sense:                                                          |
 |   If the significand by itself is the exact result then the significand   |
 |   extension (%edx) must contain 0, otherwise the significand extension    |
 |   must be non-zero.                                                       |
 |   If the significand extension is non-zero then the significand is        |
 |   smaller than the magnitude of the correct exact result by an amount     |
 |   greater than zero and less than one ls bit of the significand.          |
 |   The significand extension is only required to have three possible       |
 |   non-zero values:                                                        |
 |       less than 0x80000000  <=> the significand is less than 1/2 an ls    |
 |                                 bit smaller than the magnitude of the     |
 |                                 true exact result.                        |
 |         exactly 0x80000000  <=> the significand is exactly 1/2 an ls bit  |
 |                                 smaller than the magnitude of the true    |
 |                                 exact result.                             |
 |    greater than 0x80000000  <=> the significand is more than 1/2 an ls    |
 |                                 bit smaller than the magnitude of the     |
 |                                 true exact result.                        |
 |                                                                           |
 +---------------------------------------------------------------------------*/

#include "fpu_asm.h"
#include "fpu_emu.h"
#include "exception.h"
#include "control_w.h"


.text
	.align 2,144
.globl FPU_round
.globl FPU_round_sqrt

FPU_round:
	/* Round the result */
	movl	PARAM4,%ecx

FPU_round_sqrt:		// entry point from wm_sqrt.S
	movl	%ecx,%esi
	andl	CW_PC,%ecx
	cmpl	PR_64_BITS,%ecx
	je	LRound_To_64

	cmpl	PR_53_BITS,%ecx
	je	LRound_To_53

	cmpl	PR_24_BITS,%ecx
	je	LRound_To_24

#ifdef PARANOID
	jmp	L_bugged	// There is no bug, just a bad control word
#endif PARANOID


LRound_To_24:
	movl	%esi,%ecx
	andl	CW_RC,%ecx
	cmpl	RC_RND,%ecx
	je	LRound_nearest_24

	cmpl	RC_CHOP,%ecx
	je	LTruncate_24

	cmpl	RC_UP,%ecx		// Towards +infinity
	je	LUp_24

	cmpl	RC_DOWN,%ecx		// Towards -infinity
	je	LDown_24

#ifdef PARANOID
	jmp	L_bugged
#endif PARANOID

LUp_24:
	cmpb	SIGN_POS,SIGN(%edi)
	je	LUp_24_pos

	movl	%eax,%ecx
	andl	$0x000000ff,%ecx
	orl	%ebx,%ecx
	orl	%edx,%ecx
	jnz	LTruncate_24
	jmp	L_store

LUp_24_pos:
	movl	%eax,%ecx
	andl	$0x000000ff,%ecx
	orl	%ebx,%ecx
	orl	%edx,%ecx
	jnz	LDo_24_round_up
	jmp	L_store

LDown_24:
	cmpb	SIGN_POS,SIGN(%edi)
	je	LDown_24_pos

	movl	%eax,%ecx
	andl	$0x000000ff,%ecx
	orl	%ebx,%ecx
	orl	%edx,%ecx
	jnz	LDo_24_round_up
	jmp	L_store

LDown_24_pos:
	movl	%eax,%ecx
	andl	$0x000000ff,%ecx
	orl	%ebx,%ecx
	orl	%edx,%ecx
	jnz	LTruncate_24
	jmp	L_store

LRound_nearest_24:
	// Do rounding of the 24th bit if needed (nearest or even)
	movl	%eax,%ecx
	andl	$0x000000ff,%ecx
	cmpl	$0x00000080,%ecx
	jc	LTruncate_24		// less than half, no increment needed

	jne	LGreater_Half_24	// greater than half, increment needed

	// Possibly half, we need to check the ls bits
	orl	%ebx,%ebx
	jne	LGreater_Half_24	// greater than half, increment needed

	orl	%edx,%edx
	jne	LGreater_Half_24	// greater than half, increment needed

	// Exactly half, increment only if 24th bit is 1 (round to even)
	testl	$0x00000100,%eax
	jz	LTruncate_24

LGreater_Half_24:			// Rounding: increment at the 24th bit
LDo_24_round_up:
	andl	$0xffffff00,%eax	// Truncate to 24 bits
	xorl	%ebx,%ebx
	addl	$0x00000100,%eax
	jmp	LCheck_Round_Overflow

LTruncate_24:
	andl	$0xffffff00,%eax	// Truncate to 24 bits
	xorl	%ebx,%ebx
	jmp	L_store


LRound_To_53:
	movl	%esi,%ecx
	andl	CW_RC,%ecx
	cmpl	RC_RND,%ecx
	je	LRound_nearest_53

	cmpl	RC_CHOP,%ecx
	je	LTruncate_53

	cmpl	RC_UP,%ecx		// Towards +infinity
	je	LUp_53

	cmpl	RC_DOWN,%ecx		// Towards -infinity
	je	LDown_53

#ifdef PARANOID
	jmp	L_bugged
#endif PARANOID

LUp_53:
	cmpb	SIGN_POS,SIGN(%edi)
	je	LUp_53_pos

	movl	%ebx,%ecx
	andl	$0x000007ff,%ecx
	orl	%edx,%ecx
	jnz	LTruncate_53
	jmp	L_store

LUp_53_pos:
	movl	%ebx,%ecx
	andl	$0x000007ff,%ecx
	orl	%edx,%ecx
	jnz	LDo_53_round_up
	jmp	L_store

LDown_53:
	cmpb	SIGN_POS,SIGN(%edi)
	je	LDown_53_pos

	movl	%ebx,%ecx
	andl	$0x000007ff,%ecx
	orl	%edx,%ecx
	jnz	LDo_53_round_up
	jmp	L_store

LDown_53_pos:
	movl	%ebx,%ecx
	andl	$0x000007ff,%ecx
	orl	%edx,%ecx
	jnz	LTruncate_53
	jmp	L_store

LRound_nearest_53:
	// Do rounding of the 53rd bit if needed (nearest or even)
	movl	%ebx,%ecx
	andl	$0x000007ff,%ecx
	cmpl	$0x00000400,%ecx
	jc	LTruncate_53		// less than half, no increment needed

	jne	LGreater_Half_53	// greater than half, increment needed

	// Possibly half, we need to check the ls bits
	orl	%edx,%edx
	jne	LGreater_Half_53	// greater than half, increment needed

	// Exactly half, increment only if 53rd bit is 1 (round to even)
	testl	$0x00000800,%ebx
	jz	LTruncate_53

LGreater_Half_53:			// Rounding: increment at the 53rd bit
LDo_53_round_up:
	andl	$0xfffff800,%ebx	// Truncate to 53 bits
	addl	$0x00000800,%ebx
	adcl	$0,%eax
	jmp	LCheck_Round_Overflow

LTruncate_53:
	andl	$0xfffff800,%ebx	// Truncate to 53 bits
	jmp	L_store


LRound_To_64:
	movl	%esi,%ecx
	andl	CW_RC,%ecx
	cmpl	RC_RND,%ecx
	je	LRound_nearest_64

	cmpl	RC_CHOP,%ecx
	je	LTruncate_64

	cmpl	RC_UP,%ecx		// Towards +infinity
	je	LUp_64

	cmpl	RC_DOWN,%ecx		// Towards -infinity
	je	LDown_64

#ifdef PARANOID
	jmp	L_bugged
#endif PARANOID

LUp_64:
	cmpb	SIGN_POS,SIGN(%edi)
	je	LUp_64_pos

	orl	%edx,%edx
	jnz	LTruncate_64
	jmp	L_store

LUp_64_pos:
	orl	%edx,%edx
	jnz	LDo_64_round_up
	jmp	L_store

LDown_64:
	cmpb	SIGN_POS,SIGN(%edi)
	je	LDown_64_pos

	orl	%edx,%edx
	jnz	LDo_64_round_up
	jmp	L_store

LDown_64_pos:
	orl	%edx,%edx
	jnz	LTruncate_64
	jmp	L_store

LRound_nearest_64:
	cmpl	$0x80000000,%edx
	jc	LTruncate_64

	jne	LDo_64_round_up

	/* Now test for round-to-even */
	testb	$1,%ebx
	jz	LTruncate_64

LDo_64_round_up:
	addl	$1,%ebx
	adcl	$0,%eax

LCheck_Round_Overflow:
	jnc	L_store		/* Rounding done, no overflow */

	/* Overflow, adjust the result (to 1.0) */
	rcrl	$1,%eax
	rcrl	$1,%ebx
	incl	EXP(%edi)

LTruncate_64:
L_store:
	/* store the result */
	movl	%eax,SIGH(%edi)
	movl	%ebx,SIGL(%edi)

	movb	TW_Valid,TAG(%edi)		/* Set the tags to TW_Valid */

	// The number may have overflowed
	cmpl	EXP_OVER,EXP(%edi)
	jge	L_overflow

	cmpl	EXP_UNDER,EXP(%edi)
	jle	L_underflow

L_exit:
	popl	%ebx
	popl	%edi
	popl	%esi
	leave
	ret


/* The operations resulted in a number too large to represent */
L_overflow:
	push	%edi
	call	_arith_overflow
	pop	%edi
	jmp	L_exit

/* The operations resulted in a number too small to represent */
L_underflow:
	pushl	%edi
	call	_arith_underflow
	popl	%edi
	jmp	L_exit


#ifdef PARANOID
/* If we ever get here then we have problems! */
L_bugged:
	pushl	EX_INTERNAL|0x201
	call	EXCEPTION
	pop	%ebx
	jmp	L_exit
#endif PARANOID
