#include "asm.h"

/*
 *	_do_mask(dst, mask, func)
 *
 *	char *dst;
 *	int mask;
 *	int func;
 *
 * This function takes a byte at address 'dst', and changes it according
 * to the function specified by 'func'.
 * These functions are documented in the file 'bitmap.OPS'.
 * Only those bits which are set in 'mask' are affected.
 *
 * The resulting byte is returned in ax.
 *
 */

#define dst	1
#define mask	2
#define func	3

.text
bm_clr:				# DST & ~DST
	notb	%al
	andb	(%edi),%al
	jmp	isdone

bm_inv:				# ~DST
	xorb	(%edi),%al
	jmp	isdone

bm_set:				# DST | ~DST
	orb	(%edi),%al
	jmp	isdone

table:
	.long	bm_clr, bm_clr, bm_clr, bm_clr, bm_inv, bm_inv, bm_inv, bm_inv
	.long	isdone,  isdone,  isdone,  isdone,  bm_set, bm_set, bm_set, bm_set

.text
__do_mask:
.globl __do_mask
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%ebx

	mov	ARG(dst),%edi
	mov	ARG(mask),%eax
	or	%eax,%eax
	jz	isdone
	mov	ARG(func),%ebx

	and	$15,%ebx
	shl	$ LBPW,%ebx
	add	$table,%ebx
	cs
	mov	(%ebx),%ebx
	jmp	%ebx

isdone:
	movb	%al,(%edi)
	pop	%ebx
	pop	%edi
	pop	%ebp
	ret
