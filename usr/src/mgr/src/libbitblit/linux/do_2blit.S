#include "asm.h"

/*
 *	_do_2blit(dst, src, cnt, func, shift)
 *
 *	char *dst;
 *	char *src;
 *	int cnt;
 *	int func;
 *	int shift;
 *
 * This function takes a word at address 'src', swaps the bytes in it,
 * shifts them 'shift' positions right, and combines the least significant
 * byte with the byte at 'dst' by a function specified by 'func'.
 * These functions are documented in the file 'bitmap.OPS'.
 * After that 'dst' and 'src' are both incremented or decremented as specified
 * by the 80x86 direction flag DF, which is assumed to be set correctly.
 * These actions are repeated 'cnt' times.
 *
 * Byte swapping is necessary for the IBM PC byte order.
 * This is accomplished by adding 8 to 'shift' and using an 'rorw' instruction
 * instead of a 'shr' instruction.
 */

#define dst	1
#define src	2
#define cnt	3
#define func	4
#define shift	5

.text
bm_0:				/* set to 0 */
	xorb	%al,%al
	stosb
	dec	%edx
	jnz	bm_0
	jmp	isdone

bm_1:				/* ~SRC & ~DST == ~(SRC | DST) */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	orb	%ah,%al
	notb	%al
	stosb
	dec	%edx
	jnz	bm_1
	jmp	isdone

bm_2:				/* ~SRC & DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	notb	%al
	es
	movb	(%edi),%ah
	andb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_2
	jmp	isdone

bm_3:				/* ~SRC */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	notb	%al
	stosb
	dec	%edx
	jnz	bm_3
	jmp	isdone

bm_4:				/* SRC & ~DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	notb	%ah
	andb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_4
	jmp	isdone

bm_5:				/* ~DST */
	es
	movb	(%edi),%al
	notb	%al
	stosb
	dec	%edx
	jnz	bm_5
	jmp	isdone

bm_6:				/* SRC ^ DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	xorb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_6
	jmp	isdone

bm_7:				/* ~(SRC & DST) */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	andb	%ah,%al
	notb	%al
	stosb			/* DCM */
	dec	%edx
	jnz	bm_7
	jmp	isdone

bm_8:				/* SRC & DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	andb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_8
	jmp	isdone

bm_9:				/* ~SRC ^ DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	notb	%al
	es
	movb	(%edi),%ah
	xorb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_9
	jmp	isdone

bm_B:				/* ~SRC | DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	notb	%al
	es
	movb	(%edi),%ah
	orb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_B
	jmp	isdone

bm_C:				/* SRC */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	stosb
	dec	%edx
	jnz	bm_C
	jmp	isdone

bm_D:				/* SRC | ~DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	notb	%ah
	orb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_D
	jmp	isdone

bm_E:				/* SRC | DST */
	movb	1(%esi),%ah
	lodsb
	rorw	%cl,%ax
	es
	movb	(%edi),%ah
	orb	%ah,%al
	stosb
	dec	%edx
	jnz	bm_E
	jmp	isdone

bm_F:				/* set to 1 */
	movb	$0xFF,%al
	stosb
	dec	%edx
	jnz	bm_F
	jmp	isdone

table:
	.int	bm_0, bm_1, bm_2, bm_3, bm_4, bm_5, bm_6, bm_7
	.int	bm_8, bm_9, isdone, bm_B, bm_C, bm_D, bm_E, bm_F

__do_2blit:
.globl __do_2blit
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%esi
	push	%ebx
	push	%ecx
	push	%edx

	mov	ARG(dst),%edi
	mov	ARG(src),%esi
	mov	ARG(shift),%ecx
	add	$8,%ecx

	mov	ARG(cnt),%edx
	or	%edx,%edx
	jz	isdone

	mov	ARG(func),%ebx
	and	$15,%ebx
	shl	$ LBPW,%ebx
	add	$table,%ebx
	cs
	mov	(%ebx),%ebx
	jmp	%ebx

isdone:
	pop	%edx
	pop	%ecx
	pop	%ebx
	pop	%esi
	pop	%edi
	pop	%ebp
	ret
