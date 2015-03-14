/ Modified for Coherent 4.0 by Harry C. Pulley, IV

.include asm.h

/*
/*	_do_2mask(dst, src, mask, func, shift)
/*
/*	char *dst;
/*	char *src;
/*	int mask;
/*	int func;
/*	int shift;
/*
/* This function takes a word at address 'src', swaps the bytes in it,
/* shifts it 'shift' positions right, and combines the least significant
/* byte with the byte at 'dst' by a function specified by 'func'.
/* These functions are documented in the file 'bitmap.OPS'.
/* Only those bytes which are set in 'mask' are affected.
/*
/*

dst	.define	8
src	.define	12
mask	.define	16
func	.define	20
shift	.define	24

.text
bm_2:				/* ~SRC & DST */
	notb	%al
	andb	%dl,%al
	orb	%dh,%al
				/ es
	andb	%al,%es:(%edi)
	jmp	isdone

bm_3:				/* ~SRC */
	notb	%al
	andb	%dl,%al
				/ es
	andb	%dh,%es:(%edi)
				/ es
	orb	%al,%es:(%edi)
	jmp	isdone

bm_6:				/* SRC ^ DST */
	andb	%dl,%al
				/ es
	xorb	%al,%es:(%edi)
	jmp	isdone

bm_8:				/* SRC & DST */
	andb	%dl,%al
	orb	%dh,%al
				/ es
	andb	%al,%es:(%edi)
	jmp	isdone

bm_C:				/* SRC */
	andb	%dl,%al
				/ es
	andb	%dh,%es:(%edi)
				/ es
	orb	%al,%es:(%edi)
	jmp	isdone

bm_E:				/* SRC | DST */
	andb	%dl,%al
				/ es
	orb	%al,%es:(%edi)
	jmp	isdone

table:
	.long	isdone, isdone, bm_2, bm_3, isdone, isdone, bm_6, isdone
	.long	bm_8, isdone, isdone, isdone, bm_C, isdone, bm_E, isdone

_do_2mask:
.globl _do_2mask
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%esi
	push	%ebx
	push	%ecx
	push	%edx

	mov	dst(%ebp),%edi
	mov	src(%ebp),%esi


	mov	mask(%ebp),%edx
	or	%edx,%edx
	jz	isdone
	movb	%dl,%dh			/* dl is (8 bit) mask */
	notb	%dh			/* dh is inverted mask */
	mov	shift(%ebp),%ecx
	movb	(%esi),%ah
	movb	1(%esi),%al
	rorw	%cl,%ax

	mov	func(%ebp),%ebx
	and	$15,%ebx
	shl	$ LBPW,%ebx
	add	$table,%ebx
					/ cs
	mov	%cs:(%ebx),%ebx
	ijmp	%ebx

isdone:
	pop	%edx
	pop	%ecx
	pop	%ebx
	pop	%esi
	pop	%edi
	pop	%ebp
	ret
