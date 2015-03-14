/ Modified for Coherent 4.0 by Harry C. Pulley, IV

.include asm.h

/*
/*	_do_mask(dst, mask, func)
/*
/*	char *dst;
/*	int mask;
/*	int func;
/*
/* This function takes a byte at address 'dst', and changes it according
/* to the function specified by 'func'.
/* These functions are documented in the file 'bitmap.OPS'.
/* Only those bits which are set in 'mask' are affected.
/*
/* The resulting byte is returned in ax.
/*
/*

dst	.define	8
mask	.define	12
func	.define	16

.text
bm_clr:				/ DST & ~DST
	notb	%al
	andb	(%edi),%al
	jmp	isdone

bm_inv:				/ ~DST
	xorb	(%edi),%al
	jmp	isdone

bm_set:				/ DST | ~DST
	orb	(%edi),%al
	jmp	isdone

table:
	.long	bm_clr, bm_clr, bm_clr, bm_clr, bm_inv, bm_inv, bm_inv, bm_inv
	.long	isdone,  isdone,  isdone,  isdone,  bm_set, bm_set, bm_set, bm_set

.text
_do_mask:
.globl _do_mask
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%ebx

	mov	dst(%ebp),%edi
	mov	mask(%ebp),%eax
	or	%eax,%eax
	jz	isdone
	mov	func(%ebp),%ebx

	and	$15,%ebx
	shl	$ LBPW,%ebx
	add	$table,%ebx
					/ cs
	mov	%cs:(%ebx),%ebx
	ijmp	%ebx

isdone:
	movb	%al,(%edi)
	pop	%ebx
	pop	%edi
	pop	%ebp
	ret
