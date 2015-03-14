/ Modified for Coherent 4.0 by Harry C. Pulley, IV

.include asm.h

/*
/*	_do_blit(dst, hcnt, func)
/*
/*	char *dst;
/*	int hcnt;
/*	int func;
/*
/* This function takes a byte at address 'dst', and changes it according
/* to the function specified by 'func'.
/* These functions are documented in the file 'bitmap.OPS'.
/* After that 'dst' is incremented or decremented as specified
/* by the 80x86 direction flag DF, which is assumed to be set correctly.
/* These actions are repeated 'hcnt' times.
/*
/*

dst	.define	8
hcnt	.define	12
func	.define	16

.text
bm_clr:				/ DST & ~DST
	xorb	%al,%al
	repne
	stosb
	jmp	isdone

bm_inv:				/ ~DST
	movb	$0xFF,%al
.loop:
				/ es
	xorb	%al,%es:(%edi)
	inc	%edi
	loop	.loop
	jmp	isdone

bm_set:				/ DST | ~DST
	movb	$0xFF,%al
	repne
	stosb
	jmp	isdone

table:
	.long	bm_clr, bm_clr, bm_clr, bm_clr, bm_inv, bm_inv, bm_inv, bm_inv
	.long	isdone,  isdone,  isdone,  isdone,  bm_set, bm_set, bm_set, bm_set

_do_blit:
.globl _do_blit
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%ebx
	push	%ecx

	mov	dst(%ebp),%edi
	mov	hcnt(%ebp),%ecx
	or	%ecx,%ecx
	jz	isdone
	mov	func(%ebp),%ebx

	and	$15,%ebx
	shl	$ LBPW,%ebx
	add	$table,%ebx
				/ cs
	mov	%cs:(%ebx),%ebx
	ijmp	%ebx

isdone:
	pop	%ecx
	pop	%ebx
	pop	%edi
	pop	%ebp
	ret
