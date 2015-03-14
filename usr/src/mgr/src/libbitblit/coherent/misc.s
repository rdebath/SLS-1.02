/ Modified for Coherent 4.0 by Harry C. Pulley, IV

.include asm.h

/*
/* misc. assembler stuff for screen access support
/*

.text

_cld:
.globl _cld
	cld
	ret

_std:
.globl _std
	std
	ret
