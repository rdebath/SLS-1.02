#include "asm.h"

/*
 * misc. assembler stuff for screen access support
 */

.text

__cld:
.globl __cld
	cld
	ret

__std:
.globl __std
	std
	ret
