/* Copyright (C) 1993  Hongjiu Lu
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

	.file	"cosh.S"
        .data
#ifdef  __i486__
	.align  4
#else
	.align  2
#endif
LCW1:
	.word	0
#ifdef  __i486__
	.align  4
#else
	.align  2
#endif
LCW2:
	.word	0

	.text
#ifdef  __i486__
	.align  4,0x90
#else
	.align  2,0x90
#endif
LC2:
	.double 0d5.0000000000000000000000e-01

	.globl	_cosh
#ifdef  __i486__
	.align  4,0x90
#else
	.align  2,0x90
#endif
_cosh:
	fldl	4(%esp)
	fldl2e
	fmulp	%st,%st(1)
	fst	%st(1)
	fstcw	LCW1
	fstcw	LCW2
	fwait
	andw	$0xf3ff,LCW1
	fldcw	LCW1
	frndint
	fldcw	LCW2
	fst	%st(2)
	fsubrp	%st,%st(1)
	f2xm1
	fld1
	faddp	%st,%st(1)
	fscale
	fst	%st(1)

	fld1
	fdivp	%st,%st(1)
	faddp	%st,%st(1)

	fldl	LC2
	fmulp	%st,%st(1)

	ret
