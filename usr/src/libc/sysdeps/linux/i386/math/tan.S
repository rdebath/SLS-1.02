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

	.file	"tan.S"
	.text
	.globl	_tan

#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
_tan:
	fldl	4(%esp)
	jmp	L3
L1:
	fldpi
	fxch	%st(1)
L2:
	fprem1
	fstsw	%ax
	sahf
	jp	L2
	fxch	%st(1)
	fstp	%st(0)
L3:
	fptan
	fstsw	%ax
	sahf
	jp	L1
	fstp	%st(0)
	ret
