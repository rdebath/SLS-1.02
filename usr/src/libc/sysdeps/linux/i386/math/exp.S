/*
** Copyright (C) 1991 DJ Delorie, 24 Kirsten Ave, Rochester NH 03867-2954
**
** Modified O.ROBERT 24, Avenue de Verdun 92170 VANVES, FRANCE
**
** E-mail: roberto@germinal.ibp.fr
**
** This file is distributed under the terms listed in the document
** "copying.dj", available from DJ Delorie at the address above.
** A copy of "copying.dj" should accompany this file; if not, a copy
** should be available from where this file was obtained.  This file
** may not be distributed without a verbatim copy of "copying.dj".
**
** This file is distributed WITHOUT ANY WARRANTY; without even the implied
** warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
** or	03-Apr-1991	corrected bug about argument zero to pow
**			fyl2x didn't like it
*/

/* Modified by H.J. Lu for Linux, 1992 */

/* History:15,24 */

	.file	"exp.S"

	.data
#ifdef	__i486__
	.align	4
#else
	.align	2
#endif
LCW1:
	.word	0

#ifdef	__i486__
	.align	4
#else
	.align	2
#endif
LCW2:
	.word	0

	.text
#ifdef	__i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
LC0:
	.double	0d1.0000000000000000000e+00

	.globl	_exp
#ifdef	__i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
_exp:
	fldl	4(%esp)
	fldl2e
	fmulp	%st,%st(1)
	jmp	Lpow2

	.globl	_pow10

#ifdef	__i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
_pow10:
	fldl	4(%esp)
	fldl2t
	fmulp	%st,%st(1)
	jmp	Lpow2

	.globl	_pow2
#ifdef	__i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
_pow2:
	fldl	4(%esp)
Lpow2:
	fstcw	LCW1
	fstcw	LCW2
	andw	$0xf3ff,LCW1
	fldcw	LCW1
	fldl	%st(0)
	frndint
	fldcw	LCW2
	fxch	%st(1)
	fsub	%st(1),%st
	f2xm1
	faddl	LC0
	fscale
	fstp	%st(1)
	ret
