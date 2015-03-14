#ifndef FUNCDEFH
#define FUNCDEFH
/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Special macros designed to remove some __STDC__ ugliness from my source
   files.  Instead, I use these (which may be just as ugly.  Instead of using
extern foo();
   or
extern foo(int,double);
   I use
extern foo FUN2(int, double);
   which is expanded to the right thing depending on whether you're using an
   ANSI cc or not.

   Also, instead of saying
type
foo(x,y)
int x;
double y;
   or
type
foo(int x, double y)
   I use
type
foo FUN2(int, x, double, y)
Which is also expanded into the right thing. . .
 */

#ifdef __STDC__
#include <stdarg.h>
#define var_start(x,y) va_start(x,y)

/* These macros expand into ANSI prototypes */
#define FUN0()		(void)
#define EXT0()		(void)

#define FUN1(t1,a1)	(t1 a1)
#define EXT1(t1)	(t1)
#define FUN1N(t1,a1)	(t1 a1, ...)
#define EXT1N(t1)	(t1, ...)

#define FUN2(t1,a1,t2,a2)	(t1 a1,t2 a2)
#define EXT2(t1, t2)		(t1, t2)
#define FUN2N(t1,a1,t2,a2)	(t1 a1,t2 a2, ...)
#define EXT2N(t1, t2)		(t1, t2, ...)

#define FUN3(t1,a1,t2,a2,t3,a3)	(t1 a1, t2 a2, t3 a3)
#define EXT3(t1, t2, t3)	(t1, t2, t3)
#define FUN3N(t1,a1,t2,a2,t3,a3)(t1 a1, t2 a2, t3 a3, ...)
#define EXT3N(t1, t2, t3)	(t1, t2, t3, ...)

#define FUN4(t1,a1,t2,a2,t3,a3,t4,a4)	(t1 a1, t2 a2, t3 a3, t4 a4)
#define EXT4(t1, t2, t3, t4)		(t1, t2, t3, t4)

#define FUN5(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5)
#define EXT5(t1, t2, t3, t4, t5)		(t1, t2, t3, t4, t5)

#define FUN6(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6)
#define EXT6(t1, t2, t3, t4, t5, t6)			(t1, t2, t3, t4, t5, t6)

#define FUN7(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7)
#define EXT7(t1, t2, t3, t4, t5, t6, t7)		(t1, t2, t3, t4, t5, t6, t7)

#define FUN8(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7, t8 a8)
#define EXT8(t1, t2, t3, t4, t5, t6, t7, t8)			(t1, t2, t3, t4, t5, t6, t7, t8)

#define FUN9(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7, t8 a8, t9 a9)
#define EXT9(t1, t2, t3, t4, t5, t6, t7, t8, t9)			(t1, t2, t3, t4, t5, t6, t7, t8, t9)

#define FUN10(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7, t8 a8, t9 a9, t10 a10)
#define EXT10(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)				(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)

#define FUN11(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10,t11,a11)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7, t8 a8, t9 a9, t10 a10, t11 a11)
#define EXT11(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)				(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)

#define FUN12(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10,t11,a11,t12,a12)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7, t8 a8, t9 a9, t10 a10, t11 a11, t12 a12)
#define EXT12(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)				(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)

#define FUN13(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10,t11,a11,t12,a12,t13,a13)	(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6, t7 a7, t8 a8, t9 a9, t10 a10, t11 a11, t12 a12, t13 a13)
#define EXT13(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)				(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)


#else
#include <varargs.h>

#define var_start(x,y) va_start(x)

/* These macros expand into old-style function definitions */

#define FUN0()	()
#define EXT0()	()

#define FUN1(t1,a1)	(a1) t1 a1;
#define EXT1(t1)	()
#define FUN1N(t1,a1)	(a1,va_alist) t1 a1; va_dcl
#define EXT1N(t1)	()

#define FUN2(t1,a1,t2,a2)	(a1, a2) t1 a1; t2 a2;
#define EXT2(t1, t2)		()
#define FUN2N(t1,a1,t2,a2) (a1, a2,va_alist) t1 a1; t2 a2; va_dcl
#define EXT2N(t1, t2)		()

#define FUN3(t1,a1,t2,a2,t3,a3) (a1, a2, a3) t1 a1; t2 a2; t3 a3;
#define EXT3(t1, t2, t3)	()
#define FUN3N(t1,a1,t2,a2,t3,a3) (a1, a2, a3, va_alist) t1 a1; t2 a2; t3 a3; va_dcl
#define EXT3N(t1, t2, t3)	()

#define FUN4(t1,a1,t2,a2,t3,a3,t4,a4)	(a1, a2, a3, a4) t1 a1; t2 a2; t3 a3; t4 a4;
#define EXT4(t1, t2, t3, t4)		()

#define FUN5(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5) (a1, a2, a3, a4, a5) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5;
#define EXT5(t1, t2, t3, t4, t5)	()

#define FUN6(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6) (a1, a2, a3, a4, a5, a6) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6;
#define EXT6(t1, t2, t3, t4, t5, t6)	()

#define FUN7(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7) (a1, a2, a3, a4, a5, a6, a7) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7;
#define EXT7(t1, t2, t3, t4, t5, t6, t7)	()

#define FUN8(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8) (a1, a2, a3, a4, a5, a6, a7, a8) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7; t8 a8;
#define EXT8(t1, t2, t3, t4, t5, t6, t7, t8)	()

#define FUN9(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9) (a1, a2, a3, a4, a5, a6, a7, a8, a9) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7; t8 a8; t9 a9;
#define EXT9(t1, t2, t3, t4, t5, t6, t7, t8, t9)	()

#define FUN10(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10) (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7; t8 a8; t9 a9; t10 a10;
#define EXT10(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)	()

#define FUN11(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10,t11,a11) (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7; t8 a8; t9 a9; t10 a10; t11 a11;
#define EXT11(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)	()

#define FUN12(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10,t11,a11,t12,a12) (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7; t8 a8; t9 a9; t10 a10; t11 a11; t12 a12;
#define EXT12(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)	()

#define FUN13(t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6,t7,a7,t8,a8,t9,a9,t10,a10,t11,a11,t12,a12,t13,a13) (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6; t7 a7; t8 a8; t9 a9; t10 a10; t11 a11; t12 a12; t13 a13;
#define EXT13(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)	()


#endif
#endif
