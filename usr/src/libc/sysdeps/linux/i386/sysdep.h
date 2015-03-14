/* Copyright (C) 1992 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <sysdeps/linux/sysdep.h>

#if defined(__i486__) || defined(i486)
#define	ENTRY(name)							      \
  .globl _##name##;							      \
  .align 4;								      \
  _##name##:
#else
#define	ENTRY(name)							      \
  .globl _##name##;							      \
  .align 2;								      \
  _##name##:
#endif

#if defined(__PIC__) || defined (__pic__)
#define	PSEUDO(name, syscall_name, args)				      \
  .text;								      \
  ENTRY (name)								      \
    PUSH_##args								      \
    call L4;								      \
   L4:									      \
    popl %ebx;								      \
    addl $_GLOBAL_OFFSET_TABLE_+[.-L4],%ebx;				      \
    pushl %ebx;								      \
    movl $SYS_##syscall_name, %eax;					      \
    MOVE_##args								      \
    int $0x80;								      \
    popl %ebx;								      \
    movl %eax,%edx;							      \
    test %edx,%edx;							      \
    jge	Lexit;								      \
    negl %edx;								      \
    movl _errno@GOT(%ebx),%eax;						      \
    movl %edx,(%eax);							      \
    movl $-1,%eax;							      \
   Lexit:								      \
    POP_##args

#else

#define	PSEUDO(name, syscall_name, args)				      \
  .text;								      \
  ENTRY (name)								      \
    PUSH_##args								      \
    movl $SYS_##syscall_name, %eax;					      \
    MOVE_##args								      \
    int $0x80;								      \
    test %eax, %eax;							      \
    jge	Lexit;								      \
    negl %eax;								      \
    movl %eax,_errno;							      \
    movl $-1,%eax;							      \
   Lexit:								      \
    POP_##args

#endif

/* Linux takes system call arguments in registers:
   	1: %ebx
	2: %ecx
	3: %edx
	4: %esi
	5: %edi
 */

#define PUSH_0	/* No arguments to push.  */
#define PUSH_1	pushl %ebx;
#define PUSH_2	PUSH_1
#define PUSH_3	PUSH_1
#define PUSH_4	pushl %esi; PUSH_3
#define PUSH_5	pushl %edi; PUSH_4

#define	MOVE_0	/* No arguments to move.  */

#if defined(__PIC__) || defined (__pic__)
#define	MOVE_1	movl 12(%esp),%ebx;
#define	MOVE_2	MOVE_1 movl 16(%esp),%ecx;
#define	MOVE_3	MOVE_2 movl 20(%esp),%edx;
#define	MOVE_4	movl 16(%esp),%ebx; movl 20(%esp),%ecx; \
			movl 24(%esp),%edx;  movl 28(%esp),%esi;
#define	MOVE_5	movl 20(%esp),%ebx; movl 24(%esp),%ecx; \
			movl 28(%esp),%edx;  movl 32(%esp),%esi; \
			movl 36(%esp),%edi;

#else

#define	MOVE_1	movl 8(%esp),%ebx;
#define	MOVE_2	MOVE_1 movl 12(%esp),%ecx;
#define	MOVE_3	MOVE_2 movl 16(%esp),%edx;
#define	MOVE_4	movl 12(%esp),%ebx; movl 16(%esp),%ecx; \
			movl 20(%esp),%edx;  movl 24(%esp),%esi;
#define	MOVE_5	movl 16(%esp),%ebx; movl 20(%esp),%ecx; \
			movl 24(%esp),%edx;  movl 28(%esp),%esi; \
			movl 32(%esp),%edi;
#endif

#define POP_0	/* No arguments to pop.  */
#define POP_1	popl %ebx;
#define POP_2	POP_1
#define POP_3	POP_1
#define POP_4	POP_3 popl %esi;
#define POP_5	POP_4 popl %edi;

/* Linux doesn't use it. */
#if 0
#define	r0	%eax
#define	r1	%edx
#define MOVE(x,y)	movl x , y
#endif
