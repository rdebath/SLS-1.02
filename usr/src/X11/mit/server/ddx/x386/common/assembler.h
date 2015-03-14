/* $Header: /home/x_cvs/mit/server/ddx/x386/common/assembler.h,v 1.6 1992/08/29 10:08:23 dawes Exp $ */

/* Portability definitions for assembler routines */


#ifdef USE_GAS

#define IN_B(x)  inb	%al, x
#define OUT_B(x) outb	%al, x
#define IN_W(x)  inw	%ax, x
#define OUT_W(x) outw	%ax, x
#define STRING	.asciz
#define SHLDL(x,y)	shldl	%cl, x, y
#define SHRDL(x,y)	shrdl	%cl, x, y
#define repz repe
#define ALIGNTEXT4	.align	2,0x90
#define ALIGNTEXT2	.align	1,0x90
#define ALIGNDATA4	.align	2,0x0
#define ALIGNDATA2	.align	1,0x0

#else

#define IN_B(x)  inb	(x)
#define OUT_B(x) outb	(x)
#define IN_W(x)  inw	(x)
#define OUT_W(x) outw	(x)
#define STRING	.string
#define SHLDL(x,y)	shldl	x, y
#define SHRDL(x,y)	shrdl	x, y
#define ALIGNTEXT4	.align	4
#define ALIGNTEXT2	.align	2
#define ALIGNDATA4	.align	4
#define ALIGNDATA2	.align	2

#endif

