/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/fBitBlt.s,v 1.8 1992/08/29 11:09:47 dawes Exp $ */
/*
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Thomas Roell, roell@informatik.tu-muenchen.de
 *
 * /proj/X11/mit/server/ddx/at386/vga/RCS/fBitBlt.s,v 1.6 91/02/10 16:44:16 root Exp
 */


/*
 * Just copy the fastest way you can !!!!
 * Not questions of style here, please.
 * Since our dragon is SSOOOOOOOOOOOOOO slow don't compute timings the normal
 * way.
 * ----->>>>> a 'movsl' takes about 144(!!!) cycles on my 33MHz 386 <<<<<-----
 *
 * But cause of the BRAINDAMAGED 386 you cann't use movsl -- it does only
 * post-decrement/increment !!!
 *
 * (7/28/90 TR)
 */

#include "assembler.h"

	.file "fBitBlt.s"

#define tmp         %ebx
#define xdir        8(%ebp)
#define psrc        %esi
#define pdst        %edi
#define hcount      %edx
#define count       24(%ebp)
#define srcPitch    28(%ebp)
#define dstPitch    32(%ebp)
#define srcPitchReg %ecx
#define dstPitchReg %eax
#define xSrc        -4(%ebp)
#define xDst        -8(%ebp)
#define countS      -12(%ebp)
#define Shift       %ecx
#define low         %eax
#define lowb        %ah
#define lowbl       %al
#define loww        %ax
#define high        %edx
#define highb       %dh
#define highbl      %dl
#define highw       %dx
#define hcountS     20(%ebp)

#if !defined(SYSV) && !defined(SVR4)
#define fastBitBltCopy _fastBitBltCopy
#endif

.text
	ALIGNTEXT4
.globl	fastBitBltCopy
fastBitBltCopy:
	pushl %ebp
	movl %esp,%ebp
	subl $12,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),psrc
	movl 16(%ebp),pdst
	movl 20(%ebp),hcount
	movl count,tmp
	
	cmpl $1,xdir
	je .fastmove
	std
.fastmove:

	cmpl $5,tmp
	jg .BlockSetup
	movl srcPitch,srcPitchReg
	movl dstPitch,dstPitchReg
	cmpl $1,xdir
	jne .RightFast

.LeftFast:
	addl tmp,srcPitchReg
	addl tmp,dstPitchReg
	cmpl $4,tmp
	jg .LeftFiveLoop
	je .LeftFourLoop
	cmpl $2,tmp
	jg .LeftThreeLoop
	je .LeftTwoLoop
	jmp .LeftOneLoop

	ALIGNTEXT4
.LeftOneBody:
	movb (psrc),%bl
	movb %bl,(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.LeftOneLoop:
	decl hcount
	jns .LeftOneBody
	jmp .finish

	ALIGNTEXT4
.LeftTwoBody:
	movw (psrc),%bx
	movw %bx,(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.LeftTwoLoop:
	decl hcount
	jns .LeftTwoBody
	jmp .finish

	ALIGNTEXT4
.LeftThreeBody:
	movw (psrc),%bx
	movw %bx,(pdst)
	movb 2(psrc),%bl
	movb %bl,2(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.LeftThreeLoop:
	decl hcount
	jns .LeftThreeBody
	jmp .finish

	ALIGNTEXT4
.LeftFourBody:
	movl (psrc),%ebx
	movl %ebx,(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.LeftFourLoop:
	decl hcount
	jns .LeftFourBody
	jmp .finish

	ALIGNTEXT4
.LeftFiveBody:
	movl (psrc),%ebx
	movl %ebx,(pdst)
	movb 4(psrc),%bl
	movb %bl,4(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.LeftFiveLoop:
	decl hcount
	jns .LeftFiveBody
	jmp .finish
		
.RightFast:
	subl tmp,srcPitchReg
	subl tmp,dstPitchReg
	cmpl $4,tmp
	jg .RightFiveLoop
	je .RightFourLoop
	cmpl $2,tmp
	jg .RightThreeLoop
	je .RightTwoLoop
	jmp .RightOneLoop

	ALIGNTEXT4
.RightOneBody:
	movb -1(psrc),%bl
	movb %bl,-1(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.RightOneLoop:
	decl hcount
	jns .RightOneBody
	jmp .finish

	ALIGNTEXT4
.RightTwoBody:
	movw -2(psrc),%bx
	movw %bx,-2(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.RightTwoLoop:
	decl hcount
	jns .RightTwoBody
	jmp .finish

	ALIGNTEXT4
.RightThreeBody:
	movw -3(psrc),%bx
	movw %bx,-3(pdst)
	movb -1(psrc),%bl
	movb %bl,-1(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.RightThreeLoop:
	decl hcount
	jns .RightThreeBody
	jmp .finish

	ALIGNTEXT4
.RightFourBody:
	movl -4(psrc),%ebx
	movl %ebx,-4(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.RightFourLoop:
	decl hcount
	jns .RightFourBody
	jmp .finish

	ALIGNTEXT4
.RightFiveBody:
	movl -5(psrc),%ebx
	movl %ebx,-5(pdst)
	movb -1(psrc),%bl
	movb %bl,-1(pdst)
	addl srcPitchReg,psrc
	addl dstPitchReg,pdst
.RightFiveLoop:
	decl hcount
	jns .RightFiveBody
	jmp .finish


.BlockSetup:
	movl psrc,tmp
	andl $3,tmp
	movl pdst,Shift
	andl $3,Shift
	cmpl Shift,tmp
	jne .UnalignedBlock

	cmpl $1,xdir
	je .LeftBlockLoop
	jmp .RightBlockLoop

.LeftBlockBody:
	test $1,pdst		/* align to word ? */
	jz .LeftAlignWord
	movsb
	decl tmp
.LeftAlignWord:
	test $2,pdst            /* align to dword ? */
	jz .LeftStartBlock
	movsw
	subl $2,tmp
.LeftStartBlock:
	movl tmp,%ecx
	shrl $2,%ecx
	repz
	movsl                   /* tricky: this is really the fastest way !! */
	test $2,tmp             /* finish requires a word ? */
	jz .LeftFixupByte
	movsw
.LeftFixupByte:
	test $1,tmp             /* finish requires a byte ? */
	jz .LeftBlockEnd
	movsb
.LeftBlockEnd:
	addl srcPitch,psrc      /* ok, goto next line */
	addl dstPitch,pdst
.LeftBlockLoop:
	movl count,tmp          /* reload linecounter */
	decl hcount
	jns .LeftBlockBody
	jmp .finish

.RightBlockBody:
	test $1,pdst		/* align to word ? */
	jz .RightAlignWord
	decl psrc
	decl pdst
	movb (psrc),%al
	movb %al,(pdst)
	decl tmp
.RightAlignWord:
	test $2,pdst            /* align to dword ? */
	jz .RightStartBlock
	subl $2,psrc
	subl $2,pdst
	movw (psrc),%ax
	movw %ax,(pdst)
	subl $2,tmp
.RightStartBlock:
	movl tmp,%ecx
	shrl $2,%ecx
	subl $4,psrc
	subl $4,pdst
	repz
	movsl                   /* tricky: this is really the fastest way !! */
	addl $4,psrc
	addl $4,pdst
	test $2,tmp             /* finish requires a word ? */
	jz .RightFixupByte
	subl $2,psrc
	subl $2,pdst
	movw (psrc),%ax
	movw %ax,(pdst)
.RightFixupByte:
	test $1,tmp             /* finish requires a byte ? */
	jz .RightBlockEnd
	decl psrc
	decl pdst
	movb (psrc),%al
	movb %al,(pdst)
.RightBlockEnd:
	addl srcPitch,psrc      /* ok, goto next line */
	addl dstPitch,pdst
.RightBlockLoop:
	movl count,tmp          /* reload linecounter */
	decl hcount
	jns .RightBlockBody
	jmp .finish

/*
 * ok, now the cases when Src & Dst are not at the same offset
 */
.UnalignedBlock:

	movl tmp,xSrc
	movl Shift,xDst
	cmpl $1,xdir
	jne .rightShiftSetup

	subl tmp,Shift
	js .lsAdjustShift
	subl $4,Shift
.lsAdjustShift:
	negl Shift
	shll $3,Shift
	jmp .leftShiftLoop


.leftShiftBody:
	movl $0,low
	movl $0,high
/*
 * first load (4-xSrc) Pixels, so that psrc is dword-aligned
 */
	movl xSrc,tmp
	movl .lseTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.lseTab: .long .lse0, .lse1, .lse2, .lse3
.lse0:  movl (psrc),low
	addl $4,psrc
	subl $4,countS
	jmp .lse
.lse1:	movw 1(psrc),loww
	shll $16,low
	movb (psrc),lowb
	addl $3,psrc
	subl $3,countS
	jmp .lse
.lse2:	movw (psrc),loww
	shll $16,low
	addl $2,psrc
	subl $2,countS
	jmp .lse
.lse3:	movb (psrc),lowb
	shll $16,low
	incl psrc
	decl countS
/*
 * get now the rest of Pixels neccessary to align pdst to a dword
 * i.e  if (4 - xDst) > (4 - xSrc) get 4 additional Pixels
 * i.e  if xDst < xSrc !!!!
 */
.lse:	movl xDst,tmp
	orl tmp,tmp
	jz .lsf0
	cmpl tmp,xSrc
	jl .lsfirst
	movl low,high
	movl (psrc),low
	addl $4,psrc
	subl $4,countS
/*
 * now store (4- xDst) Pixel
 */
.lsfirst:
	SHRDL (low,high)
	movl .lsfTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.lsfTab: .long .lsf0, .lsf1, .lsf2, .lsf3
.lsf1:	movb highb,(pdst)
	shrl $16,high
	movw highw,1(pdst)
	addl $3,pdst
	jmp .lsf0
.lsf2:  shrl $16,high
	movw highw,(pdst)
	addl $2,pdst
	jmp .lsf0
.lsf3:	shrl $16,high
	movb highb,(pdst)
	incl pdst
/*
 * we have countS Pixel to load. Get them as dword, i.e as 4 Pixel group.
 * that means we can get (countS >> 2) dwords !
 * since psrc and pdst are aligned dword, this is the fast case.
 */
.lsf0:  movl low,high
	movl countS,tmp
	shrl $2,tmp
	jmp .lsdl

	ALIGNTEXT4
.lsdb:	lodsl
	SHRDL (low,high)
	xchgl low,high
	stosl
.lsdl:	decl tmp
	jns .lsdb
/*
 * the are (countS & 3) Pixles to get.
 * but be carefull, these pixels are now aligned LEFT !!!! ( on the screen)
 */
	xorl low,low
	movl countS,tmp
	andl $3,tmp
	addl tmp,psrc
	movl .lsaTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.lsaTab: .long .lsa0, .lsa1, .lsa2, .lsa3
.lsa3:	movb -1(psrc),lowbl
	shll $16,low
	movw -3(psrc),loww
	jmp .lsa0
.lsa2:	movw -2(psrc),loww
	jmp .lsa0
.lsa1:	movb -1(psrc),lowbl

.lsa0:	SHRDL (low,high)
	shrl  %cl,low
/*
 * at this point we have read all Pixels, but there are still some to store
 * the number of Pixel in [low,high] seems to be ((countS&3)+(4 - Shift))
 * that maens we have here to store 1,2,3,4,5,6 Pixel (0 & 7 are impossible)
 */
	movl countS,tmp
	andl $3,tmp
	addl $4,tmp
	shll $3,tmp
	subl Shift,tmp
	shrl $3,tmp
	addl tmp,pdst
	movl .lsgTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.lsgTab: .long .lsg0, .lsg1, .lsg2, .lsg3, .lsg4, .lsg5, .lsg6, .lsg0
.lsg6:  movl high,-6(pdst)
	movw loww,-2(pdst)
	jmp .lsg0

.lsg5:	movl high,-5(pdst)
	movb lowbl,-1(pdst)
	jmp .lsg0

.lsg4:	movl high,-4(pdst)
	jmp .lsg0

.lsg3:	movw highw,-3(pdst)
	shrl $16,high
	movb highbl,-1(pdst)
	jmp .lsg0

.lsg2:  movw highw,-2(pdst)
	jmp .lsg0

.lsg1:	movb highbl,-1(pdst)

.lsg0:	movl countS,tmp
	andl $3,tmp
	addl $4,tmp
	shll $3,tmp
	subl Shift,tmp
	shrl $3,tmp


	addl srcPitch,psrc
	addl dstPitch,pdst

.leftShiftLoop:
	movl count,tmp
	movl tmp,countS
	decl hcountS
	jns .leftShiftBody
	jmp .finish

/*
 * now, descending x-direction
 */
.rightShiftSetup:

	subl tmp,Shift
	jns .rsAdjustShift
	addl $4,Shift
.rsAdjustShift:
	shll $3,Shift
	jmp .rightShiftLoop

.rightShiftBody:
	movl $0,low
	movl $0,high
/*
 * first load (xSrc) Pixels, so that psrc is dword-aligned
 */
	movl xSrc,tmp
	movl .rseTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.rseTab: .long .rse0, .rse1, .rse2, .rse3
.rse3:	subl $3,psrc
	subl $3,countS
	movb 2(psrc),lowbl
	shll $16,low
	movw (psrc),loww
	jmp .rse0
.rse2:	subl $2,psrc
	subl $2,countS
	movw (psrc),loww
	jmp .rse0
.rse1:	decl psrc
	decl countS
	movb (psrc),lowbl
/*
 * get now the rest of Pixels neccessary to align pdst to a dword
 * i.e  if (xDst > xSrc) get 4 additional Pixels
 */
.rse0:	movl xDst,tmp
	orl tmp,tmp
	jz .rsf0
	cmpl tmp,xSrc
	jg .rsfirst
	subl $4,psrc
	subl $4,countS
	movl low,high
	movl (psrc),low
/*
 * now store (xDst) Pixels
 */
.rsfirst:
	SHLDL (low,high)
	movl .rsfTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.rsfTab: .long .rsf0, .rsf1, .rsf2, .rsf3
.rsf3:	subl $3,pdst
	movw highw,(pdst)
	shrl $16,high
	movb highbl,2(pdst)
	jmp .rsf0
.rsf2:  subl $2,pdst
	movw highw,(pdst)
	jmp .rsf0
.rsf1:	decl pdst
	movb highbl,(pdst)
/*
 * we have countS Pixel to load. Get them as dword, i.e as 4 Pixel group.
 * that means we can get (countS >> 2) dwords !
 * since psrc and pdst are aligned dword, this is the fast case.
 */
.rsf0:  movl low,high
	movl countS,tmp
	shrl $2,tmp
	jmp .rsdl

	ALIGNTEXT4
.rsdb:	subl $4,psrc
	subl $4,pdst
	movl (psrc),low
	SHLDL (low,high)
	movl high,(pdst)
	movl low,high
.rsdl:	decl tmp
	jns .rsdb
/*
 * the are (countS & 3) Pixles to get.
 * but be carefull, these pixels are now aligned RIGHT !!!! ( on the screen)
 */
	xorl low,low
	movl countS,tmp
	andl $3,tmp
	subl tmp,psrc
	movl .rsaTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.rsaTab: .long .rsa0, .rsa1, .rsa2, .rsa3
.rsa3:	movw 1(psrc),loww
	shll $16,low
	movb (psrc),lowb
	jmp .rsa0
.rsa2:	movw (psrc),loww
	shll $16,low
	jmp .rsa0
.rsa1:	movb (psrc),lowb
	shll $16,low
.rsa0:	SHLDL (low,high)
	shll  %cl,low
/*
 * at this point we have read all Pixels, but there are still some to store
 * the number of Pixel in [low,high] seems to be ((countS&3)+(4 - Shift))
 * that maens we have here to store 1,2,3,4,5,6 Pixel (0 & 7 are impossible)
 */
	movl countS,tmp
	andl $3,tmp
	addl $4,tmp
	shll $3,tmp
	subl Shift,tmp
	shrl $3,tmp
	subl tmp,pdst
	movl .rsgTab(,tmp,4),tmp
	jmp *tmp
	ALIGNTEXT4
.rsgTab: .long .rsg0, .rsg1, .rsg2, .rsg3, .rsg4, .rsg5, .rsg6, .rsg0
.rsg6:  movl high,2(pdst)
	shrl $16,low
	movw loww,(pdst)
	jmp .rsg0
.rsg5:	movl high,1(pdst)
	shrl $16,low
	movb lowb,(pdst)
	jmp .rsg0
.rsg4:	movl high,(pdst)
	jmp .rsg0
.rsg3:  movb highb,(pdst)
	shrl $16,high
	movw highw,1(pdst)
	jmp .rsg0
.rsg2:  shrl $16,high
	movw highw,(pdst)
	jmp .rsg0
.rsg1:	shrl $16,high
	movb highb,(pdst)

.rsg0:	addl srcPitch,psrc
	addl dstPitch,pdst
	
.rightShiftLoop:
	movl count,tmp
	movl tmp,countS
	decl hcountS
	jns .rightShiftBody
	jmp .finish
	
.finish:
	cld
	leal -24(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret

