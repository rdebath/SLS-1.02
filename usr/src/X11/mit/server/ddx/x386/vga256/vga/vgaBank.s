/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/vga/vgaBank.s,v 1.9 1992/08/29 11:14:28 dawes Exp $ */
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
 * $Header: /proj/X11/mit/server/ddx/x386/vga/RCS/vgaBank.s,v 1.1 1991/06/02 22:36:38 root Exp $
 */


	.file	"vgaBank.s"

/*
 * Because a modern VGA has more than 128kBytes (which are mappable into the
 * 386' memory some logic is required. The VGA's memory (logical VGA
 * address space) is devided into smaller parts (called logical segments). 
 * These segments are mapped to logical areas.
 *
 * There are there different logical mapping areas:
 *
 * Read:       an area which can be read from
 * Write:      an area which can be written to
 * ReadWrite:  here is both read an write possible
 *
 * It is permissable to use simultaneously a Read and a Write, but you can use
 * ReadWrite only as a single. 
 * For example the bitblitting code uses a Read area as source and a Write
 * area as destination. Most other routines use only a ReadWrite.
 *
 * A logical mapping area is described by some parameters (here I will for
 * example describe a Read area:
 *
 * ReadBottom     lowest accessable byte relative to the beginning of the
 *                VGA boards mapped memory.
 * 
 * ReadTop        highes accessable byte plus one.
 *
 * SegmentSize    size of such an mapped area (common for all three)
 *
 * SegmentShift   log2(SegmentSize) (used to compute the logical segment)
 *
 * SegmentMask    SegmentSize - 1 (used to mask the offset inter an area)
 *
 * 
 * All that the following routines are doing is computing for a given offset
 * into the logical VGA adress space the offset into such an logical area
 * and the logical segment number. By the way they call also the VGA board's
 * driver to set up the VGA's physical memory mapping according to the logical
 * that was requested by the calliie.
 *
 * For shake of simplicity Write and ReadWrite share the same Bottom & Top.
 * NOTE: Read & Write may have differnt starting addresses, or even common.
 *
 * There are multible routines present for the same effect. This was made
 * for effectivly interface lowlevel assembly language best.
 */

/*
 * BUGALERT: this should be gotten from vga.h. But since there some C lang.
 *           things, too ...
 */

#include "assembler.h"

#include "vgaAsm.h"

#if !defined(SYSV) && !defined(SVR4)
#define writeseg _writeseg
#define readseg _readseg
#define saveseg _saveseg
#endif

	.data
	.globl writeseg
writeseg:
	.long 0
readseg:
	.long 0
saveseg:
	.long 0

	.text
/*
 *-----------------------------------------------------------------------
 * vgaSetReadWrite ---
 *     select a memory bank of the VGA board for read & write access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 * pointer
 * vgaSetReadWrite(p)
 *     register pointer p;
 * {
 *   writeseg = ((unsigned long)p - VGABASE) >> vgaSegmentShift;
 *   (vgaSetReadWriteFunc)(writeseg);
 *   return (vgaWriteBottom + ((unsigned int)p & vgaSegmentMask));
 * }
 *
 */
	ALIGNTEXT4
	.globl vgaSetReadWrite
vgaSetReadWrite:
	movl    4(%esp),%eax
	pushl   %ecx
	pushl	%edx
	subl	VGABASE,%eax
	movl	vgaSegmentShift,%ecx
	shrl    %cl,%eax
	movl	%eax,writeseg
	movl	vgaSetReadWriteFunc,%edx
	call	*%edx
	popl    %edx
	popl	%ecx
	movl    4(%esp),%eax
	andl	vgaSegmentMask,%eax
	addl	vgaWriteBottom,%eax
 	ret

/*
 *-----------------------------------------------------------------------
 * vgaReadWriteNext ---
 *     switch to next memory bank of the VGA board for read & write access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 *
 * pointer
 * vgaReadWriteNext(p)
 *      register pointer p;
 * {
 *   (vgaSetReadWriteFunc)(++writeseg);
 *   return (p - vgaSegmentSize);
 * }
 */
	ALIGNTEXT4
	.globl	vgaReadWriteNext
vgaReadWriteNext:
	pushl	%edx
	movl	writeseg,%eax
	incl	%eax
	movl	%eax,writeseg
	movl 	vgaSetReadWriteFunc,%edx
	call 	*%edx
	popl	%edx
	movl    4(%esp),%eax
	subl	vgaSegmentSize,%eax
	ret

/*
 *-----------------------------------------------------------------------
 * vgaReadWritePrev ---
 *     switch to previous memory bank of the VGA board for read & write access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * pointer
 * vgaReadWritePrev(p)
 *      register pointer p;
 * {
 *   (vgaSetReadWriteFunc)(--writeseg); 
 *   return (p + vgaSegmentSize);
 * }
 */
	ALIGNTEXT4
	.globl	vgaReadWritePrev
vgaReadWritePrev:
	pushl	%edx
	movl	writeseg,%eax
	decl	%eax
	movl	%eax,writeseg
	movl 	vgaSetReadWriteFunc,%edx
	call 	*%edx
	popl	%edx
	movl    4(%esp),%eax
	addl	vgaSegmentSize,%eax
	ret

/*
 *-----------------------------------------------------------------------
 * vgaSetRead ---
 *     select a memory bank of the VGA board for read access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 * pointer
 * vgaSetRead(p)
 *     register pointer p;
 * {
 *   readseg = ((unsigned long)p - VGABASE) >> vgaSegmentShift;
 *   (vgaSetReadFunc)(readseg);
 *   return (vgaReadBottom + ((unsigned int)p & vgaSegmentMask));
 * }
 *
 */
	ALIGNTEXT4
	.globl	vgaSetRead
vgaSetRead:
	movl    4(%esp),%eax
	pushl   %ecx
	pushl	%edx
	subl	VGABASE,%eax
	movl	vgaSegmentShift,%ecx
	shrl    %cl,%eax
	movl	%eax,readseg
	movl	vgaSetReadFunc,%edx
	call	*%edx
	popl    %edx
	popl	%ecx
	movl    4(%esp),%eax
	andl	vgaSegmentMask,%eax
	addl	vgaReadBottom,%eax
 	ret

/*
 *-----------------------------------------------------------------------
 * vgaReadNext ---
 *     switch to next memory bank of the VGA board for read access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 *
 * pointer
 * vgaReadNext(p)
 *      register pointer p;
 * {
 *   (vgaSetReadFunc)(++readseg);
 *   return (p - vgaSegmentSize);
 * }
 */
	ALIGNTEXT4
	.globl	vgaReadNext
vgaReadNext:
	pushl	%edx
	movl	readseg,%eax
	incl	%eax
	movl	%eax,readseg
	movl 	vgaSetReadFunc,%edx
	call 	*%edx
	popl	%edx
	movl    4(%esp),%eax
	subl	vgaSegmentSize,%eax
	ret

/*
 *-----------------------------------------------------------------------
 * vgaReadPrev ---
 *     switch to previous memory bank of the VGA board for read access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * pointer
 * vgaReadPrev(p)
 *      register pointer p;
 * {
 *   (vgaSetReadFunc)(--readseg); 
 *   return (p + vgaSegmentSize);
 * }
 */
	ALIGNTEXT4
	.globl	vgaReadPrev
vgaReadPrev:
	pushl	%edx
	movl	readseg,%eax
	decl	%eax
	movl	%eax,readseg
	movl 	vgaSetReadFunc,%edx
	call 	*%edx
	popl	%edx
	movl    4(%esp),%eax
	addl	vgaSegmentSize,%eax
	ret

/*
 *-----------------------------------------------------------------------
 * vgaSetWrite ---
 *     select a memory bank of the VGA board for write access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 * pointer
 * vgaSetWrite(p)
 *     register pointer p;
 * {
 *   writeseg = ((unsigned long)p - VGABASE) >> vgaSegmentShift;
 *   (vgaSetWriteFunc)(writeseg);
 *   return (vgaWriteBottom + ((unsigned int)p & vgaSegmentMask));
 * }
 *
 */
	ALIGNTEXT4
	.globl	vgaSetWrite
vgaSetWrite:
	movl    4(%esp),%eax
	pushl   %ecx
	pushl	%edx
	subl	VGABASE,%eax
	movl	vgaSegmentShift,%ecx
	shrl    %cl,%eax
	movl	%eax,writeseg
	movl	vgaSetWriteFunc,%edx
	call	*%edx
	popl    %edx
	popl	%ecx
	movl    4(%esp),%eax
	andl	vgaSegmentMask,%eax
	addl	vgaWriteBottom,%eax
 	ret

/*
 *-----------------------------------------------------------------------
 * vgaWriteNext ---
 *     switch to next memory bank of the VGA board for write access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 *
 * pointer
 * vgaWriteNext(p)
 *      register pointer p;
 * {
 *   (vgaSetWriteFunc)(++writeseg);
 *   return (p - vgaSegmentSize);
 * }
 */
	ALIGNTEXT4
	.globl	vgaWriteNext
vgaWriteNext:
	pushl	%edx
	movl	writeseg,%eax
	incl	%eax
	movl	%eax,writeseg
	movl 	vgaSetWriteFunc,%edx
	call 	*%edx
	popl	%edx
	movl    4(%esp),%eax
	subl	vgaSegmentSize,%eax
	ret

/*
 *-----------------------------------------------------------------------
 * vgaWritePrev ---
 *     switch to previous memory bank of the VGA board for write access
 * Results:
 *      The adjusted pointer into the memory.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * pointer
 * vgaWritePrev(p)
 *      register pointer p;
 * {
 *   (vgaSetWriteFunc)(--writeseg); 
 *   return (p + vgaSegmentSize);
 * }
 */
	ALIGNTEXT4
	.globl	vgaWritePrev
vgaWritePrev:
	pushl	%edx
	movl	writeseg,%eax
	decl	%eax
	movl	%eax,writeseg
	movl 	vgaSetWriteFunc,%edx
	call 	*%edx
	popl	%edx
	movl    4(%esp),%eax
	addl	vgaSegmentSize,%eax
	ret

/*
 *-----------------------------------------------------------------------
 * vgaSaveBank --
 *     save Banking-state
 * Results:
 *      None.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * void
 * vgaSaveBank()
 * {
 *   saveseg = writeseg;
 * }
 */
	ALIGNTEXT4
	.globl	vgaSaveBank
vgaSaveBank:
	movl	writeseg,%eax
	movl	%eax,saveseg
	ret

/*
 *-----------------------------------------------------------------------
 * vgaRestoreBank --
 *     restore the banking after vgaSaveBank was called
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * void
 * vgaRestoreBank()
 * {
 *   (vgaSetWriteFunc)(saveseg);
 *   (vgaSetReadFunc)(saveseg);
 * }
 */
	ALIGNTEXT4
	.globl	vgaRestoreBank
vgaRestoreBank:
	pushl	%edx
	movl	saveseg,%eax
	movl	%eax,writeseg
	movl	vgaSetWriteFunc,%edx
	call	*%edx
	movl	saveseg,%eax
	movl	%eax,readseg
	movl	vgaSetReadFunc,%edx
	call	*%edx
	popl	%edx
	ret


/*
 *-----------------------------------------------------------------------
 * vgaPushRead ---
 *     make the write-bank also readable. no acces to the former read bank !
 * Results:
 *      None.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * void
 * vgaPushRead(p)
 * {
 *   (vgaSetReadWriteFunc)(writeseg);
 * }
 */
	ALIGNTEXT4
	.globl	vgaPushRead
vgaPushRead:
	pushl	%edx
	movl	writeseg,%eax
	movl	vgaSetReadWriteFunc,%edx
	call	*%edx
	popl	%edx
	ret

/*
 *-----------------------------------------------------------------------
 * vgaPopRead ---
 *     restore the banking after vgaPushRead was called
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      None.
 *-----------------------------------------------------------------------
 *
 * void
 * vgaPopRead(p)
 * {
 *   (vgaSetWriteFunc)(writeseg);
 *   (vgaSetReadFunc)(readseg);
 * }
 */
	ALIGNTEXT4
	.globl	vgaPopRead
vgaPopRead:
	pushl	%edx
	movl	writeseg,%eax
	movl	vgaSetWriteFunc,%edx
	call	*%edx
	movl	readseg,%eax
	movl	vgaSetReadFunc,%edx
	call	*%edx
	popl	%edx
	ret
