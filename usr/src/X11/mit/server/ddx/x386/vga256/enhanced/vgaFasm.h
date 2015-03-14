/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/vgaFasm.h,v 1.5 1992/08/29 11:10:12 dawes Exp $ */
/* Copyright 1992 by James Tsillas, Arlignton, Massachusetts.

		All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation.

JAMES TSILLAS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
*/

#include "assembler.h"
#include "fastblt.h"


#ifndef RROP_NAME_CAT
#if __STDC__ && !defined(UNIXCPP)
#define RROP_NAME_CAT(prefix,suffix)    prefix##suffix
#else
#define RROP_NAME_CAT(prefix,suffix)    prefix/**/suffix
#endif
#endif

#define fCopyAL1_C(psrc, pdst, nl, srcR, widS, psrcS)  \
  while (nl) {                                         \
    fnl = nl;                                          \
    if (fnl > srcR) fnl = srcR;                        \
    nl -= fnl; srcR -= fnl;                            \
    DuffL ( fnl, label00,                              \
	   *pdst = MROP_SOLID (*psrc, *pdst);          \
	   pdst++; psrc++; )                           \
    if (!srcR) {                                       \
      srcR = widS;                                     \
      psrc = psrcS; }                                  \
  }
#define fCopyAL2_C(psrc, pdst, nl, srcR, widS, psrcS)  \
  while (nl) {                                         \
    fnl = nl;                                          \
    if (fnl > srcR) fnl = srcR;                        \
    nl -= fnl; srcR -= fnl;                            \
    DuffL ( fnl, label01,                              \
	   *pdst = MROP_SOLID (*psrc, *pdst);          \
	   pdst++; psrc++; )                           \
    if (!srcR) {                                       \
      srcR = widS;                                     \
      psrc = psrcS; }                                  \
  }
#define fCopyAL3_C(psrc, pdst, nl, srcR, widS, psrcS)  \
  while (nl) {                                         \
    fnl = nl;                                          \
    if (fnl > srcR) fnl = srcR;                        \
    nl -= fnl; srcR -= fnl;                            \
    DuffL ( fnl, label02,                              \
	   *pdst = MROP_SOLID (*psrc, *pdst);          \
	   pdst++; psrc++; )                           \
    if (!srcR) {                                       \
      srcR = widS;                                     \
      psrc = psrcS; }                                  \
  }
#define fCopyUL_C(psrc, pdst, bits, lShift, rShift)            \
    bits1 = BitLeft(bits, (leftShift)); bits = *psrc++;        \
    *pdst = MROP_SOLID (bits1 |                                \
			BitRight(bits, (rightShift)), *pdst);  \
    pdst++;

/* The in-line assembler here only works for gcc2 */
#if __GNUC__ > 1
#define ESI __asm__ ("esi")
#define EDI __asm__ ("edi")

#define fBitBltC(xdir, dst, src, w, count, sP, dP) \
  __asm__ volatile ("pushl %8          \n\
                     pushl %7          \n\
                     pushl %6          \n\
                     pushl %5          \n\
                     pushl %4          \n\
                     call fastBitBltCopy \n\
                     addl $16, %%esp"    \
		    : "=D" ((unsigned char *) (dst)), \
                      "=S" ((unsigned char *) (src))  \
                    : "D0" ((unsigned char *) (dst)), \
		      "S1" ((unsigned char *) (src)), \
                      "g" ((unsigned int) (xdir)),    \
                      "g" ((unsigned int) (w)),       \
                      "g" ((unsigned int) (count)),   \
                      "g" ((unsigned int) (sP)),      \
                      "g" ((unsigned int) (dP))       \
		    : "edx", "ecx", "eax");

#if MROP == Mcopy
#define CLD __asm__ volatile ("cld")
#define fCopyAL1_X(psrc, pdst, nl, srcR, widS, psrcS, rep) \
  __asm__ volatile (".label00: orl %6, %6             \n\
                               jz .label02            \n\
                               movl %6, %%ecx         \n\
                               cmpl %%ecx, %2         \n\
                               ja .label01            \n\
                               movl %2, %%ecx         \n\
                     .label01: subl %%ecx, %6         \n\
                               subl %%ecx, %2         \n"\
			       ##rep##                  \
                               "movsl                 \n\
                               orl %2, %2             \n\
                               jnz .label00           \n\
                               movl %7, %2            \n\
                               movl %8, %0            \n\
                               jmp .label00           \n\
                     .label02:"                         \
		    : "=S" ((unsigned long *) (psrc)),  \
		      "=D" ((unsigned long *) (pdst)),  \
		      "=b" ((unsigned int) (srcR))      \
		    : "S0" ((unsigned long *) (psrc)),  \
		      "D1" ((unsigned long *) (pdst)),  \
                      "b2" ((unsigned int) (srcR)),     \
		      "d" ((unsigned int) (nl)),        \
		      "g" ((unsigned int) (widS)),      \
		      "g" ((unsigned long *) (psrcS))   \
		    : "ecx");
#ifdef GCCUSESGAS
#define fCopyAL1(psrc, pdst, nl, srcR, widS, psrcS)     \
  fCopyAL1_X((psrc),(pdst),(nl),(srcR),(widS),(psrcS),  \
	"repe\n");
#else
#define fCopyAL1(psrc, pdst, nl, srcR, widS, psrcS)     \
  fCopyAL1_X((psrc),(pdst),(nl),(srcR),(widS),(psrcS),  \
	"repz\n");
#endif
#define fCopyAL2_X(psrc, pdst, nl, srcR, widS, psrcS, rep) \
  __asm__ volatile (".label10: orl %6, %6             \n\
                               jz .label12            \n\
                               movl %6, %%ecx         \n\
                               cmpl %%ecx, %2         \n\
                               ja .label11            \n\
                               movl %2, %%ecx         \n\
                     .label11: subl %%ecx, %6         \n\
                               subl %%ecx, %2         \n"\
			       ##rep##                  \
                               "movsl                 \n\
                               orl %2, %2             \n\
                               jnz .label10           \n\
                               movl %7, %2            \n\
                               movl %8, %0            \n\
                               jmp .label10           \n\
                     .label12:"                         \
		    : "=S" ((unsigned long *) (psrc)),  \
		      "=D" ((unsigned long *) (pdst)),  \
		      "=b" ((unsigned int) (srcR))      \
		    : "S0" ((unsigned long *) (psrc)),  \
		      "D1" ((unsigned long *) (pdst)),  \
                      "b2" ((unsigned int) (srcR)),     \
		      "d" ((unsigned int) (nl)),        \
		      "g" ((unsigned int) (widS)),      \
		      "g" ((unsigned long *) (psrcS))   \
		    : "ecx");
#ifdef GCCUSESGAS
#define fCopyAL2(psrc, pdst, nl, srcR, widS, psrcS)     \
  fCopyAL2_X((psrc),(pdst),(nl),(srcR),(widS),(psrcS),  \
	"repe\n");
#else
#define fCopyAL2(psrc, pdst, nl, srcR, widS, psrcS)     \
  fCopyAL2_X((psrc),(pdst),(nl),(srcR),(widS),(psrcS),  \
	"repz\n");
#endif
#define fCopyAL3_X(psrc, pdst, nl, srcR, widS, psrcS, rep) \
  __asm__ volatile (".label20: orl %6, %6             \n\
                               jz .label22            \n\
                               movl %6, %%ecx         \n\
                               cmpl %%ecx, %2         \n\
                               ja .label21            \n\
                               movl %2, %%ecx         \n\
                     .label21: subl %%ecx, %6         \n\
                               subl %%ecx, %2         \n"\
			       ##rep##                  \
                               "movsl                 \n\
                               orl %2, %2             \n\
                               jnz .label20           \n\
                               movl %7, %2            \n\
                               movl %8, %0            \n\
                               jmp .label20           \n\
                     .label22:"                         \
		    : "=S" ((unsigned long *) (psrc)),  \
		      "=D" ((unsigned long *) (pdst)),  \
		      "=b" ((unsigned int) (srcR))      \
		    : "S0" ((unsigned long *) (psrc)),  \
		      "D1" ((unsigned long *) (pdst)),  \
                      "b2" ((unsigned int) (srcR)),     \
		      "d" ((unsigned int) (nl)),        \
		      "g" ((unsigned int) (widS)),      \
		      "g" ((unsigned long *) (psrcS))   \
		    : "ecx");
#ifdef GCCUSESGAS
#define fCopyAL3(psrc, pdst, nl, srcR, widS, psrcS)     \
  fCopyAL3_X((psrc),(pdst),(nl),(srcR),(widS),(psrcS),  \
	"repe\n");
#else
#define fCopyAL3(psrc, pdst, nl, srcR, widS, psrcS)     \
  fCopyAL3_X((psrc),(pdst),(nl),(srcR),(widS),(psrcS),  \
	"repz\n");
#endif
#define fCopyUL(psrc, pdst, bits, lShift, rShift)      \
  __asm__ volatile ("           movl %4, %%edx         \n\
                                movl %5, %%ecx         \n\
                                shrl %%cl, %%edx       \n\
                                lodsl                  \n\
                                movl %%eax, %4         \n\
                                movl %6, %%ecx         \n\
                                shll %%cl, %%eax       \n\
                                orl %%edx, %%eax       \n\
                                stosl"                 \
		    : "=S" ((unsigned long *) (psrc)), \
		      "=D" ((unsigned long *) (pdst))  \
		    : "S0" ((unsigned long *) (psrc)), \
		      "D1" ((unsigned long *) (pdst)), \
		      "g" ((unsigned long) (bits)),    \
		      "g"  ((unsigned long) (lShift)), \
		      "g"  ((unsigned long) (rShift))  \
		    : "edx", "ecx", "eax");
#elif !defined(RROP)
#define fCopyAL1(psrc, pdst, nl, srcR, widS, psrcS)    \
  fCopyAL1_C((psrc),(pdst),(nl),(srcR),(widS),(psrcS))
#define fCopyAL2(psrc, pdst, nl, srcR, widS, psrcS)    \
  fCopyAL2_C((psrc),(pdst),(nl),(srcR),(widS),(psrcS))
#define fCopyAL3(psrc, pdst, nl, srcR, widS, psrcS)    \
  fCopyAL3_C((psrc),(pdst),(nl),(srcR),(widS),(psrcS))
#define fCopyUL(psrc, pdst, bits, lShift, rShift)      \
  fCopyUL_C((psrc),(pdst),(bits),(lShift),(rShift))
#define CLD
#endif  /* Mcopy */
#if RROP == GXcopy
#define CLD __asm__ volatile ("cld")
#define RROP_SOLID_L_X(pdst, nl, fill, rep) \
  __asm__ volatile ( rep##                  \
                     "stosl"                \
		    : "=D" ((unsigned long *) (pdst))  \
		    : "D0" ((unsigned long *) (pdst)), \
		      "c"  ((unsigned long) (nl)),     \
		      "a"  ((unsigned long) (fill)));
#ifdef GCCUSESGAS
#define RROP_SOLID_L(pdst, nl, fill)        \
  RROP_SOLID_L_X((pdst),(nl),(fill),"repe\n")
#else
#define RROP_SOLID_L(pdst, nl, fill)        \
  RROP_SOLID_L_X((pdst),(nl),(fill),"repz\n")
#endif
#define RROP_SPAN_STD(pdst, nlm, dummy)                         \
{                                                               \
  __label__ label1;                                             \
  if (!vgaWriteFlag) goto label1;                               \
  nl = min(nlm, (unsigned long *) vgaWriteTop -                 \
                                   (unsigned long *) pdst);     \
  nlm -= nl;                                                    \
  RROP_SOLID_L(pdst, nl, rrop_xor);                             \
  if (nlm) {                                                    \
    pdst = vgaReadWriteNext(pdst);                              \
  label1:                                                       \
    RROP_SOLID_L (pdst, nlm, rrop_xor);                         \
  }                                                             \
}
#elif !defined(MROP)
#define RROP_SOLID_L(pdst, nlm, label)                          \
  DuffL(nlm, label, RROP_SOLID(pdst); pdst++;)
#define RROP_SPAN_STD(pdst, nlm, dummy)                         \
{                                                               \
  __label__ label1, label2, label3;                             \
  if (!vgaWriteFlag) goto label1;                               \
  nl = min(nlm, (unsigned long *) vgaWriteTop -                 \
	                          (unsigned long *) pdst);      \
  nlm -= nl;                                                    \
  RROP_SOLID_L(pdst, nl, label2);                               \
  if (nlm) {                                                    \
    pdst = vgaReadWriteNext(pdst);                              \
  label1:                                                       \
    RROP_SOLID_L(pdst, nlm, label3);                            \
  }                                                             \
}
#endif

#else /* __GNUC__ > 1 */

#define ESI
#define EDI
#define CLD
#define fCopyAL1(psrc, pdst, nl, srcR, widS, psrcS)    \
  fCopyAL1_C((psrc),(pdst),(nl),(srcR),(widS),(psrcS))
#define fCopyAL2(psrc, pdst, nl, srcR, widS, psrcS)    \
  fCopyAL2_C((psrc),(pdst),(nl),(srcR),(widS),(psrcS))
#define fCopyAL3(psrc, pdst, nl, srcR, widS, psrcS)    \
  fCopyAL3_C((psrc),(pdst),(nl),(srcR),(widS),(psrcS))
#define fCopyUL(psrc, pdst, bits, lShift, rShift)      \
  fCopyUL_C((psrc),(pdst),(bits),(lShift),(rShift))

#define RROP_SOLID_L(pdst, nlm, label)                          \
  DuffL(nlm, label, RROP_SOLID(pdst); pdst++;)
#define RROP_SPAN_STD(pdst, nlm, dummy)                         \
{                                                               \
  if (!vgaWriteFlag) goto RROP_NAME_CAT(_X_l1,dummy);           \
  nl = min(nlm, (unsigned long *) vgaWriteTop -                 \
	                          (unsigned long *) pdst);      \
  nlm -= nl;                                                    \
  RROP_SOLID_L(pdst, nl, RROP_NAME_CAT(_X_l2,dummy));           \
  if (nlm) {                                                    \
    pdst = vgaReadWriteNext(pdst);                              \
  RROP_NAME_CAT(_X_l1,dummy):                                   \
    RROP_SOLID_L(pdst, nlm, RROP_NAME_CAT(_X_l3,dummy));     \
  }                                                             \
}

#endif  /* __GNUC__ > 1 */

