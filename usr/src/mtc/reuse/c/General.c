/* $Id: General.c,v 1.5 1992/05/05 13:19:05 grosch rel $ */

/* $Log: General.c,v $
 * Revision 1.5  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.4  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  90/09/04  17:32:08  grosch
 * automatic determination of alignment
 * 
 * Revision 1.1  90/07/04  14:33:53  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:37  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: General.c,v 1.5 1992/05/05 13:19:05 grosch rel $";

# include "ratc.h"
# include "General.h"

cardinal Log2		/* Returns the logarithm to the base 2 of 'x'.	*/
# ifdef __STDC__
   (register unsigned long x)
# else
   (x) register unsigned long x;
# endif
   {
      register cardinal y = 0;

      if (x >= 65536) { y += 16; x >>= 16; }
      if (x >=   256) { y +=  8; x >>=  8; }
      if (x >=    16) { y +=  4; x >>=  4; }
      if (x >=     4) { y +=  2; x >>=  2; }
      if (x >=     2) { y +=  1; x >>=  1; }
      return y;
   }

unsigned long Exp2	/* Returns 2 to the power of 'x'.		*/
# ifdef __STDC__
   (register cardinal x)
# else
   (x) register cardinal x;
# endif
   {
      register long y = 1;

      if (x >= 16) { x -= 16; y <<= 16; }
      if (x >=  8) { x -=  8; y <<=  8; }
      if (x >=  4) { x -=  4; y <<=  4; }
      if (x >=  2) { x -=  2; y <<=  2; }
      if (x >=  1) { x -=  1; y <<=  1; }
      return y;
   }

static struct { char yychar; double yydouble; } yyForAlign;
short	yyMaxAlign	= sizeof (yyForAlign) - sizeof (double);
long	yyAlignMasks []	= { 0,
   0xffffffff, 0xfffffffe, 0xffffffff, 0xfffffffc,
   0xffffffff, 0xffffffff, 0xffffffff, 0xfffffff8 };
