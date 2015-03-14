# ifndef yyGeneral
# define yyGeneral

/* $Id: General.h,v 1.5 1992/08/07 14:36:51 grosch rel $ */

/* $Log: General.h,v $
 * Revision 1.5  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.4  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  91/07/17  17:23:06  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.2  90/09/04  17:32:09  grosch
 * automatic determination of alignment
 * 
 * Revision 1.1  90/07/04  14:33:54  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:37  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# include "ratc.h"

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

# define Min(a,b) ((a <= b) ? a : b)
			/* Returns the minimum of 'a' and 'b'.		*/
# define Max(a,b) ((a >= b) ? a : b)
			/* Returns the maximum of 'a' and 'b'.		*/

extern cardinal		Log2 ARGS((register unsigned long x));
			/* Returns the logarithm to the base 2 of 'x'.	*/
extern unsigned long	Exp2 ARGS((register cardinal x));
			/* Returns 2 to the power of 'x'.		*/

extern short	yyMaxAlign;
extern long	yyAlignMasks [];

# endif
