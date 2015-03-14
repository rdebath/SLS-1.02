/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include "screen.h"

/*	Return "true" (one) if the bit at the given x,y position
	is set in the given bitmap.
	Return "false" (zero) if that bit is not set or if the x,y is outside
	the bitmap.
*/

int bit_on( bp, x, y ) register BITMAP	*bp; int x, y;
{
	register int	mask = 1 << (7 - x % 8);

	register char	*ip;

	if( x < 0 || x >= BIT_WIDE(bp) || y < 0 ||  y >= BIT_HIGH(bp) )
		return  0;
	ip = (char *) BIT_DATA( bp ) + y * BIT_LINE(bp) + (x >> 3);
#ifdef NEED_ADJUST
	return  ((IS_SCREEN(bp) ? *(adjust(ip)) : *ip) & mask) != 0;
#else
	return  (*ip & mask) != 0;
#endif
}
