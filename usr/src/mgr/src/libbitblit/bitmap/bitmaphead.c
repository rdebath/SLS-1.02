/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include	<stdio.h>

#include	"bitblit.h"

/*	Read the header of a bitmap (aka icon) file using the given FILE
	pointer, fp.
	Return 0 if the file isn't in bitmap format or is unreadable.
	Otherwise, return "true" (non-zero) and populate the integers
	pointed at by:
		wp	width of the bitmap in bits
		hp	height of bitmap in bits
		dp	depth of bitmap in bits
		size1p	number of bytes in a single line (including padding)
*/

int bm_compressed=0;
	
int
bitmaphead( fp, wp, hp, dp, size1p )
FILE	*fp;
int	*wp, *hp, *dp, *size1p;
{
	struct b_header	head;
	
	if( fread( (char *)&head, sizeof(struct old_b_header), 1, fp ) != 1 )
		return  0;
	if( BS_ISHDR( &head ) ) /* compressed bitmaps */
		{
		bm_compressed=1;
		/* fprintf(stderr,"Got compressed header\n"); */
		head.magic[1]='z';
		}
	else {
		bm_compressed=0;
		}
	if( B_ISHDR8( &head ) ) {	/* modern, self-describing
					bitmap, 8-bit alignment */
		if( fread( &head.depth, sizeof head - sizeof(struct old_b_header), 1, fp ) != 1 )
			return  0;
		B_GETHDR8( &head, *wp, *hp, *dp );
		*size1p = B_SIZE8(*wp, 1, *dp);
	}
	else if( B_ISHDR32( &head ) ) {	/* 1 bit deep, 32 bits align */
		B_GETOLDHDR( &head, *wp, *hp );
		*size1p = B_SIZE32(*wp, 1, 1);
		*dp = 1;
	}
	else if ( B_ISHDR16(&head) ) {	/* 1 bit deep, 16 bits align */
		B_GETOLDHDR( &head, *wp, *hp );
		*size1p = B_SIZE16(*wp, 1, 1);
		*dp = 1;
	}
	else if ( B8_ISHDR(&head) ) {	/* 8 bits deep, 16 bits align */
		B_GETOLDHDR( &head, *wp, *hp );
		*size1p = B8_SIZE(*wp, 1);
		*dp = 8;
	}
	else {
		return  0;
	}
	/* fprintf(stderr,"In bitmaphead format %d\n",bm_compressed); */
	return  1;
}
