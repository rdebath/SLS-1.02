/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include <stdlib.h>
#include <stdio.h>
#include "bitblit.h"

/*
	Read a bitmap.
	Given an open FILE pointer to an file containing a bitmap,
	read the header, malloc() a bitmap, and return the pointer to the
	bitmap.
*/

extern int bm_compressed;

BITMAP *
bitmapread( fp )
register FILE	*fp;
{
	BITMAP		*bp = 0;
	register char	*datap;
	int		h, w, d;	/* height, width, depth of bitmap */
	int		sizefile1;	/* the size of 1 line of the bitmap
					as stored in a file, in bytes */
	int		sizemem1;	/* the size of 1 line of the bitmap
					as stored in memory, in bytes */
	long		size1diff = 0;	/* if the file padding is greater than
					the memory padding, the difference in
					bytes */

	if( bitmaphead( fp, &w , &h, &d, &sizefile1 ) ) {
		sizemem1 = bit_size(w, 1, d);
		if( sizefile1 > sizemem1 ) {
			size1diff = sizefile1 - sizemem1;
			sizefile1 = sizemem1;
		}
		if( !(bp = bit_alloc(w, h, NULL_DATA, d)) )
			return  0;
		datap = (char *)BIT_DATA(bp);
#ifdef MOVIE
		SET_DIRTY(bp);
#endif

		/*	The bytes of the bitmap data in the file may have
			different alignments than the bitmap data in memory.
			We read one line at a time in such a way as to get
			the memory alignment needed.
		*/
		if (bm_compressed==1)
			{
			while (h-- > 0)
				{
				if (b_compget(datap, sizefile1, 1, fp)!=1)
					{ free ( (char *)bp ); return(0);}
				if (size1diff)
					b_compget(datap, size1diff, 1, fp);
				datap += sizemem1;
				}
			}
		else
		while( h-- > 0 ) {
			if( fread( datap, sizefile1, 1, fp ) != 1 ) {
				free( (char *)bp );
				return  0;
			}
			if( size1diff ) {
				fseek( fp, size1diff, 1 );
			}
			datap += sizemem1;
		}
	}
	return  bp;
}

static int b_compget (datap, bcount, bct1, fp)
	char *datap;
	FILE *fp;
{/* reads bcount*bct1 compressed bytes, into datap, from file fp */
/* byte runlength  encoding scheme  is easy to
compact/uncompact.  Something like:
<control_byte>[data]<control_byte>[data]...
where:
  0<control_byte<128: repeat the next byte <control_byte> times
  128<=contropl_byte<256: the next (control_byte-127) bytes are sent
"as is".  */
/* magic number is 'y' 'x' instead of 'y' 'z' */

register int kb, c;
static int repn=0, cr=0;
kb=bcount*bct1;

while (kb-- >0)
	{
	if (repn==0)
		{
		repn = getc(fp);
		if (repn==EOF)
			{
			repn=0;
			return(0);
			}
		repn&=0377;
		if (repn<128)
			cr=getc(fp);
		}
	if (repn>=128)
		{
		*datap++ = c =getc(fp);
		if (--repn == 127) repn=0;
		if (c==EOF)
			{repn=0; return(0);}
		}
	else
		{
		*datap++= cr;
		repn--;
		}
	}
return(1);
}


/*
	Write a bitmap.
	Given an open FILE pointer to an file and a pointer to a bitmap,
	write the header and the bitmap.  Return 0 on failure, positive on
	success.
*/
int
bitmapwrite( fp, bp, flag )
register FILE	*fp;
BITMAP		*bp;
int flag;			/* 1->8 bit (new), 0->16 bit (old) */

{
	register char	*datap;
	register int	w, h, d;
	struct b_header	head;
	struct old_b_header	old_head;

	int		sizefile1;	/* the size of 1 line of the bitmap
					as stored in a file, in bytes */
	int		sizemem1;	/* the size of 1 line of the bitmap
					as stored in memory, in bytes */

	w = BIT_WIDE(bp);
	h = BIT_HIGH(bp);
	d = BIT_DEPTH(bp);
	switch(flag) {
	case NEW_BHDR:
		B_PUTHDR8( &head, w, h, d );
		sizefile1 = B_SIZE8(w, 1, d);
	   if( fwrite( (char *)&head, sizeof head, 1, fp ) != 1 )
		   return  0;
		break;
	case OLD_BHDR:
		B_PUTOLDHDR( &old_head, w, h );
		sizefile1 = B_SIZE16(w, 1, d);
	   if( fwrite( (char *)&old_head, sizeof old_head, 1, fp ) != 1 )
		   return  0;
		break;
		}

	sizemem1 = bit_size(w, 1, d);
	datap = (char *)BIT_DATA(bp);
	while( h-- > 0 ) {
		if( fwrite( datap, sizefile1, 1, fp ) != 1 )
			return  0;
		datap += sizemem1;
	}
	return(1);
}
