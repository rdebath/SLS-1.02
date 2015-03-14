/* shrink an 8-bit bitmap into a 1 bit bitmap */
/* only works for primary bitmaps for now */
/* assumes 32 bit data, 8 bits per pixel */

#include <stdio.h>
#include "bitmap.h"

BITMAP *
shrink(map,color)
BITMAP *map;		/* bitmap to shrink */
DATA color;			/* color to use as background - all else is on! */
	{
	BITMAP *result = bit_alloc(BIT_WIDE(map),BIT_HIGH(map),NULL,1);
	DATA *src = BIT_DATA(map);
	DATA *dst = BIT_DATA(result);
	DATA *end = src + BIT_LINE(map)*BIT_HIGH(map);
	DATA dbits = ~GETLSB((unsigned)~0,1);	/* most sig pixel */
	DATA sbits = ~GETLSB((unsigned)~0,8);	/* most sig pixel */

	color = color | color<<8 | color<<16 | color<<24;
	
	while(src<end) {
		if ((*src ^ color) & sbits)
			*dst |= bit;
		if (!(dbits=GETLSB(bit,1))) {
			dbits = ~GETLSB((unsigned)~0,1);
			dst++;
			}
		if (!(sbits=GETLSB(bit,8))) {
			sbits = ~GETLSB((unsigned)~0,8);
			src++;
			}
		}
	return(result);
	}
