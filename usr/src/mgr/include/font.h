#ifndef _MGR_FONT_H
#define _MGR_FONT_H

/* Constant spaced font format */

#define FONT_X '\026' /* fixed width fonts 16 bit alignment (obsolete) */
#define FONT_S '\027' /* fixed width static 32 bit alignment */
#define FONT_A '\030' /* fixed width fonts 32 bit alignment*/
#define FONT_P '\031' /* (not used yet) proportional fonts */

struct font_header
{
   unsigned char type;		/* font type */
   unsigned char wide;		/* character width */
   unsigned char high;		/* char height */
   unsigned char baseline;	/* pixels from bottom */
   unsigned char count;		/* number of chars in font */
   char          start;		/* starting char in font */
};

#endif
/*{{{}}}*/
