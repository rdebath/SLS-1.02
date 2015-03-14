#ifndef _MGR_BLITLIB_H
#define _MGR_BLITLIB_H

#include "bitmap.h"
#include "window.h"

/*
 * The macro "GET Most Significant Bits" defines how the bits in each
 * word map from memory to pixels on the display.  The top left most
 * pixel on the display comes from either the *high* order or *low* order
 * bit of the first frame buffer word.  Use "<<" in the first case, ">>"
 * in the second.
 * 
 * The macro "GET Least Significant Bits" does the inverse of GETMSB
 */

#define GETMSB(word,shift)	\
	(word << shift)					/* get most significant bits in word */
#define GETLSB(word,shift) \
	(word >> shift)					/* get least significant bits in word */

/* these probably won't need changing */

#define MSB		(~GETLSB((DATA)~0,1))	/* most sig bit set */
#define LSB		(~GETMSB((DATA)~0,1))	/* least sig bit set */

/*
 * bitmap data has 2 formats, an internal format and an external format.
 * (Sometimes the formats are the same).  The external format is native
 * 68020 SUN/3, DATA aligned 1=black, 0=white.  The internal format is
 * whatever the frame buffer is.  If DOFLIP is set, data is converted
 * from external to internal format the first time it is used.  Bitmap
 * data is recognized as being in external format if the _FLIP flag is
 * set in the type field.  The installation routine flip() does the
 * conversion.
 */

/* need to flip bytes */

#define DOFLIP (MSB==1)

/* Function declarations */

#ifdef __STDC__
BITMAP *bit_alloc(int wide, int high, DATA *data, int depth);
void bit_blit(BITMAP *dst, int dx, int dy, int width, int height, int func, BITMAP *src, int sx, int sy);
BITMAP *bit_create(BITMAP *map, int x, int y, int wide, int high);
void bit_destroy(BITMAP *map);
void bit_line(BITMAP *dst, int x0, int y0, int x1, int y1, int func);
int bit_on(BITMAP *bp, int x, int y);
BITMAP *bit_open(char *name);
int bit_point(BITMAP *map, int dx, int dy, int func);
int bit_size(int wide, int high, int depth);
void fast_scroll(BITMAP *map, int x, int y, int wide, int high, int delta);
int timestamp(void);
#else
extern BITMAP *bit_alloc();
extern void bit_blit();
extern BITMAP *bit_create();
extern void bit_destroy();
extern void bit_line();
extern int bit_on();
extern BITMAP *bit_open();
extern int bit_point();
extern int bit_size();
extern void fast_scroll();
extern int timestamp();
#endif

#endif
/*{{{}}}*/
