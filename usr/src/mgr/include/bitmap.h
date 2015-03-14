/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* format for bitmaps in files and memory */

#ifndef _MGR_BITMAP_H
#define _MGR_BITMAP_H

#include <stdio.h>

#define NEW_BHDR	1	/* flag for bitmapwrite */
#define OLD_BHDR	0	/* " */

/* given bitmap header, get w[idth] and h[eight] */

#define B_GETOLDHDR(hdr,W,H) ( \
	W = ((int)((hdr)->h_wide - ' ') << 6) + (hdr)->l_wide - ' ', \
	H = ((int)((hdr)->h_high - ' ') << 6) + (hdr)->l_high - ' ')

#define B_GETHDR8(hdr,W,H,D) ( \
	W = ((int)((hdr)->h_wide - ' ') << 6) + (hdr)->l_wide - ' ', \
	H = ((int)((hdr)->h_high - ' ') << 6) + (hdr)->l_high - ' ', \
	D = ((int)((hdr)->depth - ' ')))

/* given w[idth] and h[eight], produce header */

#define B_PUTOLDHDR(hdr,w,h) \
	(hdr)->magic[0]='z', (hdr)->magic[1]='z', \
	(hdr)->h_wide = (((w)>>6)&0x3f) + ' ', \
	(hdr)->l_wide = ((w)&0x3f) + ' ', \
	(hdr)->h_high = (((h)>>6)&0x3f) + ' ', \
	(hdr)->l_high = ((h)&0x3f) + ' '

#define B8_PUTOLDHDR(hdr,w,h) \
	(hdr)->magic[0]='z', (hdr)->magic[1]='y', \
	(hdr)->h_wide = (((w)>>6)&0x3f) + ' ', \
	(hdr)->l_wide = ((w)&0x3f) + ' ', \
	(hdr)->h_high = (((h)>>6)&0x3f) + ' ', \
	(hdr)->l_high = ((h)&0x3f) + ' '

#define B_PUTHDR8(hdr,w,h,d) ( \
	(hdr)->magic[0]='y', (hdr)->magic[1]='z', \
	(hdr)->h_wide = (((w)>>6)&0x3f) + ' ', \
	(hdr)->l_wide = ((w)&0x3f) + ' ', \
	(hdr)->h_high = (((h)>>6)&0x3f) + ' ', \
	(hdr)->l_high = ((h)&0x3f) + ' ', \
	(hdr)->depth = ((d)&0x3f) + ' ', \
	(hdr)->_reserved = ' ' )


/*	Bitmap header magic numbers for new style bitmaps.
	The formats differ only in the amount of padding required at the end
	of each row.
*/

#define B_ISHDR8(hdr) \
	((hdr)->magic[0]=='y' && (hdr)->magic[1]=='z')

#define B_ISHDR16(hdr) \
	((hdr)->magic[0]=='z' && (hdr)->magic[1]=='z')

#define B_ISHDR32(hdr) \
	((hdr)->magic[0]=='x' && (hdr)->magic[1]=='z')

#define B_ISHDR(hdr)	B_ISHDR32(hdr)

/* Old 8 bit per pixel bitmaps */

#define B8_ISHDR(hdr) \
	((hdr)->magic[0]=='x' && (hdr)->magic[1]=='y')

/* squished bitmaps */

#define BS_ISHDR(hdr) \
	((hdr)->magic[0]=='y' && (hdr)->magic[1]=='x')

/* number of bytes of data for bitmap */

#define B_SIZE8(w,h,d)	((h)*((((w*d)+7L)&~7L)>>3))
#define B_SIZE16(w,h,d)	((h)*((((w*d)+15L)&~15L)>>3))
#define B_SIZE32(w,h,d)	((h)*((((w*d)+31L)&~31L)>>3))
#define B_SIZE(w,h)	((h)*((((w)+BITS)&~BITS)>>3))
#define B8_SIZE(w,h)	((h)*(w))

struct old_b_header {
   char magic[2];
   char h_wide;
   char l_wide;
   char h_high;
   char l_high;
   };

struct b_header {
   char magic[2];
   char h_wide;
   char l_wide;
   char h_high;
   char l_high;
   char depth;
   char _reserved;	/* to pad b_header out to 8 bytes, which should be an
			exact alignment on any machine we are likely to
			encounter */
   };

/* basic frame buffer word size */
#ifndef DATA
#define DATA void
#endif

/* NULL bitmap data */
#define NULL_DATA	((DATA *) 0)

/* NULL bitmap pointer */
#define BIT_NULL	((BITMAP *) 0)

/* frame buffer */
#define _SCREEN		1

/* malloc'd space */
#define _MEMORY		2

/* don't free space at destroy time */
#define _STATIC		3

/* data is in external format */
#define _FLIP		4

/* data is "dirty" */
#define _DIRTY          8

/* member access macros */

#define IS_SCREEN(x)	((3&(x)->type)==_SCREEN)	/* bitmap is on the display */
#define IS_MEMORY(x)	((3&(x)->type)==_MEMORY)	/* bitmap space malloc'd */
#define IS_STATIC(x)	((3&(x)->type)==_STATIC)	/* bitmap space is static */
#define IS_PRIMARY(x)	((x)->primary == (x))
#define SET_FLIP(x)     ((x)->primary->type |= DOFLIP ? _FLIP : 0)

#define BIT_X(x)	x->x0
#define BIT_Y(x)	x->y0
#define BIT_DATA(x)	x->data
#define BIT_WIDE(x)	x->wide
#define BIT_HIGH(x)	x->high
#define BIT_DEPTH(x)	((int)x->depth)
#define BIT_CACHE(x)    x->primary->cache
#define BIT_CHCLR(x)    x->primary->color

#define SET_DIRTY(x) (bit_destroy(BIT_CACHE(x)),BIT_CACHE(x)=NULL)

/* structure and type definitions */

typedef struct bitmap
{
  DATA *data;              /* bitmap data */
  struct bitmap	*primary;  /* pointer to primary bitmap (server only) */
  short x0, y0;            /* starting coordinates, in bits */
  short wide, high;        /* bitmap size, in bits */
  char depth;              /* bitmap depth */
  char type;               /* bitmap type (server only) */
  struct bitmap *cache;    /* cached 8 bit map for monochrome images (server only) */
  unsigned short color;    /* cached color (op>>4) */
  unsigned short int id;   /* bitmap ID for movie mgr */
} BITMAP;

/* Macro to declare a 1 bit per pixel static bitmap */
#define bit_static(name,wide,high,data,n,id) \
BITMAP name = { (DATA *) data, &name, 0, 0, wide, high, n, _STATIC, 0L, 0, id }

#ifdef __STDC__
int bitmaphead(FILE *fp, int *wp, int *hp, int *dp, int *size1p);
BITMAP *bitmapread(FILE *fp);
int bitmapwrite(FILE *fp, BITMAP *bp, int flag);
#else
extern int bitmaphead();
extern BITMAP *bitmapread();
extern int bitmapwrite();
#endif

#endif
/*{{{}}}*/
