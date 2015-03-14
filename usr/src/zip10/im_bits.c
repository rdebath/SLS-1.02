/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  im_bits.c by Richard B. Wales & Jean-loup Gailly.
 *
 *  PURPOSE
 *
 *      Output variable-length bit strings.
 *
 *  DISCUSSION
 *
 *      The PKZIP "imploded" file format interprets compressed file data
 *      as a sequence of bits.  Multi-bit strings in the file may cross
 *      byte boundaries without restriction.
 *
 *      The first bit of each byte is the low-order bit.
 *
 *      The routines in this file allow a variable-length bit value to
 *      be output right-to-left (useful for literal values). For
 *      left-to-right output (useful for code strings from the tree routines),
 *      the bits must have been reversed first with bi_reverse().
 *
 *  INTERFACE
 *
 *      ImpErr bi_init (FILE *fp)
 *          Initialize the bit string routines and specify the output
 *          file to be written to in subsequent calls.
 *
 *      ImpErr bi_rlout (int value, int length)
 *          Write out a bit string, taking the source bits right to
 *          left.
 *
 *      int bi_reverse (int value, int length)
 *          Reverse the bits of a bit string, taking the source bits left to
 *          right and emitting them right to left.
 *
 *      ImpErr bi_windup (void)
 *          Write out any remaining bits in an incomplete byte.
 */


#include "implode.h"


/***********************************************************************
 *
 * Local data used by the "bit string" routines.
 */


/* Current file stream pointer. */
local   FILE *  bi_fp;

local unsigned short bi_buf;
/* Output buffer. bits are inserted starting at the bottom (least significant
 * bits).
 */

#define Buf_size (8 * 2*sizeof(char))
/* Number of bits used within bi_buf. (bi_buf might be implemented on
 * more than 16 bits on some systems.)
 */

local int bi_valid;                  /* number of valid bits in bi_buf */
/* All bits above the last valid bit are always zero.
 */

/* Output a 16 bit value to the bit stream, lower (oldest) byte first */
#define PUTSHORT(w) \
{  (void) zputc ((char)((w) & 0xff), bi_fp); \
   (void) zputc ((char)((US_INT)(w) >> 8), bi_fp); \
   if (ferror (bi_fp)) return IM_IOERR; \
}

/* Output an 8 bit value to the bit stream, bits right to left */
#define PUTBYTE(w) \
{  (void) zputc ((char)((w) & 0xff), bi_fp); \
   if (ferror (bi_fp)) return IM_IOERR; \
}

/***********************************************************************
 *
 * Initialize the bit string routines.
 */

ImpErr
bi_init (fp)
    FILE *fp;
{   if (fp == NULL)
    {   fprintf (stderr, "\nError in bi_init: null file pointer");
        return IM_LOGICERR;
    }
    bi_fp   = fp;
    bi_buf = 0;
    bi_valid = 0;
    return IM_OK;
}


/***********************************************************************
 *
 * Output bits from right to left.
 */

ImpErr
bi_rlout (value, length)
    int value;
    int length; /* must be <= 16 */
{
    /* Send value on length bits. If not enough room in bi_buf, use
     * (valid) bits from bi_buf and (16 - bi_valid) bits from value, leaving
     * (width - (16-bi_valid)) unused bits in value.
     */
    if (bi_valid > Buf_size - length) {
        bi_buf |= (value << bi_valid);
        PUTSHORT(bi_buf);
        bi_buf = (unsigned short)value >> (Buf_size - bi_valid);
        bi_valid += length - Buf_size;
    } else {
        bi_buf |= value << bi_valid;
        bi_valid += length;
    }
#ifdef  IMPDEBUG
    fprintf (stderr, " / ");
    while (length-- > 0)
    {
        putc ((value & 1) ? '1' : '0', stderr);
        value >>= 1;
    }
#endif  /* IMPDEBUG */
    return IM_OK;
}


/***********************************************************************
 *
 * Reverse the bits of a bit string, taking the source bits left to
 * right (starting at 2^15) and emitting them right to left.
 */

int
bi_reverse (value, length)
    int value;
    int length;
{
    int result = 0;
    unsigned short lbit = 0x8000;
    unsigned short rbit = 1;
    while (length-- > 0) {
       if (value & lbit) result |= rbit;
       lbit >>= 1, rbit <<= 1;
    }
    return result;
}


/***********************************************************************
 *
 * Flush any remaining bits.
 */

ImpErr
bi_windup ()
{
    if (bi_valid > 8) {
        PUTSHORT(bi_buf);
    } else if (bi_valid > 0) {
        PUTBYTE(bi_buf);
    }
    bi_buf = 0;
    bi_valid = 0;
    return IM_OK;
}


/**********************************************************************/
