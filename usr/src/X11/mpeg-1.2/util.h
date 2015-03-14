/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* Status codes for bit stream i/o operations. */

#define NO_VID_STREAM -1
#define UNDERFLOW -2
#define OK 1

/* Size increment of extension data buffers. */

#define EXT_BUF_SIZE 1024

/* External declarations for bitstream i/o operations. */
extern unsigned int bitMask[];
extern unsigned int nBitMask[];
extern unsigned int rBitMask[];
extern unsigned int bitTest[];
extern int next_bits();
extern char *get_ext_data();
extern int next_start_code();
extern char *get_extra_bit_info();

/* External declarations of bitstream global variables. */
extern unsigned int curBits;
extern int bitOffset;
extern int bufLength;
extern unsigned int *bitBuffer;

/* Macro for updating bit counter if analysis tool is on. */
#ifdef ANALYSIS
#define UPDATE_COUNT(numbits) bitCount += numbits
#else
#define UPDATE_COUNT(numbits)
#endif

#ifdef NO_SANITY_CHECKS
#define get_bits1(result)                                                 \
{                                                                         \
  UPDATE_COUNT(1);                                                        \
  if (bitOffset < 31) {                                                   \
    *(result) = ((curBits & bitTest[bitOffset]) != 0);                    \
    bitOffset++;                                                          \
  }                                                                       \
  else {                                                                  \
    *(result) = (curBits & 0x00000001);                                   \
    bitOffset = 0;                                                        \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
  }                                                                       \
}

#define get_bits2(result)                                                 \
{                                                                         \
  UPDATE_COUNT(2)                                                         \
  if (bitOffset < 30) {                                                   \
    *(result) = ((curBits & bitMask[bitOffset]) >>                        \
	       (30-bitOffset));                                           \
    bitOffset += 2;                                                       \
  }                                                                       \
  else if (bitOffset == 30) {                                             \
    *(result) = (curBits & 0x00000003);                                   \
    bitOffset = 0;                                                        \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
  }                                                                       \
  else {                                                                  \
    *(result) = (((curBits & 0x01) << 1) |                                \
	       ((*(bitBuffer+1) & 0x80000000) != 0));                     \
    bitOffset = 1;                                                        \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
  }                                                                       \
}

#define get_bitsX(num, thresh, result)                                    \
{                                                                         \
  int OFFnum = (bitOffset - thresh);                                      \
                                                                          \
  UPDATE_COUNT(num);                                                      \
                                                                          \
  if (OFFnum < 0) {                                                       \
    *(result) = ((curBits & bitMask[bitOffset]) >> (-OFFnum));            \
    bitOffset += num;                                                     \
  } else if (OFFnum == 0) {                                               \
    *(result) = (curBits & bitMask[bitOffset]) << OFFnum;                 \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
    bitOffset = 0;                                                        \
  } else {                                                                \
    *(result) = (((curBits & bitMask[bitOffset]) << OFFnum) |             \
	         ((*(bitBuffer+1) & (nBitMask[OFFnum])) >> (32-OFFnum))); \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
    bitOffset = OFFnum;                                                   \
  }                                                                       \
}
#else

#define get_bits1(result)                                                 \
{                                                                         \
                                                                          \
  /* Check for underflow. */                                              \
                                                                          \
  if (bufLength < 2) {                                                    \
    correct_underflow();                                                  \
  }                                                                       \
  UPDATE_COUNT(1);                                                      \
                                                                          \
  if (bitOffset < 31) {                                                   \
    *(result) = ((curBits & bitTest[bitOffset]) != 0);                    \
    bitOffset++;                                                          \
  }                                                                       \
  else {                                                                  \
    *(result) = (curBits & 0x00000001);                                   \
    bitOffset = 0;                                                        \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
  }                                                                       \
}

#define get_bits2(result)                                                 \
{                                                                         \
                                                                          \
  /* Check for underflow. */                                              \
                                                                          \
  if (bufLength < 2) {                                                    \
    correct_underflow();                                                  \
  }                                                                       \
  UPDATE_COUNT(2);                                                      \
                                                                          \
  if (bitOffset < 30) {                                                   \
    *(result) = ((curBits & bitMask[bitOffset]) >>                        \
	       (30-bitOffset));                                           \
    bitOffset += 2;                                                       \
  }                                                                       \
  else if (bitOffset == 30) {                                             \
    *(result) = (curBits & 0x00000003);                                   \
    bitOffset = 0;                                                        \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
  }                                                                       \
  else {                                                                  \
    *(result) = (((curBits & 0x01) << 1) |                                \
	       ((*(bitBuffer+1) & 0x80000000) != 0));                     \
    bitOffset = 1;                                                        \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
  }                                                                       \
}

#define get_bitsX(num, thresh, result)                                    \
{                                                                         \
  int OFFnum = (bitOffset - thresh);                                      \
                                                                          \
  /* Check for underflow. */                                              \
                                                                          \
  if (bufLength < 2) {                                                    \
    correct_underflow();                                                  \
  }                                                                       \
  UPDATE_COUNT(num);                                                      \
                                                                          \
  if (OFFnum < 0) {                                                       \
    *(result) = ((curBits & bitMask[bitOffset]) >> (-OFFnum));            \
    bitOffset += num;                                                     \
  } else if (OFFnum == 0) {                                               \
    *(result) = (curBits & bitMask[bitOffset]) << OFFnum;                 \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
    bitOffset = 0;                                                        \
  } else {                                                                \
    *(result) = (((curBits & bitMask[bitOffset]) << OFFnum) |             \
	         ((*(bitBuffer+1) & (nBitMask[OFFnum])) >> (32-OFFnum))); \
    bitBuffer++;                                                          \
    curBits = *bitBuffer;                                                 \
    bufLength--;                                                          \
    bitOffset = OFFnum;                                                   \
  }                                                                       \
}
#endif

#define get_bits3(result) get_bitsX(3, 29, result)
#define get_bits4(result) get_bitsX(4, 28, result)
#define get_bits5(result) get_bitsX(5, 27, result)
#define get_bits6(result) get_bitsX(6, 26, result)
#define get_bits7(result) get_bitsX(7, 25, result)
#define get_bits8(result) get_bitsX(8, 24, result)
#define get_bits9(result) get_bitsX(9, 23, result)
#define get_bits10(result) get_bitsX(10, 22, result)
#define get_bits11(result) get_bitsX(11, 21, result)
#define get_bits12(result) get_bitsX(12, 20, result)
#define get_bits16(result) get_bitsX(16, 16, result)
#define get_bits18(result) get_bitsX(18, 14, result)
#define get_bits32(result) get_bitsX(32, 0, result)

#define get_bitsn(num, result) get_bitsX((num),(32-(num)),result)

#ifdef NO_SANITY_CHECKS
#define show_bitsX(num, thresh, result)                                    \
{                                                                          \
  int OFFnum = (bitOffset - thresh);                                       \
                                                                          \
  if (OFFnum <= 0) {                                                        \
    *(result) = ((curBits & bitMask[bitOffset]) >> (-OFFnum));             \
  } else {                                                                 \
    *(result) = (((curBits & bitMask[bitOffset]) << OFFnum) |              \
	         ((*(bitBuffer+1) & (nBitMask[OFFnum])) >> (32-OFFnum)));  \
  }                                                                        \
}
#else
#define show_bitsX(num, thresh, result)                                    \
{                                                                          \
  int OFFnum = (bitOffset - thresh);                                       \
                                                                           \
  /* Check for underflow. */                                               \
  if (bufLength < 2) {                                                     \
    correct_underflow();                                                   \
  }                                                                        \
                                                                           \
  if (OFFnum <= 0) {                                                        \
    *(result) = ((curBits & bitMask[bitOffset]) >> (-OFFnum));             \
  } else {                                                                 \
    *(result) = (((curBits & bitMask[bitOffset]) << OFFnum) |              \
	         ((*(bitBuffer+1) & (nBitMask[OFFnum])) >> (32-OFFnum)));  \
  }                                                                        \
}
#endif

#define show_bits1(result)  show_bitsX(1,  31, result)
#define show_bits2(result)  show_bitsX(2,  30, result)
#define show_bits3(result)  show_bitsX(3,  29, result)
#define show_bits4(result)  show_bitsX(4,  28, result)
#define show_bits5(result)  show_bitsX(5,  27, result)
#define show_bits6(result)  show_bitsX(6,  26, result)
#define show_bits7(result)  show_bitsX(7,  25, result)
#define show_bits8(result)  show_bitsX(8,  24, result)
#define show_bits9(result)  show_bitsX(9,  23, result)
#define show_bits10(result) show_bitsX(10, 22, result)
#define show_bits11(result) show_bitsX(11, 21, result)
#define show_bits12(result) show_bitsX(12, 20, result)
#define show_bits13(result) show_bitsX(13, 19, result)
#define show_bits14(result) show_bitsX(14, 18, result)
#define show_bits15(result) show_bitsX(15, 17, result)
#define show_bits16(result) show_bitsX(16, 16, result)
#define show_bits17(result) show_bitsX(17, 15, result)
#define show_bits18(result) show_bitsX(18, 14, result)
#define show_bits19(result) show_bitsX(19, 13, result)
#define show_bits20(result) show_bitsX(20, 12, result)
#define show_bits21(result) show_bitsX(21, 11, result)
#define show_bits22(result) show_bitsX(22, 10, result)
#define show_bits23(result) show_bitsX(23,  9, result)
#define show_bits24(result) show_bitsX(24,  8, result)
#define show_bits25(result) show_bitsX(25,  7, result)
#define show_bits26(result) show_bitsX(26,  6, result)
#define show_bits27(result) show_bitsX(27,  5, result)
#define show_bits28(result) show_bitsX(28,  4, result)
#define show_bits29(result) show_bitsX(29,  3, result)
#define show_bits30(result) show_bitsX(30,  2, result)
#define show_bits31(result) show_bitsX(31,  1, result)
#define show_bits32(result) show_bitsX(32,  0, result)

#define show_bitsn(num,result) show_bitsX((num),(32-(num)),result)

#ifdef NO_SANITY_CHECKS
#define flush_bits(num)                                               \
{                                                                     \
  bitOffset += num;                                                   \
                                                                      \
  UPDATE_COUNT(num);                                                      \
                                                                          \
  if (bitOffset > 31) {                                               \
    bitBuffer++;                                                      \
    curBits = *bitBuffer;                                             \
    bufLength--;                                                      \
    bitOffset -= 32;                                                  \
  }                                                                   \
}
#else
#define flush_bits(num)                                               \
{                                                                     \
  if (curVidStream == NULL) {                                         \
    /* Deal with no vid stream here. */                               \
  }                                                                   \
                                                                      \
  if (bufLength < 2) {                                                \
    correct_underflow();                                              \
  }                                                                   \
                                                                      \
  UPDATE_COUNT(num);                                                      \
                                                                          \
  bitOffset += num;                                                   \
                                                                      \
  if (bitOffset > 31) {                                               \
    bitBuffer++;                                                      \
    curBits = *bitBuffer;                                             \
    bufLength--;                                                      \
    bitOffset -= 32;                                                  \
  }                                                                   \
}
#endif

