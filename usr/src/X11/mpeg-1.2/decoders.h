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
/*
 * decoders.h
 *
 * This file contains the declarations of structures required for Huffman
 * decoding
 *
 */

/* Include util.h for bit i/o parsing macros. */

#include "util.h"

/* Code for unbound values in decoding tables */
#define ERROR -1
#define DCT_ERROR 63

#define MACRO_BLOCK_STUFFING 34
#define MACRO_BLOCK_ESCAPE 35

/* Two types of DCT Coefficients */
#define DCT_COEFF_FIRST 0
#define DCT_COEFF_NEXT 1

/* Special values for DCT Coefficients */
#define END_OF_BLOCK 62
#define ESCAPE 61

/* Structure for an entry in the decoding table of 
 * macroblock_address_increment */
typedef struct {
  unsigned int value;       /* value for macroblock_address_increment */
  int num_bits;             /* length of the Huffman code */
} mb_addr_inc_entry;

/* Decoding table for macroblock_address_increment */
mb_addr_inc_entry mb_addr_inc[2048];


/* Structure for an entry in the decoding table of macroblock_type */
typedef struct {
  unsigned int mb_quant;              /* macroblock_quant */
  unsigned int mb_motion_forward;     /* macroblock_motion_forward */
  unsigned int mb_motion_backward;    /* macroblock_motion_backward */
  unsigned int mb_pattern;            /* macroblock_pattern */
  unsigned int mb_intra;              /* macroblock_intra */
  int num_bits;                       /* length of the Huffman code */
} mb_type_entry;

/* Decoding table for macroblock_type in predictive-coded pictures */
mb_type_entry mb_type_P[64];

/* Decoding table for macroblock_type in bidirectionally-coded pictures */
mb_type_entry mb_type_B[64];


/* Structures for an entry in the decoding table of coded_block_pattern */
typedef struct {
  unsigned int cbp;            /* coded_block_pattern */
  int num_bits;                /* length of the Huffman code */
} coded_block_pattern_entry;

/* External declaration of coded block pattern table. */

extern coded_block_pattern_entry coded_block_pattern[512];



/* Structure for an entry in the decoding table of motion vectors */
typedef struct {
  int code;              /* value for motion_horizontal_forward_code,
			  * motion_vertical_forward_code, 
			  * motion_horizontal_backward_code, or
			  * motion_vertical_backward_code.
			  */
  int num_bits;          /* length of the Huffman code */
} motion_vectors_entry;


/* Decoding table for motion vectors */
motion_vectors_entry motion_vectors[2048];


/* Structure for an entry in the decoding table of dct_dc_size */
typedef struct {
  unsigned int value;    /* value of dct_dc_size (luminance or chrominance) */
  int num_bits;          /* length of the Huffman code */
} dct_dc_size_entry;

/* External declaration of dct dc size lumiance table. */

extern dct_dc_size_entry dct_dc_size_luminance[128];

/* External declaration of dct dc size chrom table. */

extern dct_dc_size_entry dct_dc_size_chrominance[256];


/* DCT coeff tables. */

#define RUN_MASK 0xfc00
#define LEVEL_MASK 0x03f0
#define NUM_MASK 0x000f
#define RUN_SHIFT 10
#define LEVEL_SHIFT 4

/* External declaration of dct coeff tables. */

extern unsigned short int dct_coeff_tbl_0[256];
extern unsigned short int dct_coeff_tbl_1[16];
extern unsigned short int dct_coeff_tbl_2[4];
extern unsigned short int dct_coeff_tbl_3[4];
extern unsigned short int dct_coeff_next[256];
extern unsigned short int dct_coeff_first[256];

#define DecodeDCTDCSizeLum(macro_val)                    \
{                                                    \
  unsigned int index;                                \
                                                     \
  show_bits7(&index);                              \
                                                     \
  *(macro_val) = dct_dc_size_luminance[index].value;       \
                                                     \
  flush_bits(dct_dc_size_luminance[index].num_bits); \
}

#define DecodeDCTDCSizeChrom(macro_val)                      \
{                                                        \
  unsigned int index;                                    \
                                                         \
  show_bits8(&index);                                  \
                                                         \
  *(macro_val) = dct_dc_size_chrominance[index].value;         \
                                                         \
  flush_bits(dct_dc_size_chrominance[index].num_bits);   \
}

#define DecodeDCTCoeff(type, run, level)                                   \
{                                                                          \
  unsigned int temp_run, index, temp_level, num_bits;                   \
  unsigned short int *dct_coeff_tbl;                                       \
  unsigned int value;                                     \
                                                                           \
  show_bits8(&index);                                                    \
  if (type == DCT_COEFF_FIRST)                                             \
    dct_coeff_tbl = dct_coeff_first;                                       \
  else                                                                     \
    dct_coeff_tbl = dct_coeff_next;                                        \
                                                                           \
  if (index > 3) {                                                         \
    value = dct_coeff_tbl[index];                                     \
    temp_run = (value & RUN_MASK) >> RUN_SHIFT;             \
    if (temp_run == END_OF_BLOCK) {                                        \
      *run = END_OF_BLOCK;                                                 \
      *level = END_OF_BLOCK;                                               \
    }                                                                      \
    else {                                                                 \
      num_bits = (value & NUM_MASK) + 1;                      \
      flush_bits(num_bits);                                                \
      if (temp_run != ESCAPE) {                                            \
	 value = dct_coeff_tbl[index];                                     \
         temp_level = (value & LEVEL_MASK) >> LEVEL_SHIFT;                 \
         *run = temp_run;                                                  \
         get_bits1(&value);                                                    \
         *level = value ? -temp_level : temp_level;                           \
       }                                                                   \
       else {    /* temp_run == ESCAPE */                                  \
         get_bits6(run);                                                   \
         get_bits8(&temp_level);                                           \
         if (temp_level != 0 && temp_level != 128)                         \
	    *level = ((int) (temp_level << 24)) >> 24;                     \
         else if (temp_level == 0) {                                       \
            get_bits8(&temp_level);                                        \
       	    *level = temp_level;                                           \
 	    assert(*level >= 128);                                         \
         }                                                                 \
         else {                                                            \
            get_bits8(&temp_level);                                        \
      	    *level = temp_level - 256;                                     \
	    assert(*level <= -128 && *level >= -255);                      \
         }                                                                 \
       }                                                                   \
    }                                                                      \
  }                                                                        \
  else {  \
    if (index == 2) {                                                   \
      show_bits10(&index);                                                 \
      value = dct_coeff_tbl_2[index & 3];                                     \
    }                                                                        \
    else if (index == 3) {                                                   \
      show_bits10(&index);                                                 \
      value = dct_coeff_tbl_3[index & 3];                                     \
    }                                                                        \
    else if (index == 1) {                                                   \
      show_bits12(&index);                                                 \
      value = dct_coeff_tbl_1[index & 15];                                     \
    }                                                                        \
    else {   /* index == 0 */                                                \
      show_bits16(&index);                                                 \
      value = dct_coeff_tbl_0[index & 255];                                     \
    }                                                                        \
    *run = (value & RUN_MASK) >> RUN_SHIFT;               \
    temp_level = (value & LEVEL_MASK) >> LEVEL_SHIFT;     \
    num_bits = (value & NUM_MASK) + 1;                    \
    flush_bits(num_bits);                                                  \
    get_bits1(&value);                                                         \
    *level = value ? -temp_level : temp_level;                                 \
  }  \
}

#define DecodeDCTCoeffFirst(runval, levelval)         \
{                                                     \
  DecodeDCTCoeff(DCT_COEFF_FIRST, runval, levelval);  \
}          

#define DecodeDCTCoeffNext(runval, levelval)          \
{                                                     \
  DecodeDCTCoeff(DCT_COEFF_NEXT, runval, levelval);   \
}

/*
 *--------------------------------------------------------------
 *
 * DecodeMBAddrInc --
 *
 *      Huffman Decoder for macro_block_address_increment; the location
 *      in which the result will be placed is being passed as argument.
 *      The decoded value is obtained by doing a table lookup on
 *      mb_addr_inc.
 *
 * Results:
 *      The decoded value for macro_block_address_increment or ERROR
 *      for unbound values will be placed in the location specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */
#define DecodeMBAddrInc(ptr)				\
{							\
    unsigned int index;					\
    show_bits11(&index);				\
    *(ptr) = mb_addr_inc[index].value;			\
    flush_bits(mb_addr_inc[index].num_bits);		\
}

/*
 *--------------------------------------------------------------
 *
 * DecodeMotionVectors --
 *
 *      Huffman Decoder for the various motion vectors, including
 *      motion_horizontal_forward_code, motion_vertical_forward_code,
 *      motion_horizontal_backward_code, motion_vertical_backward_code.
 *      Location where the decoded result will be placed is being passed
 *      as argument. The decoded values are obtained by doing a table
 *      lookup on motion_vectors.
 *
 * Results:
 *      The decoded value for the motion vector or ERROR for unbound
 *      values will be placed in the location specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

#define DecodeMotionVectors(ptr)			\
{							\
  unsigned int index;					\
  show_bits11(&index);					\
  *(ptr) = motion_vectors[index].code;			\
  flush_bits(motion_vectors[index].num_bits);		\
}
