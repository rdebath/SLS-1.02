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
#define NO_SANITY_CHECKS
#include <assert.h>
#include "video.h"
#include "decoders.h"

/* External declarations. */

extern int zigzag_direct[];

/* Macro for returning 1 if num is positive, -1 if negative, 0 if 0. */

#define Sign(num) ((num > 0) ? 1 : ((num == 0) ? 0 : -1))


/*
 *--------------------------------------------------------------
 *
 * ParseReconBlock --
 *
 *	Parse values for block structure from bitstream.
 *      n is an indication of the position of the block within
 *      the macroblock (i.e. 0-5) and indicates the type of 
 *      block (i.e. luminance or chrominance). Reconstructs
 *      coefficients from values parsed and puts in 
 *      block.dct_recon array in vid stream structure.
 *      sparseFlag is set when the block contains only one
 *      coeffictient and is used by the IDCT.
 *
 * Results:
 *	
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

#define DCT_recon blockPtr->dct_recon
#define DCT_dc_y_past blockPtr->dct_dc_y_past
#define DCT_dc_cr_past blockPtr->dct_dc_cr_past
#define DCT_dc_cb_past blockPtr->dct_dc_cb_past

#define DECODE_DCT_COEFF_FIRST DecodeDCTCoeffFirst
#define DECODE_DCT_COEFF_NEXT DecodeDCTCoeffNext

int 
ParseReconBlock(n)
     int n;
{
  int sparseFlag;
  unsigned int temp_curBits;
  int temp_bitOffset;
  int temp_bufLength;
  unsigned int *temp_bitBuffer;
  Block *blockPtr = &curVidStream->block;
  
  if (bufLength < 100)
    correct_underflow();

  temp_curBits = curBits;
  temp_bitOffset = bitOffset;
  temp_bufLength = bufLength;
  temp_bitBuffer = bitBuffer;

  {

    register unsigned int curBits;
    register int bitOffset;
    register int bufLength;
    register unsigned int *bitBuffer;

    int diff;
    int size, level, i, run, pos;
    short int *reconptr, coeff;
    unsigned char *iqmatrixptr, *niqmatrixptr;
    int qscale;

    curBits = temp_curBits;
    bitOffset = temp_bitOffset;
    bufLength = temp_bufLength;
    bitBuffer = temp_bitBuffer;

    sparseFlag = 0;

    reconptr = DCT_recon[0];

    /* 
     * Hand coded version of memset that's a little faster...
     * Old call:
     *	memset((char *) DCT_recon, 0, 64*sizeof(short int));
     */
    {
      int *p;
      p = (int *) reconptr;
      p[0] = p[1] = p[2] = p[3] = p[4] = p[5] = p[6] = p[7] = p[8] = p[9] = 
      p[10] = p[11] = p[12] = p[13] = p[14] = p[15] = p[16] = p[17] = p[18] = p[19] =
      p[20] = p[21] = p[22] = p[23] = p[24] = p[25] = p[26] = p[27] = p[28] = p[29] =
      p[30] = p[31] = 0;
    }

    if (curVidStream->mblock.mb_intra) {

      if (n < 4) {

	DecodeDCTDCSizeLum(&size);

	if (size != 0) {
	  get_bitsn(size, &diff);
          if (!(diff & bitTest[32-size])) {
	    diff = rBitMask[size] | (diff + 1);
	  }
	}
	else diff = 0;

	if (n == 0) {
	  coeff = diff << 3;
	  if (curVidStream->mblock.mb_address -
	      curVidStream->mblock.past_intra_addr > 1) 
	    coeff += 1024;
	  else coeff += DCT_dc_y_past;
	  DCT_dc_y_past = coeff;

	  *reconptr = coeff;
	  sparseFlag++;

	}
	else {
	  coeff = DCT_dc_y_past + (diff << 3);

	  *reconptr = coeff;
	  sparseFlag++;

	  DCT_dc_y_past = coeff;
	}
      }
      
      else {
	
	DecodeDCTDCSizeChrom(&size);
	
	if (size != 0) {
	  get_bitsn(size, &diff);
          if (!(diff & bitTest[32-size])) {
	    diff = rBitMask[size] | (diff + 1);
	  }
	}
	else diff = 0;
	
	if (n == 4) {
	  coeff = diff << 3;
	  if (curVidStream->mblock.mb_address -
	      curVidStream->mblock.past_intra_addr > 1) 
	    coeff += 1024;
	  else coeff += DCT_dc_cr_past;
	  DCT_dc_cr_past = coeff;

	  *reconptr = coeff;
	  sparseFlag++;

	}
	else {
	  coeff = diff << 3;
	  if (curVidStream->mblock.mb_address -
	      curVidStream->mblock.past_intra_addr > 1) 
	    coeff += 1024;
	  else coeff += DCT_dc_cb_past;
	  DCT_dc_cb_past = coeff;

	  *reconptr = coeff;
	  sparseFlag++;

	}
      }
      
      i = 0; pos = 0;
    
      if (curVidStream->picture.code_type != 4) {
	
	qscale = curVidStream->slice.quant_scale;
	iqmatrixptr = curVidStream->intra_quant_matrix[0];
	
	while(1) {
	  
	  DECODE_DCT_COEFF_NEXT(&run, &level);

	  if (run == END_OF_BLOCK) break;

	  i = i + run + 1;

	  pos = zigzag_direct[i];

	  if (i > 63) {
	    goto end;
	  }

	  coeff = (level * qscale * ((int) (*(iqmatrixptr+pos))))
	    >> 3;
	  if ((coeff & 1) == 0) 
	    coeff -= Sign(coeff);
	  
	  if (coeff > 2047) coeff = 2047;
	  else if (coeff < -2048) coeff = -2048;

	  *(reconptr+pos) = coeff;
	  sparseFlag++;

	}

	/* If only one coefficient, store position + 1 in sparseFlag,
	   otherwise reset to zero.
	*/

#ifdef ANALYSIS 

	{
	  extern unsigned int *mbCoeffPtr;
	  mbCoeffPtr[sparseFlag]++;
	}
#endif

	if (sparseFlag == 1) sparseFlag = pos+1;
	else sparseFlag = 0;

	flush_bits(2);
	goto end;
      }
    }
    
    else {
      
      niqmatrixptr = curVidStream->non_intra_quant_matrix[0];
      qscale = curVidStream->slice.quant_scale;
      
      DECODE_DCT_COEFF_FIRST(&run, &level);
      i = run;

      pos = zigzag_direct[i];
      
      coeff = (((level<<1) + Sign(level)) * qscale * 
	       ((int) (*(niqmatrixptr+pos)))) >> 4; 
      
      if ((coeff & 1) == 0) 
	coeff -= Sign(coeff);
      
      if (coeff > 2047) coeff = 2047;
      else if (coeff < -2048) coeff = -2048;


      *(reconptr+pos) = coeff;
      sparseFlag++;

      if (curVidStream->picture.code_type != 4) {
	
	while(1) {
	  
	  DECODE_DCT_COEFF_NEXT(&run, &level);

	  if (run == END_OF_BLOCK) break;

	  i = i+run+1;
	  
	  if (i > 63) {
	    goto end;
	  }

	  pos = zigzag_direct[i];
	  
	  coeff = (((level<<1) + Sign(level)) * qscale * 
		   ((int) (*(niqmatrixptr+pos)))) >> 4; 
	  
	  if ((coeff & 1) == 0) 
	    coeff -= Sign(coeff);
	  
	  if (coeff > 2047) coeff = 2047;
	  else if (coeff < -2048) coeff = -2048;
	  
	  *(reconptr+pos) = coeff;
	  sparseFlag++;

	}

	/* If only one coefficient, store which one in sparseFlag,
	   otherwise reset to zero.
	*/
#ifdef ANALYSIS
	{
	  extern unsigned int *mbCoeffPtr;
	  mbCoeffPtr[sparseFlag]++;
	}
#endif

	if (sparseFlag == 1) sparseFlag = pos+1;
	else sparseFlag = 0;
	
	flush_bits(2);
	goto end;
      }
    }
    
  end:

    j_rev_dct(reconptr, sparseFlag);

    temp_curBits = curBits;
    temp_bitOffset = bitOffset;
    temp_bufLength = bufLength;
    temp_bitBuffer = bitBuffer;

  }

  curBits = temp_curBits;
  bitOffset = temp_bitOffset;
  bufLength = temp_bufLength;
  bitBuffer = temp_bitBuffer;

  return sparseFlag;
}
	
#undef DCT_recon 
#undef DCT_dc_y_past 
#undef DCT_dc_cr_past 
#undef DCT_dc_cb_past 


/*
 *--------------------------------------------------------------
 *
 * ParseAwayBlock --
 *
 *	Parses off block values, throwing them away.
 *      Used with grayscale dithering.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int 
ParseAwayBlock(n)
     int n;
{
  unsigned int diff;
  unsigned int size, run;
  int level;

  if (bufLength < 100)
    correct_underflow();

  if (curVidStream->mblock.mb_intra) {

    /* If the block is a luminance block... */

    if (n < 4) {

      /* Parse and decode size of first coefficient. */

      DecodeDCTDCSizeLum(&size);

      /* Parse first coefficient. */

      if (size != 0) {
	get_bitsn(size, &diff);
      }
    }

    /* Otherwise, block is chrominance block... */

    else {

      /* Parse and decode size of first coefficient. */

      DecodeDCTDCSizeChrom(&size);

      /* Parse first coefficient. */

      if (size != 0) {
	get_bitsn(size, &diff);
      }
    }
  }

  /* Otherwise, block is not intracoded... */

  else {

    /* Decode and set first coefficient. */

    DECODE_DCT_COEFF_FIRST(&run, &level);
  }

  /* If picture is not D type (i.e. I, P, or B)... */

  if (curVidStream->picture.code_type != 4) {

    /* While end of macroblock has not been reached... */

    while (1) {

      /* Get the dct_coeff_next */

      DECODE_DCT_COEFF_NEXT(&run, &level);

      if (run == END_OF_BLOCK) break;
    }

    /* End_of_block */

    flush_bits(2);
  }

  return PARSE_OK;
}

