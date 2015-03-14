/*
 * $XConsortium: xcalc.h,v 1.3 89/12/15 18:48:59 converse Exp $
 * 
 * xcalc.h - symbolic constants for xcalc
 *
 * Copyright 1989 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Donna Converse, MIT X Consortium
 */

#ifdef SIGNALRETURNSINT
#define signal_t int
#else
#define signal_t void
#endif

#define kRECIP 0	/* reciprocal */
#define kSQR   1	/* square */
#define kSQRT  2	/* square root */
#define kCLR   3	/* clear */
#define kOFF   4	/* clear and quit */
#define kINV   5	/* inverse */
#define kSIN   6	/* sine */
#define kCOS   7	/* cosine */
#define kTAN   8	/* tangent */
#define kDRG   9	/* degree radian grad */
#define kE     10	/* the natural number e */
#define kEE    11	/* scientific notation */
#define kLOG   12	/* logarithm */
#define kLN    13	/* natural logarithm */
#define kPOW   14	/* power */
#define kPI    15	/* pi */
#define kFACT  16	/* factorial */
#define kLPAR  17	/* left paren */
#define kRPAR  18	/* right paren */
#define kDIV   19	/* division */
#define kSTO   20	/* store */
#define kSEVEN 21	/* 7 */
#define kEIGHT 22	/* 8 */
#define kNINE  23	/* 9 */
#define kMUL   24	/* multiplication */
#define kRCL   25	/* recall */
#define kFOUR  26	/* 4 */
#define kFIVE  27	/* 5 */
#define kSIX   28	/* 6 */
#define kSUB   29	/* subtraction */
#define kSUM   30	/* summation */
#define kONE   31	/* 1 */
#define kTWO   32	/* 2 */
#define kTHREE 33	/* 3 */
#define kADD   34	/* addition */
#define kEXC   35	/* exchange display and memory */
#define kZERO  36	/* 0 */
#define kDEC   37	/* decimal point */
#define kNEG   38	/* negation */
#define kEQU   39	/* equals */
#define kENTR  40	/* enter */
#define kXXY   41	/* exchange X and Y registers */
#define kEXP   42	/* exponent */
#define k10X   43	/* 10 raised to a power */
#define kROLL  44	/* roll stack */
#define kNOP   45	/* no operation */
#define kBKSP  46	/* backspace */

#define XCalc_MEMORY	0	/* memory indicator */
#define XCalc_INVERSE   1	/* inverse function indicator */
#define XCalc_DEGREE	2	/* degree indicator */
#define XCalc_RADIAN	3	/* radian indicator */
#define XCalc_GRADAM	4	/* grad indicator */
#define XCalc_PAREN	5	/* parenthesis indicator */
