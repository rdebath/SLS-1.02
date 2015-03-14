/*
 * Copyright 1989 Chris Torek
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Chris Torek or M.I.T.
 * not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.  Chris
 * Torek and M.I.T. make no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * CHRIS TOREK AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS.  IN NO EVENT SHALL CHRIS TOREK OR M.I.T. BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * Original Author:
 * 	Chris Torek
 * 	Dept. of Computer Science
 * 	Univ. of Maryland
 * 	chris@cs.umd.edu
 */ 
/* imPRESS command codes */

#define	imP_SP		128	/* advance one space */
#define imP_SP1		129	/* advance one space plus 1 pixel */

#define imP_OLD_MMOVE   130
#define imP_Forw	131	/* one pixel forward */
#define imP_Backw	132	/* one pixel backward */
#define imP_MMove	133	/* move in main advance dir. */
#define imP_SMove	134	/* move in secondary advance dir. */
#define imP_SetHAbs	135	/* set absolute H pos */
#define imP_SetHRel	136	/* set relative H pos */
#define imP_SetVAbs	137	/* set absolute V pos */
#define imP_SetVRel	138	/* set relative V pos */

/*
 *	rephrase for imagen1-special.c
 */
#define imP_SET_ABS_H	135	/* set absolute H pos */
#define imP_SET_REL_H	136	/* set relative H pos */
#define imP_SET_ABS_V	137	/* set absolute V pos */
#define imP_SET_REL_V	138	/* set relative V pos */

#define CIRC_ARC	150
#define ELLIPSE_ARC	151
#define CIRC_SEGM	160

#define imSRULE		192
#define imP_Rule	193	/* print a rule */

#define imP_SET_HPOS	195
#define imP_SET_VPOS	196
#define imP_CRLF	197	/* move to begin. of line */
#define imP_SGLY	198

#define imP_DefGlyph	199	/* define a glyph */

#define imP_BGLY	199	/* for imagen1-special.c */

#define imP_DelGlyph	200	/* mark a glyph for deletion */
#define imP_DELC	201
#define imP_DELF	202

#define imP_SetHVSystem	205	/* set the H/V coordinate system */
#define imP_SET_HV_SYSTEM	205	/* for imagen1-special.c */

#define imP_SetAdvDirs	206	/* set the advance directions */
#define imP_SetFamily	207	/* use this particular family */
#define imP_SetILSpace	208	/* set the interline spacing */
#define imP_SetBOL	209	/* define the beginning of line */
#define imP_SetSP	210	/* define the space between words */

#define imP_CreateFam	211	/* define a family table */
#define imP_PUSH	211	/* for imagen1-special.c */
#define imP_POP		212

#define imP_Page	213	/* go to (0,0) */
#define imP_SET_PUSH_MASK	214

#define imP_EndPage	219	/* print the current page */

#define imP_CREATE_FAMILY_TABLE 221
#define imP_CREATE_MAP	222

#define SET_PUM		225

#define imP_CREATE_PATH	230
#define imP_SET_TEXTURE	231
#define imP_SET_PEN	232
#define imP_FILL_PATH	233
#define imP_DRAW_PATH	234
#define imP_BITMAP	235
#define imP_SET_MAGN	236


#define imP_ForceDel	240	/* force glyph deletion */

#define imP_DEFINE_MACRO	242
#define imP_EXEC_MACRO		243
#define imP_EOF		255	/* end of document */
