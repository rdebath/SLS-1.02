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

/* imagen globals */

/* Accounting: */
#ifndef ACCOUNT_FILE
/*
#define ACCOUNT_FILE "/usr/adm/imagen_acct"/* if defined, the name of the
					      imagen page accounting file */
#endif	ACCOUNT_FILE

#define MaxImFamily	128	/* hardware limitation on font family index */
#define DefaultDPI	300	/* 300 for Imagen 8/300, 240 for Imprint-10 */

#define DefaultMaxDrift	3	/* default value for MaxDrift */

/* Default margins, in dots */
/* CRUFT ALERT: depending on DPI variable */
#define DefaultTopMargin   (DPI)	/* 1" margins */
#define DefaultLeftMargin  (DPI)
