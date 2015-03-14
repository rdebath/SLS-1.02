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

/* search structures and routines for 32-bit key, arbitrary data */

struct search {
	unsigned s_dsize;	/* data object size (includes key size) */
	unsigned s_space;	/* space left (in terms of objects) */
	unsigned s_n;		/* number of objects in the table */
	char	*s_data;	/* data area */
};

/* returns a pointer to the search table (for future search/installs) */
struct	search *SCreate();	/* create a search table */

/* returns a pointer to the data object found or created */
char	*SSearch();		/* search for a data object */

/* the third argument to SSearch controls operation as follows: */
#define	S_LOOKUP	0x00	/* pseudo flag */
#define	S_CREATE	0x01	/* create object if not found */
#define	S_EXCL		0x02	/* complain if already exists */

/* in addition, it is modified before return to hold status: */
#define	S_COLL		0x04	/* collision (occurs iff S_EXCL set) */
#define	S_FOUND		0x08	/* found (occurs iff existed already) */
#define	S_NEW		0x10	/* created (occurs iff S_CREATE && !S_EXCL) */
#define	S_ERROR		0x20	/* problem creating (out of memory) */
