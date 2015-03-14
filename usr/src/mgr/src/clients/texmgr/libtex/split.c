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

#ifndef lint
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/split.c,v 1.3 89/02/13 14:31:24 grunwald Exp Locker: grunwald $";
#endif

#include <ctype.h>

/*
 * Split a line into an array of words.  This is destructive of
 * the original line; the word pointers point to places within
 * that line.
 *
 * Return the number of words made, or -1 for overflow.
 */

/*
 * The lexical states are much like `sh's, except that we also do
 * C-style backslash-escapes.
 */
enum lexstate {
	S_BLANK,		/* outside a word */
	S_WORD,			/* inside a word, no quoting */
	S_SQUOTE,		/* inside a single quote */
	S_DQUOTE,		/* inside a double quote */
	S_BKSL0,		/* last char was \ */
	S_BKSL1,		/* last chars were \, [0-7] */
	S_BKSL2			/* last chars were \, [0-7][0-7] */
};

int
split(s, w, nw)
	register char *s, **w;
	int nw;
{
	register int c;
	register char *canon = s;
	register int wleft = nw;
	enum lexstate state, prebkstate;

	/*
	 * Start out in the `blank' state (outside a word).  Handle
	 * quotes and things.  Backslashes are handled by saving the
	 * `pre-backslash' state, doing the backslash, and restoring
	 * that state at the end of the backslash sequence.
	 */
	state = S_BLANK;
	while ((c = *s++) != 0) {
reswitch:
		switch (state) {

		/*
		 * Blanks: spaces stay in blank state; anything
		 * else starts a word.  However, quotes may put
		 * us into quote states, rather than word states.
		 */
		case S_BLANK:
			if (isspace(c))
				continue;
			if (--wleft < 0)
				return (-1);
			*w++ = canon;
			state = S_WORD;
			/* FALLTHROUGH */

		/*
		 * In a word.  Spaces take us out (and end the
		 * current word).  Quotes, however, put us into
		 * quote states.
		 */
		case S_WORD:
			if (isspace(c)) {
				*canon++ = 0;
				state = S_BLANK;
				break;
			}
			if (c == '\'') {
				state = S_SQUOTE;
				break;
			}
			if (c == '"') {
				state = S_DQUOTE;
				break;
			}
			if (c == '\\') {
				prebkstate = S_WORD;
				state = S_BKSL0;
				break;
			}
			*canon++ = c;
			break;

		/*
		 * Inside a single quote, the only special character
		 * is another single quote.  This matches the Bourne
		 * shell quoting convention exactly.
		 */
		case S_SQUOTE:
			if (c == '\'')
				state = S_WORD;
			else
				*canon++ = c;
			break;

		/*
		 * Inside a double quote, double quotes get us out,
		 * but backslashes must be interpreted.
		 */
		case S_DQUOTE:
			if (c == '\\') {
				prebkstate = S_DQUOTE;
				state = S_BKSL0;
			} else if (c == '"')
				state = S_WORD;
			else
				*canon++ = c;
			break;

		/*
		 * If we are handling a backslash, we will either
		 * restore the state, or go to BKSL1 state.  In
		 * the latter case, do not advance the canonicalisation
		 * pointer, since we might have more octal digits
		 * to insert.
		 */
		case S_BKSL0:
			state = prebkstate;	/* probably */
			switch (c) {

			case 'b':
				*canon++ = '\b';
				break;

			case 'f':
				*canon++ = '\f';
				break;

			case 'n':
				*canon++ = '\n';
				break;

			case 'r':
				*canon++ = '\r';
				break;

			case 't':
				*canon++ = '\t';
				break;

			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
				*canon = c - '0';
				state = S_BKSL1;
				break;

			default:
				*canon++ = c;
				break;
			}
			break;


		/*
		 * In BKSL1, we have seen backslash and one octal
		 * digit.  There may be more (in which case just
		 * count them on in), or there might be something
		 * that requires we restore the state and try again.
		 */
		case S_BKSL1:
			switch (c) {

			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
				*canon <<= 3;
				*canon |= c - '0';
				state = S_BKSL2;
				break;

			default:
				canon++;
				state = prebkstate;
				goto reswitch;
			}
			break;

		/*
		 * BKSL2 is like BKSL1, except that it cannot
		 * help but restore the original state, since
		 * there are no four-character octal sequences.
		 */
		case S_BKSL2:
			state = prebkstate;	/* assuredly */
			switch (c) {

			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
				*canon <<= 3;
				*canon++ |= c - '0';
				break;

			default:
				canon++;
				goto reswitch;
			}
			break;
		}
	}
#ifdef notdef
	if (state != S_WORD && state != S_BLANK)
		error(0, 0, "warning: unclosed quote");
#endif
	if (state != S_BLANK)
		*canon = 0;
	return (nw - wleft);
}
