/****************************************************************
Copyright 1990 by AT&T Bell Laboratories and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T Bell Laboratories or
Bellcore or any of their entities not be used in advertising or
publicity pertaining to distribution of the software without
specific, written prior permission.

AT&T and Bellcore disclaim all warranties with regard to this
software, including all implied warranties of merchantability
and fitness.  In no event shall AT&T or Bellcore be liable for
any special, indirect or consequential damages or any damages
whatsoever resulting from loss of use, data or profits, whether
in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of
this software.
****************************************************************/

/* safe_strncpy

	Copies at most   max_length   characters, stopping at the first   \0
   character in   source.   The algorithm correctly handles overlapping
   buffer areas. */

#include <stdio.h>
#ifndef NULL
/* ANSI C */
#include <stddef.h>
#endif

char *safe_strncpy (dest, source, max_length)
char *dest, *source;
int max_length;
{

/* There are several conditions to be considered in determining buffer
   area overlap:

   Buffer Overlap?		Picture		Direction In Which To Copy
---------------------------------------------------------------------------
1. dest == source	   | dest/src |		  no copy necessary
			   ============

2. tail of dest against	  |   dest | | src   |	  left to right
   head of source	  ---------===--------

3. head of dest against	  |   src | | dest   |	  right to left
   tail of source	  --------===---------

4. no overlap	|src| |dest|   or   |dest| |src|  either direction
		----- ------	    ------ -----
*/

    register char *ret_val = dest;
    register int real_length;

    if (source == NULL || dest == NULL)
	return NULL;

/* Compute the actual length of the text to be copied */

    for (real_length = 0; real_length < max_length && source[real_length];
	    real_length++);

/* Account for condition 3,  dest head v. source tail */

    if (source + real_length >= dest && source < dest)
	for (; real_length >= 0; real_length--)
	    dest[real_length] = source[real_length];

/* Account for conditions 2 and 4,  dest tail v. source head  or no overlap */

    else if (source != dest)
	for (; real_length >= 0; real_length--)
	    *dest++ = *source++;

/* Implicitly handle condition 1, by not performing the copy */

    return ret_val;
} /* safe_strncpy */

