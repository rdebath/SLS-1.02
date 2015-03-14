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

#include <stdio.h>
#include "defines.h"
#include "machdefs.h"



/* prchars -- write two adjacent integers */

prchars(fp, s)
FILEP fp;
int *s;
{

	fprintf(fp, "0%o,0%o\n", s[0], s[1]);
}



pruse(fp, s)
FILEP fp;
char *s;
{
	fprintf(fp, "\t%s\n", s);
}



/* prskip -- output the mnemonic for skipping k short words */

prskip(fp, k)
FILEP fp;
ftnint k;
{
	fprintf(fp, "\t%ld\n", k);
}





prcomblock(fp, name)
FILEP fp;
char *name;
{
	fprintf(fp, LABELFMT, name);
}
