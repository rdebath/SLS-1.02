#include "stdio.h"
#include "f2c.h"

/* called when a subscript is out of range */

VOID s_rnge(varn, offset, procn, line)
char *varn, *procn;
long int offset;
int line;
{
register int i;

fprintf(stderr, "Subscript out of range on file line %d, procedure ", line);
while((i = *procn) && i != '_' && i != ' ')
	putc(*procn++, stderr);
fprintf(stderr, ".\nAttempt to access the %ld-th element of variable ", offset+1);
while((i = *varn) && i != ' ')
	putc(*varn++, stderr);
fprintf(stderr, ".\n");
_cleanup();
abort();
}
