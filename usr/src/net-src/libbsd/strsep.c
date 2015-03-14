/* strsep.c - emulate BSD strsep with strpbrk - rick sladkey */

#include <string.h>
#include <stdlib.h>

char *strsep(char **pp, char *seps)
{
	char *p, *q;

	if (!(p = *pp))
		return 0;
	if (q = strpbrk(p, seps)) {
		*pp = q + 1;
		*q = '\0';
	}
	else
		*pp = 0;
	return p;
}

