#include <stdio.h>

char *p_type(int n)
{
	static char tmp[16];

	sprintf(tmp, "%d", n);
	return tmp;
}

