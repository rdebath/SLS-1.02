#include <stdio.h>

char *p_time(int n)
{
	static char tmp[16];

	sprintf(tmp, "%d", n);
	return tmp;
}

