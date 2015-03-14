#include <stdio.h>

main()
{
int i;
#if 0
	printf("\033(B\033)U\016");
#else
	printf("\033(B\033)U");
#endif
	for ( i = 32; i < 255; i++) {
		printf("%x = %c     ", i, i);
	}
#if 0
	printf("\033(B\033)0\n\017");
#else
	printf("\033(B\033)0");
#endif
}

