/* test shrink */

#include "bitmap.h"

main(argc,argv)
int argc;
char **argv;
	{
	BITMAP *screen = bit_open("/dev/cgthree0");
	BITMAP *screen2 = bit_open("/dev/bwtwo0");
	int c = argc>1 ? atoi(argv[1]) : 0;

	bit_blit(screen2,0,0,1152,900,BUILDOP(BIT_SRC,c,c),screen,0,0);
	}
