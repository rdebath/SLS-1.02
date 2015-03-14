/* new version of putscreen */

#include <stdio.h>
#include "bitmap.h"
#include "dump.h"

main()
	{
	BITMAP *map, *bitmapread();
	BITMAP *screen = bit_open("/dev/fb");
	map = bitmapread(stdin);
	bit_blit(screen,0,0,BIT_WIDE(map), BIT_HIGH(map),BIT_SRC,map,0,0);
	exit(0);
	}
