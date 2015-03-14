/* new version of getscreen */

#include <stdio.h>
#include "bitmap.h"
#include "dump.h"

main()
	{
	BITMAP *screen = bit_open("/dev/fb");
	bitmapwrite(stdout,screen,1);
	exit(0);
	}
