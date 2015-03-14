/* generic device dependent stuff */

#include <stdio.h>
#include "sun.h"

/* open the display - There isn't one */

BITMAP *
display_open(name)
char *name;			/* name of frame buffer */
{
   return(NULL);
}

/* free resources required by the display  - there aren't any */

int
display_close(bitmap)
BITMAP *bitmap;
	{
	return(0);
	}
