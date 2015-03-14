/* test random bitblit functions - color version - (S. A. Uhler) */

#include <stdio.h>
#include "bitmap.h"

#define message(x) \
	if(bit_debug){printf("%s\n",x);fflush(stdout);}

#define FRACTION		850				/* maximum blit size (parts/1000)*/

int bit_debug = 0;

main(argc,argv)
char **argv;
   {
   register BITMAP *screen;
   register int x,y,w,h,op,xs,ys;
	int maxx, maxy;
	int min,max;
	int count;
	int color;

   bit_debug = getenv("DEBUG");

	
	if (argc < 3) {
		printf("usage: %s min_size max_size count\n",*argv);
		exit(1);
		}

   screen = bit_open("/dev/cgthree0");

	if (!screen) {
		fprintf(stderr,"Cant open %s\n","cgthree0");
		exit(1);
		}

	min = atoi(argv[1]);
	max = atoi(argv[2]);
	count = atoi(argv[3]);

	if (min >= max)
		max = min +1;
	maxx = max-min;
	maxy = max-min;

	while (count-- > 0) {
		op = random()&15;
		w = min + random()%maxx;
		h = min + random()%maxy;
		x = random()%(BIT_WIDE(screen)-w);
		y = random()%(BIT_HIGH(screen)-h);
		color = random()&0x07070;	/* fg + bg color */
      mem_rop8(screen,x,y,w,h,op+color,0,0,0);
      }
   }
