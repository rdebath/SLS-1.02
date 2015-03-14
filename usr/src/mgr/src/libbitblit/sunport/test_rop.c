/* test small bitblit functions */

#include <stdio.h>
#include "bitmap.h"

extern int bit_debug;

#define message(x) \
	if(bit_debug){printf("%s\n",x);fflush(stdout);}

#define SIZE		32				/* basic size */

int bit_debug = 0;

main(argc,argv)
char **argv;
   {
   register BITMAP *screen;
   BITMAP *src,*dst;
   int wait=1;
	int size;
	int x0;
   register int i,j,x;

	fprintf(stderr,"START\n");
   if (argc>1) 
		size = atoi(argv[1]);
	else
		size=SIZE;
   bit_debug = getenv("DEBUG");

   screen = bit_open("/dev/cgthree0");
	mem_rop8(screen,0,0,1152,900,PUTCOLOR(BIT_SRC,0,0),0,0,0);
   message("Make 2 100 pixel rectangles at edge of display");

	mem_rop8(screen,1,1,100,100,BIT_NOT(BIT_DST),0,0,0);
	mem_rop8(screen,1051,799,100,100,BIT_NOT(BIT_DST),0,0,0);

	message("try all 16 ops with colors 3 and 4");
	
	for(i=0;i<16;i++)
		mem_rop8(screen,150+35*i,150+30*i,75,75,PUTCOLOR(i,3,4),0,0,0);

	getchar();
	mem_rop8(screen,0,0,1152,900,PUTCOLOR(BIT_SRC,0,0),0,0,0);

	message(" now try some blits with dest defined");

	src = bit_alloc(size,size,NULL,8);
	dst = bit_alloc(size,size,NULL,8);

	mem_rop8(src,0,0,size,size,PUTCOLOR(BIT_SRC,3,4),0,0,0);
	mem_rop8(dst,0,0,size,size,PUTCOLOR(BIT_NOT(BIT_SRC),3,4),0,0,0);

	for(i=0;i<16;i++) {
		mem_rop8(screen,(size+5)*i+10,50,size,size,BIT_SRC,src,0,0);
		mem_rop8(screen,(size+5)*i+13,90,size,size,BIT_SRC,src,0,0);
   	}
   }
