/* test bitmap expansion */

#include <stdio.h>
#include "bitmap.h"

main(argc,argv)
int argc;
char **argv;
	{
	unsigned char fg,bg;
	BITMAP *old, *new;
	BITMAP *bitmapread(), *bit_expand();

	if (argc<3) {
		fprintf(stderr,"usage: < 1_bit %s <fg> <bg> > 8_bit\n",*argv);
		exit(1);
		}

	old = bitmapread(stdin);
	if (!old) {
		fprintf(stderr,"Invalid bitmap on stdin\n");
		exit(2);
		}

	fg = atoi(argv[1]);
	bg = atoi(argv[2]);

	new = bit_expand(old,fg,bg);
	bitmapwrite(stdout,new,1);
	bit_destroy(old);
	bit_destroy(new);
	exit(0);
	}
