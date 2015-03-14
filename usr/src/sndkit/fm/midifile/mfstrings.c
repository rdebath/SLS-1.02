#include <stdio.h>
#include <ctype.h>
#include "midifile.h"

FILE *F;

mygetc() { return(getc(F)); }

mytext(type,leng,msg)
char *msg;
{
	char *p;
	char *ep = msg + leng;

	for ( p=msg; p<ep ; p++ )
		putchar( isprint(*p) ? *p : '?' );
	putchar('\n');
}

main(argc,argv)
char **argv;
{
	if ( argc > 1 )
		F = fopen(argv[1],"r");
	else
		F = stdin;

	Mf_getc = mygetc;
	Mf_text = mytext;

	midifile();

	exit(0);
}
