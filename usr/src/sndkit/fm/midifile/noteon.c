#include <stdio.h>
#include <ctype.h>
#include "midifile.h"
#include "console.h"

FILE *F;
extern long Mf_currtime;

mygetc() { return(getc(F)); }

mynoteon(chan,note,velocity)
int chan;
unsigned char note;
unsigned char velocity;
{
	printf("%ld: chan=%d, note=%d, velocity=%d\n",Mf_currtime, chan, note, velocity);
}

main(argc,argv)
char **argv;
{
	argc = ccommand(&argv);
	
	if ( argc > 1 )
		F = fopen(argv[1],"r");
	else
		F = stdin;

	Mf_getc = mygetc;
	Mf_noteon = mynoteon;

	midifile();

	exit(0);
}

