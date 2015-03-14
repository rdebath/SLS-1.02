#include <stdio.h>
#include "midifile.h"

mygetc() { return(getchar()); }

main()
{
	Mf_getc = mygetc;
	midifile ();
	exit(0);
}
