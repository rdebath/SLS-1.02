#include "sort.h"                                    /*  SCXTRACT.C  */

scxtract(scin, scout, xfile)        /* called from xmain.c or some other main */
 FILE  *scin, *scout, *xfile;       /*   extracts events from each score sect */
{                                   /*   according to the controlling xfile   */
	int n;

	readxfil(xfile);
	SCOREIN = scin;
	SCOREOUT = scout;

	sectcnt = 0;
        do      if ((n = sread()) > 0) {
		    /*  allout();   */
		    /*  textout();  */
			extract();
			swrite();
		}
	while (n > 1);
	sfree();        /* return all memory used */
	return(0);
}         
