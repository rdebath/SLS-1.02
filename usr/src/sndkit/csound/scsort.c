#include "sort.h"                                          /*   SCSORT.C  */

scsort(scin, scout)             /* called from smain.c or some other main */
 FILE *scin, *scout;     /* reads,sorts,timewarps each score sect in turn */
{
	int n;

	SCOREIN = scin;
	SCOREOUT = scout;

	sectcnt = 0;
	do      if ((n = sread()) > 0) {
		    /*  allout();   */
                        sort();
		    /*  textout();  */
			twarp();
			swrite();
		}
	while (n > 1);
	sfree();        /* return all memory used */
	return(0);
}         

