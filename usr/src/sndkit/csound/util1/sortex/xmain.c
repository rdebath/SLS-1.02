#include "../../sort.h"                                    /*   XMAIN.C  */

main(ac,av)                  /* stdio stub for standalone extract */
 int ac; char **av;          /*     first opens the control xfile */
{
  FILE *xfp;
  
	ac--;  av++;
	if (ac != 1) {
	  fprintf(stderr,"usage: extract xfile <in >out\n");
	  exit(1);
	}
	if ((xfp = fopen(*av,"r")) == NULL) {
	  fprintf(stderr,"extract: can't open %s\n", *av);
	  exit(1);
	}
        return(scxtract(stdin,stdout,xfp));
}
