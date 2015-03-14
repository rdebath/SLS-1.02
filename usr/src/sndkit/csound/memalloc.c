#include "cs.h"				/*		MEMALLOC.C		*/

#ifndef	size_t	/* more complex conditions..  dpwe 08sep90 */
/* typedef	unsigned int	size_t;  */
#endif

static memdie()
{
  fprintf(stderr,"memory allocate failure\n");
  exit(1);
}

 char *
mcalloc(nbytes)		/* allocate new memory space, cleared to 0 */
 register long nbytes;
{
	register char *p;

	if ((p = calloc((size_t)1,(size_t)nbytes)) == NULL)
		memdie();
	return(p);
}

 char *
mmalloc(nbytes)	     /* allocate new memory space, NOT cleared to 0 */
 register long nbytes;
{
	register char *p;

	if ((p = malloc((size_t)nbytes)) == NULL)
		memdie();
	return(p);
}


