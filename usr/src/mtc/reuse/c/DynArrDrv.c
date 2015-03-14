/* $Id: DynArrDrv.c,v 1.4 1992/05/05 13:19:05 grosch rel $ */

/* $Log: DynArrDrv.c,v $
 * Revision 1.4  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  90/09/20  09:12:21  grosch
 * calmed down lint
 * 
 * Revision 1.1  90/07/04  14:33:50  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:34  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: DynArrDrv.c,v 1.4 1992/05/05 13:19:05 grosch rel $";

# include "ratc.h"
# include "DynArray.h"
# include "Memory.h"
# include <stdio.h>

static	long		i;
static	cardinal	j;
static	long *		p;
static	unsigned long	s;

main ()
{
   InitMemory ();
   s = 10;

   MakeArray ((char * *) & p, & s, (long) sizeof (long));
   for (i = 0; i < s; i ++) {
      p [i] = i;
   }

   for (j = 1; j <= 13; j ++) {
      ExtendArray ((char * *) & p, & s, (long) sizeof (long));

      if (p == (long *) NULL) {
	 (void) printf ("Extend Error\n");
      }

      for (i = s / 2; i < s; i ++) {
	 p [i] = i;
      }

      for (i = 0; i < s; i ++) {
	 if (p [i] != i) {
	    (void) printf ("Error j, i, p [i] =");
	    (void) printf ("%5d", j);
	    (void) printf ("%5ld", i);
	    (void) printf ("%10ld\n", p [i]);
	 }
      }

      (void) printf ("j, size = ");
      (void) printf ("%5d", j);
      (void) printf ("%10ld", s);
      (void) printf (" ok\n");
   }
   return 0;
}
