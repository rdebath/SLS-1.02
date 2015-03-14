/* $Id: SetsDrv.c,v 1.7 1992/09/24 13:03:56 grosch rel $ */

/* $Log: SetsDrv.c,v $
 * Revision 1.7  1992/09/24  13:03:56  grosch
 * adaption to MS-DOS
 *
 * Revision 1.6  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.5  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.4  90/09/20  09:12:27  grosch
 * calmed down lint
 * 
 * Revision 1.3  90/07/04  14:34:06  grosch
 * introduced conditional include
 * 
 * Revision 1.2  89/12/08  17:25:04  grosch
 * complete redesign in order to increase efficiency
 * 
 * Revision 1.1  89/01/09  17:29:45  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:44:46  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: SetsDrv.c,v 1.7 1992/09/24 13:03:56 grosch rel $";

# include "ratc.h"
# include <stdio.h>
# include "Sets.h"

# define max		1000

static	tSet		s, t, u	;
static	int		i	;
static	FILE *		f	;

main ()
{
   MakeSet (& s, max);
   MakeSet (& t, max);
   MakeSet (& u, max);

   for (i = 2; i <= max; i ++) { Include (& t, i); }

   AssignEmpty (& s);
   AssignElmt (& s, 1);
   Assign (& u, & t);
   Union (& s, & t);

   AssignEmpty (& t);
   for (i = 0; i <= max; i += 2) { Include (& t, i); }
   Difference (& s, & t);

   for (i = 0; i <= max; i += 3 ) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 5 ) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 7 ) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 11) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 13) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 17) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 19) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 23) { Exclude (& s, i); }
   for (i = 0; i <= max; i += 29) { Exclude (& s, i); }

   f = fopen ("t", "w");
   WriteSet (f, & s);
   (void) fprintf (f, "\n");
   (void) fclose (f);

   f = fopen ("t", "r");
   ReadSet (f, & t);
   (void) fclose (f);

   WriteSet (stdout, & t);
   (void) printf ("\n%5d%5d%5d\n", Size (& t), Minimum (& t), Maximum (& t));
   ReleaseSet (& s);
   ReleaseSet (& t);
   ReleaseSet (& u);

   MakeSet	(& s, 10);
   Include	(& s, 3);
   Include	(& s, 7);
   (void) printf ("\nenter Size and Set like below! (Size=0 terminates)\n10 ");
   WriteSet	(stdout, & s);
   (void) printf ("\n");
   ReleaseSet	(& s);

   for (;;) {
      (void) printf ("\n");
      (void) scanf ("%d", & i);
      if (i == 0) break;
      MakeSet	(& s, i);
      ReadSet	(stdin, & s);
      WriteSet	(stdout, & s);
      (void) printf (" Card = %d\n", Card (& s));
      Complement(& s);
      WriteSet	(stdout, & s);
      (void) printf (" Card = %d\n", Card (& s));
      ReleaseSet(& s);
   }
   return 0;
}
