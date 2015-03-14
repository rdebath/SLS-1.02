/* $Id: MemoryDrv.c,v 1.6 1992/05/05 13:19:05 grosch rel $ */

/* $Log: MemoryDrv.c,v $
 * Revision 1.6  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.5  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.4  91/01/21  12:13:23  grosch
 * some performance improvements
 * 
 * Revision 1.3  90/09/20  09:12:24  grosch
 * calmed down lint
 * 
 * Revision 1.2  90/09/04  17:32:12  grosch
 * automatic determination of alignment
 * 
 * Revision 1.1  90/07/04  14:34:01  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:43  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: MemoryDrv.c,v 1.6 1992/05/05 13:19:05 grosch rel $";

# include "ratc.h"
# include "Memory.h"
# include <stdio.h>

static	char		* p1, * p2, * p3, * p4;
static	unsigned long	i;
static	unsigned long	small, best, notbest, large;

char * AllocPrint (n)
   unsigned long	n;
   {
      char *	a = Alloc (n);

      (void) printf ("Alloc:  n = ");
      (void) printf ("%10ld", n);
      (void) printf (", ADR = ");
      (void) printf ("%8lx\n", a);
      return a;
   }

main ()
{
   InitMemory ();

   for (i = 0; i <= 62; i ++) {
      p1 = AllocPrint (i);
      p2 = AllocPrint (i);
      Free (i, p1);
      Free (i, p2);
      p3 = AllocPrint (i);
      p4 = AllocPrint (i);

      if (p3 != p2) {
	 (void) printf ("Alloc/Free small not inverse ");
	 (void) printf ("%10ld\n", i);
      }

      if (p4 != p1) {
	 (void) printf ("Alloc/Free small not inverse ");
	 (void) printf ("%10ld\n", i);
      }
   }

   small	= 80;
   best		= 96;
   notbest	= 112;
   large	= 128;

   for (;;) {
      for (i = 7; i <= 24 /* 32 */; i ++) {
	 (void) printf ("        i = ");
	 (void) printf ("%10ld\n", i);

	 p1 = AllocPrint (small);
	 p2 = AllocPrint (best);
	 p3 = AllocPrint (notbest);
	 p4 = AllocPrint (large);

	 if (p1 == NULL || p2 == NULL || p3 == NULL || p4 == NULL) {
	    (void) printf ("\nMemory used: %10d\n", MemoryUsed);
	    return 0;
	 }

	 Free (large	, p4);
	 Free (notbest	, p3);
	 Free (best	, p2);
	 Free (small	, p1);

	 p1 = AllocPrint (best);
	 if (p1 != p2) {
	    (void) printf ("Alloc/Free large not inverse ");
	    (void) printf ("%10ld\n", i);
	 }

	 p1 = AllocPrint (best);
	 if (p1 != p3) {
	    (void) printf ("Alloc/Free large not inverse ");
	    (void) printf ("%10ld\n", i);
	 }

	 p1 = AllocPrint (best);
	 if (p1 != p4) {
	    (void) printf ("Alloc/Free large not inverse ");
	    (void) printf ("%10ld\n", i);
	 }
	  
	 small   += small;
	 best    += best;
	 notbest += notbest;
	 large   += large;
      }
   }
}
