/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: startup.c,v 4.2 88/06/22 14:38:09 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/startup.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/startup.c,v $$Revision: 4.2 $";

/* generate a startup file from existing window configuration */

#include "term.h"


main(argc,argv)
int argc;
char **argv;
   {
   register int i;
   int count;
   struct window_data data;

   ckmgrterm( *argv );

   m_setup(0);
   while( get_eachwin( &data ) )
      if (data.num == 0)
         printf("window %d\t%d\t%d\t%d\n",
            data.x, data.y, data.w, data.h);
   printf("done\n");
   } 
