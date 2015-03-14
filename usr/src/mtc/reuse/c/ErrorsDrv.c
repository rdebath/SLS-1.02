/* $Id: ErrorsDrv.c,v 1.0 1992/08/07 14:31:41 grosch rel $ */

/* $Log: ErrorsDrv.c,v $
 * Revision 1.0  1992/08/07  14:31:41  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Aug. 1992 */

static char rcsid [] = "$Id: ErrorsDrv.c,v 1.0 1992/08/07 14:31:41 grosch rel $";

# include "ratc.h"
# include <stdio.h>
# include "Positions.h"
# include "Errors.h"
# include "Sets.h"
# include "Idents.h"

int	Integer		= 1;
short	Short		= 2;
long	Long		= 3;
float	Real		= 4.0;
bool	Boolean		= false;
char	Character	= 'a';
char	String [] 	= "abc";
tSet	Set		;
tIdent	Ident		;

errors ()
{
   MessageI ("Integer	", xxError, NoPosition, xxInteger	, (char *) & Integer	);
   MessageI ("Short	", xxError, NoPosition, xxShort		, (char *) & Short	);
   MessageI ("Long	", xxError, NoPosition, xxLong		, (char *) & Long	);
   MessageI ("Real	", xxError, NoPosition, xxReal		, (char *) & Real	);
   MessageI ("Boolean	", xxError, NoPosition, xxBoolean	, (char *) & Boolean	);
   MessageI ("Character	", xxError, NoPosition, xxCharacter	, (char *) & Character	);
   MessageI ("String	", xxError, NoPosition, xxString	,	     String	);
   MessageI ("Set	", xxError, NoPosition, xxSet		, (char *) & Set	);
   MessageI ("Ident	", xxError, NoPosition, xxIdent		, (char *) & Ident	);
}

main ()
{
   MakeSet (& Set, 10); Include (& Set, 5); Include (& Set, 6);
   Ident = MakeIdent ("def", 3);
   errors ();		   (void) fprintf (stderr, "\n");
   StoreMessages (true);
   errors ();
   WriteMessages (stderr); (void) fprintf (stderr, "\n");
   WriteMessages (stdout); (void) fprintf (stdout, "\n");
   StoreMessages (true);
   WriteMessages (stdout);
   return 0;
}
