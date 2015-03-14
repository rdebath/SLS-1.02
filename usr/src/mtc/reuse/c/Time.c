static char rcsid [] = "$Id: Time.c,v 1.4 1992/05/05 13:19:05 grosch rel $";

# include "Time.h"
# include <stdio.h>
# include "System.h"

static int old = 0;

int StepTime ()
{
   int new = Time ();
   int result = new - old;
   old = new;
   return result;
}

void WriteStepTime
# ifdef __STDC__
   (char * string)
# else
   (string) char * string;
# endif
{
   (void) printf ("%s %5d\n", string, StepTime ());
}
