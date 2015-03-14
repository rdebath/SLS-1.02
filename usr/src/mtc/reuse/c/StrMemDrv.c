static char rcsid [] = "$Id: StrMemDrv.c,v 1.4 1992/09/24 13:03:56 grosch rel $";

# include <stdio.h>
# include "StringMem.h"

static void loop ()
{
   char string [256];
   tStringRef stringref;

   (void) printf ("enter strings, one per line, - terminates\n");
   do {
      (void) scanf ("%s", string);
      stringref = PutString (string, strlen (string));
      WriteString (stdout, stringref);
      (void) printf ("\n");
   } while (string [0] != '-' || string [1] != '\0');
   (void) printf ("\n");
   WriteStringMemory ();
}

main ()
{
   loop ();
   InitStringMemory ();
   loop ();
   return 0;
}
