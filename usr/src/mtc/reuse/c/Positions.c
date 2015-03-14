/* $Id: Positions.c,v 1.2 1992/08/13 13:47:33 grosch rel $ */

/* $Log: Positions.c,v $
 * Revision 1.2  1992/08/13  13:47:33  grosch
 * increase format in WritePosition
 *
 * Revision 1.1  1992/08/13  12:29:12  grosch
 * fix bugs with ANSI C
 *
 * Revision 1.0  1992/08/07  14:31:42  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

static char rcsid [] = "$Id: Positions.c,v 1.2 1992/08/13 13:47:33 grosch rel $";

# include "Positions.h"

tPosition NoPosition = {0, 0};

int Compare
# if defined __STDC__ | defined __cplusplus
   (tPosition Position1, tPosition Position2)
# else
   (Position1, Position2) tPosition Position1, Position2;
# endif
{
   if (Position1.Line   < Position2.Line  ) return -1;
   if (Position1.Line   > Position2.Line  ) return  1;
   if (Position1.Column < Position2.Column) return -1;
   if (Position1.Column > Position2.Column) return  1;
   return 0;
}

void WritePosition
# if defined __STDC__ | defined __cplusplus
   (FILE * File, tPosition Position)
# else
   (File, Position) FILE * File; tPosition Position;
# endif
{
   (void) fprintf (File, "%4d,%3d", Position.Line, Position.Column);
}
