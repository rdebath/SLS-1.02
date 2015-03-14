/* $Id: Source.c,v 1.0 1992/08/07 14:31:43 grosch rel $ */

/* $Log: Source.c,v $
 * Revision 1.0  1992/08/07  14:31:43  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

static char rcsid [] = "$Id: Source.c,v 1.0 1992/08/07 14:31:43 grosch rel $";

# include "Source.h"

# ifdef __cplusplus
extern "C" {
# include "System.h"
}
# else
# include "System.h"
# endif

int BeginSource
# if defined __STDC__ | defined __cplusplus
   (char * FileName)
# else
   (FileName) char * FileName;
# endif
{
   return OpenInput (FileName);
}

int GetLine
# if defined __STDC__ | defined __cplusplus
   (int File, char * Buffer, int Size)
# else
   (File, Buffer, Size) int File; char * Buffer; int Size;
# endif
{
   register int n = Read (File, Buffer, Size);
# ifdef Dialog
# define IgnoreChar ' '
   /* Add dummy after newline character in order to supply a lookahead for rex. */
   /* This way newline tokens are recognized without typing an extra line.      */
   if (n > 0 && Buffer [n - 1] == '\n') Buffer [n ++] = IgnoreChar;
# endif
   return n;
}

void CloseSource
# if defined __STDC__ | defined __cplusplus
   (int File)
# else
   (File) int File;
# endif
{
   Close (File);
}
