# ifndef xySource
# define xySource

/* $Id: Source.h,v 1.0 1992/08/07 14:31:44 grosch rel $ */

/* $Log: Source.h,v $
 * Revision 1.0  1992/08/07  14:31:44  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

# if defined __STDC__ | defined __cplusplus
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

extern int  BeginSource  ARGS((char * FileName));

   /*
      BeginSource is called from the scanner to open files.
      If not called input is read form standard input.
   */

extern int  GetLine      ARGS((int File, char * Buffer, int Size));

   /*
      GetLine is called to fill a buffer starting at address 'Buffer'
      with a block of maximal 'Size' characters. Lines are terminated
      by newline characters (ASCII = 0xa). GetLine returns the number
      of characters transferred. Reasonable block sizes are between 128
      and 2048 or the length of a line. Smaller block sizes -
      especially block size 1 - will drastically slow down the scanner.
   */

extern void CloseSource  ARGS((int File));

   /*
      CloseSource is called from the scanner at end of file respectively
      at end of input. It can be used to close files.
   */

# endif
