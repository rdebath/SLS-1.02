/* File text2font.c
 * Aronsson was here
 * 13 February 1991
 *
 * This file contains no Bellcore-copyrighted code,
 * but is sensitive to changes in the font file format.
 *
 * Convert from text to MGR font file
 */

#include <stdlib.h>			/* malloc */
#include <stdio.h>

#define ONE '*'
#define ZERO '.'

int main()
{
  int f_type, f_wide, f_high, f_base, f_count, f_start;
  int nbits;				/* # of bits total */
  int c;
  int bpl;				/* # of bits per scan line */
  char *bigbuf;
  char *cp;
  int bit;
  int i;
  int line;
  
  if (6 != scanf ("%d %d %d %d %d %d",
		  &f_type, &f_wide, &f_high,
		  &f_base, &f_count, &f_start))
    {
      fprintf (stderr, "Sorry, cannot read input\n");
      exit (1);
    }
      
  if (f_type != 24)
    {
      fprintf(stderr,"Sorry, Input is not a font\n");
      exit(3);
    }
  
  putc (f_type, stdout);
  putc (f_wide, stdout);
  putc (f_high, stdout);
  putc (f_base, stdout);
  putc (f_count, stdout);
  putc (f_start, stdout);
  
  bpl = ((f_wide*f_count)+31)&~31; /* fonts always 32 bit padded */
  nbits = bpl * f_high;
  
  cp = bigbuf = malloc (nbits);
  if (! cp)
    {
      fprintf (stderr, "Sorry, cannot malloc\n");
      exit(19);
    }
  
  
  for (i = 0; i < nbits; i++)
    *(bigbuf + i) = ZERO;
  
  for (i = 0; i < f_count; i++)
    {
      for (line = 0; line < f_high; line++)
	{
	  for (bit = 0; bit < f_wide; bit++)
	    {
	      do
		{
		  c = getchar ();
		}
	      while (c != ZERO && c != ONE && c != EOF);
	      if (c == EOF)
		{
		  fprintf (stderr, "Input ended at char %d.\n",
			   i + f_start);
		  goto premature;
		}
	      (*(bigbuf+i*f_wide+line*bpl+bit)) = (char) c;
	    }
	}
    }

 premature:
  cp = bigbuf;
  for (i = 0; i < nbits/8; i++)
    {
      c = 0;
      for (bit=7; bit>=0; bit--)
	c |= (*(cp++) == ONE) ? (1<<bit) : 0;
      putchar (c);
    }
  fflush (stdout);
  
  return 0;
}

