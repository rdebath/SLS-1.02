
/*
dumpfont (c) 1993 Pavel Zaboj under linux copyright
*/

#include <stdio.h>
#include <errno.h>

main (int argc, char *argv[])
{
  char a, b;
  int i, j, size;
  FILE *file1, *file2;
  if (argc != 3)
    {
      fprintf (stderr, "usage: dumpfont infile outfile\n");
      exit (1);
    }
  if ((file1 = fopen (argv[1], "r")) == NULL)
    {
      fprintf (stderr, "dumpfont: cannot open %s: %s\n", argv[1], strerror (errno));
      return 1;
    }

  fgetc (file1);
  fgetc (file1);
  fgetc (file1);
  size = fgetc (file1);

  if ((file2 = fopen (argv[2], "w")) == NULL)
    {
      fprintf (stderr, "dumpfont: cannot open %s: %s\n", argv[2], strerror (errno));
      return 1;
    }
  fprintf (file2, "size %u\n", size);
  for (i = 0; i < 256; i++)
    {
      fprintf (file2, "\nchar %u\n", i);
      for (b = 0; b < size; b++)
	{
	  a = fgetc (file1);
	  for (j = 0; j < 8; j++)
	    if ((a << j) & 128)
	      fputc ('@', file2);
	    else
	      fputc ('.', file2);
	  fputc ('\n', file2);
	}
    }
  fclose (file2);
  fclose (file1);
}
