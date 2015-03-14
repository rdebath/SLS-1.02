
/*
makefont (c) 1993 Pavel Zaboj under linux copyright
*/

#include <stdio.h>
#include <errno.h>

#define MAXLINE 256

se ()
{
  fprintf (stderr, "loadfont: syntax error\n");
  exit (1);
}

void
usage ()
{
  fprintf (stderr, "Usage: makefont infile outfile\n");
  exit (1);
}

int
main (int argc, char *argv[])
{
  int i, j = 0, ch = -1;
  FILE *file;
  unsigned char buf[8192];
  char size;
  char line[MAXLINE + 2];
  char *start;

  if (argc != 3)
    usage ();

  for (i = 0; i < 8192; i++)
    buf[i] = 0;

  if ((file = fopen (argv[1], "r")) == NULL)
    {
      fprintf (stderr, "makefont: cannot open %s: %s\n", argv[2], strerror (errno));
      return 1;
    }

  while (fgets (line, MAXLINE, file))
    {
      start = line;
      if (*start && *start != '\n' && *start != '#')
	{
	  while (*start == ' ' || *start == '\t')
	    start++;
	  switch (*start)
	    {
	    case '.':
	    case '@':
	      if (!size || ch < 0 || j > size)
		se ();
	      for (i = 0; i < 8; i++)
		{
		  if (!(*start == '@' || *start == '.'))
		    se ();
		  buf[32 * ch + j] |= (*start == '@') ? 128 >> i : 0;
		  start++;
		}
	      if (j > size - 2)
		{
		  ch = -1;
		  j = 0;
		}
	      else
		j++;
	      break;
	    case 's':
	      if (size || (size = strtol (start + 4, &start, 0)) < 1 || size > 32)
		se ();
	      break;
	    case 'c':
	      if (!size)
		se ();
	      if (j || (ch = strtol (start + 4, &start, 0)) < 0 || ch > 255)
		se ();
	      break;
	    case '\n':
	    case '#':
	      break;
	    default:
	      se ();
	    }
	}
    }

  fclose (file);

  if ((file = fopen (argv[2], "w")) == NULL)
    {
      fprintf (stderr, "makefont: cannot open %s: %s\n", argv[2], strerror (errno));
      return 1;
    }
  fputc (0x36, file);
  fputc (0x04, file);
  fputc (0x00, file);
  fputc (size, file);

  for (i = 0; i < 256; i++)
    for (j = 0; j < size; j++)
      fputc (buf[32 * i + j], file);

}
