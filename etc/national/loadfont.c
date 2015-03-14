
/*
loadfont (c) 1993 Pavel Zaboj under linux copyright
*/

#include <stdio.h>
#include <errno.h>
#include <getopt.h>

#include "config.h"

#define ESC 0x1b
#define MAXLINE 256
#define MAXCP   2

char
hinyb (char ch)
{
  ch >>= 4;
  ch &= 0x0f;
  ch += '0';
  if (ch > '9')
    ch += 7;
  return ch;
}

char
lonyb (char ch)
{
  ch &= 0x0f;
  ch += '0';
  if (ch > '9')
    ch += 7;
  return ch;
}

void
usage ()
{
  fprintf (stderr, "Usage: loadfont [-d dir] font_file page\n");
  exit (1);
}

int
main (int argc, char *argv[])
{
  int i, j = 0;
  char *dir = FONTDIR;		/* directory with font files */
  FILE *file;
  unsigned char buf[8192];
  char size, page;
  int optch;

  if ((optch = getopt (argc, argv, "d:")) != EOF)
    if (optch == 'd')
      dir = optarg;
    else
      usage ();

  if (argc < 3)
    usage ();
  page = atoi (argv[optind + 1]);
  for (i = 0; i < 8192; i++)
    buf[i] = 0;
  if (page < 1 || page > MAXCP)
    {
      fprintf (stderr, "loadfont: page must be 1 or 2\n");
      return 1;
    }

  if (chdir (dir) < 0)
    {
      fprintf (stderr, "loadfont: cannot change to directory %s: %s\n", dir, strerror (errno));
      return 1;
    }

  if ((file = fopen (argv[optind], "r")) == NULL)
    {
      fprintf (stderr, "loadfont: cannot open %s: %s\n", argv[optind], strerror (errno));
      return 1;
    }
  fgetc (file);
  fgetc (file);
  fgetc (file);
  size = fgetc (file);
  for (i = 0; i < 256; i++)
    for (j = 0; j < size; j++)
      buf[i * 32 + j] = fgetc (file);
  putchar (ESC);
  putchar ('F');
  putchar ('P');
  putchar (*argv[optind + 1]);
  for (i = 0; i < 8192; i++)
    {
      putchar (hinyb (buf[i]));
      putchar (lonyb (buf[i]));
    }


}
