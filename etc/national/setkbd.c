/*
setkbd - change keymap on a linux system.
(c) Pavel Zaboj 1993 under linux copyright
*/

#include <stdio.h>
#include <getopt.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <linux/kd.h>


#include "config.h"
#define MAXLINE 256

se ()
{
  fprintf (stderr, "setkbd: syntax error\n");
  exit (1);
}

void
usage ()
{
  fprintf (stderr, "Usage: setkbd [-d dir] keymap_file diacr_file diacr_mask\n");
  exit (1);
}

int
main (int argc, char *argv[])
{
  int optch;
  char *dir = KBDDIR;
  struct kd_keymap map;
  FILE *file;
  char line[MAXLINE + 2];
  char *start;
  int p[4], i, j;
  char ch;

  if ((optch = getopt (argc, argv, "d:")) != EOF)
    if (optch == 'd')
      dir = optarg;
    else
      usage ();

  if (argc < 4)
    usage ();

  if (chdir (dir) < 0)
    {
      fprintf (stderr, "setkbd: cannot change to directory %s: %s\n", dir, strerror (errno));
      return 1;
    }

  if ((map.kbd_flags = atoi (argv[optind + 2])) < 0)
    {
      fprintf (stderr, "setkbd: diacr_mask error\n");
      return 1;
    }

  for (i = 0; i < 97; i++)
    map.key_map[i] = map.shift_map[i] = map.alt_map[i] = 0;
  for (i = 0; i < NUM_ACC; i++)
    {
      map.accent_table[i][0] = 32;
      for (j = 1; j < 63; j++)
	map.accent_table[i][j] = j + 64;
    }
  for (i = 0; i < 13; i++)
    map.lock_state[i] = 0;
  for (i = 0; i <= NUM_ACC; i++)
    map.diacr_table[i] = 0;

  if ((file = fopen (argv[optind], "r")) == NULL)
    {
      fprintf (stderr, "setkbd: cannot open %s: %s\n", argv[optind], strerror (errno));
      return 1;
    }
  while (fgets (line, MAXLINE, file))
    {
      start = line;
      while (*start == ' ' || *start == '\t')
	start++;
      if (*start && *start != '\n' && *start != '#')
	{
	  for (i = 0; i < 4; i++)
	    p[i] = -1;
	  ch = 0;
	  for (i = 0; i < 4; i++)
	    {
	      while (*start == ' ' || *start == '\t')
		start++;
	      if (*start == '\'')
		{
		  p[i] = *(++start);
		  start++;
		  start++;
		}
	      else if ((p[i] = strtol (start, &start, 0)) < 0 || p[i] > 255)
		se ();
	    }
	  while (*start == ' ' || *start == '\t')
	    start++;
	  if ((ch = *start) != 'C' && ch != 'O')
	    se ();
	}
      map.key_map[p[0]] = p[1];
      map.shift_map[p[0]] = p[2];
      map.alt_map[p[0]] = p[3];
      map.lock_state[p[0] / 8] |= (ch == 'C') ? 1 << (p[0] % 8) : 0;
    }
  i = -1;
  if ((file = fopen (argv[optind + 1], "r")) == NULL)
    {
      fprintf (stderr, "setkbd: cannot open %s: %s\n", argv[optind + 1], strerror (errno));
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
	    case 'd':
	      if ((p[2] = strtol (start + 4, &start, 0)) < 0
		  || p[2] > 255)
		se ();
	      i++;
	      if (i == NUM_ACC)
		{
		  fprintf (stderr, "setkbd: too many dead keys\n");
		  exit (1);
		}
	      map.diacr_table[i] = p[2];
	      goto aaa;
	      break;
	    case '#':
	    case '\n':
	      break;
	    default:
	      if (i < 0)
		se ();
	      for (j = 0; j < 2; j++)
		p[j] = -1;
	      for (j = 0; j < 2; j++)
		{
		  while (*start == ' ' || *start == '\t')
		    start++;
		  if (*start == '\'')
		    {
		      p[j] = *(++start);
		      start++;
		      start++;
		    }
		  else if ((p[j] = strtol (start, &start, 0)) < 0 || p[j] > 255)
		    se ();
		}
	      map.accent_table[i][p[0] - 64] = p[1];
	      break;
	    }
	}
    aaa:
    }
  if (ioctl (2, KDSETKBD, &map) < 0)
    {
      fprintf (stderr, "setkbd: ioctl failed: %s\n", strerror (errno));
      return 1;
    }

  return 0;
}
