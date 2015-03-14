/* swapon(8)/swapoff(8) for Linux 0.97.3.
   $Header: /usr/src/mount/RCS/swapon.c,v 1.1 1992/09/06 13:30:53 root Exp root $ */

#include "sundries.h"

static char const * progname;

static volatile void
usage (void)
{
  fprintf (stderr, "Usage: %s [-a] [special_file ...]\n", progname);
  exit (2);
}

static int
swap (const char *special)
{
  int status;

  if (streq (progname, "swapon"))
    status = swapon (special);
  else
    status = swapoff (special);

  if (status < 0)
    fprintf (stderr, "%s: %s: %s\n", progname, special, strerror (errno));

  return status;
}

int
main(int argc, char **argv)
{
  struct fstab *fstab;
  int status;
  int opt;
  int all = 0;

  if (strrchr (argv[0], '/') != NULL)
    progname = strrchr (argv[0], '/') + 1;
  else
    progname = argv[0];

  while ((opt = getopt(argc, argv, "a")) != EOF)
    switch(opt)
      {
      case 'a':	++all; break;
      default:  usage ();
      }

  argv += optind;

  status = 0;

  if (all)
    {
      while ((fstab = getfsent()) != NULL)
	if (streq (fstab->fs_type, FSTAB_SW))
	  status |= swap (fstab->fs_spec);
    }
  else if (*argv == NULL)
    {
      usage ();
    }
  else
    {
      while (*argv != NULL)
	status |= swap (*argv++);
    }
  return status;
}
