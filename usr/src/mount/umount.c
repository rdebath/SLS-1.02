/*
 * A umount(8) for Linux 0.97.
 * $Header: /usr/src/mount/RCS/umount.c,v 1.1 1992/09/06 13:30:53 root Exp root $
 */

#include "sundries.h"


char *usage = "\
Usage: umount [-av] [-t vfstypes]
       umount [-v] special | node";

#ifdef notyet
/* Nonzero for force mount (-f).  This needs kernel support we don't have.  */
int force = 0;
#endif

/* Nonzero for chatty (-v).  This is a nonstandard flag (not in BSD).  */
int verbose = 0;


/* Update the locked mtab by removing any SPECIAL entries and unlocking it.  */
static void
update_mtab (const char *special)
{
  struct mntent *mnt;

  open_mtab ("r");

  while ((mnt = getmntent (F_mtab)))
    if (!streq (mnt->mnt_fsname, special))
      if (addmntent(F_lock, mnt) == 1)
	die (1, "umount: error writing %s: %s",
	     MOUNTED_LOCK, strerror (errno));

  endmntent (F_mtab);
  if (fchmod (fileno (F_lock), S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH) < 0)
    die (1, "umount: error changing mode of %s: %s",
	MOUNTED_LOCK, strerror (errno));
  endmntent (F_lock);

  if (rename (MOUNTED_LOCK, MOUNTED) < 0)
    die (1, "umount: can't rename %s to %s: %s",
	 MOUNTED_LOCK, MOUNTED, strerror(errno));
}

/* Umount a single device.  Return a status code, so don't exit
   on a non-fatal error.  We lock/unlock around each umount.  */
static int
umount_one (const char *spec, const char *node)
{
  const char *what;
  int umnt_err;

  what = (*spec == '/') ? spec : node;
  lock_mtab ();

  if (umount (what) >= 0)
    /* Umount succeeded, update mtab.  */
    {
      if (verbose)
	printf ("%s umounted\n", spec);

      update_mtab (spec);
      return 0;
    }

  /* Umount failed, complain, but don't die.  */
  umnt_err = errno;
  unlock_mtab ();

  switch (umnt_err)
    {
    case ENXIO:   error ("umount: %s: invalid block device", spec); break;
    case EINVAL:  error ("umount: %s: not mounted", spec); break;
    case EIO:     error ("umount: %s: can't write superblock", spec); break;
    case EBUSY:   error ("umount: %s: device is busy", spec); break;
    case ENOENT:  error ("umount: %s: not mounted", spec); break;
    case EPERM:   error ("umount: %s: must be superuser to umount", spec); break;
    case EACCES:  error ("umount: %s: block devices not permitted on fs", spec); break;
    default:      error ("umount: %s: %s", spec, strerror (umnt_err)); break;
    }
  return 1;
}

/* Unmount all filesystems of type VFSTYPES found in mtab.  Naturally
   we never make the futile attempt to unmount "/".  Since we are
   concurrently updating mtab after every succesful umount, we have to
   slurp in the entire file before we start.  This isn't too bad, because
   in any case it's important to umount mtab entries in reverse order
   to umount, e.g. /usr/spool before /usr.  */
static int
umount_all (string_list types)
{
  string_list spec_list = NULL;
  string_list node_list = NULL;
  struct mntent *mnt;
  int errors;

  open_mtab ("r");

  while ((mnt = getmntent (F_mtab)))
    if (matching_type (mnt->mnt_type, types)
	&& !streq (mnt->mnt_dir, "/")
	&& !streq (mnt->mnt_dir, "root"))
      {
	spec_list = cons (xstrdup (mnt->mnt_fsname), spec_list);
	node_list = cons (xstrdup (mnt->mnt_dir), node_list);
      }

  close_mtab ();

  errors = 0;
  while (spec_list != NULL)
    {
      errors |= umount_one (car (spec_list), car (node_list));
      spec_list = cdr (spec_list);
      node_list = cdr (node_list);
    }

  sync(); /* since we can't umount root */

  return errors;
}

/* Given the name FILE, try to find it in mtab.  */ 
static struct mntent *
getmntfile (const char *file)
{
  struct mntent *mnt;

  open_mtab ("r");

  while ((mnt = getmntent (F_mtab)) != NULL)
    {
      if (streq (mnt->mnt_dir, file))
	break;
      if (streq (mnt->mnt_fsname, file))
	break;
    }

  close_mtab ();

  return mnt;
}

int
main (int argc, char **argv)
{
  int opt;
  int all = 0;
  string_list types = NULL;
  struct mntent *fs;
  char *file;

  while ((opt = getopt (argc, argv, "aft:v")) != EOF)
    switch (opt)
      {
      case 'a':			/* umount everything (except root!) */
	++all;
	break;
#ifdef notyet
      case 'f':			/* force umount (needs kernel support) */
	++force;
	break;
#endif
      case 't':			/* specify file system type */
	types = parse_types (optarg);
	break;
#ifdef notyet
      case 'u':			/* change mount flags (update a mounted fs) */
	++update;
	break;
#endif
      case 'v':			/* make noise */
	++verbose;
	break;
      default:
	die (2, usage);
      }

  argc -= optind;
  argv += optind;

  if (all)
    return umount_all (types);

  if (argc != 1)
    die (2, usage);

  file = canonicalize (*argv); /* mtab paths are canonicalized */

  if ((fs = getmntfile (file)) != NULL)
    return umount_one (xstrdup (fs->mnt_fsname), xstrdup(fs->mnt_dir));

  return umount_one (*argv, NULL);
}
