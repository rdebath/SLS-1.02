/*
 * A mount(8) for Linux 0.97.
 * $Header: /usr/src/mount/RCS/mount.c,v 1.1 1992/09/06 13:30:53 root Exp root $
 */ 

#include "sundries.h"

#include <linux/fs.h>

const char *usage = "\
Usage: mount [-afrvw] [-t vfstypes]
       mount [-frvw] special | node
       mount [-frvw] [-t vfstype] [-o options] special node";

#ifdef notyet
/* True for change options of an already mounted fs (-u).  */
int update = 0;
#endif

/* True for fake mount (-f).  */
int fake = 0;

/* True for readonly (-r).  */
int readonly = 0;

/* Nonzero for chatty (-v).  */
int verbose = 0;

/* True for read/write (-w).  */
int readwrite = 0;

/* True for all mount (-a).  */
int all = 0;

/* Map from -o and fstab option strings to the flag argument to mount(2).  */
struct opt_map
{
  const char *opt;		/* option name */
  int  inv;			/* true if flag value should be inverted */
  int  mask;			/* flag mask value */
};

const struct opt_map opt_map[] =
{
  { "defaults",	0, 0		},	/* default options */
  { "ro",	0, MS_RDONLY	},	/* read-only */
  { "rw",	1, MS_RDONLY	},	/* read-write */
  { "exec",	1, MS_NOEXEC	},	/* permit execution of binaries */
  { "noexec",	0, MS_NOEXEC	},	/* don't execute binaries */
  { "suid",	1, MS_NOSUID	},	/* honor suid executables */
  { "nosuid",	0, MS_NOSUID	},	/* don't honor suid executables */
  { "dev",	1, MS_NODEV	},	/* interpret device files  */
  { "nodev",	0, MS_NODEV	},	/* don't interpret devices */
  { "sync",	0, MS_SYNC	},	/* synchronous I/O */
  { "async",	1, MS_SYNC	},	/* asynchronous I/O */
#ifdef MS_NOSUB
  { "sub",	1, MS_NOSUB	},	/* allow submounts */
  { "nosub",	0, MS_NOSUB	},	/* don't allow submounts */
#endif
  /* add new options here */
  { NULL,	0, 0		}
};


/* Report on a single mount.  */
static void
print_one (const struct mntent *mnt)
{
  printf ("%s on %s", mnt->mnt_fsname, mnt->mnt_dir);
  if ((mnt->mnt_type != NULL) && *mnt->mnt_type != '\0')
    printf (" type %s", mnt->mnt_type);
  if (mnt->mnt_opts != NULL)
    printf (" (%s)", mnt->mnt_opts);
  printf ("\n");
}

/* Report on everything in mtab (of the specified types if any).  */
static volatile int
print_all (string_list types)
{
  struct mntent *mnt;
  
  open_mtab ("r");

  while ((mnt = getmntent (F_mtab)) != NULL)
    if (matching_type (mnt->mnt_type, types))
      print_one (mnt);

  if (ferror (F_mtab))
    die (1, "mount: error reading %s: %s", MOUNTED, strerror (errno));

  exit (0);
}


/* Look for OPT in opt_map table and return mask value.  If OPT isn't found,
   tack it onto extra_opts.  Due to laziness, the first char of extra_opts
   will be ',' if any extra options are found.  */
static inline void
parse_opt (const char *opt, int *mask, char *extra_opts)
{
  const struct opt_map *om;

  for (om = opt_map; om->opt != NULL; ++om)
    if (streq (opt, om->opt))
      {
	if (om->inv)
	  *mask &= ~om->mask;
	else
	  *mask |= om->mask;
	return;
      }
  strcat (extra_opts, ",");
  strcat (extra_opts, opt);
}
  
/* Take -o options list and compute 4th and 5th args to mount(2).  flags
   gets the standard options and extra_opts anything we don't recognize.  */
static void
parse_opts (char *opts, int *flags, char **extra_opts, int *noauto)
{
  char *opt;

  *noauto = 0;
  if (opts != NULL)
    {
      *extra_opts = xmalloc (strlen (opts) + 2); 
      **extra_opts = '\0';

      for (opt = strtok (opts, ",");
	   opt != NULL;
	   opt = strtok (NULL, ","))
	if (streq (opt, "noauto"))
	  *noauto = 1;
	else
	  parse_opt (opt, flags, *extra_opts);
    }

  if (readonly)
    *flags |= MS_RDONLY;
  if (readwrite)
    *flags &= ~MS_RDONLY;
}

/* Fix opts_string to defaults if NULL, else merge readonly and readwrite.
   Simplistic, merging ``-r'' with ``-o rw''a will give ``ro,rw''.  */
static char *
fix_opts_string (char *opts)
{
  char *fixed;

  if (readonly || readwrite)
    {
      if (opts == NULL)
	return (readonly ? "ro" : "rw");

      fixed = xmalloc (strlen (opts) + 4);
	
      strcpy (fixed, readonly ? "ro," : "rw,");
      strcat (fixed, opts);
      return fixed;
    }
  return (opts == NULL ? "defaults" : opts);
}

/* Mount a single file system.  Return status,
   so don't exit on non-fatal errors.  */
static int
mount_one (char *spec, char *node, char *type, char *opts)
{
  struct mntent mnt = {0};
  int mnt_err;
  int flags = 0;
  char *extra_opts = NULL;
  int noauto = 0;

  if (opts && streq (opts, "defaults"))
    opts = NULL;

  if (type == NULL)
    {
      if (strchr (spec, ':') != NULL)
	type = "nfs";
      else
	type = FSTYPE_DEFAULT;
    }

  parse_opts (xstrdup (opts), &flags, &extra_opts, &noauto);

  /* quietly succeed for fstab entries that don't get mounted automatically */
  if (all && noauto)
    return 0;

  if (!fake && streq (type, "nfs"))
#ifdef HAVE_NFS
    if (nfsmount (spec, node, &flags, &opts, &extra_opts) != 0)
      return 1;
#else
    die (1, "mount: this version of mount doesn't support the type nfs");
#endif

  block_signals (SIG_BLOCK);

  if (fake || (mount5 (spec, node, type, flags, extra_opts)) == 0)
    /* Mount succeeded, write mtab entry.  */
    {
      mnt.mnt_fsname = canonicalize (spec);
      mnt.mnt_dir = canonicalize (node);
      mnt.mnt_type = type;
      mnt.mnt_opts = fix_opts_string (opts);
      
      /* We get chatty now rather than after the update to mtab since the
	 mount succeeded, even if the write to /etc/mtab should fail.  */
      if (verbose)
	print_one (&mnt);

      if (!fake && (addmntent (F_mtab, &mnt)) == 1)
	die (1, "mount: error writing %s: %s", MOUNTED, strerror (errno));

      block_signals (SIG_UNBLOCK);
      return 0;
    }
    else
      mnt_err = errno; /* work around for errno bug in sigprocmask */

  block_signals (SIG_UNBLOCK);

  /* Mount failed, complain, but don't die.  */
  switch (mnt_err)
    {
    case EPERM:
      if (geteuid() == 0)
	error ("mount: mount point %s is not a directory", node);
      else
	error ("mount: must be superuser to use mount");
      break;
    case EBUSY:
      error ("mount: wrong fs type, %s already mounted, %s busy, "
	"or other error", spec, node);
      break;
    case ENOENT:
      error ("mount: mount point %s does not exist", node); break;
    case ENOTDIR:
      error ("mount: mount point %s is not a directory", node); break;
    case EINVAL:
      error ("mount: %s not a mount point", spec); break;
    case EMFILE:
      error ("mount table full"); break;
    case EIO:
      error ("mount: %s: can't read superblock", spec); break;
    case ENODEV:
      error ("mount: fs type %s not supported by kernel", type); break;
    case ENOTBLK:
      error ("mount: %s is not a block device", spec); break;
    case ENXIO:
      error ("mount: %s is not a valid block device", spec); break;
    case EACCES:
      error ("mount: block device %s is not permitted on its filesystem", spec);
      break;
    default:
      error ("mount: %s", strerror (mnt_err)); break;
    }
  return 1;
}

/* Check if an fsname/dir pair was already in the old mtab.  */
static int
mounted (char *spec, char *node, string_list spec_list, string_list node_list)
{
  spec = canonicalize (spec);
  node = canonicalize (node);

  while (spec_list != NULL)
    {
      if (streq (spec, car (spec_list)) && streq (node, car (node_list)))
	return 1;
      spec_list = cdr (spec_list);
      node_list = cdr (node_list);
    }
    return 0;
}

/* Mount all filesystems of the specified types except swap and root.  */
static int
mount_all (string_list types)
{
  struct mntent *fstab;
  struct mntent *mnt;
  string_list spec_list = NULL;
  string_list node_list = NULL;
  int status;

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

  lock_mtab ();
  open_mtab ("a+");
  unlink (MOUNTED_LOCK);

  status = 0;
  while ((fstab = getfsent ()) != NULL)
    if (matching_type (fstab->mnt_type, types)
	 && !streq (fstab->mnt_dir, "/")
	 && !streq (fstab->mnt_dir, "root"))
      if (mounted (fstab->mnt_fsname, fstab->mnt_dir, spec_list, node_list))
	{
	  if (verbose)
	    printf("mount: %s already mounted on %s\n",
		   fstab->mnt_fsname, fstab->mnt_dir);
	}
      else
        status |= mount_one (fstab->mnt_fsname,
			     fstab->mnt_dir,
			     fstab->mnt_type,
			     fstab->mnt_opts);

  return status;
}

int
main (int argc, char **argv)
{
  int opt;
  char *options = NULL;
  string_list types = NULL;
  struct mntent *fs;
  char *spec;

  while ((opt = getopt (argc, argv, "aft:o:ruvw")) != EOF)
    switch (opt)
      {
      case 'a':			/* mount everything in fstab */
	++all;
	break;
      case 'f':			/* fake (don't actually do mount(2) call) */
	++fake;
	break;
      case 't':			/* specify file system types */
	types = parse_types (optarg);
	break;
      case 'o':			/* specify mount options */
	options = optarg;
	break;
      case 'r':			/* mount readonly */
	++readonly;
	break;
#ifdef notyet
/* Needs kernel support that we don't have yet, & some rewrites here also.  */
      case 'u':			/* change options on an already mounted fs */
	++change; 
	break;
#endif
      case 'v':			/* be chatty */
	++verbose;
	break;
      case 'w':			/* mount read/write */
	++readwrite;
	break;
      default:
	die (2, usage);
      }

  argc -= optind;
  argv += optind;

  if (readonly && readwrite)
    die (2, "mount: the -r and -w options conflict\n%s", usage);

  if (argc == 0)
    if (options)
      die (2, usage);
    else if (all)
      return mount_all (types);
    else
      return print_all (types);

  lock_mtab ();
  open_mtab ("a+");
  unlink (MOUNTED_LOCK);

  switch (argc)
    {
    case 1:
      /* mount [-frvw] special | node */
      if ((types != NULL) || (options != NULL))
	die (2, usage);
      /* Try to find the other pathname in fstab.  */ 
      spec = canonicalize (*argv);
      if (!(fs = getfsspec (spec)) && !(fs = getfsfile (spec)))
	die (2, "mount: can't find %s in %s\n", spec, _PATH_FSTAB);
      return mount_one (fs->mnt_fsname, fs->mnt_dir,
			fs->mnt_type, fs->mnt_opts);
    case 2:
      /* mount [-frvw] [-t vfstype] [-o options] special node */
      if (types == NULL)
	return mount_one (argv[0], argv[1], NULL, options);
      if (cdr (types) == NULL)
	return mount_one (argv[0], argv[1], car (types), options);
      die (2, usage);
      
    default:
      die (2, usage);
    }
}
