/*
 * checkfn - check file names for bad stuff
 *
 *    1.  non-RCS filenames are 12 characters or less
 *    2.  filenames contain [a-zA-Z0-9_-.] and , if in RCS directory
 *    3.  file not a symbolic link
 *    4.  directory has at least 0775
 *    5.  non-directory does not have multiple hard links
 *    6.  protection bits are at least 444 and less than or equal to 777
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Jim Fulton, MIT X Consortium
 */

#include <stdio.h>			/* for printf */
#include <X11/Xos.h>			/* for types and strings */
#include <sys/stat.h>			/* for stat */
#include <errno.h>			/* for errno */

extern int errno;			/* errno.h is very stupid */
extern int sys_nerr;
extern char *sys_errlist[];

#define streq(a,b) (strcmp ((a), (b)) == 0)

#define CHARSALLOWED \
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."

char *ProgramName;			/* for printing error messages */
unsigned int fmode_bits_minset = 0444;	/* want every file to have */
unsigned int fmode_bits_maxset = 0777;	/* don't want outside of this */
unsigned int dmode_bits_minset = 0775;	/* want every dir to have */
int dotfiles_allowed = 0;		/* complain about hidden files */

static void usage ()
{
    fprintf (stderr, "usage:  %s [-min modes] [-max modes] [directory]\n",
	     ProgramName);
    fprintf (stderr, "\n");
    exit (1);
}

static char *SysError ()
{
    return (errno > 0 && errno < sys_nerr) ? sys_errlist[errno] :
	    "unknown error";
}

unsigned int parse_num (s)
    char *s;
{
    char *fmt = "%u";
    unsigned int retval = 0;

    if (*s == '0') s++, fmt = "%o";
    if (*s == 'x' || *s == 'X') s++, fmt = "%x";
    (void) sscanf (s, fmt, &retval);
    return retval;
}

main (argc, argv)
    int argc;
    char *argv[];
{
    int i;

    ProgramName = argv[0];

    for (i = 1; i < argc; i++) {
	char *arg = argv[i];

	if (streq (arg, "-min")) {
	    if (++i >= argc) usage ();
	    fmode_bits_minset = parse_num (argv[i]);
	    continue;
	} else if (streq (arg, "-max")) {
	    if (++i >= argc) usage ();
	    fmode_bits_maxset = parse_num (argv[i]);
	    continue;
	} else if (streq (arg, "-dot")) {
	    dotfiles_allowed = 1;
	    continue;
	} else if (arg[0] != '-') {
	    check (arg);
	    continue;
	}
	usage ();
    }

    exit (0);
}

check (filename)
    char *filename;
{
    char *cp= rindex (filename, '/');
    char *base = cp ? cp + 1 : filename;
    int maxlen = 12, len = strlen (base);
    int rcsslop = 0;
    struct stat st;
    unsigned int mode;

    if (base[0] == '.' && !dotfiles_allowed) {
	if (!(base[1] == '\0' || (base[1] == '.' && base[2] == '\0'))) {
	    printf ("%s\t\thidden file beginning with dot\n", filename);
	}
	return;
    }

    for (len = 0, cp = base; *cp; len++, cp++) {
	if (*cp == ',') {
	    if (cp[1] != 'v' || cp[2] != '\0') {
		printf ("%s\t\tbad character ',' in filename\n", filename);
		return;
	    }
	    if ((base - 4) < filename || strncmp (base - 4, "RCS/", 4) != 0) {
		printf ("%s\t\tRCS file not in RCS directory\n", filename);
		return;
	    }
	    rcsslop = 2;
	} else if (index (CHARSALLOWED, *cp) == NULL) {
	    printf ("%s\t\tbad character '%c' in filename\n", filename, *cp);
	    return;
	}
#ifdef S_IFLNK
	if (lstat (filename, &st) != 0) {
	    printf ("%s\t\tunable to lstat file, errno %d, %s\n",
		    filename, errno, SysError());
	    return;
	}
	if ((st.st_mode & S_IFLNK) == S_IFLNK) {
	    printf ("%s\t\tsymbolic links not allowed\n", filename);
	    return;
	}
#endif
	mode = st.st_mode & (~S_IFMT);	/* just mode bits */
	if ((st.st_mode & S_IFDIR) == S_IFDIR) {
	    maxlen = 14;
	    if ((mode & dmode_bits_minset) != dmode_bits_minset) {
		printf ("%s\t\tdirectory mode 0%o not minimum 0%o\n",
			filename, mode, dmode_bits_minset);
		return;
	    }
	} else if ((st.st_mode & S_IFREG) != S_IFREG) {
	    printf ("%s\t\tnot a regular file\n", filename);
	    return;
	} else {
	    if ((mode & fmode_bits_minset) != fmode_bits_minset) {
		printf ("%s\t\tfile mode 0%o not minimum 0%o\n",
			filename, st.st_mode, fmode_bits_minset);
		return;
	    }
	    if (st.st_nlink != 1) {
		printf ("%s\t\thas %d links instead of 1\n", 
			filename, st.st_nlink);
		return;
	    }
	}
	if ((mode & ~fmode_bits_maxset) != 0) {
	    printf ("%s\t\tmode 0%o outside maximum set 0%o\n",
		    filename, mode, fmode_bits_maxset);
	    return;
	}
    }

    maxlen += rcsslop;
    if (len > maxlen) {
	printf ("%s\t\tfilename %stoo long, %d chars instead of %d\n", 
		filename, ((len > 14 && rcsslop == 0) ? "much " : ""),
		len, maxlen);
	return;
    }

    return;
}
