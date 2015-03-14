/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : sigfile.c
 *  Author    : M.Gleason & I.Lea
 *  Created   : 17-10-92
 *  Updated   : 25-11-92
 *  Notes     : Generate random signature for posting/mailing etc.
 *  Copyright : (c) Copyright 1989-92 by Mike Gleason & Iain Lea 
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

#define MAXLOOPS 1000

#ifndef S_ISDIR
#	ifndef AMIGA
#		define S_ISDIR(m)   (((m) & S_IFMT) == S_IFDIR)
#	else
#		define stmode st_attr
#		define S_ISDIR(m)   (((m) & S_DIRECT) == S_DIRECT)
#	endif
#endif

#ifndef AMIGA
#	define CURRENTDIR "."
#else
#	define CURRENTDIR ""
#endif

static char sigfile[PATH_LEN];


void add_signature (fp, flag)
	FILE *fp;
	int flag;
{
	char path[PATH_LEN];
	FILE *fixfp;
	FILE *sigfp;
	int i;
	
#ifdef NNTP_INEWS
	if (read_news_via_nntp) {
		flag = TRUE;
	}
#endif
	i = my_group[cur_groupnum];

	if (! strfpath (active[i].attribute.sigfile, path, sizeof (path), 
	    homedir, (char *) 0, (char *) 0, active[i].name)) {
		if (! strfpath (default_sigfile, path, sizeof (path), 
		    homedir, (char *) 0, (char *) 0, active[i].name)) {
			joinpath (path, homedir, ".Sig");
		}
	}
	
	/*
	 *  Check to see if sigfile is a directory & if it is  generate a 
	 *  random signature from sigs in sigdir. If the file ~/.sigfixed
	 *  exists (fixed part of random sig) then  read it  in first and 
	 *  append the random sig part onto the end.
	 */
	if ((sigfp = open_random_sig (path)) != (FILE *) 0) {
if (debug == 2) {
	error_message ("USING random sig=[%s]", sigfile);
}
		fprintf (fp, "\n--\n");
		joinpath (path, homedir, ".sigfixed");
		if ((fixfp = fopen (path, "r")) != (FILE *) 0) {
			copy_fp (fixfp, fp, "");
			fclose (fixfp);	
		}
		copy_fp (sigfp, fp, "");
		fclose (sigfp);
		return;
	}
	
	/*
	 *  Use ~/.signature or ~/.Sig or custom .Sig files
	 */
	if ((sigfp = fopen (default_signature, "r")) != (FILE *) 0) {
		if (flag) {
			fprintf (fp, "\n--\n");
			copy_fp (sigfp, fp, "");
		}
		fclose (sigfp);
		return;
	}

	if ((sigfp = fopen (path, "r")) != (FILE *) 0) {
		fprintf (fp, "\n--\n");
		copy_fp (sigfp, fp, "");
		fclose (sigfp);
	}
}


FILE *open_random_sig (sigdir)
	char *sigdir;
{
	long epoch;
	struct stat st;

	if (stat (sigdir, &st) != -1) {
		if (S_ISDIR(st.st_mode)) {
			time (&epoch);
			srand ((unsigned int) epoch);

			chdir (sigdir);
			if (thrashdir (sigdir) || ! sigfile[0]) { 
if (debug == 2) {
	error_message ("NO sigfile=[%s]", sigfile);
}
				return (FILE *) 0;
			} else {
if (debug == 2) {
	error_message ("sigfile=[%s]", sigfile);
}
				return fopen (sigfile, "r");
			}
		}
	}
	
	return (FILE *) 0;
}


int thrashdir (sigdir)
	char *sigdir;
{
	char *cwd;
	int safeguard, recurse;
	register DIR *dirp;
	register DIR_BUF *dp;
	register int c, numentries, pick;
	struct stat st;

	sigfile[0] = '\0';

	if ((dirp = opendir (CURRENTDIR)) == NULL) {
		return (1);
	}

	numentries = 0;
	while ((dp = readdir (dirp)) != NULL) {
		numentries++;
	}

	/* 
	 * consider "." and ".." non-entries 
	 */
	cwd = (char *) my_malloc (PATH_LEN + 1);
#ifndef AMIGA
	if (numentries < 3 || cwd == (char *) 0) {
#else
	if (numentries == 0 || cwd == (char *) 0) {
#endif
		closedir (dirp);
		return (-1);
	}

	get_cwd (cwd);
	recurse = strcmp (cwd, sigdir);

	/* If we are using the root sig directory, we don't want
	 * to recurse, or else we might use a custom sig intended
	 * for a specific newsgroup (and not this one). 
	 */
	for (safeguard=0, dp=NULL; safeguard<MAXLOOPS && dp==NULL; safeguard++) {
if (debug == 2) {
	sprintf (msg, "sig loop=[%d] recurse=[%d]", safeguard, recurse);
	error_message (msg, "");
}
#ifdef HAVE_REWINDDIR
		rewinddir (dirp);
#else
		closedir (dirp);
		if ((dirp = opendir (CURRENTDIR)) == NULL) {
			return (1);
		}
#endif
		pick = rand () % numentries + 1;
		while (--pick >= 0) {
			if ((dp = readdir (dirp)) == NULL) {
				break;
			}
		}
		if (dp != NULL) {	/* if we could open the dir entry */
			if (! strcmp (dp->d_name, CURRENTDIR) || 
			    ! strcmp(dp->d_name, "..")) {
				dp = NULL;
			} else {	/* if we have a non-dot entry */
				if (stat (dp->d_name, &st) == -1) {
if (debug == 2) {
	error_message ("error: before GAK stat(%s)", dp->d_name);
}
gak:
					closedir (dirp);
if (debug == 2) {
	error_message ("error: after GAK stat(%s)", dp->d_name);
}
					return (1);
				}
				if (S_ISDIR(st.st_mode)) {
if (debug == 2) {
	error_message ("Is a dir %s", dp->d_name);
}
					if (recurse) {
						/* 
						 * do subdirectories 
						 */
						if (chdir (dp->d_name) < 0) {
							goto gak;
						}
						if ((c = thrashdir (sigdir)) == 1) {
							goto gak;
						} else if (c == -1) {
							/* 
							 * the one we picked was an 
							 * empty dir so try again. 
							 */
							dp = NULL;
							chdir (cwd);
						}
					} else {
						dp = NULL;
					}
				} else {	/* end dir; we have a file */
					get_cwd (sigfile);
					strcat (sigfile, "/");
					strcat (sigfile, dp->d_name);
if (debug == 2) {
	error_message ("Found a file=[%s]", sigfile);
}
				}
			}
		}
	}
	free (cwd);

if (debug == 2) {
	error_message ("return 0: sigfile=[%s]", sigfile);
}
	closedir (dirp);
	
	return (0);
}
