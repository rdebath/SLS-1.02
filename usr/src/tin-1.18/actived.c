/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : actived.c
 *  Author    : M.Tomlinson & I.Lea
 *  Created   : 23-08-92
 *  Updated   : 17-09-92
 *  Notes     : Creates an active file  by looking through all the 
 *              .next files in  the news directories, and  writing 
 *              this to UULIB:newactive. The UULIB:newsgroups file 
 *              must exist. 
 *  Copyright : (c) Copyright 1991-92 by Mark Tomlinson & Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include <stdio.h>

#define	NEWSGROUPS_FILE	"UULIB:newsgroups"
#define	NEWSACTIVE_FILE	"UULIB:newsactive"

main ()
{
	char groupname[81];
	char next_path[90];
	char *p, last[21];
	FILE *fp, *ng, *active;
	long x;

	if ((ng = fopen (NEWSGROUPS_FILE, "r")) == (FILE *) 0) {
		perror (NEWSGROUPS_FILE);
		exit (1);
	}

	if ((active = fopen (NEWSACTIVE_FILE, "w")) == (FILE *) 0) {
		perror (NEWSACTIVE_FILE);
		exit (1);
	}

	while (fgets (groupname, 80, ng))
	{
		for (p = groupname; *p && *p != ' ' && *p != '\t' && *p != '\n'; p++) {
			;
		}
		*p = 0;
		
		sprintf (next_path,"UUNEWS:%s",groupname);
		for (p = &next_path[7]; *p ; p++) {
			if (*p == '.') {
				*p = '/'; /* convert to tree structure */
			}
		}
		strcat (next_path,"/.next");
		if (fp = fopen (next_path,"r")) {
			fgets (last,20,fp);
			x = atol (last) - 1;
			fclose (fp);
		} else {
			x = 0;
		}
		fprintf (active, "%s %05d 00001 y\n", groupname, x);
	}
}
