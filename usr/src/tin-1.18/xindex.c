/*
 *  Project   : NNTP (RFC 977) extension
 *  Module    : xindex.c
 *  Author    : I.Lea
 *  Created   : 07-03-92
 *  Updated   : 18-11-92
 *  Notes     : Add a command to retieve tin style index files 
 *              from the NNTP server so as to save space on the
 *              client.
 *              Ideas borrowed from XTHREAD nntp extension code
 *              posted by Tim Iverson to alt.sources in mid'91.
 *  Copyright : (c) Copyright 1992 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "common.h"

#ifdef XINDEX

#undef	DEBUG_XINDEX		/* set to define to turn on more debug info */
#define HASH_VALUE 1409		/* mod value for hashing group name */

#ifndef MAXPATHLEN
#	define MAXPATHLEN 256
#endif

#ifdef __STDC__
void xindex (int argc, char *argv[]);
static void find_index_file (char *group, char *index_file);
static long hash_groupname (char *group);
#else
void xindex ();
static void find_index_file ();
static long hash_groupname ();
#endif

/*
 *  Usage: XINDEX groupname
 *
 *  Retrieve an index file for the specified newsgroup (ie. alt.sources)
 *
 *  This command is NOT documented in RFC977.
 */

void xindex (argc, argv)
	int	argc;
	char	*argv[];
{
	char	line[NNTP_STRLEN];
	char	group[MAXPATHLEN];
	char	index_file[MAXPATHLEN];
	char	*cp;
	FILE	*fp;
	
	/*
	 * "parse" the argument list
	 */
	if (argc == 1) {
		printf("%d Usage: XINDEX group\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	} else {
		strncpy (group, argv[1], sizeof (group)-1);
#if defined(SYSLOG) && defined(DEBUG_XINDEX)
		syslog(LOG_INFO, "%s xindex %s", hostname, group);
#endif

		find_index_file(group, index_file);
		
		if ((fp = fopen(index_file, "r")) == NULL) {
#ifdef SYSLOG
			syslog(LOG_INFO, "%s xindex cannot open %s (%s)",
				hostname, group, index_file);
#endif
			printf("%d XINDEX Cannot open %s\r\n",
				ERR_XINDEX, group);
			(void) fflush(stdout);
			return;
		}

		printf("%d XINDEX %s index file follows\r\n", 
			OK_XINDEX, group);
		(void) fflush(stdout);
		
		while (fgets(line, sizeof(line), fp) != NULL) {
			if ((cp = index(line, '\n')) != NULL)
				*cp = '\0';
			putline(line);
		}
		(void) fclose(fp);
	
		putline(".");
		(void) fflush(stdout);
	}
}

/*
 *  Look in XINDEX_DIR dir (defined  in conf.h) for the index file 
 *  for the given group. Hashing the group name gets a number. 
 *  See if that #.1 file exists; if so, read first line. Is it the
 *  Group we want? If not try #.2. Repeat until no such file or we 
 *  find the right file.
 */

static void find_index_file (group, index_file)
	char *group;
	char *index_file;
{
	char buf[MAXPATHLEN], *p;
	FILE *fp;
	int i = 1;
	unsigned long hash;
	struct stat sb;
	
	hash = hash_groupname (group);

	while (1) {
		sprintf (index_file, "%s/%lu.%d", XINDEX_DIR, hash, i);
		
		if ((fp = fopen (index_file, "r")) == NULL) {
			return;
		}

		if (fgets (buf, sizeof (buf), fp) == NULL) {
			fclose (fp);
			return;
		}
		fclose (fp);

		for (p = buf; *p && *p != '\n'; p++) {
			continue;
		}	
		*p = '\0';

		if (strcmp (buf, group) == 0) {
			return;
		}	
		i++;
	}	
}

/*
 * hash group name for filename of group
 */

static long hash_groupname (group)
	char *group;
{
	unsigned long hash_value;
	unsigned char *ptr = (unsigned char *) group;

	hash_value = *ptr++;

	while (*ptr)
		hash_value = ((hash_value << 1) ^ *ptr++) % HASH_VALUE;

	return (hash_value);
}

#endif /* XINDEX */

