/* -*- Mode:Text -*- */

/*
 * lookup.c - see if a word appears in the dictionary
 *
 * Pace Willisson, 1983
 */

#include <stdio.h>
#include "config.h"
#include "ispell.h"


struct dent *treelookup();
struct dent *hashtbl;
int hashsize;

extern char hashname[];

static inited = 0;

linit ()
{
	int hashfd;
	register int i;
	register struct dent *dp;

	if (inited)
		return;

	if ((hashfd = open (hashname, 0)) < 0) {
		fprintf (stderr, "can't open %s\r\n", hashname);
		return (-1);
	}

	hashsize = read (hashfd, &hashheader, sizeof hashheader);
	if (hashsize == 0) {
		/*
		 * Empty file - create an empty dummy table.  We
		 * actually have to have one entry since the hash
		 * algorithm involves a divide by the table size
		 * (actually modulo, but zero is still unacceptable).
		 * So we create an entry with a word of all lowercase,
		 * which can't match because the comparison string has
		 * been converted to uppercase by then.
		 */
		close (hashfd);
		hashsize = 1;		/* This prevents divides by zero */
		hashtbl = (struct dent *) calloc (1, sizeof (struct dent));
		if (hashtbl == NULL) {
			(void) fprintf (stderr,
			    "Couldn't allocate space for hash table\n");
			return (-1);
		}
		hashtbl[0].word = "xxxxxxxxxxx";
		hashtbl[0].next = NULL;
		hashtbl[0].keep = 0;
		hashtbl[0].used = 1;
		/* The flag bits don't matter, but calloc cleared them. */
		inited = 1;
		return 0;
	}
	else if (hashsize < 0  ||  hashheader.magic != MAGIC) {
		fprintf (stderr, "Illegal format hash table\r\n");
		return (-1);
	}
	hashstrings = (char *) malloc (hashheader.stringsize);
	hashtbl = (struct dent *) malloc (hashheader.tblsize * sizeof (struct dent));
	if (hashtbl == NULL  ||  hashstrings == NULL) {
		(void) fprintf (stderr,
		    "Couldn't allocate space for hash table\n");
		return (-1);
	}
	hashsize = hashheader.tblsize;

	read (hashfd, hashstrings, hashheader.stringsize);
	read (hashfd, hashtbl, hashheader.tblsize * sizeof (struct dent));
	close (hashfd);

	for (i = hashsize, dp = hashtbl;  --i >= 0;  dp++) {
		dp->word = &hashstrings [ (int)(dp->word) ];
		if (dp->next == (struct dent *) -1)
			dp->next = NULL;
		else
			dp->next = &hashtbl [ (int)(dp->next) ];
	}

	inited = 1;
	return (0);
}

/* n is length of s */
struct dent *
lookup (s, n, dotree)
register char *s;
{
	int i;
	register struct dent *dp;
	register char *s1, *s2;

	dp = &hashtbl [ hash (s, n, hashsize) ];
	for (  ; dp != NULL; dp = dp->next) {
		/* quick strcmp, but only for equality */
		s1 = dp->word;
		s2 = s;
		while (*s1 == *s2++)
			if (*s1++=='\0') {
				lastdent = dp;
				return (lastdent);
			}
	}
	if (dotree) {
		i = s[n];
		s[n] = '\0';
		if ((dp = treelookup (s)) != NULL)
			lastdent = dp;
		s[n] = i;
		return dp;
	}
	else
		return NULL;
}

