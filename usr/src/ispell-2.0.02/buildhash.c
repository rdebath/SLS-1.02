/* -*- Mode: Text -*- */

#define MAIN

/*
 * buildhash.c - make a hash table for ispell
 *
 * Pace Willisson, 1983
 */

#include <ctype.h>
#include <stdio.h>
#ifdef USG
#include <sys/types.h>
#endif
#include <sys/param.h>
#include <sys/stat.h>
#include "config.h"
#include "ispell.h"

#define NSTAT 100
struct stat dstat, cstat;

int numwords, hashsize;

char *malloc();
char *realloc ();

struct dent *hashtbl;

char *Dfile;
char *Hfile;

char Cfile[MAXPATHLEN];
char Sfile[MAXPATHLEN];

main (argc,argv)
int argc;
char **argv;
{
	FILE *countf;
	FILE *statf;
	int stats[NSTAT];
	int i;

	if (argc > 1) {
		++argv;
		Dfile = *argv;
		if (argc > 2) {
			++argv;
			Hfile = *argv;
		}
		else
			Hfile = DEFHASH;
	}
	else {
		Dfile = DEFDICT;
		Hfile = DEFHASH;
	}

	sprintf(Cfile,"%s.cnt",Dfile);
	sprintf(Sfile,"%s.stat",Dfile);

	if (stat (Dfile, &dstat) < 0) {
		fprintf (stderr, "No dictionary (%s)\n", Dfile);
		exit (1);
	}

	if (stat (Cfile, &cstat) < 0 || dstat.st_mtime > cstat.st_mtime)
		newcount ();

	if ((countf = fopen (Cfile, "r")) == NULL) {
		fprintf (stderr, "No count file\n");
		exit (1);
	}
	numwords = 0;
	fscanf (countf, "%d", &numwords);
	fclose (countf);
	if (numwords == 0) {
		fprintf (stderr, "Bad count file\n");
		exit (1);
	}
	hashsize = numwords;
	readdict ();

	if ((statf = fopen (Sfile, "w")) == NULL) {
		fprintf (stderr, "Can't create %s\n", Sfile);
		exit (1);
	}

	for (i = 0; i < NSTAT; i++)
		stats[i] = 0;
	for (i = 0; i < hashsize; i++) {
		struct dent *dp;
		int j;
		if (hashtbl[i].used == 0) {
			stats[0]++;
		} else {
			for (j = 1, dp = &hashtbl[i]; dp->next != NULL; j++, dp = dp->next)
				;
			if (j >= NSTAT)
				j = NSTAT - 1;
			stats[j]++;
		}
	}
	for (i = 0; i < NSTAT; i++)
		fprintf (statf, "%d: %d\n", i, stats[i]);
	fclose (statf);

	filltable ();

	output ();
	exit(0);
}

output ()
{
	FILE *outfile;
	struct hashheader hashheader;
	int strptr, n, i;

	if ((outfile = fopen (Hfile, "w")) == NULL) {
		fprintf (stderr, "can't create %s\n",Hfile);
		return;
	}
	hashheader.magic = MAGIC;
	hashheader.stringsize = 0;
	hashheader.tblsize = hashsize;
	fwrite (&hashheader, sizeof hashheader, 1, outfile);
	strptr = 0;
	for (i = 0; i < hashsize; i++) {
		n = strlen (hashtbl[i].word) + 1;
#ifdef CAPITALIZE
		if (hashtbl[i].followcase)
			n += (hashtbl[i].word[n] & 0xFF) * (n + 1) + 1;
#endif
		fwrite (hashtbl[i].word, n, 1, outfile);
		hashtbl[i].word = (char *)strptr;
		strptr += n;
	}
	/* Pad file to a struct dent boundary for efficiency. */
	n = (strptr + sizeof hashheader) % sizeof (struct dent);
	if (n != 0) {
		n = sizeof (struct dent) - n;
		strptr += n;
		while (--n >= 0)
		    putc ('\0', outfile);
	}
	for (i = 0; i < hashsize; i++) {
		if (hashtbl[i].next != 0) {
			int x;
			x = hashtbl[i].next - hashtbl;
			hashtbl[i].next = (struct dent *)x;
		} else {
			hashtbl[i].next = (struct dent *)-1;
		}
	}
	fwrite (hashtbl, sizeof (struct dent), hashsize, outfile);
	hashheader.stringsize = strptr;
	rewind (outfile);
	fwrite (&hashheader, sizeof hashheader, 1, outfile);
	fclose (outfile);
}

filltable ()
{
	struct dent *freepointer, *nextword, *dp;
	int i;

	for (freepointer = hashtbl; freepointer->used; freepointer++)
		;
	for (nextword = hashtbl, i = numwords; i != 0; nextword++, i--) {
		if (nextword->used == 0) {
			continue;
		}
		if (nextword->next == NULL) {
			continue;
		}
		if (nextword->next >= hashtbl && nextword->next < hashtbl + hashsize) {
			continue;
		}
		dp = nextword;
		while (dp->next) {
			if (freepointer > hashtbl + hashsize) {
				fprintf (stderr, "table overflow\n");
				getchar ();
				break;
			}
			*freepointer = *(dp->next);
			dp->next = freepointer;
			dp = freepointer;

			while (freepointer->used)
				freepointer++;
		}
	}
}


readdict ()
{
	struct dent d;
	register struct dent *dp;
	char lbuf[100];
	FILE *dictf;
	int i;
	int h;
	int len;
	register char *p;

	if ((dictf = fopen (Dfile, "r")) == NULL) {
		fprintf (stderr, "Can't open dictionary\n");
		exit (1);
	}

	hashtbl = (struct dent *) calloc (numwords, sizeof (struct dent));
	if (hashtbl == NULL) {
		fprintf (stderr, "couldn't allocate hash table\n");
		exit (1);
	}

	i = 0;
	while (fgets (lbuf, sizeof lbuf, dictf) != NULL) {
		if ((i & 1023) == 0) {
			printf ("%d ", i);
			fflush (stdout);
		}
		i++;

		p = &lbuf [ strlen (lbuf) - 1 ];
		if (*p == '\n')
			*p = 0;

		if (makedent (lbuf, &d) < 0)
			continue;

		len = strlen (lbuf);
#ifdef CAPITALIZE
		if (d.followcase)
			d.word = malloc (2 * len + 4);
		else
			d.word = malloc (len + 1);
#else
		d.word = malloc (len + 1);
#endif
		if (d.word == NULL) {
			fprintf (stderr, "couldn't allocate space for word %s\n", lbuf);
			exit (1);
		}
		strcpy (d.word, lbuf);
#ifdef CAPITALIZE
		if (d.followcase) {
			p = d.word + len + 1;
			*p++ = 1;		/* Count of capitalizations */
			*p++ = '-';		/* Don't keep in pers dict */
			strcpy (p, lbuf);
			
		}
		for (p = d.word;  *p;  p++) {
			if (mylower (*p))
				*p = toupper (*p);
		}
#endif

		h = hash (d.word, len, hashsize);

		dp = &hashtbl[h];
		if (dp->used == 0) {
			*dp = d;
		} else {

#ifdef CAPITALIZE
			while (dp != NULL  &&  strcmp (dp->word, d.word) != 0)
			    dp = dp->next;
			if (dp != NULL) {
			    if (d.followcase
			      ||  (dp->followcase  &&  !d.allcaps
				&&  !d.capitalize)) {
				/* Add a specific capitalization */
				if (dp->followcase) {
				    p = &dp->word[len + 1];
				    (*p)++;	/* Bump counter */
				    dp->word = realloc (dp->word,
				      ((*p & 0xFF) + 1) * (len + 2));
				    if (dp->word == NULL) {
					fprintf (stderr,
					  "couldn't allocate space for word %s\n",
					  lbuf);
					exit (1);
				    }
				    p = &dp->word[len + 1];
				    p += ((*p & 0xFF) - 1) * (len + 2) + 1;
				    *p++ = '-';
				    strcpy (p,
				      d.followcase ? &d.word[len + 3] : lbuf);
				}
				else {
				    /* d.followcase must be true */
				    /* thus, d.capitalize and d.allcaps are */
				    /* clear */
				    free (dp->word);
				    dp->word = d.word;
				    dp->followcase = 1;
				    dp->k_followcase = 1;
				    /* Code later will clear dp->allcaps. */
				}
			    }
			    /* Combine two capitalizations.  If d was */
			    /* allcaps, dp remains unchanged */
			    if (d.allcaps == 0) {
				/* dp is the entry that will be kept.  If */
				/* dp is followcase, the capitalize flag */
				/* reflects whether capitalization "may" */
				/* occur.  If not, it reflects whether it */
				/* "must" occur. */
				if (d.capitalize) {	/* ie lbuf was cap'd */
				    if (dp->followcase)
					dp->capitalize = 1;	/* May */
				    else if (dp->allcaps) /* ie not lcase */
					dp->capitalize = 1;	/* Must */
				}
				else {		/* lbuf was followc or all-lc */
				    if (!dp->followcase)
					dp->capitalize == 0;	/* May */
				}
				dp->k_capitalize == dp->capitalize;
				dp->allcaps = 0;
				dp->k_allcaps = 0;
			    }
			}
			else {
#endif
			    dp = (struct dent *) malloc (sizeof (struct dent));
			    if (dp == NULL) {
				fprintf (stderr,
				  "couldn't allocate space for collision\n");
				exit (1);
			    }
			    *dp = d;
			    dp->next = hashtbl[h].next;
			    hashtbl[h].next = dp;
			}
		}
#ifdef CAPITALIZE
	}
#endif
	printf ("\n");
}

/*
 * fill in the flags in d, and put a null after the word in s
 */

makedent (lbuf, d)
char *lbuf;
struct dent *d;
{
	char *p, *index();

	d->next = NULL;
	d->used = 1;
	d->v_flag = 0;
	d->n_flag = 0;
	d->x_flag = 0;
	d->h_flag = 0;
	d->y_flag = 0;
	d->g_flag = 0;
	d->j_flag = 0;
	d->d_flag = 0;
	d->t_flag = 0;
	d->r_flag = 0;
	d->z_flag = 0;
	d->s_flag = 0;
	d->p_flag = 0;
	d->m_flag = 0;
	d->keep = 0;
#ifdef CAPITALIZE
	d->allcaps = 0;
	d->capitalize = 0;
	d->followcase = 0;
	/*
	** Figure out the capitalization rules from the capitalization of
	** the sample entry.  Only one of followcase, allcaps, and capitalize
	** will be set.  Combinations are generated by higher-level code.
	*/
	for (p = lbuf;  *p  &&  *p != '/';  p++) {
		if (mylower (*p))
			break;
	}
	if (*p == '\0'  ||  *p == '/')
		d->allcaps = 1;
	else {
		for (  ;  *p  &&  *p != '/';  p++) {
			if (myupper (*p))
				break;
		}
		if (*p == '\0'  ||  *p == '/') {
			/*
			** No uppercase letters follow the lowercase ones.
			** If the first two letters are capitalized, it's
			** "followcase". If the first one is capitalized, it's
			** "capitalize".
			*/
			if (myupper (lbuf[0])) {
				if (myupper (lbuf[1]))
					d->followcase = 1;
				else
					d->capitalize = 1;
			}
		}
		else
			d->followcase = 1;	/* .../lower/upper */
	}
	d->k_allcaps = d->allcaps ;
	d->k_capitalize = d->capitalize;
	d->k_followcase = d->followcase;
#endif

	p = index (lbuf, '/');
	if (p != NULL)
		*p = 0;
	if (strlen (lbuf) > WORDLEN - 1) {
		printf ("%s: word too big\n", lbuf);
		return (-1);
	}

	if (p == NULL)
		return (0);

	p++;
	while (*p != '\0'  &&  *p != '\n') {
		if (mylower (*p))
			*p = toupper (*p);
		switch (*p) {
		case 'V': d->v_flag = 1; break;
		case 'N': d->n_flag = 1; break;
		case 'X': d->x_flag = 1; break;
		case 'H': d->h_flag = 1; break;
		case 'Y': d->y_flag = 1; break;
		case 'G': d->g_flag = 1; break;
		case 'J': d->j_flag = 1; break;
		case 'D': d->d_flag = 1; break;
		case 'T': d->t_flag = 1; break;
		case 'R': d->r_flag = 1; break;
		case 'Z': d->z_flag = 1; break;
		case 'S': d->s_flag = 1; break;
		case 'P': d->p_flag = 1; break;
		case 'M': d->m_flag = 1; break;
		case 0:
 			fprintf (stderr, "no flags on word %s\n", lbuf);
			continue;
		default:
			fprintf (stderr, "unknown flag %c word %s\n", 
					*p, lbuf);
			break;
		}
		p++;
		if (*p == '/')		/* Handle old-format dictionaries too */
			p++;
	}
	return (0);
}

newcount ()
{
	char buf[200];
	char lastbuf[200];
	FILE *d;
	int i;
	register char *cp;

	fprintf (stderr, "Counting words in dictionary ...\n");

	if ((d = fopen (Dfile, "r")) == NULL) {
		fprintf (stderr, "Can't open dictionary\n");
		exit (1);
	}

	for (i = 0, lastbuf[0] = '\0';  fgets (buf, sizeof buf, d);  ) {
		for (cp = buf;  *cp;  cp++) {
			if (mylower (*cp))
				*cp = toupper (*cp);
		}
		if (strcmp (buf, lastbuf) != 0) {
			if ((++i & 1023) == 0) {
				printf ("%d ", i);
				fflush (stdout);
			}
			strcpy (lastbuf, buf);
		}
	}
	fclose (d);
	printf ("\n%d words\n", i);
	if ((d = fopen (Cfile, "w")) == NULL) {
		fprintf (stderr, "can't create %s\n", Cfile);
		exit (1);
	}
	fprintf (d, "%d\n", i);
	fclose (d);
}
