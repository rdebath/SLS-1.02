/* -*- Mode:Text -*- */

/*
 * tree.c - a hash style dictionary for user's personal words
 *
 * Pace Willisson, 1983
 * Hash support added by Geoff Kuenning, 1987
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include "config.h"
#include "ispell.h"

char *getenv();
struct dent *lookup();
char *upcase();

static int cantexpand = 0;	/* NZ if an expansion fails */
static struct dent *htab = NULL; /* Hash table for our stuff */
static int hsize = 0;		/* Space available in hash table */
static int hcount = 0;		/* Number of items in hash table */

/*
 * Hash table sizes.  Prime is probably a good idea, though in truth I
 * whipped the algorithm up on the spot rather than looking it up, so
 * who knows what's really best?  If we overflow the table, we just
 * use a double-and-add-1 algorithm.
 *
 * The strange pattern in the table is because this table is sometimes
 * used with huge dictionaries, and we want to get the table bigger fast.
 * 23003 just happens to be such that the original dict.191 will fill
 * the table to just under 70%.  31469 is similarly selected for dict.191
 * combined with /usr/dict/words.  The other numbers are on 10000-word
 * intervals starting at 30000.  (The table is still valid if MAXPCT
 * is changed, but the dictionary sizes will no longer fall on neat
 * boundaries).
 */
static int goodsizes[] = {
	53, 223, 907,
#if ((BIG_DICT * 100) / MAXPCT) <= 23003
	23003,				/* ~16000 words */
#endif
#if ((BIG_DICT * 100) / MAXPCT) <= 31469
	31469,				/* ~22000 words */
#endif
#if ((BIG_DICT * 100) / MAXPCT) <= 42859
	42859,				/* ~30000 words */
#endif
#if ((BIG_DICT * 100) / MAXPCT) <= 57143
	57143,				/* ~40000 words */
#endif
	71429				/* ~50000 words */
};

struct dent *treeinsert();
struct dent *tinsert();
struct dent *treelookup();
char *upcase ();
char *lowcase ();

static char personaldict[MAXPATHLEN];
static FILE *dictf;
static newwords = 0;

extern char *index ();
extern char *calloc ();
extern char *malloc ();
extern char *realloc ();

extern struct dent *hashtbl;
extern int hashsize;

treeinit (p)
char *p;
{
	register char *h;
	char *orig;
	char buf[BUFSIZ];
	register struct dent *dp;

	/*
	** if p exists and begins with '/' we don't really need HOME,
	** but it's not very likely that HOME isn't set anyway.
	*/
	orig = p;
	if (p == NULL)
		p = getenv (PDICTVAR);
	if ((h = getenv ("HOME")) == NULL)
		return;

	if (p == NULL)
		sprintf(personaldict,"%s/%s",h,DEFPDICT);
	else {
		if (*p == '/')
			strcpy(personaldict,p);
		else {
			/*
			** The user gave us a relative pathname.  How we
			** interpret it depends on how it was given:
			**
			** -p switch:  as-is first, then $HOME/name
			** PDICTVAR:   $HOME/name first, then as-is
			**/
			if (orig == NULL)
				sprintf (personaldict, "%s/%s", h, p);
			else			/* -p switch */
				strcpy (personaldict, p);
		}
	}

	if ((dictf = fopen (personaldict, "r")) == NULL) {
		/* The file doesn't exist. */
		if (p != NULL) {
			/* If pathname is relative, try another place */
			if (*p != '/') {
				if (orig == NULL)
					strcpy (personaldict, p);
				else			/* -p switch */
					sprintf (personaldict, "%s/%s", h, p);
				dictf = fopen (personaldict, "r");
			}
			if (dictf == NULL) {
				(void) fprintf (stderr, "Couldn't open ");
				perror (p);
				if (*p != '/') {
					/*
					** Restore the preferred default, so
					** that output will go th the right
					** place.
					*/
					if (orig == NULL)
						sprintf (personaldict,
						  "%s/%s", h, p);
					else			/* -p switch */
						strcpy (personaldict, p);
				}
			}
		}
		/* If the name wasn't specified explicitly, we don't object */
		return;
	}

	while (fgets (buf, sizeof buf, dictf) != NULL) {
		int len = strlen (buf) - 1;

		if (buf [ len ] == '\n')
			buf [ len-- ] = '\0';
		if ((h = index (buf, '/')) != NULL)
			*h++ = '\0';
		dp = treeinsert (buf, 1);
		if (h != NULL) {
			while (*h != '\0'  &&  *h != '\n') {
				switch (*h++) {
				case 'D':
				case 'd':
					dp->d_flag = 1;
					break;
				case 'G':
				case 'g':
					dp->g_flag = 1;
					break;
				case 'H':
				case 'h':
					dp->h_flag = 1;
					break;
				case 'J':
				case 'j':
					dp->j_flag = 1;
					break;
				case 'M':
				case 'm':
					dp->m_flag = 1;
					break;
				case 'N':
				case 'n':
					dp->n_flag = 1;
					break;
				case 'P':
				case 'p':
					dp->p_flag = 1;
					break;
				case 'R':
				case 'r':
					dp->r_flag = 1;
					break;
				case 'S':
				case 's':
					dp->s_flag = 1;
					break;
				case 'T':
				case 't':
					dp->t_flag = 1;
					break;
				case 'V':
				case 'v':
					dp->v_flag = 1;
					break;
				case 'X':
				case 'x':
					dp->x_flag = 1;
					break;
				case 'Y':
				case 'y':
					dp->y_flag = 1;
					break;
				case 'Z':
				case 'z':
					dp->z_flag = 1;
					break;
				default:
					fprintf (stderr,
					  "Illegal flag in personal dictionary - %c (word %s)\n",
					  h[-1], buf);
					break;
				}
				/* Accept old-format dicts with extra slashes */
				if (*h == '/')
					h++;
			}
		}
	}

	fclose (dictf);

	newwords = 0;

	if (!lflag && !aflag && access (personaldict, 2) < 0)
		fprintf (stderr,
		    "Warning: Cannot update personal dictionary (%s)\r\n",
		    personaldict);
}

struct dent *
treeinsert (word, keep)
char *word;
{
	register int i;
	register struct dent *dp;
	struct dent *olddp;
	struct dent *oldhtab;
	int oldhsize;
	char nword[BUFSIZ];
	int len;
#ifdef CAPITALIZE
	register char *cp;
	char *saveword;
	int capspace;
#endif

	strcpy (nword, word);
	upcase (nword);
	len = strlen (nword);
	if ((dp = lookup (nword, len, 0)) != NULL)
		dp->keep = keep;
	/*
	 * Expand hash table when it is MAXPCT % full.
	 */
	else if (!cantexpand  &&  (hcount * 100) / MAXPCT >= hsize) {
		oldhsize = hsize;
		oldhtab = htab;
		for (i = 0;  i < sizeof goodsizes / sizeof (goodsizes[0]);  i++)
			if (goodsizes[i] > hsize)
				break;
		if (i >= sizeof goodsizes / sizeof goodsizes[0])
			hsize += hsize + 1;
		else
			hsize = goodsizes[i];
		htab = (struct dent *) calloc (hsize, sizeof (struct dent));
		if (htab == NULL) {
			(void) fprintf (stderr,
			    "Ran out of space for personal dictionary\n");
			/*
			 * Try to continue anyway, since our overflow
			 * algorithm can handle an overfull (100%+) table,
			 * and the malloc very likely failed because we
			 * already have such a huge table, so small mallocs
			 * for overflow entries will still work.
			 */
			if (oldhtab == NULL)
				exit (1);	/* No old table, can't go on */
			(void) fprintf (stderr,
			    "Continuing anyway (with reduced performance).\n");
			cantexpand = 1;		/* Suppress further messages */
			hsize = oldhsize;	/* Put this back how the were */
			htab = oldhtab;		/* ... */
			newwords = 1;		/* And pretend it worked */
			return tinsert (nword, (struct dent *) NULL, keep);
		}
		else {
			/*
			 * Re-insert old entries into new table
			 */
			for (i = 0;  i < oldhsize;  i++) {
				dp = &oldhtab[i];
				if (oldhtab[i].used) {
					tinsert ((char *) NULL, dp, 0);
					dp = dp->next;
					while (dp != NULL) {
						tinsert ((char *) NULL, dp, 0);
						olddp = dp;
						dp = dp->next;
						free ((char *) olddp);
					}
				}
			}
			if (oldhtab != NULL)
				free ((char *) oldhtab);
			dp = NULL;	/* This will force the insert below */
		}
	}
	newwords |= keep;
	if (dp == NULL)
		dp = tinsert (nword, (struct dent *) NULL, keep);
#ifdef CAPITALIZE
	if (dp == NULL)
		return NULL;
	/*
	** Figure out the capitalization rules from the
	** capitalization of the sample entry.  If the sample is
	** all caps, we don't change the existing flags, since
	** all-caps gives us no information.  Tinsert initializes
	** new entries with "allcaps" set, so if the word is truly
	** required to appear in capitals, the correct result
	** will be achieved.
	*/
	for (cp = word;  *cp;  cp++) {
		if (mylower (*cp))
			break;
	}
	if (*cp) {
		/*
		** Sample entry has at least some lowercase.  See if
		** the case is mixed.
		*/
		for (cp = word;  *cp;  cp++) {
			if (myupper (*cp))
				break;
		}
		if (*cp == '\0'  &&  !dp->followcase) {
			/*
			** Sample entry is all lowercase, and word is not
			** followcase.  Clear all of the capitalization flags.
			*/
			dp->allcaps = 0;
			dp->capitalize = 0;
			if (keep) {
				dp->k_allcaps = 0;
				dp->k_capitalize = 0;
				dp->k_followcase = 0;
			}
		}
		else {
			/*
			** The sample entry is mixed case (or all-lower and the
			** entry is already followcase).  If it's simply
			** capitalized, set the capitalize flag and that's that.
			*/
			for (cp = word + 1;  *cp  &&  !myupper (*cp);  )
				cp++;
			if (*cp == 0  &&  myupper (*word)) {
				dp->allcaps = 0;
				dp->capitalize = 1;
				if (keep) {
					dp->k_allcaps = 0;
					dp->k_capitalize = 1;
				}
			}
			else {
				/*
				** The sample entry is followcase.  Make the
				** dictionary entry followcase if necessary.
				*/
				if (!dp->followcase) {
					dp->followcase = 1;
					if (keep)
						dp->k_followcase = 1;
					capspace = 2 * len + 4;
					if (dp->word >= hashstrings
					  &&  dp->word <=
					    hashstrings
					     + hashheader.stringsize) {
						cp = dp->word;
						dp->word = malloc (capspace);
						if (dp->word)
							strcpy (dp->word, cp);
					}
					else
						dp->word = realloc (dp->word,
								    capspace);
					if (dp->word == NULL) {
						fprintf (stderr,
						  "Ran out of space for personal dictionary\n");
						exit (1);
					}
					cp = dp->word + len + 1;
					if (dp->capitalize  ||  dp->allcaps)
						*cp++ = 0;
					else {
						*cp++ = 1;
						strcpy (cp + 1, dp->word);
						lowcase (cp + 1);
					}
					*cp = dp->keep ? '+' : '-';
				}
				dp->allcaps = 0;
				if (keep)
					dp->k_allcaps = 0;
				cp = dp->word + len + 1;
				/* See if the capitalization is already there */
				for (i = 0, saveword = cp + 1;
				    i < (*cp & 0xFF);
				    i++) {
					if (strcmp (saveword + 1, word) == 0)
						break;
					saveword += len + 2;
				}
				if (i != (*cp & 0xFF)) {
					if (keep)
						*saveword = '+';
				}
				else {
					/* Add a new capitalization */
					(*cp)++;
					capspace = (cp - dp->word + 1)
						    * ((*cp & 0xFF) + 1);
					if (dp->word >= hashstrings
					  &&  dp->word <=
					    hashstrings + hashheader.stringsize) {
						saveword = dp->word;
						dp->word = malloc (capspace);
						if (dp->word) {
							cp = dp->word;
							while (--capspace >= 0)
								*cp++ =
								  *saveword++;
						}
					}
					else
						dp->word =
						  realloc (dp->word, capspace);
					if (dp->word == NULL) {
						fprintf (stderr,
						  "Ran out of space for personal dictionary\n");
						exit (1);
					}
					cp = dp->word + len + 1;
					cp += ((*cp & 0xFF) - 1)
						  * (cp - dp->word + 1) + 1;
					*cp++ = keep ? '+' : '-';
					strcpy (cp, word);
				}
			}
		}
	}
#endif
	return dp;
}

static
struct dent *
tinsert (word, proto, keep)
char *word;			/* One of word/proto must be null */
struct dent *proto;
{
	register int hcode;
	register struct dent *hp; /* Next trial entry in hash table */
	register struct dent *php;	/* Previous value of hp, for chaining */
	register char *cp;

	if (word == NULL)
		word = proto->word;
	hcode = hash (word, strlen (word), hsize);
	php = NULL;
	hp = &htab[hcode];
	if (hp->used) {
		while (hp != NULL) {
			if (strcmp (word, hp->word) == 0) {
				if (keep)
					hp->keep = 1;
				return hp;
			}
			php = hp;
			hp = hp->next;
		}
		hp = (struct dent *) calloc (1, sizeof (struct dent));
		if (hp == NULL) {
			(void) fprintf (stderr,
			    "Ran out of space for personal dictionary\n");
			exit (1);
		}
	}
	if (proto != NULL) {
		*hp = *proto;
		if (php != NULL)
			php->next = hp;
		hp->next = NULL;
		return &htab[hcode];
	} else {
		if (php != NULL)
			php->next = hp;
		hp->word = (char *) malloc (strlen (word) + 1);
		if (hp->word == NULL) {
			(void) fprintf (stderr,
			    "Ran out of space for personal dictionary\n");
			exit (1);
		}
		strcpy (hp->word, word);
		hp->used = 1;
		hp->next = NULL;
		hp->d_flag = 0;
		hp->g_flag = 0;
		hp->h_flag = 0;
		hp->j_flag = 0;
		hp->m_flag = 0;
		hp->n_flag = 0;
		hp->p_flag = 0;
		hp->r_flag = 0;
		hp->s_flag = 0;
		hp->t_flag = 0;
		hp->v_flag = 0;
		hp->x_flag = 0;
		hp->y_flag = 0;
		hp->z_flag = 0;
#ifdef CAPITALIZE
		hp->allcaps = 1;		/* Assume word is all-caps */
		hp->k_allcaps = 1;
		hp->capitalize = 0;
		hp->k_capitalize = 0;
		hp->followcase = 0;
		hp->k_followcase = 0;
#endif
		hp->keep = keep;
		hcount++;
		return (hp);
	}
}

struct dent *
treelookup (word)
char *word;
{
	register int hcode;
	register struct dent *hp;
	char nword[BUFSIZ];

	if (hsize <= 0)
		return NULL;
	strcpy (nword, word);
	hcode = hash (nword, strlen (nword), hsize);
	hp = &htab[hcode];
	while (hp != NULL  &&  hp->used) {
		if (strcmp (nword, hp->word) == 0)
			break;
		hp = hp->next;
	}
	if (hp != NULL  &&  hp->used)
		return hp;
	else
		return NULL;
}

#if SORTPERSONAL != 0
/* Comparison routine for sorting the personal dictionary with qsort */
pdictcmp (enta, entb)
struct dent **enta;
struct dent **entb;
{
	/* The parentheses around *enta/*entb below are NECESSARY!
	** Otherwise the compiler reads it as *(enta->word), or
	** enta->word[0], which is illegal (but pcc takes it and
	** produces wrong code).
	**/
	return casecmp ((*enta)->word, (*entb)->word);
}
#endif

treeoutput ()
{
	register struct dent *cent;	/* Current entry */
	register struct dent *lent;	/* Linked entry */
#if SORTPERSONAL != 0
	int pdictsize;			/* Number of entries to write */
	struct dent **sortlist;		/* List of entries to be sorted */
	register struct dent **sortptr;	/* Handy pointer into sortlist */
#endif
	register struct dent *ehtab;	/* End of htab, for quick looping */

	if (newwords == 0)
		return;

	if ((dictf = fopen (personaldict, "w")) == NULL) {
		fprintf (stderr, "Can't create %s\r\n", personaldict);
		return;
	}

#if SORTPERSONAL != 0
	/*
	** If we are going to sort the personal dictionary, we must know
	** how many items are going to be sorted.
	*/
	if (hcount >= SORTPERSONAL)
		sortlist = NULL;
	else {
		pdictsize = 0;
		for (cent = htab, ehtab = htab + hsize;
		    cent < ehtab;
		    cent++) {
			for (lent = cent;  lent != NULL;  lent = lent->next) {
				if (lent->used  &&  lent->keep)
					pdictsize++;
			}
		}
		for (cent = hashtbl, ehtab = hashtbl + hashsize;
		    cent < ehtab;
		    cent++) {
			if (cent->keep  &&  cent->used)
				pdictsize++;
		}
		sortlist = (struct dent **)
		    malloc (pdictsize * sizeof (struct dent));
	}
	if (sortlist == NULL) {
#endif
		for (cent = htab, ehtab = htab + hsize;
		    cent < ehtab;
		    cent++) {
			for (lent = cent;  lent != NULL;  lent = lent->next) {
				if (lent->used  &&  lent->keep)
					toutent (lent);
			}
		}
		for (cent = hashtbl, ehtab = hashtbl + hashsize;
		    cent < ehtab;
		    cent++) {
			if (cent->used  &&  cent->keep)
				toutent (cent);
		}
#if SORTPERSONAL != 0
		return;
	}
	/*
	** Produce dictionary in sorted order.  We used to do this
	** destructively, but that turns out to fail because in some modes
	** the dictionary is written more than once.  So we build an
	** auxiliary pointer table (in sortlist) and sort that.  This
	** is faster anyway, though it uses more memory. 
	*/
	sortptr = sortlist;
	for (cent = htab, ehtab = htab + hsize;  cent < ehtab;  cent++) {
		for (lent = cent;  lent != NULL;  lent = lent->next) {
			if (lent->used  &&  lent->keep)
				*sortptr++ = lent;
		}
	}
	for (cent = hashtbl, ehtab = hashtbl + hashsize;
	    cent < ehtab;
	    cent++) {
		if (cent->used  &&  cent->keep)
			*sortptr++ = cent;
	}
	/* Sort the list */
	qsort ((char *) sortlist, pdictsize, sizeof (sortlist[0]), pdictcmp);
	/* Write it out */
	for (sortptr = sortlist;  --pdictsize >= 0;  )
		toutent (*sortptr++);
	free ((char *) sortlist);
#endif

	newwords = 0;

	fclose (dictf);
}

static int hasslash;

static
toutent (cent)
register struct dent *cent;
{
#ifdef CAPITALIZE
	register char *cp;
	int len;
	register int wcount;
	char wbuf[WORDLEN + 1];

	strcpy (wbuf, cent->word);
	if (cent->k_followcase) {
		if (cent->k_capitalize) {
			lowcase (wbuf);
			if (mylower (wbuf[0]))
				wbuf[0] = toupper (wbuf[0]);
			toutword (wbuf, cent);
		}
		len = strlen (wbuf) + 1;
		cp = cent->word + len;
		wcount = *cp++ & 0xFF;
		while (--wcount >= 0) {
			if (*cp++ == '+')
				toutword (cp, cent);
			cp += len;
		}
	}
	else {
		if (!cent->k_allcaps)
			lowcase (wbuf);
		if (cent->k_capitalize  &&  mylower (wbuf[0]))
			wbuf[0] = toupper (wbuf[0]);
		toutword (wbuf, cent);
	}
#else
	toutword (cent->word, cent);
#endif
}
		
static
toutword (word, cent)
char *word;
register struct dent *cent;
{
	hasslash = 0;
	fprintf (dictf, "%s", word);
	if (cent->d_flag)
		flagout ('D');
	if (cent->g_flag)
		flagout ('G');
	if (cent->h_flag)
		flagout ('H');
	if (cent->j_flag)
		flagout ('J');
	if (cent->m_flag)
		flagout ('M');
	if (cent->n_flag)
		flagout ('N');
	if (cent->p_flag)
		flagout ('P');
	if (cent->r_flag)
		flagout ('R');
	if (cent->s_flag)
		flagout ('S');
	if (cent->t_flag)
		flagout ('T');
	if (cent->v_flag)
		flagout ('V');
	if (cent->x_flag)
		flagout ('X');
	if (cent->y_flag)
		flagout ('Y');
	if (cent->z_flag)
		flagout ('Z');
	fprintf (dictf, "\n");
}

static
flagout (flag)
{
	if (!hasslash)
		putc ('/', dictf);
	hasslash = 1;
	putc (flag, dictf);
}

char *
upcase (s)
register char *s;
{
	register char *os = s;

	while (*s) {
		if (mylower (*s))
			*s = toupper (*s);
		s++;
	}
	return (os);
}

char *
lowcase (s)
register char *s;
{
	register char *os = s;

	while (*s) {
		if (myupper (*s))
			*s = tolower (*s);
		s++;
	}
	return (os);
}
