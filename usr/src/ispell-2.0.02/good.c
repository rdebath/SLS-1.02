/* -*- Mode:Text -*- */

/*
 * good.c - see if a word or its root word
 * is in the dictionary.
 *
 * Pace Willisson, 1983
 */

#include <stdio.h>
#include <ctype.h>
#include "config.h"
#include "ispell.h"

extern struct dent *lookup();
extern char *index();

static int wordok;
static char *orig_word;

extern int cflag;

good (w)
char *w;
{
	char nword[100];
	register char *p, *q;
	register n;

	/*
	** Make an uppercase copy of the word we are checking.
	*/
	for (p = w, q = nword; *p; p++, q++) {
		if (mylower (*p))
			*q = toupper (*p);
		else
			*q = *p;
	}
	*q = 0;

	rootword[0] = 0;

	if (cflag) {
		printf ("%s\n", w);
		orig_word = w;
	}
	else if (lookup (nword, q - nword, 1) != NULL) {
#ifdef CAPITALIZE
		return cap_ok (w, lastdent);
#else
		return (1);
#endif
	}

	/* try stripping off suffixes */

	n = strlen (w);
	if (n == 1)
		return (1);

	if (n < 4)
		return 0;

	wordok = 0;

	/* this part from 'check.mid' */
	switch (q[-1]) {
	case 'D': d_ending (nword,n); break;	/* FOR "CREATED", "IMPLIED", "CROSSED" */
	case 'T': t_ending (nword,n); break;	/* FOR "LATEST", "DIRTIEST", "BOLDEST" */
	case 'R': r_ending (nword,n); break;	/* FOR "LATER", "DIRTIER", "BOLDER" */
	case 'G': g_ending (nword,n); break;	/* FOR "CREATING", "FIXING" */
	case 'H': h_ending (nword,n); break;	/* FOR "HUNDREDTH", "TWENTIETH" */
	case 'S': s_ending (nword,n); break;	/* FOR ALL SORTS OF THINGS ENDING IN "S" */
	case 'N': n_ending (nword,n); break;	/* "TIGHTEN", "CREATION", "MULIPLICATION" */
	case 'E': e_ending (nword,n); break;	/* FOR "CREATIVE", "PREVENTIVE" */
	case 'Y': y_ending (nword,n); break;	/* FOR "QUICKLY" */
	default:
		break;
	}
	
	if (wordok) {
		strcpy (rootword, lastdent->word);
#ifdef CAPITALIZE
		return cap_ok (w, lastdent);
#else
		return 1;
#endif
	}
	return 0;
}

#ifdef CAPITALIZE
cap_ok (word, dent)
register char *word;
register struct dent *dent;
{
	register char *dword;
	register char *w;
	int wcount;

	/*
	** All caps is always legal.
	*/
	for (dword = word;  *dword;  dword++) {
		if (mylower (*dword))
			break;
	}
	if (*dword == '\0')
		return 1;		/* It was all caps */
	if (dent->allcaps)
		return 0;		/* Not all caps and required to be */
	if (dent->followcase) {
		/*
		** It's a followcase word.  The correct capitalizations are
		** found following the main dent word.  When we find a
		** mismatch between letters, we assume we are in the suffix,
		** and begin insisting on the same case as the last letter
		** that matched.
		*/
		dword = dent->word + strlen (dent->word) + 1;
		wcount = *dword++ & 0xFF;
		while (--wcount >= 0) {
		    dword++;			/* Skip over keep flag */
		    for (w = word;  *w;  w++, dword++) {
			if (*dword != *w) {
			    /* Begin suffix processing.  */
			    if (myupper (dword[-1])) {
				while (*w  &&  !mylower (*w))
				    w++;
				if (*w == '\0')
				    return 1;
			    }
			    else {
				while (*w  &&  !myupper (*w))
				    w++;
				if (*w == '\0')
				    return 1;
			    }
			    break;
			}
		    }
		    if (*w == '\0')
			return 1;
		    while (*dword++)	/* Skip to next prototype */
			;
		}
	}
	/*
	** If it's a capitalize word, and the first letter is lowercase,
	** it's illegal.  Note that all-lowercase followcase words will
	** be found by the string scan above.
	*/
	if (dent->capitalize  &&  mylower (*word))
		return 0;
	/*
	** If it's not a followcase word, or if the capitalize flag is set,
	** capitalization (e.g. at the beginning of a sentence) is always
	** legal.  All-lowercase is also legal for non-followcase words.
	*/
	if (!dent->followcase  ||  dent->capitalize) {
		for (dword = word + 1;  *dword;  dword++) {
			if (myupper (*dword))
				break;
		}
		if (*dword == '\0')
			return 1;	/* It was all-lower or capitalized */
	}
	return 0;			/* Word has a bad mix of cases */
}
#endif

flagpr (w, flag, modpoint)
register char *w;
int flag;
register char *modpoint;	/* Must be in w and greater than w */
{
	register char *orig;

	/*
	** We refuse to print if the case at and after modpoint isn't
	** consistent with the case just before there.  This prevents
	** things like "OEM's" from being turned into OEM/M, which in
	** turn will only accept "OEM'S".
	*/
	orig = orig_word + (modpoint - w);
	if (myupper(orig[-1])) {
		while (*orig) {
			if (mylower (*orig++))
				return;
		}
	}
	else {
		while (*orig) {
			if (myupper (*orig++))
				return;
		}
	}
	/* Case is ok.  Now print it. */
	for (orig = orig_word;  *w  &&  w < modpoint;  orig++, w++)
		putchar (*orig);
	if (myupper (orig[-1]))
		printf ("%s", w);
	else {
		for (  ;  *w;  w++) {
			if (myupper (*w))
				putchar (tolower (*w));
			else
				putchar (*w);
		}
	}
	printf ("/%c\n", flag);
}

g_ending (w,n)
register char *w;
register int n;
{
	register char *p;
	register struct dent *dent;

	p = w + n - 3;	/* if the word ends in 'ing', then *p == 'i' */
	
	if (strcmp (p, "ING") != 0)
		return;

	*p = 'E';	/* change I to E, like in CREATING */
	*(p+1) = 0;
	n -= 2;

	if (n < 2)
		return;

	if (cflag)
		flagpr (w, 'G', p);
	else if ((dent = lookup (w, n, 1)) != NULL
	  &&  dent->g_flag) {
		wordok = 1;
		return;
	}


	*p = 0;
	n--;

	if (n < 2)
		return;

	if (p[-1] == 'E')
		return;	/* this stops CREATEING */

	if (cflag)
		flagpr (w, 'G', p);
	else if ((dent = lookup (w, n, 1)) != NULL) {
		if (dent->g_flag)
			wordok = 1;
		return;
	}
	return;
}

d_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n - 2;

	if (strcmp (p, "ED") != 0)
		return;

	p[1] = 0;	/* kill 'D' */
	n--;

	if (cflag)
		flagpr (w, 'D', p + 1);
	else if ((dent = lookup (w, n, 1)) != NULL) { /* eg CREATED */
		if (dent->d_flag) {
			wordok = 1;
			return;
		}
	}

	if (n < 3)
		return;

	p[0] = 0;
	n--;
	p--;

	/* ED is now completely gone */

	if (p[0] == 'I' && !vowel (p[-1])) {
		p[0] = 'Y';
		if (cflag)
			flagpr (w, 'D', p);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->d_flag) {
			wordok = 1;
			return;
		}
		p[0] = 'I';
	}

	if ((p[0] != 'E' && p[0] != 'Y') ||
	    (p[0] == 'Y' && vowel (p[-1]))) {
		if (cflag)
			flagpr (w, 'D', p + 1);
		else if ((dent = lookup (w, n, 1)) != NULL) {
			if (dent->d_flag)
				wordok = 1;
			return;
		}
	}
}

t_ending (w,n)
register char *w;
register n;
{

	register char *p;
	register struct dent *dent;

	p = w + n - 3;

	if (strcmp (p, "EST") != 0)
		return;

	p[1] = 0;	/* kill "ST" */
	n -= 2;

	if (cflag)
		flagpr (w, 'T', p);
	else if ((dent = lookup (w, n, 1)) != NULL
	    &&  dent->t_flag) {
		wordok = 1;
		return;
	}

	if (n < 3)
		return;

	p[0] = 0;	/* kill 'E' */
	n--;
	p--;

	/* EST is now completely gone */

	if (p[0] == 'I' && !vowel (p[-1])) {
		p[0] = 'Y';
		if (cflag)
			flagpr (w, 'T', p);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->t_flag) {
			wordok = 1;
			return;
		}
		p[0] = 'I';
	}

	if ((p[0] != 'E' && p[0] != 'Y') ||
	    (p[0] == 'Y' && vowel (p[-1]))) {
		if (cflag)
			flagpr (w, 'T', p + 1);
		else if ((dent = lookup (w, n, 1)) != NULL) {
			if (dent->t_flag)
				wordok = 1;
			return;
		}
	}

}


r_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n - 2;

	if (strcmp (p, "ER") != 0)
		return;

	p[1] = 0;	/* kill 'R' */
	n--;

	if (cflag)
		flagpr (w, 'R', p + 1);
	else if ((dent = lookup (w, n, 1)) != NULL
	    &&  dent->r_flag) {
		wordok = 1;
		return;
	}

	if (n < 3)
		return;

	p[0] = 0;	/* kill 'E' */
	n--;
	p--;

	/* ER is now completely gone */

	if (p[0] == 'I' && !vowel (p[-1])) {
		p[0] = 'Y';
		if (cflag)
			flagpr (w, 'R', p);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->r_flag) {
			wordok = 1;
			return;
		}
		p[0] = 'I';
	}

	if ((p[0] != 'E' && p[0] != 'Y') ||
	    (p[0] == 'Y' && vowel (p[-1]))) {
		if (cflag)
			flagpr (w, 'R', p + 1);
		else if ((dent = lookup (w, n, 1)) != NULL) {
			if (dent->r_flag)
				wordok = 1;
			return;
		}
	}

}

h_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n - 2;

	if (strcmp (p, "TH") != 0)
		return;

	*p = 0;		/* kill "TH" */
	n -= 2;

	p -= 2;

	if (p[1] != 'Y') {
		if (cflag)
			flagpr (w, 'H', p + 2);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->h_flag)
			wordok = 1;
	}

	if (strcmp (p, "IE") != 0)
		return;

	p[0] = 'Y';	/* change "IE" to "Y" */
	p[1] = 0;
	n--;

	if (cflag)
		flagpr (w, 'H', p + 1);
	else if ((dent = lookup (w, n, 1)) != NULL)
		if (dent->h_flag)
			wordok = 1;

}

/*
 * check for flags: X, J, Z, S, P, M
 *
 * X	-ions or -ications or -ens
 * J	-ings
 * Z	-ers or -iers
 * S	-ies or -es or -s
 * P	-iness or -ness
 * M	-'S
 */

s_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n;

	p[-1] = 0;	/* kill 'S' */
	n--;

	if (index ("SXZHY", p[-2]) == NULL || (p[-2] == 'Y' && vowel (p[-3]))) {
		if (cflag)
			flagpr (w, 'S', p - 1);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->s_flag) {
			wordok = 1;
			return;
		}
	}


	switch (p[-2]) {	/* letter before S */
	case 'N':	/* X */
		if (strcmp (p-4, "ION") == 0) {
			p[-4] = 'E';	/* change "ION" to "E" */
			p[-3] = 0;
			n -= 2;
			if (cflag)
				flagpr (w, 'X', p - 4);
			else if ((dent = lookup (w, n, 1)) != NULL
			  &&  dent->x_flag) {
				wordok = 1;
				return;
			}
		}
		if (strcmp (p-8, "ICATE") == 0) {
			p[-8] = 'Y';	/* change "ICATE" to "Y" */
			p[-7] = 0;
			n -= 4;
			if (cflag)
				flagpr (w, 'X', p - 8);
			else if ((dent = lookup (w, n, 1)) != NULL
			    && dent->x_flag)
				wordok = 1;
			return;
		}
		if (strcmp (p-3, "EN") == 0 && p[-4] != 'E' && p[-4] != 'Y') {
			p[-3] = 0;	/* kill "EN" */
			n -= 2;
			if (cflag)
				flagpr (w, 'X', p - 3);
			else if ((dent = lookup (w, n, 1)) != NULL
			    && dent->x_flag)
				wordok = 1;
			return;
		}
		return;
	case 'G':	/* J */
		if (strcmp (p-4, "ING") != 0)
			return;
		p[-4] = 'E';	/* change "ING" to "E" */
		p[-3] = 0;
		n -= 2;
		if (cflag)
			flagpr (w, 'J', p - 4);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->j_flag) {
			wordok = 1;
			return;
		}
		if (p[-5] == 'E')
			return;		/* This stops CREATEING */
		p[-4] = 0;	/* kill 'E' */
		n--;
		if (cflag)
			flagpr (w, 'J', p - 4);
		else if ((dent = lookup (w, n, 1)) != NULL
		    && dent->j_flag)
			wordok = 1;
		return;
	case 'R':	/* Z */
		if (strcmp (p-3, "ER") != 0)
			return;

		p[-2] = 0;	/* kill 'R' */
		n--;
		if (cflag)
			flagpr (w, 'Z', p - 2);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->z_flag) {
			wordok = 1;
			return;
		}
		if (p[-4] == 'I'  &&  !vowel (p[-5])) {
			p[-4] = 'Y';	/* change "IE" to "Y" */
			p[-3] = 0;
			n--;
			if (cflag)
				flagpr (w, 'Z', p - 4);
			else if ((dent = lookup (w, n, 1)) != NULL
			    && dent->z_flag) {
				wordok = 1;
				return;
			}
			p[-4] = 'I';	/* change 'Y' to 'I' */
		}
		if ((p[-4] != 'E' && p[-4] != 'Y') ||
		    (p[-4] == 'Y' && vowel (p[-5]))) {
			if(p[-3]) n--;
			p[-3] = 0;
			if (cflag)
				flagpr (w, 'Z', p - 3);
			else if ((dent = lookup (w, n, 1)) != NULL
			  && dent->z_flag)
				wordok = 1;
		}
		return;
	case 'E': /* S (except simple adding of an S) */
		p[-2] = 0;	/* drop the E */
		n--;
		if (index ("SXZH", p[-3]) != NULL) {
			if (cflag)
				flagpr (w, 'S', p - 2);
			else if ((dent = lookup (w, n, 1)) != NULL) {
				if (dent->s_flag)
					wordok = 1;;
				return;
			}
		}
		if (p[-3] == 'I'  &&  !vowel (p[-4])) {
			p[-3] = 'Y';
			if (cflag)
				flagpr (w, 'S', p - 3);
			else if ((dent = lookup (w, n, 1)) != NULL
			    && dent->s_flag)
				wordok = 1;
			return;
		}
		return;

	case 'S':	/* P */
		if (strcmp (p-4, "NES") != 0)
			return;

		p[-4] = 0;	/* kill "NES" */
		n -= 3;
		if (p[-5] != 'Y' || vowel (p[-6])) {
			if (cflag)
				flagpr (w, 'P', p - 4);
			else if ((dent = lookup (w, n, 1)) != NULL
			    &&  dent->p_flag) {
				wordok = 1;
				return;
			}
		}
		if (p[-5] == 'I') {
			p[-5] = 'Y';
			if (cflag)
				flagpr (w, 'P', p - 5);
			else if ((dent = lookup (w, n, 1)) != NULL
			    && dent->p_flag)
				wordok = 1;
		}
		return;
	case '\'':	/* M */
		p[-2] = '\0';	/* kill "'" */
		n--;
		if (cflag)
			flagpr (w, 'M', p - 2);
		else if ((dent = lookup (w, n, 1)) != NULL
		    &&  dent->m_flag)
			wordok = 1;
		return;
	}
}

/* only the N flag */
n_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n;

	if (p[-2] == 'E') {
		if (p[-3] == 'E' || p[-3] == 'Y')
			return;
		p[-2] = 0;	/* kill "EN" */
		n -= 2;
		if (cflag)
			flagpr (w, 'N', p - 2);
		else if ((dent = lookup (w, n, 1)) != NULL
		    && dent->n_flag)
			wordok = 1;
		return;
	}

	if (strcmp (p-3, "ION") != 0)
		return;

	p[-3] = 'E';	/* change "ION" to "E" */
	p[-2] = 0;
	n -= 2;

	if (cflag)
		flagpr (w, 'N', p - 3);
	else if ((dent = lookup (w, n, 1)) != NULL) {
		if (dent->n_flag)
			wordok = 1;
		return;
	}

	if (strcmp (p-7, "ICATE") != 0)	/* check is really against "ICATION" */
		return;

	p[-7] = 'Y';	/* change "ICATE" to "Y" */
	p[-6] = 0;
	n -= 4;
	
	if (cflag)
		flagpr (w, 'N', p - 7);
	else if ((dent = lookup (w, n, 1)) != NULL && dent->n_flag)
		wordok = 1;
	return;
}

/* flags: v */
e_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n;

	if (strcmp (p-3, "IVE") != 0)
		return;
	p[-3] = 'E';	/* change "IVE" to "E" */
	p[-2] = 0;
	n -= 2;

	if (cflag)
		flagpr (w, 'V', p - 3);
	else if ((dent = lookup (w, n, 1)) != NULL
	  &&  dent->v_flag) {
		wordok = 1;
		return;
	}

	if (p[-4] == 'E')
		return;

	p[-3] = 0;	/* kill 'E' */
	n--;

	if (cflag)
		flagpr (w, 'V', p - 3);
	else if ((dent = lookup (w, n, 1)) != NULL && dent->v_flag)
		wordok = 1;
	return;
}

/* flags: y */
y_ending (w,n)
register char *w;
register n;
{
	register char *p;
	register struct dent *dent;

	p = w + n;

	if (strcmp (p-2, "LY") != 0)
		return;

	p[-2] = 0;	/* kill "LY" */
	n -= 2;

	if (cflag)
		flagpr (w, 'Y', p - 2);
	else if ((dent = lookup (w, n, 1)) != NULL && dent->y_flag)
		wordok = 1;
	return;
}

vowel (c)
register c;
{
	return (c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U');
}
