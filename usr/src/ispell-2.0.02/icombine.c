/* 
   icombine:  combine multiple ispell dictionary entries into a single 
              entry with the options of all entries

   Author:  Gary Puckering
            Cognos, Inc.

   Written:  January 29, 1987
   
   Notes:  Input lines consist of a word followed optionally by
           by one or more flags.  e.g CREATE/V/N/S
           
           Flags on lines with identical root words are combined.
           No editing on flags is performed.
           Flags are forced to uppercase, but roots are left alone.
           Old-style flags, like /X/N will be output as /NX.
           Flags are output in alphabetical order.
           Non-letters appearing before the first "/" are retained,
             those after are dropped.
           Root words that differ only in capitalization are combined.
*/

#include <stdio.h>
#include <ctype.h>
#include "config.h"
#include "ispell.h"

#define MAXFLAGS 26     /* letters A-Z */
#define MAXLINE 255     /* maximum line size */

#define TRUE 1
#define FALSE 0
typedef int bool;

bool flagtbl[MAXFLAGS]; /* array of flag options */

char line[MAXLINE];     /* current line */
char lastword[MAXLINE]; /* previous word */
char uclastword[MAXLINE]; /* uppercase version of lastword */
char word[MAXLINE];     /* current word */
char ucword[MAXLINE];	/* uppercase version of current word */
char flags[MAXLINE];    /* current flags */
int  expand = 0;	/* if NZ, expand instead of combining */

extern char *strcpy ();


main(argc,argv)
int argc;
char *argv[];
{

    if (argc > 1  &&  strcmp (argv[1], "-e") == 0)
	expand = 1;
    if (gets(line)) 
    {
        parse(line,lastword,flags);
        uccopy (uclastword, lastword);
        getflags(flags);
    }
    else
	return 0;

    while (gets(line))
    {
        parse(line,word,flags);
        uccopy (ucword, word);
        if (strcmp(word,lastword)!=0)   /* possibly different word */
        {
	    if (strcmp (ucword, uclastword) != 0 /* truly different word */
	      ||  resolvecaps (word, ucword, lastword, uclastword))
	    {					/* or caps differ */
		putword();
		strcpy(lastword,word);
		strcpy(uclastword,ucword);
	    }
        }
        getflags(flags);
    }
    putword();
    return 0;
}

putword()
{
    printf("%s",lastword);
    putflags();
}

parse(ln,wrd,flgs)
    char ln[];
    char wrd[];
    char flgs[];
{
    register char *p, *q;

    /* copy line up to first "/" or to end */
    for (p=ln,q=wrd; *p && *p != '/'; p++,q++) *q = *p;
    *q = NULL;

    strcpy(flgs,p);     /* copy from "/" to end */
}

getflags(flgs)
    char *flgs;
{
    register char *p;

    for (p=flgs; *p; p++) 
        if (*p != '/')
	{
	    if (islower (*p))
		*p = toupper (*p);
	    if (isupper(*p))
		flagtbl[(*p)-'A'] = TRUE;
	}
}

putflags()
{
    register int i;
    int slashout = 0;

    if (expand)
	putchar ('\n');

    for (i=0; i<MAXFLAGS; i++) 
        if (flagtbl[i]) 
        {
	    if (expand)
		printf("%s/%c\n", lastword, i + 'A');
            else
	    {
		if (!slashout)
		    putchar('/');
		slashout = 1;
		putchar(i+'A');
	    }
            flagtbl[i]=FALSE;
        }
    if (!expand)
	putchar('\n');
}

/*
 * This routine resolves capitalization conflicts.  The idea is to combine
 * only those cases that ispell can "uncombine".
 *
 * Entry: word and lastword differ, but only by case.
 *
 * Exit: Returns 1 if word and lastword both need to be in the dictionary,
 *	 0 if they can be handled by a single entry.  If the return is zero,
 *	 lastword may have been modified to reflect the union of the two
 *	 entries.
 *
 * Rules:
 *
 * (1) If either word is entirely in upper case, it "loses" to the other
 *     word.  The "winning" word is copied to lastword, and 0 is returned.
 * (2) If either word is "followcase" (defined as being mixed case with a
 *     capital letter appearing after the first character), the two
 *     variants are considered to differ, and 1 is returned.  Furthermore,
 *     a flag is set (by copying the word to "lastfollow") so that all
 *     future variants fo the word will be considered to differ.
 * (3) If one word is capitalized and the other is all-lowercase, the
 *     lowercase word "wins".  It is copied to lastword, and 0 is returned.
 *     HOWEVER, if a "followcase" variant of the word has been seen, this
 *     rule does not apply, and rule (4) will cause the words to be
 *     considered different.
 * (4) If a "followcase" variant of the word has been seen, the words are
 *     always considered to differ.  1 is returned.
 *
 * Note that the input must be sorted with "sort -t/ +0f -1 +0 -1" for this
 * code to work.
 */
resolvecaps (word, ucword, lastword, uclastword)
char *word;
char *ucword;
char *lastword;
char *uclastword;
{
    register char *w;
    register char *lw;
    static char lastfollow[200] = "";

    /* Rule (1): Upper case loses */
    for (w = word;  *w  &&  !mylower (*w);  w++)
	;
    if (*w == '\0')
	return 0;
    for (lw = lastword;  *lw  &&  !mylower (*lw);  lw++)
	;
    if (*lw == '\0')
    {
	strcpy (lastword, word);
	strcpy (uclastword, ucword);
	return 0;
    }
    /* Rule (4):  followcase forces all subsequent variants to be different. */
    if (strcmp (ucword, lastfollow) == 0)
	return 1;
    /* Rule (2):  "followcase" is different. */
    for (w = word + 1, lw = lastword + 1;
      *w  &&  !myupper (*w)  &&  !myupper (*lw);
      w++, lw++)
	;
    if (*w)		/* We don't test *lw 'cause lengths are the same */
	{
	strcpy (lastfollow, ucword);
	return 1;
	}
    /* Rule (3):  all-lowercase beats capitalized */
    if (myupper (lastword[0]))
    {
	strcpy (lastword, word);
	strcpy (uclastword, ucword);
    }
    return 0;
}

uccopy (dest, src)
register char *dest;
register char *src;
{
    while (*src)
    {
	if (mylower (*src))
	    *dest++ = toupper (*src++);
	else
	    *dest++ = *src++;
    }
    *dest = '\0';
}
