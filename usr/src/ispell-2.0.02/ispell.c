/* -*- Mode:Text -*- */

#define MAIN

/*
 * ispell.c - An interactive spelling corrector.
 *
 * Copyright (c), 1983, by Pace Willisson
 * Permission for non-profit use is hereby granted.
 * All other rights reserved.
 *
 * 1987, Robert McQueer, added:
 *	-w option & handling of extra legal word characters
 *	-d option for alternate dictionary file
 *	-p option & WORDLIST variable for alternate personal dictionary
 *	-x option to suppress .bak files.
 *	8 bit text & config.h parameters
 * 1987, Geoff Kuenning, added:
 *	-c option for creating suffix suggestions from raw words
 *	suffixes in personal dictionary file
 *	hashed personal dictionary file
 *	-S option for unsorted word lists
 * 1987, Greg Schaffer, added:
 *	-T option (for TeX and LaTeX instead of troff) [later changed to -t]
 *	   passes over \ till next whitespace.
 *	   does not recognize % (comment)
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#ifdef USG
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include "config.h"
#include "ispell.h"
#include "version.h"

#define ISTEXTERM(c)   (((c) == '{') || \
			((c) == '}') || \
			((c) == '[') || \
			((c) == ']'))

FILE *infile;
FILE *outfile;

char hashname[MAXPATHLEN];

extern struct dent *treeinsert();
extern char *upcase ();
extern char *lowcase ();

extern char *rindex();
extern char *strcpy ();

/*
** we use extended character set range specifically to allow intl.
** character set characters.  We are being REALLY paranoid about indexing
** this array - explicitly cast into unsigned INTEGER, then mask
** If NO8BIT is set, text will be masked to ascii range.
*/
static int Trynum;
#ifdef NO8BIT
static char Try[128];
static char Checkch[128];
#define iswordch(X) (Checkch[((unsigned)(X))&0x7f])
#else
static char Try[256];
static char Checkch[256];
#define iswordch(X) (Checkch[((unsigned)(X))&0xff])
#endif

static int sortit = 1;

givehelp ()
{
	erase ();
	printf ("Whenever a word is found that is not in the dictionary,\r\n");
	printf ("it is printed on the first line of the screen.  If the dictionary\r\n");
	printf ("contains any similar words, they are listed with a single digit\r\n");
	printf ("next to each one.  You have the option of replacing the word\r\n");
	printf ("completely, or choosing one of the suggested words.\r\n");
	printf ("\r\n");
	printf ("Commands are:\r\n\r\n");
	printf ("R       Replace the misspelled word completely.\r\n");
	printf ("Space   Accept the word this time only\r\n");
	printf ("A       Accept the word for the rest of this file.\r\n");
	printf ("I       Accept the word, and put it in your private dictionary.\r\n");
	printf ("0-9     Replace with one of the suggested words.\r\n");
	printf ("L       Look up words in system dictionary.\r\n");
	printf ("Q       Write the rest of this file, ignoring misspellings, ");
	printf (         "and start next file.\r\n");
	printf ("X       Exit immediately.  Asks for confirmation.  ");
	printf (         "Leaves file unchanged.\r\n");
	printf ("!       Shell escape.\r\n");
	printf ("^L      Redraw screen.\r\n");
	printf ("\r\n\r\n");
	printf ("-- Type space to continue --");
	fflush (stdout);
	while (getchar () != ' ')
		;
}


char *getline();

int cflag = 0;
int lflag = 0;
int incfileflag = 0;
int aflag = 0;
int fflag = 0;
#ifndef USG
int sflag = 0;
#endif
int xflag = 0;
int tflag = 0;

char *askfilename;

static char *Cmd;

usage ()
{
	fprintf (stderr,
	    "Usage: %s [-dfile | -pfile | -wchars | -t | -x | -S] file .....\n",
	    Cmd);
	fprintf (stderr,
	    "       %s [-dfile | -pfile | -wchars | -t] -l\n",
	    Cmd);
#ifndef USG
	fprintf (stderr,
	    "       %s [-dfile | -pfile | -ffile | -t | -s] {-a | -A}\n",
	    Cmd);
#else
	fprintf (stderr,
	    "       %s [-dfile | -pfile | -ffile | -t] {-a | -A}\n",
	    Cmd);
#endif
	fprintf (stderr, "       %s [-wchars] -c\n", Cmd);
	fprintf (stderr, "       %s -v\n", Cmd);
	exit (1);
}

static initckch()
{
	register int c;

	Trynum = 0;
#ifdef NO8BIT
	for (c = 0; c < 128; ++c) {
#else
	for (c = 0; c < 256; ++c) {
#endif
		if (myalpha((char) c)) {
			Checkch[c] = (char) 1;
			if (myupper((char) c)) {
				Try[Trynum] = (char) c;
				++Trynum;
			}
		}
		else
			Checkch[c] = (char) 0;
	}
}

main (argc, argv)
char **argv;
{
	char *p;
	char *cpd;
	char num[4];
	unsigned mask;
	static char outbuf[BUFSIZ];

	Cmd = *argv;

	initckch();
	sprintf(hashname,"%s/%s",LIBDIR,DEFHASH);

	cpd = NULL;

	argv++;
	argc--;
	while (argc && **argv == '-') {
		switch ((*argv)[1]) {
		case 'v':
			printf ("%s\n", Version_ID);
			exit (0);
		case 't':
			tflag++;
			break;
		case 'A':
			incfileflag = 1;
			aflag = 1;
			break;
		case 'a':
			aflag++;
			break;
		case 'c':
			cflag++;
			lflag++;
			break;
		case 'x':
			xflag++;
			break;
		case 'f':
			fflag++;
			p = (*argv)+2;
			if (*p == '\0') {
				argv++; argc--;
				if (argc == 0)
					usage ();
				p = *argv;
			}
			askfilename = p;
			break;
		case 'l':
			lflag++;
			break;
#ifndef USG
		case 's':
			sflag++;
			break;
#endif
		case 'S':
			sortit = 0;
			break;
		case 'p':
			cpd = (*argv)+2;
			if (*cpd == '\0') {
				argv++; argc--;
				if (argc == 0)
					usage ();
				cpd = *argv;
			}
			break;
		case 'd':
			p = (*argv)+2;
			if (*p == '\0') {
				argv++; argc--;
				if (argc == 0)
					usage ();
				p = *argv;
			}
			if (*p == '/')
				strcpy(hashname,p);
			else
				sprintf(hashname,"%s/%s",LIBDIR,p);
			break;
		case 'w':
			num[3] = '\0';
#ifdef NO8BIT
			mask = 0x7f;
#else
			mask = 0xff;
#endif
			p = (*argv)+2;
			if (*p == '\0') {
				argv++; argc--;
				if (argc == 0)
					usage ();
				p = *argv;
			}
			while (Trynum <= mask && *p != '\0') {
				if (*p != 'n'  &&  *p != '\\') {
					Checkch[((unsigned)(*p))&mask] = (char) 1;
					Try[Trynum] = *p & mask;
					++p;
				}
				else {
					++p;
					num[0] = '\0'; 
					num[1] = '\0'; 
					num[2] = '\0'; 
					num[3] = '\0';
					if (isdigit (p[0]))
						num[0] = p[0];
					if (isdigit (p[1]))
						num[1] = p[1];
					if (isdigit (p[2]))
						num[2] = p[2];
					if (p[-1] == 'n') {
						p += strlen (num);
						num[0] = atoi (num);
					}
					else {
						p += strlen (num);
						if (num[0])
							num[0] -= '0';
						if (num[1]) {
							num[0] <<= 3;
							num[0] += num[1] - '0';
						}
						if (num[2]) {
							num[0] <<= 3;
							num[0] += num[2] - '0';
						}
					}
					Try[Trynum] = num[0] & mask;
					Checkch[num[0] & mask] = 1;
				}
				++Trynum;
			}
			break;
		default:
			usage();
		}
		argv++; argc--;
	}

	if (!argc && !lflag && !aflag)
		usage ();

	if (linit () < 0)
		exit (0);

	treeinit (cpd);

	if (aflag) {
		askmode ();
		exit (0);
	}

	setbuf (stdout, outbuf);
	if (lflag) {
		infile = stdin;
		checkfile ();
		exit (0);
	}

	terminit ();

	while (argc--)
		dofile (*argv++);

	done ();
}

char firstbuf[BUFSIZ], secondbuf[BUFSIZ];
char *currentchar;
char token[BUFSIZ];

int quit;

char *currentfile = NULL;

dofile (filename)
char *filename;
{
	int c;
	char	bakfile[256];
	struct stat statbuf;
	char *cp;

	currentfile = filename;

	if ((infile = fopen (filename, "r")) == NULL) {
		fprintf (stderr, "Can't open %s\r\n", filename);
		sleep (2);
		return;
	}

	if (access (filename, 2) < 0) {
		fprintf (stderr, "Can't write to %s\r\n", filename);
		sleep (2);
		return;
	}

	fstat (fileno (infile), &statbuf);
	strcpy(tempfile, TEMPNAME);
	mktemp (tempfile);
	chmod (tempfile, statbuf.st_mode);
	if ((outfile = fopen (tempfile, "w")) == NULL) {
		fprintf (stderr, "Can't create %s\r\n", tempfile);
		sleep (2);
		return;
	}

	quit = 0;

	/* See if the file is a .tex file.  If so, set the appropriate flag. */
	if ((cp = rindex (filename, '.')) != NULL  &&  strcmp (cp, ".tex") == 0)
		tflag = 1;
	checkfile ();

	fclose (infile);
	fclose (outfile);

	if (!cflag)
		treeoutput ();

	if ((infile = fopen (tempfile, "r")) == NULL) {
		fprintf (stderr, "temporary file disappeared (%s)\r\n", tempfile);	
		sleep (2);
		return;
	}

	sprintf(bakfile, "%s%s", filename, BAKEXT);
	if(link(filename, bakfile) == 0)
		unlink(filename);

	/* if we can't write new, preserve .bak regardless of xflag */
	if ((outfile = fopen (filename, "w")) == NULL) {
		fprintf (stderr, "can't create %s\r\n", filename);
		sleep (2);
		return;
	}

	chmod (filename, statbuf.st_mode);

	while ((c = getc (infile)) != EOF)
		putc (c, outfile);

	fclose (infile);
	fclose (outfile);

	unlink (tempfile);
	if (xflag)
		unlink(bakfile);
}

checkfile ()
{
	register int c;
	register char *p;
	register int len;

	secondbuf[0] = 0;

	while (1) {
		strcpy (firstbuf, secondbuf);
		if (quit) {	/* quit can't be set in l mode */
			while (fgets (secondbuf, sizeof secondbuf, infile) != NULL)
				fputs (secondbuf, outfile);
			break;
		}

		if (fgets (secondbuf, sizeof secondbuf, infile) == NULL)
			break;
		currentchar = secondbuf;
		
		len = strlen (secondbuf) - 1;

		if(!tflag) {
		    /* skip over .if */
		    if (strncmp(currentchar,".if t",5) == 0 
		    ||  strncmp(currentchar,".if n",5) == 0) {
			    copyout(&currentchar,5);
			    while (*currentchar && isspace(*currentchar)) 
				    copyout(&currentchar, 1);
		    }

		    /* skip over .ds XX or .nr XX */
		    if (strncmp(currentchar,".ds ",4) == 0 
		    ||  strncmp(currentchar,".de ",4) == 0
		    ||  strncmp(currentchar,".nr ",4) == 0) {
			    copyout(&currentchar, 3);
			    while (*currentchar && isspace(*currentchar)) 
				    copyout(&currentchar, 1);
			    while (*currentchar && !isspace(*currentchar))
				    copyout(&currentchar, 1);
			    if (*currentchar == 0) {
				    if (!lflag) putc ('\n', outfile);
				    continue;
			    }
		    }
		}

		if (secondbuf [ len ] == '\n')
			secondbuf [ len ] = 0;

		/* if this is a formatter command, skip over it */
		if (!tflag && *currentchar == '.') {
			while (*currentchar && !myspace (*currentchar)) {
				if (!lflag)
					putc (*currentchar, outfile);
				currentchar++;
			}
			if (*currentchar == 0) {
				if (!lflag)
					putc ('\n', outfile);
				continue;
			}
		}

		while (1) {
			while (*currentchar && !iswordch(*currentchar)) {
			    if (tflag)		/* TeX or LaTeX stuff */
			    {
				if (*currentchar == '\\') {
				    /* skip till whitespace */
				    while (*currentchar && 
					(!isspace(*currentchar) &&
					 !ISTEXTERM(*currentchar))) {
					    if (!lflag)
						putc(*currentchar, outfile);
					    currentchar++;
					}
				    continue;
				}
			    }
			    else
			    {
				/* formatting escape sequences */
				if (*currentchar == '\\') {
				switch ( currentchar [1] ) {
				case 'f':
					if(currentchar[2] != '(') {
						/* font change: \fX */
						copyout(&currentchar, 3);
					}
					else {
						/* font change: \f(XY */
						copyout(&currentchar, 5);
					}
					continue;
				case 's':
					/* size change */
					p = currentchar + 2;
					if (*p == '+'  ||  *p == '-')
						p++;
					/* This looks wierd 'cause we assume
					** *p is now a digit.
					*/
					if (isdigit (p[1]))
						p++;
					copyout (&currentchar,
						    p - currentchar + 1);
					continue;
				case '(':
					/* extended char set escape: \(XX */
					copyout(&currentchar, 4);
					continue;
				case '*':
					if ( currentchar[2] != '(' )
						copyout(&currentchar, 3);
					else
						copyout(&currentchar, 5);
					continue;
				default:
					break;
				    }
				}
			    }

				if (!lflag)
					putc (*currentchar, outfile);
				currentchar++;
			}

			if (*currentchar == 0)
				break;

			p = token;
			while (iswordch(*currentchar) ||
			       (*currentchar == '\'' &&
				iswordch(*(currentchar + 1))))
			  *p++ = *currentchar++;
			*p = 0;
			if (lflag) {
				if (!good (token)  &&  !cflag)
					printf ("%s\n", token);
			} else {
				if (!quit)
				correct (token, &currentchar);
			}
			if (!lflag)
				fprintf (outfile, "%s", token);
		}
		if (!lflag)
			putc ('\n', outfile);
	}
}

#define MAXPOSSIBLE	100	/* Max no. of possibilities to generate */

char possibilities[MAXPOSSIBLE][BUFSIZ];
int pcount;
int maxposslen;

correct (token, currentchar)
char *token;
char **currentchar;
{
	register int c;
	register int i;
	int col_ht;
	int ncols;
	register char *p;
	char *start_l2;
	char *begintoken;

	begintoken = *currentchar - strlen (token);

checkagain:
	if (good (token))
		return;

	erase ();
	printf ("    %s", token);
	if (currentfile)
		printf ("              File: %s", currentfile);
	printf ("\r\n\r\n");

	makepossibilities (token);

	/*
	 * Make sure we have enough room on the screen to hold the
	 * possibilities.  Reduce the list if necessary.  co / (maxposslen + 8)
	 * is the maximum number of columns that will fit.
	 */
	col_ht = li - 6;		/* Height of columns of words */
	ncols = co / (maxposslen + 8);
	if (pcount > ncols * col_ht)
		pcount = ncols * col_ht;

#ifdef EQUAL_COLUMNS
	/*
	 * Equalize the column sizes.  The last column will be short.
	 */
	col_ht = (pcount + ncols - 1) / ncols;
#endif

	for (i = 0; i < pcount; i++) {
		move (2 + (i % col_ht), (maxposslen + 8) * (i / col_ht));
		printf ("%2d: %s", i, possibilities[i]);
	}

	move (li - 3, 0);
	show_line (firstbuf, firstbuf, 0);

	start_l2 = secondbuf;
	if (line_size (secondbuf, *currentchar) > co - 1) {
		start_l2 = begintoken - (co / 2);
		while (start_l2 < begintoken) {
			i = line_size (start_l2, *currentchar) + 1;
			if (i <= co)
				break;
			start_l2 += i - co;
		}
		if (start_l2 > begintoken)
			start_l2 = begintoken;
		if (start_l2 < secondbuf)
			start_l2 = secondbuf;
	}
	show_line (start_l2, begintoken, strlen (token));


	while (1) {
		fflush (stdout);
		switch (c = (getchar () & NOPARITY)) {
#ifndef USG
		case 'Z' & 037:
			stop ();
			erase ();
			goto checkagain;
#endif
		case ' ':
			erase ();
			fflush (stdout);
			return;
		case 'x': case 'X':
			printf ("Are you sure you want to throw away your changes? ");
			fflush (stdout);
			c = (getchar () & NOPARITY);
			if (c == 'y' || c == 'Y') {
				erase ();
				fflush (stdout);
				done ();
			}
			putchar (7);
			goto checkagain;
		case 'i': case 'I':
			treeinsert (token, 1);
			erase ();
			fflush (stdout);
			return;
		case 'a': case 'A':
			treeinsert (token, 0);
			erase ();
			fflush (stdout);
			return;
		case 'L' & 037:
			goto checkagain;
		case '?':
			givehelp ();
			goto checkagain;
		case '!':
			{
				char buf[200];
				move (li - 1, 0);
				putchar ('!');
				if (getline (buf) == NULL) {
					putchar (7);
					erase ();
					fflush (stdout);
					goto checkagain;
				}
				printf ("\r\n");
				fflush (stdout);
				shellescape (buf);
				erase ();
				goto checkagain;
			}
		case 'r': case 'R':
			move (li - 1, 0);
			printf ("Replace with: ");
			if (getline (token) == NULL) {
				putchar (7);
				erase ();
				goto checkagain;
			}
			inserttoken (secondbuf, begintoken, token, currentchar);
			erase ();
			goto checkagain;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			i = c - '0';
			if (pcount > 10
			    &&  i > 0  &&  i <= (pcount - 1) / 10) {
				c = getchar () & NOPARITY;
				if (c >= '0'  &&  c <= '9')
					i = i * 10 + c - '0';
				else if (c != '\r'  &&  c != '\n') {
					putchar (7);
					break;
				}
			}
			if (i < pcount) {
				strcpy (token, possibilities[i]);
				inserttoken (secondbuf, begintoken,
				    token, currentchar);
				erase ();
				return;
			}
			putchar (7);
			break;
		case '\r':	/* This makes typing \n after single digits */
		case '\n':	/* ..less obnoxious */
			break;
		case 'l': case 'L':
			{
				char buf[100];
				move (li - 1, 0);
				printf ("Lookup string ('*' is wildcard): ");
				if (getline (buf) == NULL) {
					putchar (7);
					erase ();
					goto checkagain;
				}
				printf ("\r\n\r\n");
				fflush (stdout);
				lookharder (buf);
				erase ();
				goto checkagain;
			}
		case 'q': case 'Q':
			quit = 1;
			erase ();
			fflush (stdout);
			return;
		default:
			putchar (7);
			break;
		}
	}
}

show_line (line, invstart, invlen)
register char *line;
register char *invstart;
register int invlen;
{
	register int width;

	width = 0;
	while (line != invstart  &&  width < co - 1)
		width += show_char (*line++, width);
	if (invlen) {
		inverse ();
		while (--invlen >= 0  &&  width < co - 1)
			width += show_char (*line++, width);
		normal ();
	}
	while (*line  &&  width < co - 1)
		width += show_char (*line++, width);
	printf ("\r\n");
}

show_char (ch, linew)
register int ch;
int linew;
{
	if (ch == '\t') {
		putchar ('\t');
		return 8 - (linew & 0x07);
	}
	else if (ch < ' ') {
		putchar ('^');
		putchar (ch + 'A' - '\001');
		return 2;
	}
	putchar (ch);
	return 1;
}

line_size (buf, bufend)
register char *buf;
register char *bufend;
{
	register int width;

	for (width = 0;  buf < bufend  &&  *buf;  buf++) {
		if (*buf == '\t')
			width = (width + 8) & ~0x07;
		else if (*buf < ' ')
			width += 2;
		else
			width++;
	}
	return width;
}

inserttoken (buf, start, token, currentchar)
char *buf, *start; 
register char *token;
char **currentchar;
{
	char copy[BUFSIZ];
	register char *p, *q;

	strcpy (copy, buf);

	for (p = buf, q = copy; p != start; p++, q++)
		*p = *q;
	q += *currentchar - start;
	while (*token  &&  iswordch (*token))
		*p++ = *token++;
	*currentchar = p;
	if (*token) {

		/*
		** The token changed to two words.  Split it up and save the
		** second one for later.
		*/

		*p++ = *token;
		*token++ = '\0';
		while (*token)
			*p++ = *token++;
	}
	while (*p++ = *q++)
		;
}

int casecmp (a, b)
char *a;
char *b;
{
	register char *ap;
	register char *bp;

	for (ap = a, bp = b;  *ap;  ap++, bp++) {
		if (mylower (*ap)) {
			if (mylower (*bp)) {
				if (*ap != *bp)
					return *ap - *bp;
			}
			else {
				if (toupper (*ap) != *bp)
					return toupper (*ap) - *bp;
			}
		}
		else {
			if (myupper (*bp)) {
				if (*ap != *bp)
					return *ap - *bp;
			}
			else {
				if (tolower (*ap) != *bp)
					return tolower (*ap) - *bp;
			}
		}
	}
	if (*bp != '\0')
		return -*bp;
	return strcmp (a, b);
}

makepossibilities (word)
register char *word;
{
	register int i;

	for (i = 0; i < MAXPOSSIBLE; i++)
		possibilities[i][0] = 0;
	pcount = 0;
	maxposslen = 0;

#ifdef CAPITALIZE
	wrongcapital (word);
#endif
	if (pcount < MAXPOSSIBLE) wrongletter (word);
	if (pcount < MAXPOSSIBLE) extraletter (word);
	if (pcount < MAXPOSSIBLE) missingletter (word);
	if (pcount < MAXPOSSIBLE) transposedletter (word);

	if (sortit  &&  pcount)
		qsort ((char *) possibilities, pcount,
		    sizeof (possibilities[0]), casecmp);
}

insert (word)
register char *word;
{
	register int i;

	for (i = 0; i < pcount; i++)
		if (strcmp (possibilities[i], word) == 0)
			return (0);

	strcpy (possibilities[pcount++], word);
	i = strlen (word);
	if (i > maxposslen)
		maxposslen = i;
	if (pcount >= MAXPOSSIBLE)
		return (-1);
	else
		return (0);
}

#ifdef CAPITALIZE
wrongcapital (word)
register char *word;
{
	char newword[BUFSIZ];

	/*
	** All-uppercase is always legal.  If the word matches, "ins_cap"
	** will recapitalize it correctly.
	*/
	strcpy (newword, word);
	upcase (newword);
	if (good (newword))
		return ins_cap (newword, word);
	return 0;
}
#endif

wrongletter (word)
register char *word;
{
	register int i, j, c, n;
	char newword[BUFSIZ];

	n = strlen (word);
	strcpy (newword, word);
#ifdef CAPITALIZE
	upcase (newword);
#endif

	for (i = 0; i < n; i++) {
		for (j=0; j < Trynum; ++j) {
			newword[i] = Try[j];
			if (good (newword)) {
				if (ins_cap (newword, word) < 0)
					return;
			}
		}
#ifdef CAPITALIZE
		c = word[i];
		if (islower (c))
		    newword[i] = toupper (c);
		else
		    newword[i] = c;
#else
		newword[i] = word[i];
#endif
	}
}

extraletter (word)
register char *word;
{
	char newword[BUFSIZ];
	register char *p, *s, *t;

	if (strlen (word) < 3)
		return;

	for (p = word; *p; p++) {
		for (s = word, t = newword; *s; s++)
			if (s != p)
				*t++ = *s;
		*t = 0;
#ifdef CAPITALIZE
		if (good (upcase (newword))) {
			if (ins_cap (newword, word) < 0)
				return;
		}
#else
		if (good (newword)) {
			if (ins_cap (newword, word) < 0)
				return;
		}
#endif
	}
}

missingletter (word)
char word[];
{
	char newword[BUFSIZ]; 
	register char *p, *r, *s, *t;
	register int i;

	for (p = word; p == word || p[-1]; p++) {
		for (s = newword, t = word; t != p; s++, t++)
			*s = *t;
		r = s++;
		while (*t)
			*s++ = *t++;
		*s = 0;
		for (i=0; i < Trynum; ++i) {
			*r = Try[i];
#ifdef CAPITALIZE
			if (good (upcase (newword))) {
				if (ins_cap (newword, word) < 0)
					return;
			}
#else
			if (good (newword)) {
				if (ins_cap (newword, word) < 0)
					return;
			}
#endif
		}
	}
}

transposedletter (word)
register char *word;
{
	char newword[BUFSIZ];
	register int t;
	register char *p;

	strcpy (newword, word);
	for (p = newword; p[1]; p++) {
		t = p[0];
		p[0] = p[1];
		p[1] = t;
#ifdef CAPITALIZE
		if (good (upcase (newword))) {
			if (ins_cap (newword, word) < 0)
				return;
		}
#else
		if (good (newword)) {
			if (ins_cap (newword, word) < 0)
				return;
		}
#endif
		t = p[0];
		p[0] = p[1];
		p[1] = t;
	}
}

/* Insert one or more correctly capitalized versions of pattern */
ins_cap (word, pattern)
register char *word, *pattern;
{
	static char newword[BUFSIZ];
	register char *p, *q;
	char *psave;
	register int wcount;

	if (*word == 0)
		return;

	strcpy (newword, word);
#ifdef CAPITALIZE
	if (lastdent->allcaps)
		return insert (upcase (newword)); /* Uppercase required */
	for (p = pattern;  *p;  p++)
		if (mylower (*p))
			break;
	if (*p == '\0')
		return insert (upcase (newword)); /* Pattern was all caps */
	for (p = pattern;  *p;  p++)
		if (myupper (*p))
			break;
	if (*p == '\0') {			/* Pattern was all lower */
		if (!lastdent->followcase  &&  !lastdent->capitalize)
			return insert (lowcase (newword));
		/*
		** If there's a followcase version that's all-lower,
		** insert only that version.
		*/
		if (lastdent->followcase) {
			p = lastdent->word;
			p += strlen (p) + 1;
			wcount = (*p++ & 0xFF);
			while (--wcount >= 0) {
				for (psave = ++p;
				    *p  &&  !myupper (*p);
				    p++)
					;
				if (*p == '\0')	/* Was it all lowercase? */
					return insert (psave);	/* Yup, quit */
				while (*p++)
					;	/* Skip to next case sample */
			}
		}
	}
	/*
	** The sample wasn't all-upper, and either it wasn't all-lower or
	** all-lower is illegal.  Insert all legal capitalizations.  In
	** some cases, this may include all-lowercase.
	*/
	if (lastdent->capitalize) {
		lowcase (newword);
		if (mylower (newword[0]))
			newword[0] = toupper (newword[0]);
		insert (newword);
	}
	if (lastdent->followcase) {
		p = lastdent->word;
		p += strlen (p) + 1;
		wcount = (*p++ & 0xFF);
		while (--wcount >= 0) {
			/* Insert every variation;  it's easier */
			if (insert (++p) < 0)
				return -1;
			while (*p++)
				;		/* Skip to end of sample */
		}
		return 0;
	}
	if (lastdent->capitalize)
		return 0;
	/*
	** We get here only if none of the special capitalization flags are
	** set.  If first letter of the pattern is capitalized, capitalize
	** the first letter of the result.  Otherwise produce all lowercase.
	*/
	lowcase (newword);
	if (myupper (pattern[0])  &&  mylower (newword[0]))
		newword[0] = toupper (newword[0]);
	return insert (newword);
#else
	if (myupper (pattern[0])) {
		if (myupper (pattern[1])) {
			for (p = word, q = newword; *p; p++, q++) {
				if (mylower (*p))
					*q = toupper (*p);
				else
					*q = *p;
			}
			*q = 0;
		} else {
			if (mylower (word [0]))
				newword[0] = toupper (word[0]);
			else
				newword[0] = word[0];

			for (p = word + 1, q = newword + 1; *p; p++, q++)
				if (myupper (*p))
					*q = tolower (*p);
				else
					*q = *p;

			*q = 0;
		}
	} else {
		for (p = word, q = newword; *p; p++, q++)
			if (myupper (*p))
				*q = tolower (*p);
			else
				*q = *p;
		*q = 0;
	}
	return insert (newword);
#endif
}

char *
getline (s)
register char *s;
{
	register char *p;
	register int c;

	p = s;

	while (1) {
		fflush (stdout);
		c = (getchar () & NOPARITY);
		if (c == '\\') {
			putchar ('\\');
			fflush (stdout);
			c = (getchar () & NOPARITY);
			backup ();
			putchar (c);
			*p++ = c;
		} else if (c == ('G' & 037)) {
			return (NULL);
		} else if (c == '\n' || c == '\r') {
			*p = 0;
			return (s);
		} else if (c == erasechar) {
			if (p != s) {
				p--;
				backup ();
				putchar (' ');
				backup ();
			}
		} else if (c == killchar) {
			while (p != s) {
				p--;
				backup ();
				putchar (' ');
				backup ();
			}
		} else {
			*p++ = c;
			putchar (c);
		}
	}
}

askmode ()
{
	char buf[BUFSIZ];
	register int i;

	if (fflag) {
		if (freopen (askfilename, "w", stdout) == NULL) {
			fprintf (stderr, "Can't create %s\n", askfilename);
			exit (1);
		}
	}

	setbuf (stdin, NULL);
	setbuf (stdout, NULL);

	while (xgets (buf) != NULL) {
		/* *line is like `i', @line is like `a' */
		if (buf[0] == '*' || buf[0] == '@') {
			treeinsert(buf + 1, buf[0] == '*');
			printf("*\n");
			treeoutput ();
		} else if (good (buf)) {
			if (rootword[0] == 0) {
				printf ("*\n");	/* perfect match */
			} else {
				printf ("+ %s\n", rootword);
			}
		} else {
			makepossibilities (buf);
			if (possibilities[0][0]) {
				printf ("& ");
				for (i = 0; i < MAXPOSSIBLE; i++) {
					if (possibilities[i][0] == 0)
						break;
					printf ("%s ", possibilities[i]);
				}
				printf ("\n");
			} else {
				printf ("#\n");
			}
		}
#ifndef USG
		if (sflag) {
			stop ();
			if (fflag) {
				rewind (stdout);
				creat (askfilename, 0666);
			}
		}
#endif
	}
}

/* Copy/ignore "cnt" number of characters pointed to by *cc. */
copyout(cc, cnt)
register char	**cc;
register cnt;
{
	while (--cnt >= 0) {
		if (*(*cc) == 0)
			break;
		if (!lflag)
			putc (*(*cc), outfile);
		(*cc)++;
	}
}

lookharder(string)
char *string;
{
	char cmd[150], grepstr[100];
	register char *g, *s;
	register int wild = 0;

	g = grepstr;
	for (s = string; *s != '\0'; s++)
		if (*s == '*') {
			wild++;
			*g++ = '.';
			*g++ = '*';
		} else
			*g++ = *s;
	*g = '\0';
	if (grepstr[0]) {
#ifdef LOOK
		if (wild)
			/* string has wild card characters */
			sprintf (cmd, "%s '^%s$' %s", EGREPCMD, grepstr, WORDS);
		else
			/* no wild, use look(1) */
			sprintf (cmd, "%s %s %s", LOOK, grepstr, WORDS);
#else
		sprintf (cmd, "%s '^%s$' %s", EGREPCMD, grepstr, WORDS);
#endif
		shellescape (cmd);
	}
}
