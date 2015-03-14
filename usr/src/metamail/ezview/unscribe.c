/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
	unscribe.c -- Remove WriteScribe formatting for vanilla interpretation

This function is an amazingly cheap rendition of a Scribe compilation with @device(File).

It also has a function, PrintQuotingFormatting, which can be used to take
unformatted text (e.g. headers you are forwarding) and quote it past a given
version of the datastream interpretation.  

*/

#include <stdio.h>
#include <andrewos.h> /* strings.h */
#include <ctype.h>
#include <util.h>
#include "mail.h"
#include <config.h>

#ifndef	MIN
#define	MIN(a,b) (((a)<(b))?(a):(b))
#endif

/* #define SPECIALFACES 1 */

#define Version1		0
#define Version2		1
#define Version10		10
#define	Version12		12

#define v1StateInit		0
#define v1StateSeenAt	1

#define v2StateInit		0
#define v2StateSeenAt	1
#define v2StateSeenNL	2
#define v2StateSeen2NL	3

#define v10StateInit		0
#define	v10StateSeenBS		1   /* BackSlash */
#define v10StateInKeyword	2
#define v10StateNukeStyle	3
#define v10StateBufferStyle	4
#define v10StateDropChar	5
#define	v10StateSeenNL		6   /* Newline */
#define	v10StateSeen2NL		7   /* Second newline */
#define	v10State2Deep		8   /* begindatacount >=2 */
#define	v10State2DeepSeenBS	9
#define	v10State2DeepInKeyword 10

#define LINEMAX  72
#define LINEFUDGE  7
#define MAXNETMAILLINE 255 /* Things longer than this will be crudely wrapped by unscribe */
#define MAXLONGLINE 1000 /* ... unless in a literal style, which will wrap lines at this length */
#define KEYWORDSIZE 200
#define TABINTERVAL 8

#define INDENT_NOCHANGE (-LINEMAX)
#define JUSTIFY_NOCHANGE -1
#define JUSTIFY_LEFT 0
#define JUSTIFY_CENTER 1
#define JUSTIFY_RIGHT 2

/* This macro exists to avoid a function call on every character */
#define ADDCHAR(State, fp, chr) \
    if ((State)->lmargin + (State)->writecount \
	+ (State)->specials > (State)->rmargin) \
		WriteCount += WriteFrag((State), (fp), (chr)); \
    else { \
	if ((State)->writecount == 0) StartFrag(State); \
	(State)->linefrag[(State)->writecount++] = (chr); \
    }

struct StateVector {
    short lmargin;
    short rmargin;
    short indent;     /* how much to indent (or outdent if neg) the first line */
    short face;       /* 0=plain, 1=italics, 2=bold */
    short justify;    /*-1=don't change, 0=leftj, 1=center, 2=rightj */
    short quotedepth;
    short style;
    short newpara;    /* flag: indicates that the first line of the paragraph is yet to be printed */
    struct StateVector *next;
};

struct styletable {
    char *name;
    long hash;
    struct StateVector vec;
} Styles[] =
{
    { "define",		0, { 0,	0, 0, 0, 0, 0,} },	/* first pseudo-style */
    { "template",	0, { 0, 0, 0, 0, 0, 0,} },
    { "textdsversion",	0, { 0, 0, 0, 0, 0, 0,} },
    { "enddata",	0, { 0, 0, 0, 0, 0, 0,} },
    { "dsversion",	0, { 0, 0, 0, 0, 0, 0,} },
    { "begindata",	0, { 0, 0, 0, 0, 0, 0,} },
    { "view",		0, { 0,	0, 0, 0, 0, 0,} },	/* last pseudo-style */
    { "literal",        0, { 0, MAXLONGLINE, 0, 0, 0, 0,} },      /* no formatting style */
    { "italic",		0, { 0, 0,INDENT_NOCHANGE, 1,JUSTIFY_NOCHANGE, 0,} },
    { "bold",		0, { 0, 0,INDENT_NOCHANGE, 2,JUSTIFY_NOCHANGE, 0,} },
    { "chapter",	0, { 0, 0, 0, 2,JUSTIFY_LEFT, 0,} },
    { "section",	0, { 0, 0, 0, 2,JUSTIFY_LEFT, 0,} },
    { "subsection",	0, { 0, 0, 0, 2,JUSTIFY_LEFT, 0,} },
    { "paragraph",	0, { 0, 0, 0, 1,JUSTIFY_LEFT, 0,} },
    { "bigger",		0, { 0, 0,INDENT_NOCHANGE, 0,JUSTIFY_NOCHANGE, 0,} },
    { "indent",		0, { 4, 4, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { "typewriter",	0, { 0, 0,INDENT_NOCHANGE, 0,JUSTIFY_NOCHANGE, 0,} },
    { "display",	0, { 4, 4, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { "example",	0, { 4, 0, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { "itemize",	0, { 4, 0, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { "description",	0, { 4, 0,-4, 0,JUSTIFY_NOCHANGE, 0,} },
    { "enumerate",	0, { 4, 0, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { "programexample", 0, { 4, 0, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { "quotation",	0, { 0, 0,INDENT_NOCHANGE, 0,JUSTIFY_NOCHANGE, 1,} },
    { "subscript",	0, { 0, 0,INDENT_NOCHANGE, 0,JUSTIFY_NOCHANGE, 0,} },
    { "superscript",	0, { 0, 0,INDENT_NOCHANGE, 0,JUSTIFY_NOCHANGE, 0,} },
    { "smaller",	0, { 0, 0,INDENT_NOCHANGE, 0,JUSTIFY_NOCHANGE, 0,} },
    { "heading",	0, { 0, 0, 0, 1,JUSTIFY_LEFT, 0,} },
    { "majorheading",	0, { 0, 0, 0, 0,JUSTIFY_CENTER, 0,} },
    { "formatnote",	0, { 0, 0, 0, 0,JUSTIFY_LEFT, 0,} },
    { "subheading",	0, { 0, 0, 0, 2,JUSTIFY_LEFT, 0,} },
    { "center",		0, { 0, 0, 0, 0,JUSTIFY_CENTER, 0,} },
    { "flushleft",	0, { 0, 0, 0, 0,JUSTIFY_LEFT, 0,} },
    { "flushright",	0, { 0, 0, 0, 0,JUSTIFY_RIGHT, 0,} },
    { "underline",	0, { 0, 0,INDENT_NOCHANGE, 1,JUSTIFY_NOCHANGE, 0,} },
    { "leftindent",	0, { 4, 0, 0, 0,JUSTIFY_NOCHANGE, 0,} },
    { 0,		0, { 0, 0, 0, 0, 0, 0,} }
};

static int usVersion(val)
char *val;
{ /* Return the VersionXXX value corresponding to the probe, or -1 if there's no match. */
    int numVal;
    char *Src;

    while (*val != '\0' && index(" \t\r\n", *val) != NULL) ++val;
    if (ULstrcmp(val, "Yes") == 0) {
	return Version1;
    } else if (ULstrcmp(val, "2") == 0) {
	return Version2;
    } else if (ULstrcmp(val, "10") == 0) {
	return Version10;
    } else if (ULstrcmp(val, "12") == 0) {
	return Version12;	/* ***Check next clause when adding a new type*** */
    } else {
	numVal = 0;	/* Interpret any type >= 12 as Version12 */
	for (Src = val; *Src != '\0'; ++Src) if (! isdigit(*Src)) {numVal = 1; break;}
	if (numVal == 0) {
		numVal = atoi(val);
		if (numVal >= 12) return Version12;
	}
	return -1;		/* don't understand header value */
    }
}

static long
hash(str)
char *str;
{
    unsigned long h = 0;
    unsigned long g;

    for ( ; *str; str++) {
	h = (h << 4) + *str;
	if (g = h & 0xf0000000) {
	    h = h ^ (g >> 24);
	    h = h ^ g;
	}
    }
    return h;
}

static struct styletable *
findstyle(str)
char *str;
{
    struct styletable *stp;
    long strhash;

    strhash = hash(str);
    for (stp = Styles; stp->name; stp++) {
	if (strhash == stp->hash && !strcmp(stp->name, str)) return stp;
    }
    return 0;
}

int UnScribeInit(fieldvalue, refstate)
char *fieldvalue;
struct ScribeState **refstate;
{
    /* Pass it the value of the X-Andrew-ScribeFormat: header to see if the package can handle this format.  Returns a code value >= 0 for OK, < 0 for error.  Remember this code value to pass to UnScribe.  In addition, pass the address of an integer variable to hold the UnScribe state between calls.  This variable will be initialized with the UnScribeInit call.
 */
    int Vers;

    /* Initialize keyword table, but only once */
    if (Styles->hash == 0) {
	struct styletable *stp;

	for (stp = Styles; stp->name; stp++) stp->hash = hash(stp->name);
    }

    *refstate = (struct ScribeState *) malloc(sizeof(struct ScribeState));
    if (!*refstate) return -2;
    (*refstate)->writecount = 0;
    (*refstate)->begindatacount = 0;
    (*refstate)->keywordpos = 0;
    (*refstate)->linefrag = NULL;
    (*refstate)->keyword = NULL;
    (*refstate)->stylebuf = NULL;
    (*refstate)->vector = NULL;
    Vers = usVersion(fieldvalue);
    switch (Vers) {
	case Version1:
	    (*refstate)->statecode = v1StateInit;	/* initialize it */
	    break;
	case Version2:
	    (*refstate)->statecode = v2StateInit;
	    break;
	case Version10:
	case Version12:
	    (*refstate)->statecode = v10StateInit;
	    (*refstate)->linefrag = (char *) malloc(MAXLONGLINE+2);
	    (*refstate)->keyword = (char *) malloc(KEYWORDSIZE);
	    (*refstate)->stylebuf = (char *) malloc(KEYWORDSIZE);
	    (*refstate)->lmargin = 0;
	    (*refstate)->rmargin = LINEMAX;
	    (*refstate)->indent = 0;
	    (*refstate)->justify = JUSTIFY_LEFT;
	    (*refstate)->specials = 0;
	    (*refstate)->face = 0;
	    (*refstate)->newpara = 1;
	    (*refstate)->vector = (struct StateVector *) malloc(sizeof(struct StateVector));
	    if ((*refstate)->linefrag == NULL
	      || (*refstate)->keyword == NULL
	      || (*refstate)->stylebuf == NULL
	      || (*refstate)->vector == NULL) {
	    	if ((*refstate)->linefrag != NULL) free((*refstate)->linefrag);
		if ((*refstate)->keyword != NULL) free((*refstate)->keyword);
		if ((*refstate)->stylebuf != NULL) free((*refstate)->stylebuf);
		if ((*refstate)->vector != NULL) free((*refstate)->vector);
		free(*refstate);
		return -2;
	    }
	    (*refstate)->vector->next = NULL;
	    (*refstate)->vector->lmargin = 0;
	    (*refstate)->vector->rmargin = LINEMAX;
	    (*refstate)->vector->indent = 0;
	    (*refstate)->vector->justify = JUSTIFY_LEFT;
	    (*refstate)->vector->face = 0;
	    (*refstate)->vector->newpara = 1;
	    (*refstate)->vector->quotedepth = 0;
	    break;
	default:
	    free(*refstate);
	    *refstate = NULL;
	    return -1;		/* don't understand header value */
    }
    return Vers;
}

static void
StartFrag(State)
struct ScribeState *State;
{
#ifdef SPECIALFACES
    int oldfaces;
    int newfaces;
#endif /* SPECIALFACES */

    /* set current state from stack */
    State->lmargin = State->vector->lmargin;
    State->rmargin = State->vector->rmargin;
    State->justify = State->vector->justify;
    State->indent = State->vector->indent;

#ifdef SPECIALFACES
    oldfaces = State->face & State->vector->face;
    newfaces = State->vector->face ^ oldfaces;

    /* incorporate specials into start of string */
    if (oldfaces & 1) State->linefrag[State->writecount++] = '_';
    if (oldfaces & 2) State->linefrag[State->writecount++] = '*';
/*
    if (oldfaces & 4) State->linefrag[State->writecount++] = '-';
    if (oldfaces & 8) State->linefrag[State->writecount++] = '+';
*/

    if (newfaces & 1) State->linefrag[State->writecount++] = '_';
    if (newfaces & 2) State->linefrag[State->writecount++] = '*';
/*
    if (newfaces & 4) State->linefrag[State->writecount++] = '-';
    if (newfaces & 8) State->linefrag[State->writecount++] = '+';
*/
#endif /* SPECIALFACES */
}

static int
WriteFrag(State, fPtr, Chr)
struct ScribeState *State;
FILE *fPtr;
char Chr;
{
    int	NumWritten = 1;	/* at least a NewLine */
    char *tail;
#ifdef TRIMTRAILINGWHITESPACE
    char *c;
#endif /* TRIMTRAILINGWHITESPACE */
    int i;
    int len;
    int width;
    int quotedepth;

    if (Chr == '\n' && State->writecount == 0) {
	putc('\n', fPtr);
	State->newpara = 1;  /* Yes, this is a new paragraph */
	return 1;
    } 

    /* do we really _have_ to wrap it? */
    if (Chr != '\n' && State->newpara && ((State)->lmargin + (State)->writecount + (State)->specials < ((State)->rmargin + LINEFUDGE)) ) {
	/* if we're outputting the one and only line of a paragraph,
	   we can let the line grow a little longer. */
	(State)->linefrag[(State)->writecount++] = (Chr);
	return 0;
    } /* if (State->newpara ... ) */

    /* find a break point in the string */
    if (Chr != '\n') {
        State->linefrag[State->writecount] = '\0';
        tail = State->linefrag + MIN((State)->rmargin - (State)->lmargin - (State)->specials + 1, (State)->writecount);

	while (tail-- > State->linefrag && *tail != ' ') ;
	if (*tail != ' ') {
				/* couldn't find a break point */
	    tail = State->linefrag + State->writecount;
	    len = State->writecount;
	} else {
				/* found a break point */
	    *tail++ = '\0';
	    len = tail - State->linefrag - 1;
	}
    } else {
        tail = State->linefrag + State->writecount;
	*tail = '\0';
	len = State->writecount;
    }

#ifdef TRIMTRAILINGWHITESPACE
    /* remove trailing white space */
    for (c = State->linefrag + len; *--c == ' '; len--) ;
    *++c = '\0';
#endif /* TRIMTRAILINGWHITESPACE */

    /* quote marks :-) */
    /* putting quote mark handling here means that quotes appear at the left margin only--no more in-line quotes */
    for (quotedepth = State->vector->quotedepth; quotedepth; quotedepth--, NumWritten++) putc('>', fPtr);
    if (State->vector->quotedepth) {
      putc(' ', fPtr);
      NumWritten++;
    }

    /* left margin */
    if (State->newpara) {
      for (i = State->lmargin + State->indent; i; i--, NumWritten++) putc(' ', fPtr);
    } else {
      for (i = State->lmargin; i; i--, NumWritten++) putc(' ', fPtr);
    }

    /* justification */
    width = State->rmargin - State->lmargin + 1;
    len += State->specials;
    if (State->justify == JUSTIFY_CENTER) i = (width - len) / 2;
    else if (State->justify == JUSTIFY_RIGHT) i = width - len;
    else i = 0;	    /* default to JUSTIFY_LEFT */
    while ((i--) > 0) { putc(' ', fPtr); NumWritten++; }

    /* shove out the actual text */
    if (len) {
	fputs(State->linefrag, fPtr);

#ifdef SPECIALFACES
/*
	if (State->vector->face & 8) putc('+', fPtr);
	if (State->vector->face & 4) putc('-', fPtr);
*/
	if (State->vector->face & 2) putc('*', fPtr);
	if (State->vector->face & 1) putc('_', fPtr);
#endif /* SPECIALFACES */
	NumWritten += len;
    }
    putc('\n', fPtr);
    State->writecount = 0;
    State->face = State->vector->face;
    if (Chr != '\n') {
        State->newpara = 0;		/* no longer */
	StartFrag(State);
	len = strlen(tail);
	strcpy(State->linefrag + State->writecount, tail);
	State->writecount += len;
	State->linefrag[State->writecount++] = Chr;
    } else {
      State->newpara = 1;  /* Yes, this is a new paragraph */
    }

    return NumWritten;
}

static int
HandleKeyword(State, fPtr)
struct ScribeState *State;
FILE *fPtr;
{
    struct styletable *style;
    short idx = -1;
    struct StateVector *vec;
    int WriteCount = 0;
#ifdef SPECIALFACES
    unsigned int bitvec;
#endif /* SPECIALFACES */

    style = findstyle(State->keyword);
    if (style) idx = style - Styles;

    switch (idx) {
	case -1:    /* unknown keyword, ignore */
	    State->statecode = State->previous_state;
	    vec = (struct StateVector *) malloc(sizeof(struct StateVector));
	    *vec = *(State->vector);
	    vec->next = State->vector;
	    State->vector = vec;
	    vec->style = idx;
	    break;
	case 0:	case 1:	case 2:	case 3:	case 4:	/* garbage keywords */
	    State->statecode = v10StateNukeStyle;
	    break;
	case 5:	    /* begindata */
	    State->statecode = v10StateNukeStyle;
	    State->begindatacount++;
	    break;
	case 6:	    /* view */
	    State->statecode = v10StateBufferStyle;
	    State->keywordpos = 0;
	    break;
	default:    /* a normal style */
	    State->statecode = State->previous_state;

	    /* do silly things to mark beginning of environment */
	    if (State->writecount) {
#ifdef INLINEQUOTING
		if (style->vec.quotedepth) {
		    ADDCHAR(State, fPtr, '>');
		    ADDCHAR(State, fPtr, ' ');
		}
#endif /* INLINEQUOTING */
#ifdef SPECIALFACES
		if (style->vec.face & 1) ADDCHAR(State, fPtr, '_');
		if (style->vec.face & 2) ADDCHAR(State, fPtr, '*');
/*
		if (style->vec.face & 4) ADDCHAR(State, fPtr, '-');
		if (style->vec.face & 8) ADDCHAR(State, fPtr, '+');
*/
#endif /* SPECIALFACES */
	    }

	    /* push to top of stack */
	    vec = (struct StateVector *) malloc(sizeof(struct StateVector));
	    *vec = *(State->vector);
	    vec->next = State->vector;
	    State->vector = vec;

	    /* play style over vector */
	    vec->style = idx;
	    vec->lmargin += style->vec.lmargin;
	    if (style->vec.rmargin != MAXLONGLINE) {
	      vec->rmargin -= style->vec.rmargin;
	    } else {
	      vec->rmargin = MAXLONGLINE;
	    }
	    vec->face |= style->vec.face;
	    if ((style->vec.indent) != INDENT_NOCHANGE) vec->indent = style->vec.indent;
	    if ((style->vec.justify) != JUSTIFY_NOCHANGE) vec->justify = style->vec.justify;
	    vec->quotedepth += style->vec.quotedepth;

	    /* play style over current state */
	    State->specials = 0;
#ifdef SPECIALFACES
	    bitvec = vec->face;
	    while (bitvec) {
		if (bitvec & 1) State->specials++;
		bitvec >>= 1;
	    }
#endif /* SPECIALFACES */
    }
    return WriteCount;
}

static int
HandleClose(State, fPtr)
struct ScribeState *State;
FILE *fPtr;
{
    struct StateVector *temp;
    int WriteCount = 0;
#ifdef SPECIALFACES
    struct styletable *style;
    int bitvec;
#endif /* SPECIALFACES */

    if (State->vector->next) {
	/* pop top of stack */
	temp = State->vector;
	State->vector = State->vector->next;

	/* play new top over current state */
	State->specials = 0;
#ifdef SPECIALFACES
	bitvec = State->vector->face;
	while (bitvec) {
	    if (bitvec & 1) State->specials++;
	    bitvec >>= 1;
	}
#endif /* SPECIALFACES */

	/* close the old environment and nuke it */
#ifdef SPECIALFACES
	if (temp->style >= 0 && State->writecount) {
	    style = Styles + temp->style;
/*
	    if (style->vec.face & 8) ADDCHAR(State, fPtr, '+');
	    if (style->vec.face & 4) ADDCHAR(State, fPtr, '-');
*/
	    if (style->vec.face & 2) ADDCHAR(State, fPtr, '*');
	    if (style->vec.face & 1) ADDCHAR(State, fPtr, '_');
	}
#endif /* SPECIALFACES */
	free(temp);
    } else {
	fprintf(stderr, "Popped too far!\n");
	ADDCHAR(State, fPtr, '}');
    }
    return WriteCount;
}

int UnScribe(code, refstate, text, textlen, fileptr)
int code, textlen;
struct ScribeState **refstate;
char *text;
FILE *fileptr;
{
    /* Pass it the initial UnScribeInit return value and the address of the integer state variable.  Also pass the address of the text to be unscribed, and the number of bytes at that address.  The unscribed text will be written onto stdio file *fileptr.  Return values are something like fwrite: >= 0 for OK, < 0 for errors, with a code either in the return value or in errno.  Basically, if -1 is returned, errno is valid; otherwise, the code is specific to this procedure.
    */
    register int Char;
    struct ScribeState *MyState;
    int	ReadCount;
    int WriteCount;
    char *Src;

    MyState = *refstate;
    ReadCount = textlen;
    Src = text;
    WriteCount = 0;
    if (ReadCount < 0) return -2;
    switch (code) {
	case Version1:
	    if (MyState->statecode != v1StateInit && MyState->statecode != v1StateSeenAt) return -2;
	    while (--ReadCount >= 0) {
		Char = *Src++;
		switch (MyState->statecode) {
		    case v1StateInit:
			if (Char == '@') MyState->statecode = v1StateSeenAt;
			else {
			    WriteCount++;
			    if (putc(Char, fileptr) == EOF) return -1;
			    if (Char == '\n') MyState->writecount = -1;
			    if (++MyState->writecount > MAXNETMAILLINE) {
				MyState->writecount = 0; WriteCount++;
				if(putc('\n', fileptr) == EOF) return -1;
			    }
			}
			break;
		    case v1StateSeenAt:
			if (Char == '@') {		/* write a single at-sign for ``@@'' */
			    WriteCount++;
			    if (putc(Char, fileptr) == EOF) return -1;
			    if (++MyState->writecount > MAXNETMAILLINE) {
				MyState->writecount = 0; WriteCount++;
				if(putc('\n', fileptr) == EOF) return -1;
			    }
			    /* write nothing for ``@*'' */
			} else if (Char != '*') {	/* write the at-sign and the character */
			    WriteCount += 2;
			    if (putc('@', fileptr) == EOF) return -1;
			    if (putc(Char, fileptr) == EOF) return -1;
			    if (Char == '\n') MyState->writecount = -1;
			    if (++MyState->writecount > MAXNETMAILLINE) {
				MyState->writecount = 0; WriteCount++;
				if(putc('\n', fileptr) == EOF) return -1;
			    }
			}
			MyState->statecode = v1StateInit;
			break;
		    default:
			return -3;
		}
	    }
	    break;

	case Version2:
	    if (MyState->statecode != v2StateInit &&
		MyState->statecode != v2StateSeenAt &&
		MyState->statecode != v2StateSeenNL &&
		MyState->statecode != v2StateSeen2NL) return -2;
	    while (--ReadCount >= 0) {
		Char = *Src++;
		switch (MyState->statecode) {
		    case v2StateInit:
			WriteCount++;
			if (putc(Char, fileptr) == EOF) return -1;
			if (Char == '\n') MyState->writecount = -1;
			if (++MyState->writecount > MAXNETMAILLINE) {
			    MyState->writecount = 0; WriteCount++;
			    if(putc('\n', fileptr) == EOF) return -1;
			}
			if (Char == '@') MyState->statecode = v2StateSeenAt;
			else if (Char == '\n') MyState->statecode = v2StateSeenNL;
			break;
		    case v2StateSeenAt:
			if (Char == '@') {		/* write a single at-sign for ``@@'' */
			    MyState->statecode = v2StateInit;
			} else {	/* write the at-sign and the character */
			    WriteCount++;
			    if (putc(Char, fileptr) == EOF) return -1;
			    if (Char == '\n') MyState->writecount = -1;
			    if (++MyState->writecount > MAXNETMAILLINE) {
				MyState->writecount = 0; WriteCount++;
				if(putc('\n', fileptr) == EOF) return -1;
			    }
			    if (Char == '\n') MyState->statecode = v2StateSeenNL;
			    else MyState->statecode = v2StateInit;
			}
			break;
		    case v2StateSeenNL:
			if (Char == '\n') MyState->statecode = v2StateSeen2NL;	/* consume NL */
			else {
			    WriteCount++;
			    if (putc(Char, fileptr) == EOF) return -1;
			    if (Char == '\n') MyState->writecount = -1;
			    if (++MyState->writecount > MAXNETMAILLINE) {
				MyState->writecount = 0; WriteCount++;
				if(putc('\n', fileptr) == EOF) return -1;
			    }
			    MyState->statecode = (Char == '@' ? v2StateSeenAt : v2StateInit);
			}
			break;
		    case v2StateSeen2NL:
			WriteCount++;
			if (putc(Char, fileptr) == EOF) return -1;
			if (Char == '\n') MyState->writecount = -1;
			if (++MyState->writecount > MAXNETMAILLINE) {
			    MyState->writecount = 0; WriteCount++;
			    if(putc('\n', fileptr) == EOF) return -1;
			}
			if (Char != '\n') MyState->statecode = (Char == '@' ? v2StateSeenAt : v2StateInit);
			break;
		    default:
			return -3;
		}
	    }
	    break;

	case Version12:	/* Slight modification to V10 */
	case Version10:
	    if (MyState->linefrag == NULL ||
		MyState->keyword == NULL) return -5;
	    while (--ReadCount >= 0) {
		Char = *Src++;
		switch (MyState->statecode) {
		case v10StateSeen2NL:
		case v10StateSeenNL:
		case v10StateInit:
		  switch (Char) {
		  case '\n':
		    switch(MyState->statecode) {
		    case v10StateSeen2NL:
		      WriteCount += WriteFrag(MyState, fileptr, Char);
		      break;
		    case v10StateSeenNL:
		      WriteCount += WriteFrag(MyState, fileptr, Char);
		      MyState->statecode = v10StateSeen2NL;
		      break;
		    case v10StateInit:
		      if (code == Version12) {
			MyState->statecode = v10StateSeenNL;
		      } else {
			WriteCount += WriteFrag(MyState, fileptr, Char);
		      }
		    }
		    break;
		  case '\\':
		    MyState->previous_state = MyState->statecode;
		    MyState->statecode = v10StateSeenBS;
		    break;
		  case '}':
		    WriteCount += HandleClose(MyState, fileptr);
		    break;
		  case '\t':
		    if (MyState->lmargin > 0) {
		      do {
			ADDCHAR(MyState, fileptr, ' ');
		      } while ((MyState->lmargin + MyState->writecount) % TABINTERVAL);
		      break;
		    }				  
		  default:
		    ADDCHAR(MyState, fileptr, Char);
		  }
				/* turn off NL flags */
		  if (((Char != '\n') && (Char != '}')) &&
		      ((MyState->statecode == v10StateSeenNL) ||
		      (MyState->statecode == v10StateSeen2NL))) {
		    MyState->statecode = v10StateInit;
		  }
		  break;
		case v10StateSeenBS:
		  switch (Char) {
		  case '\\': case '{': case '}':
		    MyState->statecode = v10StateInit;
		    ADDCHAR(MyState, fileptr, Char);
		    break;
		  case '\n':	/* only present in version12 */
		    MyState->statecode = v10StateInit;
#ifdef BOGUS
				/* I don't know why this is here.
				   As far as I can tell, it shouldn't be. */
		    WriteCount += WriteFrag(MyState, fileptr, Char);
#endif /* BOGUS */
		    break;
		  default:
		    MyState->statecode = v10StateInKeyword;
		    MyState->keywordpos = 0;
		    MyState->keyword[MyState->keywordpos++] = Char;
		  }
		  break;
		case v10StateInKeyword:
		  if (Char != '{')
		    MyState->keyword[MyState->keywordpos++] = Char;
		  else {
		    MyState->keyword[MyState->keywordpos] = 0;
		    WriteCount += HandleKeyword(MyState, fileptr);
		  }
		  break;
		case v10StateNukeStyle:
		  if (Char == '}') {
		    MyState->statecode = v10StateDropChar;
		    if (!strcmp(MyState->keyword, "enddata")) MyState->begindatacount--;
		  }
		  break;
		case v10StateBufferStyle:
		  if (Char != '}') MyState->stylebuf[MyState->keywordpos++] = Char;
		  else {
		    char *c, *d;
		    
		    for (c = MyState->stylebuf; *c && *c != ','; c++) ;
		    *c = '\0';
		    c = MyState->stylebuf;
		    MyState->statecode = v10StateInit;
		    MyState->stylebuf[MyState->keywordpos] = 0;
		    if (!strcmp(c, "bpv")) {
		      ADDCHAR(MyState, fileptr, '\f');
		    } else {
		      d = "[An Andrew ToolKit view (";
		      while (*d) { ADDCHAR(MyState, fileptr, *d); d++; }
		      if (!strcmp(c, "fadview")) c = "an animated drawing";
		      else if (!strcmp(c, "rasterview")) c = "a raster image";
		      else if (!strcmp(c, "zipview")) c = "a drawing";
		      else if (!strcmp(c, "textview")) c = "embedded text";
		      else if (!strcmp(c, "spread")) c = "a spreadsheet";
		      else if (!strcmp(c, "fnotev")) c = "a footnote";
		      else if (!strcmp(c, "linkview")) c = "a hyperlink";
		      while (*c) { ADDCHAR(MyState, fileptr, *c); c++; }
		      c = ") was included here, but could not be displayed.]";
		      while (*c) { ADDCHAR(MyState, fileptr, *c); c++; }
		    }
		  }
		  break;
		case v10StateDropChar:
		  if (MyState->begindatacount < 2) MyState->statecode = v10StateInit;
		  else MyState->statecode = v10State2Deep;
		  break;
		case v10State2Deep:
		  if (Char == '\\') MyState->statecode = v10State2DeepSeenBS;
		  break;
		case v10State2DeepSeenBS:
		  switch (Char) {
		  case 'b': case 'e':	    /* begindata/enddata */
		    MyState->statecode = v10State2DeepInKeyword;
		    MyState->keywordpos = 0;
		    MyState->keyword[MyState->keywordpos++] = Char;
		    break;
		  default:
		    MyState->statecode = v10State2Deep;
		  }
		  break;
		case v10State2DeepInKeyword:
		  if (Char != '{') {
		    MyState->keyword[MyState->keywordpos++] = Char;
		    if (MyState->keywordpos > sizeof("begindata") + 2)
		      MyState->statecode = v10State2Deep;
		  } else {
		    MyState->keyword[MyState->keywordpos] = 0;
		    if (!strcmp(MyState->keyword, "enddata")) {
		      MyState->statecode = v10StateNukeStyle;
		    } else if (!strcmp(MyState->keyword, "begindata")) {
		      MyState->statecode = v10StateNukeStyle;
		      MyState->begindatacount++;
		    } else {
		      MyState->statecode = v10State2Deep;
		    }
		  }
		  break;
		default:
		  return -3;
		}
		if (MyState->keywordpos >= (KEYWORDSIZE - 1)) return -6;
	    }
	    MyState->linefrag[MyState->writecount] = '\0';
	    break;

	default:
	    return -4;
    }
    return WriteCount;
}

int UnScribeFlush(code, refstate, fileptr)
int code;
struct ScribeState **refstate;
FILE *fileptr;
{/* Unbuffer data from an UnScribe sequence.  Return just like fflush(). */
    int Res = 0;

    if (refstate != NULL) {
	if (*refstate != NULL) {
	    if ((*refstate)->linefrag != NULL) {
		if ((*refstate)->writecount) WriteFrag((*refstate), fileptr, '\n');
		Res = fflush(fileptr);
		free((*refstate)->linefrag);
	    }
	    if ((*refstate)->keyword != NULL) free((*refstate)->keyword);
	    if ((*refstate)->stylebuf != NULL) free((*refstate)->stylebuf);
	    while ((*refstate)->vector != NULL) {
		struct StateVector *vp;

		vp = (*refstate)->vector;
		(*refstate)->vector = (*refstate)->vector->next;
		free(vp);
	    }
	    free(*refstate);
	    *refstate = NULL;
	}
    }
    return Res;
}

int UnScribeAbort(code, refstate)
int code;
struct ScribeState **refstate;
{/* Close off the UnScribeInit state without needing a valid file to send to. */

    if (refstate != NULL) {
	if (*refstate != NULL) {
	    if ((*refstate)->linefrag != NULL) free((*refstate)->linefrag);
	    if ((*refstate)->keyword != NULL) free((*refstate)->keyword);
	    if ((*refstate)->stylebuf != NULL) free((*refstate)->stylebuf);
	    while ((*refstate)->vector != NULL) {
		struct StateVector *vp;

		vp = (*refstate)->vector;
		(*refstate)->vector = (*refstate)->vector->next;
		free(vp);
	    }
	    free(*refstate);
	    *refstate = NULL;
	}
    }
    return 0;
}

int PrintMaybeFoldQuotingFormatting(fp, text, format, len, DoFold)
FILE *fp;
char *text, *format;
int len, DoFold;
{ /* return the number of characters written, or negative error value */
    int whichformat;
    register char *s;
    int numWritten, WasNL, Buffered, Column, CurrentLineMax;
    char *newNL, oldC, BreakCode;
    char LineBuf[2*MAXLONGLINE + 10], *LBP;

    if (!format || !*format) {
	whichformat = -1;
    } else {
	whichformat = usVersion(format);
    }
    CurrentLineMax = (DoFold ? LINEMAX : MAXLONGLINE);
    numWritten = 0; LBP = LineBuf;
    WasNL = 1; Buffered = Column = 0;
    for (s = text; *s && len--; ++s) {
	switch(*s) {
	    case '\\': case '{': case '}':	/* sometimes need quoting */
		if (whichformat == Version10 || whichformat == Version12)
			{*LBP++ = '\\'; ++Buffered;}
		goto NormalChar;
	    case '@':			/* sometimes needs quoting */
		if (whichformat == Version1 || whichformat == Version2)
			{*LBP++ = '@'; ++Buffered;}
		goto NormalChar;
	    case '\n':
		if (WasNL == 0) {		/* First newline in a sequence */
		    *LBP++ = '\0';	/* Unbuffer what's been buffered. */
		    if (fputs(LineBuf, fp) == EOF) return -1;
		    numWritten += Buffered;
		    LBP = LineBuf; Buffered = Column = 0;
		    switch (whichformat) {	/* Handle the end-of-line as format dictates. */
		case Version1:	/* Scribe newline convention */
			if (putc('@', fp) == EOF) return -1;
			if (putc('*', fp) == EOF) return -1;
			numWritten += 2;
			break;
		case Version2:
		case Version12:	/* add one NL to a sequence of newlines */
			if (putc('\n', fp) == EOF) return -1;
			++numWritten;
			break;
		case Version10:	/* no special treatment (BOGUS!) */
		default:		/* if not formatted, do nothing. */
			break;
		    }
		}
		if (putc('\n', fp) == EOF) return -1;	/* write the newline itself. */
		++numWritten;
		WasNL = 1;
		break;
	    default:  NormalChar:
		*LBP++ = *s;
		++Buffered;
		if (*s == '\t') Column |= 07;	/* Hack for 8-character tab stops */
		++Column;
		if (Buffered >= CurrentLineMax || Column >= CurrentLineMax) {
			*LBP = '\0';	/* Do newline processing in LineBuf. */
			newNL = LBP - 1;
			while (newNL > LineBuf && *newNL != ' ')
				--newNL;
			if (*newNL == ' ') { /* have a space to use */
				while (*newNL == ' ') ++newNL;
				if (whichformat < 0) --newNL;
				BreakCode = 1;
			} else {		/* no space at which to break it */
				newNL = LBP;
				BreakCode = 0;
			}
			oldC = *newNL;
			*newNL = '\0';
			if (fputs(LineBuf, fp) == EOF) return -1;
			*newNL = oldC;
			numWritten += (newNL - LineBuf);
			if (BreakCode == 0) {	/* Trying to break a word in the middle */
			    if (whichformat == Version12) {
				if (fputs("\\\n", fp) == EOF) return -1;
				numWritten += 2;
			    }
			} else {
				while (*newNL == ' ') ++newNL;
				if (putc('\n', fp) == EOF) return -1;
				++numWritten;
			}
			strcpy(LineBuf, newNL);
			Buffered = LBP - newNL;
			LBP = &LineBuf[Buffered];
			Column = 0;
			for (newNL = LineBuf; newNL < LBP; ++newNL) {
				if (*newNL == '\t') Column |= 07;
				++Column;	/* Get column count straight */
			}
		}
		WasNL = 0;
		break;
	}
    }
    if (Buffered > 0) {
	*LBP++ = '\0';	/* Unbuffer what's been buffered. */
	if (fputs(LineBuf, fp) == EOF) return -1;
	numWritten += Buffered;
    }
    return numWritten;
}

int PrintQuotingFormatting(fp, text, format, len)
FILE *fp;
char *text, *format;
int len;
{ /* return the number of characters written, or negative error value */
    return PrintMaybeFoldQuotingFormatting(fp, text, format, len, 0);
}

#ifdef TESTINGONLYTESTING
main(argc, argv)
int argc;
char *argv[];
{
  int version, err;
  char buf[500];
  struct ScribeState *ussp;
  FILE *fptr;

  if (argc == 1) {
    fptr = stdin;
  } else {
    if ((fptr = fopen(argv[1], "r")) == NULL) {
      fprintf(stderr, "Could not open file '%s'.\n", argv[1]);
      exit(1);
    }
  }
  version = UnScribeInit(" 12", &ussp);
  while (fgets(buf,sizeof(buf)-1,fptr)) {
    err = UnScribe(version, &ussp, buf, strlen(buf), stdout);
    if (err < 0) fprintf(stderr, "Error: UnScribe == %d.\n", err);
  }

}
#endif /* TESTINGONLYTESTING */
