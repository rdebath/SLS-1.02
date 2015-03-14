/*
 * Copyright (c) 1990 Rene' Seindal
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
#define MAXNAMLEN  FILENAME_MAX
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <stdio.h>
#ifdef SHADOW_PWD
#	include <shadow.h>
#endif
#include <pwd.h>

extern char *malloc();
extern char *realloc();
extern char *getenv();

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];


/* glob_match matches pattern against string according to the normal
 * rules for shell globbing.  It returns SUCCES if string matches
 * pattern, FAIL if it doesn't, and ERROR if pattern is illformed.
 *
 * To be more specific, a pattern can be:
 *	?	which matches any single character
 *	*	which matches zero or more character
 *	[...]	which matches any single character in ...
 *	[^...]	which matches any single character not in ...
 *	\x	where x is any character, matches that character literally
 *	x	where x is any character except ? * [ and \, matches x
 * 
 * Character within [...] can include single characters and ranges of
 * the form x-y, which matches any characters in the ranges x - y
 * inclusive.  It is considered an error is y < x.  An ] can be included
 * as the very first character in ..., and a - as the first (after a
 * possibly [) or last character in ...
 */

#define SUCCES	1
#define FAIL	0
#define ERROR	0

#define NEXTP {if ( (p = *++pattern) == '\0') return ERROR;}

int glob_match( pattern, string )
    register char *pattern;
    register char *string;
{
    register char p;
    char seenstar = '\0';

    for ( ; p = *pattern; pattern++ ) {

	if ( p == '\\' ) {
	    NEXTP;		/* matches next char literally (but not \0) */
	    if ( p != *string++ )
		return FAIL;	/* string too short */
	    continue;
	} else if ( p == '?' ) {
	    if ( *string++ )	/* matches any character */
		continue;
	    else
		return FAIL;	/* string too short */
	} else if ( p == '*' ) {
	    seenstar = '\1';
	    continue;
	} else {

	    if ( seenstar ) {
		int tmp;
		while ( *string ) {
		    tmp = glob_match( pattern, string );
		    if ( tmp != FAIL )
			return tmp;
		    string++;
		}
		return FAIL;
		/* NOTREACHED */
	    }

	    if ( p == '\0' )
		return FAIL;

	    if ( p == '[' ) {
		register char s = *string++;
		char reverse = '\0';
		char first, last;
		char gotcha = '\0';

		NEXTP;
		if ( p == '^' ) {
		    reverse = '\1';
		    NEXTP;
		}
		if ( p == ']' ) { /* special case */
		    gotcha = (s==p);
		    NEXTP;
		}

		while (  p != ']' && !gotcha ) {
		    first = p;
		    NEXTP;
		    if ( p == '-' && pattern[1] != ']' ) {
			NEXTP;
			last = p;
			NEXTP;
		    } else
			last = first;
		    if ( first > last )
			return ERROR;
		    gotcha = (first <= s && s <= last );
		}
		while ( p != ']' )
		    NEXTP;

		if ( reverse ? gotcha : !gotcha )
		    return FAIL;
	    } else if ( p != *string )
		return FAIL;
	    else
		string++;
	}
    }
    if ( seenstar )
	return SUCCES;

    if ( *string )
	return FAIL;
    return SUCCES;
}


void do_glob();			/* do simple globbing, on segment at a time */
void matchdir();		/* non-simple globbing */
void add_name();		/* add a name to namelist */
int split_pat();		/* split pattern into segments */

static char *main_path;		/* ptr to scratchpad */
static int offset;		/* no of leading char in main_path to ignore */
static char **namelist;		/* name list buildup */
static int nnames;		/* no of names found */
static int left;		/* no of slots allocated but not used yet */

#define MAXSEG	50		/* max segments in pattern */
#define CHUNK	20		/* no of slots to allocate at a time */

int glob_path( pattern, names )
    char *pattern;
    char *(*names[]);
{
    char mpath[ MAXPATHLEN + MAXNAMLEN + 1 ];
    char *gpat[MAXSEG];
    register char *pat;

    if ( pattern == 0)
	return -1;

    if ( (pat = malloc( strlen(pattern) + 1 )) == NULL )
	return -1;

    strcpy( pat, pattern );

    if ( split_pat( pat, gpat ) < 0 ) {
	free( pat );
	return -1;
    }
    
    main_path = mpath;		/* initalisation of static storage */
    namelist = 0;
    nnames = left = 0;

    if ( *gpat && **gpat == '/' ) {
	main_path[0] = '/';
	main_path[1] = '\0';
	offset = 0;
	do_glob( main_path, gpat+1 );
    } else {
	offset = 2;
	strcpy( main_path, "." );
	do_glob( main_path + 1, gpat );
    }

    free( pat );

    if (namelist == 0)
	*names = (char **)malloc( sizeof(char *) );
    else
	*names = (char **)realloc( namelist, (nnames+1)*sizeof(char *) );

    if ( *names == 0 )
	return -1;
    (*names)[nnames] = 0;
    return nnames;
}

static int split_pat( pattern, table )
    register char *pattern;
    register char **table;
{
    register char *pp = pattern;
    int size = MAXSEG;

    if ( *pattern == '/' ) {
	*table++ = "/";
	--size;
    }
    do {
	while ( *pp == '/' ) *pp++ = '\0';
	if ( *pp == '\0' )
	    break;
	if ( --size < 0 )
	    return -1;
	*table++ = pp;
	while ( *pp && *pp != '/' ) pp++;
    } while ( *pp );
    *table = 0;
    return 0;
}


#define ISGLOB(x) ((x)=='*' || (x)=='?' || (x)=='[')

static int no_glob( pat )
    register char *pat;
{
    while ( *pat && !ISGLOB(*pat) )
	pat++;
    return (*pat == '\0');
}
	
static void do_glob( path_end, gpat )
    register char *path_end;	/* ptr to the end of main_path */
    register char **gpat;	/* the rest of the pattern segments */
{
    char *saved_end = path_end;	/* saved to be resored */
    register char *pat;		/* current pattern segment */
    struct stat st;		/* to check if file exists */

#ifdef DEBUG
    printf( "do_glob: path = '%s', pat = '%s'\n", main_path, *gpat );
#endif DEBUG

    for ( ; (pat = *gpat) != 0 && no_glob(pat); gpat++ ) {
#ifdef DEBUG
	printf( "no_glob: path = '%s', pat = '%s'\n", main_path, pat );
#endif DEBUG
	*path_end = '/';
	strcpy( path_end+1, pat );
	path_end += strlen(pat) + 1;

	if ( stat( main_path, &st ) != 0 ) {
	    *saved_end = '\0';
	    return;
	}
    }
    if ( pat )
	matchdir( path_end, gpat );
    else
	add_name();

    *saved_end = '\0';
    return;
}

static void matchdir( path_end, gpat )
    register char *path_end;	/* ptr to end of main_path */
    register char **gpat;	/* the rest of the pattern segments */
{
    register char *x;		/* scratch */
    DIR *dirp;			/* for directory reading */
    struct direct *dp;		/* directory entry */
    struct stat st;		/* to determine files type */

#ifdef DEBUG
    printf( "matchdir: path = '%s', pat = '%s'\n", main_path, *gpat );
#endif DEBUG
    if ( (dirp = opendir( main_path )) == NULL )
	return;

    *path_end = '/';
    
    while ( (dp = readdir(dirp)) != NULL ) {
	x = dp->d_name;
	if (*x == '.' && (*++x == '\0' || (*x == '.' && *++x == '\0')))
	    continue;
	if (*dp->d_name == '.' && **gpat != '.')
	    continue;

	strcpy( path_end+1, dp->d_name );

	if ( glob_match(*gpat, dp->d_name) ) { /* this is a match */
	    if ( *(gpat+1) == 0 ) { /* and it is the last */
		add_name();	/* so eat it */
		continue;
	    }
	    if ( stat( main_path, &st ) == 0 /* else not the last */
		&& (st.st_mode & S_IFMT) == S_IFDIR )
		do_glob( path_end + strlen(dp->d_name) + 1, gpat+1 );
	} 
    }
    *path_end = '\0';
}

static void add_name()
{
    register char *np;
#ifdef DEBUG
    printf( "Globbed: %s\n", main_path+offset);
#endif DEBUG

    if ( --left <= 0 ) {
	if (namelist == 0)
	    namelist = (char **)malloc( CHUNK * sizeof(char *) );
	else
	    namelist = (char **)realloc( namelist, (nnames+CHUNK)*sizeof(char *) );

	if ( namelist == NULL )
	    return;
	
	left = CHUNK;
    }
    np = malloc( strlen(main_path) - offset + 1 );
    if ( np == 0 )
	return;
    strcpy( np, main_path+offset );
    namelist[nnames++] = np;
    return;
}

/* the following is not general purpose code. */
char *globerr;
char globerr_buf[1024];
char *home = 0;

extern int qsort();

static int name_cmp(s1, s2)
    char **s1;
    char **s2;
{
    return strcmp(*s1, *s2);
}


char *expand_tilde(pat)
    char *pat;
{
    static char buf[BUFSIZ];

    struct passwd *pw;
    register char *tmp;
    register char *bp;

    if (*pat != '~')
	return 0;

    pat++;
    if (*pat && *pat != '/') {
	bp = buf;
	for (tmp = pat; *tmp && *tmp != '/'; )
	    *bp++ = *tmp++;
	*bp = '\0';

	pw = getpwnam(buf);
	if (pw == 0) {
	    sprintf(globerr_buf, "%s: Unknown user.", pat);
	    globerr = globerr_buf;
	    return 0;
	}
	pat = tmp ? tmp : "";
	tmp = pw->pw_dir;
    } else {
	if (*pat)
	    pat++;
	tmp = home;
    }
    for (bp = buf; *tmp; )
	*bp++ = *tmp++;
    *bp++ = '/';

    while (*pat)
	*bp++ = *pat++;
    *bp = '\0';

    return buf;
}

char **glob(pat)
    char *pat;
{
    char **names;
    int nnames;
    char pattern[1024];

    if (*pat == '~') {
	pat = expand_tilde(pat);
	if (pat == 0)
	    return 0;
    }

    nnames = glob_path(pat, &names);

    switch (nnames) {
    case -1:
	globerr = sys_errlist[errno];
	return 0;
    case 0:
	globerr = "No match.";
	return 0;
    default:
	qsort( names, nnames, sizeof(char *), name_cmp );
	return names;
    }
}

#ifdef TEST
main(argc, argv)
    int argc;
    char **argv;
{
    char **names;

    globerr = 0;
    home = getenv("HOME");

    names = glob(argv[1]);
    if (names == 0) {
	fprintf(stderr, "glob error: %s\n", globerr);
	exit(1);
    }
    while ( *names )
	printf("%s\n", *names++);
    exit(0);
}
#endif 

/* following added by Rene' Seindal Wed Jan 24 15:56:13 1990 */

char **copyblk(v)
    char **v;
{
    int n;
    register char **vv;
    char **svv;
    char *copy();

    n = 0; 
    for (vv = v; *vv != 0; vv++)
	n++;

    vv = (char **)malloc((n+1) * sizeof(char *));
    if (vv == 0)
	fatal("Out of memory.");

    svv = vv;
    while (--n >= 0)
	*vv++ = copy(*v++);
    *vv = 0;
    return svv;
}

blkfree(v)
    char **v;
{
    char **sv = v;

    while (*v)
	free(*v++);
    free(sv);
}
