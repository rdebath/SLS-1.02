/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : envarg.c
 *  Author    : B.Davidson
 *  Created   : 10-13-91
 *  Updated   : 01-10-92
 *  Notes     : Adds default options from environment to command line
 *  Copyright : (c) Copyright 1991-92 by Bill Davidsen
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "tin.h"


int count_args (s)
	char *s;
{
	int count = 0;
	int ch;

	do {
		/* 
		 * count and skip args 
		 */
		++count;
		while ((ch = *s) != '\0' && ch != ' ') 
			++s;
		while ((ch = *s) != '\0' && ch == ' ') 
			++s;
	} while (ch);

	return count;
}


void envargs (Pargc, Pargv, envstr)
	int *Pargc;
	char ***Pargv;
	char *envstr;
{
	char *getenv();
	char *envptr;			/* value returned by getenv */
	char *bufptr;			/* copy of env info */
	int argc = 0;			/* internal arg count */
	int ch;				/* spare temp value */
	char **argv;			/* internal arg vector */
	char **argvect;			/* copy of vector address */

	/*
	 * see if anything in the environment 
	 */
	envptr = getenv (envstr);
	if (envptr == (char *) 0 || *envptr == 0) {
		return;
	}
	
	/* 
	 * count the args so we can allocate room for them 
	 */
	argc = count_args (envptr);
	bufptr = (char *) my_malloc (1+strlen (envptr));
	strcpy (bufptr, envptr);

	/* 
	 * allocate a vector large enough for all args 
	 */
	argv = (char **) my_malloc ((argc+*Pargc+1)*sizeof(char *));
	argvect = argv;

	/* 
	 * copy the program name first, that's always true 
	 */
	*(argv++) = *((*Pargv)++);

	/* 
	 * copy the environment args first, may be changed 
	 */
	do {
		*(argv++) = bufptr;
		/* 
		 * skip the arg and any trailing blanks 
		 */
		while ((ch = *bufptr) != '\0' && ch != ' ') 
			++bufptr;
		if (ch == ' ') 
			*(bufptr++) = '\0';
		while ((ch = *bufptr) != '\0' && ch == ' ') 
			++bufptr;
	} while (ch);

	/* 
	 * now save old argc and copy in the old args 
	 */
	argc += *Pargc;
	while (--(*Pargc))
		*(argv++) = *((*Pargv)++);

	/* 
	 * finally, add a NULL after the last arg, like UNIX 
	 */
	*argv = (char *) 0;

	/* 
	 * save the values and return 
	 */
	*Pargv = argvect;
	*Pargc = argc;
}


#ifdef TEST

void main (argc, argv)
	int argc;               /* number of tokens in command line */
	char **argv;            /* command line tokens */
{
	int ch;

	envargs (&argc, &argv, "OPTS"); 

	while ((ch = getopt (argc, argv, "abc")) != EOF) {
		switch (ch) {
			case 'a':
				puts ("Option 'a' specified");
				break;
			case 'b':
				puts ("Option 'b' specified");
				break;
			case 'c':
				puts ("Option 'c' specified");
				break;
			default:
				puts ("Option %c unknown");
				break;
		}
	}
}

#endif
