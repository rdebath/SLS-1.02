
#include "config.h"
#include <stdio.h>

#ifndef MAXINCLUDEFILES
#define MAXINCLUDEFILES	1	/* maximum number of new files in stack */
#endif

/*
 * xgets () acts just like gets () except that if a line matches
 * "&Include_File&<something>" xgets () will start reading from the
 * file <something>.
 *
 *  Andrew Vignaux -- andrew@vuwcomp  Fri May  8 16:40:23 NZST 1987
 * modified
 *  Mark Davies -- mark@vuwcomp  Mon May 11 22:38:10 NZST 1987
 */

extern int incfileflag;		/* whether xgets() acts exactly like gets() */

char *
xgets (str)
char str [];
{
#if MAXINCLUDEFILES == 0
    return gets (str);
#else
    static char * Include_File = DEFINCSTR;
    static int    Include_Len = 0, strlen ();
    static FILE * F [MAXINCLUDEFILES+1], ** current_F = F;
    char * s = str;
    int c;

    /* read the environment variable if we havent already */
    if (Include_Len == 0) {
	char * env_variable, * getenv ();

	if ((env_variable = getenv (INCSTRVAR)) != NULL)
	    Include_File = env_variable;
	Include_Len = strlen (Include_File);

	/* initialise the file stack */
	*current_F = stdin;
    }

    while (1) {
        if ((c = getc (*current_F)) != EOF && c != '\n') {
	    *s++ = c;
	    continue;
	}
	*s = '\0';		/* end of line */
	if (c == EOF)
	    if (current_F == F) { /* if end of standard input */
		if (s == str) return (NULL);
	    } else {
	        (void) fclose (*(current_F--));
	      	if (s == str) continue;
	    }

	if (incfileflag != 0 && strncmp (str, Include_File, Include_Len) == 0) {
	    char * file_name = str + Include_Len;
	    if (current_F - F < MAXINCLUDEFILES && strlen (file_name) > 0) {
		FILE * f;
		if (f = fopen (file_name, "r"))
		    *(++current_F) = f;
	    }
	    s = str;
	    continue;
	}
	break;
    }
    
    return (str);
#endif
}
