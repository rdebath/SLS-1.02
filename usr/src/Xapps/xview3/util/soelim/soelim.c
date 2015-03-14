/*
 * soelim - expand .so filename lines in *roff sources
 *
 * $XConsortium: soelim.c,v 1.2 88/10/09 11:21:58 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * This version is not is not derived from AT&T or Berkeley sources and is
 * provided for sites that do not have an soelim program.
 *
 * Author:  Jim Fulton, MIT X Consortium
 *
 */

#include <stdio.h>
#include <ctype.h>
#ifdef SYSV
#include <string.h>
#else
#ifdef macII
#include <string.h>
#else
#include <strings.h>
#endif
#endif

#define MAXLINELEN 1024			/* length of a single line */

char *ProgramName;
int errors;

main (argc, argv)
    int argc;
    char **argv;
{
    int i;
    FILE *fp;

    ProgramName = argv[0];
    errors = 0;

    for (i = 1; i < argc; i++) {
	doit (strcmp(argv[i], "-") == 0 ? NULL : argv[i]);
    }
    exit (errors);
}


#define skipspace(var) \
  for (; *var && isascii(*var) && isspace(*var); var++) ;

#define skipword(var) \
  for (; *var && isascii(*var) && !isspace(*var); var++) ;

doit (inputfilename)
    char *inputfilename;
{
    FILE *fp;				/* stream for inputfilename */
    char buf[MAXLINELEN];		/* buffer for gets of fp */
    char *line;				/* for walking around in buf */
    int len;				/* length of buf */
    char *sofilename;			/* .so arg */

    if (inputfilename) {
	fp = fopen (inputfilename, "r");
	if (!fp) {
	    fprintf (stderr, "%s:  can't open file \"%s\" for reading\n",
		     ProgramName, inputfilename);
	    errors++;
	    return;
	}
    } else {
	fp = stdin;
    }

    /*
     * loop through looking for lines that have .so at the start
     */
    while (1) {
	buf[0] = '\0';
	if (fgets (buf, sizeof buf, fp) == NULL) break;
	len = strlen (buf);
	line = buf;
	if (strncmp (line, ".so", 3) == 0 &&
	    line[3] && isascii(line[3]) && isspace(line[3])) {
	    line += 4;
	    skipspace (line);
	    sofilename = line;
	    skipword (line);
	    *line = '\0';
	    doit (sofilename);
	    continue;
	}
	fputs (buf, stdout);
	/* see if the line was too long */
	if (buf[len - 1] != '\n') {
	    int c;

	    while ((c = getc (fp)) != '\0' && c != '\n') putc (c, stdout);
	    putc ('\n', stdout);
	}
    }
    if (inputfilename) {
	fclose (fp);
    }
}
