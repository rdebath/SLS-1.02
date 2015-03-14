/*
 * Copyright 1989, 1990, 1991, 1992, John F. Haugh II
 * All rights reserved.
 *
 * Permission is granted to copy and create derivative works for any
 * non-commercial purpose, provided this copyright notice is preserved
 * in all copies of source code, or included in human readable form
 * and conspicuously displayed on all copies of object code or
 * distribution media.
 */

#include <stdio.h>
#include <ctype.h>
#ifndef	BSD
#include <string.h>
#include <memory.h>
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif

#ifndef	lint
static	char	sccsid[] = "@(#)login.c	3.2	20:37:17	3/7/92";
#endif

void	setenv ();

/*
 * login - prompt the user for their login name
 *
 * login() displays the standard login prompt.  If the option
 * ISSUE_FILE_ENAB is set, the file /etc/issue is displayed
 * before the prompt.
 */

void
login (name, prompt)
char	*name;
char	*prompt;
{
	char	buf[BUFSIZ];
	char	*envp[32];
	int	envc;
	char	*cp;
	int	i;
	FILE	*fp;

	/*
	 * See if the user has configured the /etc/issue file to
	 * be displayed and display it before the prompt.
	 */

	if (prompt) {
		if (getdef_bool ("ISSUE_FILE_ENAB")) {
			if (fp = fopen ("/etc/issue", "r")) {
				while ((i = getc (fp)) != EOF)
					putc (i, stdout);

				fflush (stdout);
				fclose (fp);
			}
		}
		fputs (prompt, stdout);
	}

	/* 
	 * Read the user's response.  The trailing newline will be
	 * removed.
	 */

#ifndef	BSD
	(void) memset (buf, '\0', sizeof buf);
#else
	bzero (buf, sizeof buf);
#endif
	if (fgets (buf, BUFSIZ, stdin) != buf)
		exit (1);

	buf[strlen (buf) - 1] = '\0';	/* remove \n [ must be there ] */

	for (cp = buf;*cp == ' ' || *cp == '\t';cp++)
		;

	for (i = 0;i < BUFSIZ - 1 && isgraph (*cp);name[i++] = *cp++)
		;

	if (*cp)
		cp++;

	name[i] = '\0';

	if (*cp != '\0') {		/* process new variables */
		for (envc = 0;envc < 32;envc++) {
			envp[envc] = strtok (envc == 0 ? cp:(char *) 0, " \t,");

			if (envp[envc] == (char *) 0)
				break;
		}
		setenv (envc, envp);
	}
}
