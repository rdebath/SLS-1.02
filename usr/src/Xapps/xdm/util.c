/*
 * xdm - display manager daemon
 *
 * $XConsortium: util.c,v 1.4 89/03/28 16:51:16 keith Exp $
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
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * util.c
 *
 * various utility routines
 */

# include   <sys/signal.h>

extern void	free ();

printEnv (e)
char	**e;
{
	while (*e)
		Debug ("%s\n", *e++);
}

static char *
makeEnv (name, value)
char	*name;
char	*value;
{
	char	*result, *malloc (), *sprintf ();

	result = malloc ((unsigned) (strlen (name) + strlen (value) + 2));
	if (!result) {
		LogOutOfMem ("makeEnv");
		return 0;
	}
	sprintf (result, "%s=%s", name, value);
	return result;
}

char *
getEnv (e, name)
	char	**e;
	char	*name;
{
	int	l = strlen (name);

	while (*e) {
		if (strlen (*e) > l && !strncmp (*e, name, l) &&
			(*e)[l] == '=')
			return (*e) + l + 1;
		++e;
	}
	return 0;
}

char **
setEnv (e, name, value)
	char	**e;
	char	*name;
	char	*value;
{
	char	**new, **old;
	char	*newe;
	int	envsize;
	int	l;
	char	*malloc (), *realloc ();

	l = strlen (name);
	newe = makeEnv (name, value);
	if (!newe) {
		LogOutOfMem ("setEnv");
		return e;
	}
	if (e) {
		for (old = e; *old; old++)
			if (strlen (*old) > l && !strncmp (*old, name, l) && (*old)[l] == '=')
				break;
		if (*old) {
			free (*old);
			*old = newe;
			return e;
		}
		envsize = old - e;
		new = (char **) realloc ((char *) e,
				(unsigned) ((envsize + 2) * sizeof (char *)));
	} else {
		envsize = 0;
		new = (char **) malloc (2 * sizeof (char *));
	}
	if (!new) {
		LogOutOfMem ("setEnv");
		free (newe);
		return e;
	}
	new[envsize] = newe;
	new[envsize+1] = 0;
	return new;
}


# define isblank(c)	((c) == ' ' || c == '\t')

char **
parseArgs (argv, string)
char	**argv;
char	*string;
{
	char	*word;
	char	*save;
	int	i;
	char	*malloc (), *realloc (), *strcpy (), *strncpy ();

	i = 0;
	while (argv && argv[i])
		++i;
	if (!argv) {
		argv = (char **) malloc (sizeof (char *));
		if (!argv) {
			LogOutOfMem ("parseArgs");
			return 0;
		}
	}
	word = string;
	for (;;) {
		if (!*string || isblank (*string)) {
			if (word != string) {
				argv = (char **) realloc ((char *) argv,
					(unsigned) ((i + 2) * sizeof (char *)));
				save = malloc ((unsigned) (string - word + 1));
				if (!argv || !save) {
					LogOutOfMem ("parseArgs");
					if (argv)
						free ((char *) argv);
					if (save)
						free (save);
					return 0;
				}
				argv[i] = strncpy (save, word, string-word);
				argv[i][string-word] = '\0';
				i++;
			}
			if (!*string)
				break;
			word = string + 1;
		}
		++string;
	}
	argv[i] = 0;
	return argv;
}

CleanUpChild ()
{
#ifdef SYSV
	setpgrp ();
#else
	setpgrp (0, getpid ());
	sigsetmask (0);
#endif
#ifdef SIGCHLD
	(void) signal (SIGCHLD, SIG_DFL);
#endif
	(void) signal (SIGTERM, SIG_DFL);
	(void) signal (SIGPIPE, SIG_DFL);
	(void) signal (SIGALRM, SIG_DFL);
	(void) signal (SIGHUP, SIG_DFL);
	CloseOnFork ();
}

