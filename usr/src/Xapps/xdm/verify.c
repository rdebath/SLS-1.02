/*
 * xdm - display manager daemon
 *
 * $XConsortium: verify.c,v 1.9 89/12/12 13:58:18 rws Exp $
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
 * verify.c
 *
 * typical unix verification routine.
 */

# include	"dm.h"

# include	<pwd.h>
# ifdef NGROUPS
# include	<grp.h>
# endif

struct passwd joeblow = {
	"Nobody", "***************"
};

Verify (d, greet, verify)
struct display		*d;
struct greet_info	*greet;
struct verify_info	*verify;
{
	struct passwd	*p;
	char		*crypt ();
	char		**userEnv (), **systemEnv (), **parseArgs ();
	char		*shell, *home;
	char		**argv;

	p = getpwnam (greet->name);
	if (!p || strlen (greet->name) == 0)
		p = &joeblow;
	Debug ("Verify %s %s\n", greet->name, greet->password);
	if (strcmp (crypt (greet->password, p->pw_passwd), p->pw_passwd)) {
		Debug ("verify failed\n");
		bzero(greet->password, strlen(greet->password));
		return 0;
	}
	Debug ("verify succeeded\n");
	bzero(greet->password, strlen(greet->password));
	verify->uid = p->pw_uid;
#ifdef NGROUPS
	getGroups (greet->name, verify, p->pw_gid);
#else
	verify->gid = p->pw_gid;
#endif
	home = p->pw_dir;
	shell = p->pw_shell;
	argv = 0;
	if (d->session)
		argv = parseArgs (argv, d->session);
	if (greet->string)
		argv = parseArgs (argv, greet->string);
	if (!argv)
		argv = parseArgs (argv, "xsession");
	verify->argv = argv;
	verify->userEnviron = userEnv (d, greet->name, home, shell);
	Debug ("user environment:\n");
	printEnv (verify->userEnviron);
	verify->systemEnviron = systemEnv (d, greet->name, home);
	Debug ("system environment:\n");
	printEnv (verify->systemEnviron);
	Debug ("end of environments\n");
	return 1;
}

extern char **setEnv ();

char **
userEnv (d, user, home, shell)
struct display	*d;
char	*user, *home, *shell;
{
	char	**env = 0;
	
	env = setEnv (env, "DISPLAY", d->name);
	env = setEnv (env, "HOME", home);
	env = setEnv (env, "USER", user);
	env = setEnv (env, "PATH", d->userPath);
	env = setEnv (env, "SHELL", shell);
	return env;
}

char **
systemEnv (d, user, home)
struct display	*d;
char	*user, *home;
{
	char	**env = 0;
	
	env = setEnv (env, "DISPLAY", d->name);
	env = setEnv (env, "HOME", home);
	env = setEnv (env, "USER", user);
	env = setEnv (env, "PATH", d->systemPath);
	env = setEnv (env, "SHELL", d->systemShell);
	return env;
}

#ifdef NGROUPS
groupMember (name, members)
char	*name;
char	**members;
{
	while (*members) {
		if (!strcmp (name, *members))
			return 1;
		++members;
	}
	return 0;
}

getGroups (name, verify, gid)
char			*name;
struct verify_info	*verify;
int			gid;
{
	int		ngroups;
	struct group	*g;
	int		i;

	ngroups = 0;
	verify->groups[ngroups++] = gid;
	setgrent ();
	while (g = getgrent()) {
		/*
		 * make the list unique
		 */
		for (i = 0; i < ngroups; i++)
			if (verify->groups[i] == g->gr_gid)
				break;
		if (i != ngroups)
			continue;
		if (groupMember (name, g->gr_mem)) {
			if (ngroups >= NGROUPS)
				LogError ("%s belongs to more than %d groups, %s ignored\n",
					name, NGROUPS, g->gr_name);
			else
				verify->groups[ngroups++] = g->gr_gid;
		}
	}
	verify->ngroups = ngroups;
	endgrent ();
}
#endif
