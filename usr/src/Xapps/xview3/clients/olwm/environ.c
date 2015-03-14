/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)environ.c	1.6	91/09/14 SMI"

#include <stdio.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
extern char *strrchr();
extern char *strchr();
#endif
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "mem.h"

extern	char **environ;

/* -----------------------------------------------------------------------
 *	Local Data Structures
 * -----------------------------------------------------------------------*/

/*
 * 	Env - environment object
 */
typedef struct _env {
	char	**environ;	/* array of environment strings */
	int	length;		/* length of environ array */
	int	used;		/* number of entries actually used */
} Env;

/* -----------------------------------------------------------------------
 *	Local Functions
 * -----------------------------------------------------------------------*/

/*
 *	createEnv - Creates a new environment array that is the length of
 *		    of the current environment plus the number of additions.
 */
static void
createEnv(env,nadditions)
	Env	*env;
	int	nadditions;
{
	int	i = 0;

	/* find the number of items in the current environ */
	while (environ[i] != (char *)NULL) {
		i++;
	}

	/* create space for the environ strings */
	env->used = i;
	env->length = env->used + nadditions + 1;
	env->environ = MemAlloc(env->length*sizeof(char *));

	/* copy the current environ into the new one */
	for (i=0; i<env->used; i++) {
		env->environ[i] = MemNewString(environ[i]);
	}
	env->environ[i] = (char *)NULL;
}

/*
 *	putEnv - Puts the name,value pair into the specified environment
 *	         replacing any existing values.
 *		 Assumes there is space for the new setting.
 */
static void
putEnv(env,name,value)
	Env	*env;
	char	*name;
	char	*value;
{
	int	nameLen = strlen(name);
	char	*envVar;
	int	count;

	/* create new env string with space for '=' and null */
	envVar = (char *)MemAlloc(nameLen + strlen(value) +2);

	(void)sprintf(envVar,"%s=%s",name,value);

	/* search through, checking for variable in question */
	for (count=0 ; count<env->used; count++) {
		if (!strncmp(env->environ[count],name,nameLen))
			break;
	}
	

	if (count == env->used)		/* finished loop without match */
		env->used++;		/* added 1 more var to the env */
	else
		MemFree(env->environ[count]);	/* don't need */

	env->environ[count] = envVar;

	/* make sure the last entry in the vector is NULL */
	env->environ[env->used] = (char *)NULL;

}

/*
 *	putDisplayEnv - sets the DISPLAY env to the appropriate screen
 */
static void
putDisplayEnv(env,dpy,screen)
	Env	*env;
	Display	*dpy;
	int	screen;
{
	char	*display = DisplayString(dpy);
	char	*colon,*dot;
	char	value[128];
	int	len;

	if ((colon = strrchr(display,':')) == (char *)NULL) {
		return;
	}
	if ((dot = strchr(colon,'.')) != (char *)NULL) {
		len = dot - display;
	} else {
		len = colon - display;
	}

	(void)sprintf(value,"%.*s.%d",len,display,screen);

	putEnv(env,"DISPLAY",value);
}

#ifndef NOSVENV
/*
 *	putSunViewEnv - sets the various SV environment variables
 */
static void
putSunViewEnv(env,dpy,screen)
	Env	*env;
	Display *dpy;
	int	screen;
{
	static char	*svEnv[] = { "WINDOW_PARENT", 
				     "WMGR_ENV_PLACEHOLDER", 
				     "WINDOW_TTYPARMS" };
	int		i, svEnvLen = sizeof(svEnv)/sizeof(char *);
	char		*result,*curpos;
	unsigned long	nitems,remainder;
	extern void	*GetWindowProperty();
	extern Atom	AtomSunViewEnv;

	result = (char *)GetWindowProperty(dpy,RootWindow(dpy,screen),
			AtomSunViewEnv,0L,100000L,
			XA_STRING,8,&nitems,&remainder);

	if (result == NULL)
		return;

	curpos = result;
	for (i=0; i<svEnvLen; i++) {
		putEnv(env,svEnv[i],curpos);
		curpos += strlen(curpos) + 1;
	}

}
#endif /* NOSVENV */

/* -----------------------------------------------------------------------
 *	Global Functions
 * -----------------------------------------------------------------------*/

/*
 *	MakeEnviron - returns a new environment array that contains the
 *		      current environ plus a modified DISPLAY and
 *		      SunView environment variables.
 */
char **
MakeEnviron(dpy,screen)
	Display	*dpy;
	int	screen;
{
	Env	newEnv;
	int	nadditions;

	nadditions = 1;		/* for DISPLAY */

#ifndef NOSVENV
	nadditions += 3;	/* for SV environment */
#endif /* NOSVENV */

	createEnv(&newEnv,nadditions);

	putDisplayEnv(&newEnv,dpy,screen);

#ifndef NOSVENV
	putSunViewEnv(&newEnv,dpy,screen);
#endif /* NOSVENV */

	return newEnv.environ;
}
