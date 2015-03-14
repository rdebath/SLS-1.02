/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 *
 *	Written for Sun Microsystems by Crucible, Santa Cruz, CA.
 */
#ifndef lint
#ifdef sccs
static	char	sccsid[] = "@(#) mem.c 26.1 90/08/14 Crucible";
#endif
#endif

/*
 * Safe memory allocation/free routines - front-ends the C library functions
 *
 */

#include <malloc.h>
#include <memory.h>
#include <stdio.h>
#include <sys/types.h>

void *
MemAlloc(sz)
unsigned int sz;
{
#ifdef __STDC__
	void *p;
#else
	char *p;
#endif

	if ((p = malloc(sz)) == NULL)
		ErrorGeneral("Memory allocation failure.");
	memset((char *)p, 0, (int)sz);
	return p;
}

void *
MemCalloc(num,sz)
unsigned int num;
unsigned int sz;
{
#ifdef __STDC__
	void *p;
#else
	char *p;
	char *calloc();
#endif

	if ((p = calloc(num,sz)) == NULL)
		ErrorGeneral("Memory array allocation failure.");
	memset((char *)p, 0, (int)sz*(int)num);
	return p;
}

void
MemFree(p)
void *p;
{
	if (p != NULL)
		free(p);
}

ErrorGeneral(txt)
char	*txt;
{
	(void)fprintf(stderr,"olwmslave: Fatal Error: %s\n",txt);
	exit(-1);
}
