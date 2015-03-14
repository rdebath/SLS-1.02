/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 *
 *	Written for Sun Microsystems by Crucible, Santa Cruz, CA.
 */

/* @(#) mem.h 26.1 90/08/14 Crucible */

extern void *MemAlloc();	/* malloc frontend */
extern void *MemCalloc();	/* calloc frontend */
extern void MemFree();		/* free frontend */

#define MemNew(t) ((t *)MemAlloc((unsigned int)sizeof(t)))
#define MemNewString(s) (strcpy(MemAlloc(strlen(s)+1),s))
