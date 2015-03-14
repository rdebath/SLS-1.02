/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	propscmdsend.c
 * ---------------------------------------------------------------------*/
#ifndef line
#ifdef sccs
static char     sccsid[] = "@(#) propssend.c 1.3 91/09/14 Sun Micro";
#endif
#endif

#include "propscmd.h"

/* ----------------------------------------------------------------------
 *	ShowPropsWindow
 * ---------------------------------------------------------------------*/
int
ShowPropsWindow(nscreen)
	int	nscreen;
{
	propsCmdAttr[PW_SCREEN_NO].value.ival 	= nscreen;
	return(SendCmd(&propsCommand));
}
