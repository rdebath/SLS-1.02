/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	helpsend.c
 * ---------------------------------------------------------------------*/
static char	sccsid[] = "@(#) helpsend.c 1.3 91/09/14 Sun Micro";

#include "helpcmd.h"

/* ----------------------------------------------------------------------
 *	ShowHelpWindow
 * ---------------------------------------------------------------------*/
int
ShowHelpWindow(nscreen,mousex,mousey,helpkey)
	int	nscreen;
	int	mousex,mousey;
	char	*helpkey;
{
	helpCmdAttr[HW_SCREEN_NO].value.ival 	= nscreen;
	helpCmdAttr[HW_MOUSE_X].value.ival 	= mousex;
	helpCmdAttr[HW_MOUSE_Y].value.ival 	= mousey;
	helpCmdAttr[HW_HELPKEY].value.sval 	= helpkey;
	return (SendCmd(&helpCommand));
}
