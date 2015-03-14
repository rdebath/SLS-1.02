/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	helpcmdrecv.c
 * ---------------------------------------------------------------------*/
#ifndef line
#ifdef sccs
static	char	sccsid[] = "@(#) helprecv.c 1.3 91/09/14 Sun Micro";
#endif
#endif

#include "helpcmd.h"

extern	int	ShowHelpWindow();
	void	ReceiveHelpCmd();

/* ----------------------------------------------------------------------
 *	RegisterHelpWindow
 * ---------------------------------------------------------------------*/
void
RegisterHelpCmd()
{
	helpCommand.callback = ReceiveHelpCmd;
	RegisterCmd(&helpCommand);
}

/* ----------------------------------------------------------------------
 *	ReceiveHelpCmd
 * ---------------------------------------------------------------------*/
static void
ReceiveHelpCmd(cmd)
	Command		*cmd;
{
	CmdAttr		*attr;

	attr = cmd->attrList;
	ShowHelpWindow(	attr[HW_SCREEN_NO].value.ival,
			attr[HW_MOUSE_X].value.ival,
			attr[HW_MOUSE_Y].value.ival,
			attr[HW_HELPKEY].value.sval
		      );
}
