/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	propscmdrecv.c
 * ---------------------------------------------------------------------*/
#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#) propsrecv.c 1.3 91/09/14 Sun Micro";
#endif
#endif

#include "propscmd.h"

extern	int	ShowWindowProps();
	void	ReceivePropsCmd();

/* ----------------------------------------------------------------------
 *	RegisterPropsWindow
 * ---------------------------------------------------------------------*/
void
RegisterPropsCmd()
{
	propsCommand.callback = ReceivePropsCmd;
	RegisterCmd(&propsCommand);
}

/* ----------------------------------------------------------------------
 *	ReceivePropsCmd
 * ---------------------------------------------------------------------*/
static void
ReceivePropsCmd(cmd)
	Command		*cmd;
{
	CmdAttr		*attr;

	attr = cmd->attrList;
	ShowWindowProps(attr[PW_SCREEN_NO].value.ival);
}
