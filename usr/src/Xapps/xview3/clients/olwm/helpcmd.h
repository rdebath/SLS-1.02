/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

/* ----------------------------------------------------------------------
 *	helpcmd.h
 * ---------------------------------------------------------------------*/

#ident	"@(#)helpcmd.h	26.6	91/09/14 SMI"

#include "cmdstream.h"

#define		HW_SCREEN_NO		0
#define		HW_MOUSE_X		1
#define		HW_MOUSE_Y		2
#define		HW_HELPKEY		3
#define		HW_ATTR_COUNT		4

static	CmdAttr	helpCmdAttr[] = {
	{ "SCREEN_NO",	INT },
	{ "MOUSE_X",	INT },
	{ "MOUSE_Y",	INT },
	{ "HELPKEY",	STRING }
};

static Command helpCommand = {
	"SHOWHELP", 0, HW_ATTR_COUNT, helpCmdAttr
};

