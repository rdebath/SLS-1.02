/* ----------------------------------------------------------------------
 *	helpcmd.h
 * ---------------------------------------------------------------------*/

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

