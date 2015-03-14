/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: mgrmode.c,v 4.2 88/06/22 14:37:52 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/mgrmode.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/mgrmode.c,v $$Revision: 4.2 $";

/* set or clear an MGR window mode */

#include <signal.h>
#include "term.h"


#define	SET	0
#define	CLEAR	1

#define	EQ(a,b)	(!strcmp(a,b))

static char	*allmodes[] = {
	"ABS",
	/*	ACTIVATE is not really a mode; it is actually an action.
		We won't deactivate a window when CLEARALL is specified.
	*/
	"AUTOEXPOSE",
	"BACKGROUND",
	"DUPKEY",
	"NOBUCKEY",
	"NOINPUT",
	"NOWRAP",
	"OVERSTRIKE",
	"SNARFHARD",
	"SNARFLINES",
	"SNARFTABS",
	"STACK",
	"STANDOUT",
	"WOB",
	0
};
static char	*pgm;


main(argc,argv)
int	argc;
char	**argv;
{
	int	setclear = SET;

	pgm = *argv;
	ckmgrterm( pgm );

	if( argc < 2 )
        {
		fprintf( stderr, "Usage:  %s [ SETMODE | CLEARMODE ] mgr-modes ...\n", pgm);
		fputs("\
Set or clear MGR window modes.\n\
SETMODE		(Default) Set the mode\n\
CLEARMODE	Clear the mode\n\
CLEARALL	Clear all modes, except ACTIVATE\n\
Modes include:\n\
M_ABS		Turn on Absolute Coordinate mode.\n\
M_ACTIVATE	Activate window (not really a mode).\n\
M_AUTOEXPOSE	Expose window when written to.\n\
M_BACKGROUND	Do not block window update when window is obscured.\n\
M_DUPKEY	Duplicate the Escape character when from keyboard.\n\
M_NOBUCKEY	Turn off mgr interpreting Left and Right (Buckey) keys;\n\
		Pass them on to the application.\n\
M_NOINPUT	Keyboard input is held until cleared or another window\n\
		becomes active.\n\
M_NOWRAP	Do not wrap around when text reaches right edge of window.\n\
M_OVERSTRIKE	Turn on overstriking of text.\n\
M_SNARFHARD	Snarf (cut) even if there are errors.\n\
M_SNARFLINES	Only cut entire lines.\n\
M_SNARFTABS	Change spaces to tabs within the snarf buffer.\n\
M_STACK		Turn on event stacking.\n\
M_STANDOUT	Write characters in standout mode (foreground and background\n\
		colors reversed).\n\
M_WOB		White On Black, entire window's sense of white and black is\n\
		reversed.\n\
`M_' may be omitted.  Lower case letters are permitted.  M_ABS == ABS == abs.\n\
",
			stderr );
		exit(1);
	}
	m_setup( M_FLUSH );
	argv++;
	argc--;
	for(  ;  *argv;  argv++, argc-- ) {
		register char	*mode = *argv;

		/* Uppercase modes are optional.  abs == ABS
		*/
		do {
			if( *mode >= 'a'  &&  *mode <= 'z' )
				*mode -= 'a' - 'A';
		} while( *(mode++) );

		mode = *argv;

		if( EQ( mode, "SETMODE" ) ) {
			setclear = SET;
			continue;
		}
		if( EQ( mode, "CLEARMODE" ) ) {
			setclear = CLEAR;
			continue;
		}
		if( EQ( mode, "CLEARALL" ) ) {
			char	**cpp;
			for( cpp = allmodes;  *cpp;  cpp++ )
				action( CLEAR, *cpp, *cpp );
			continue;
		}

		/* M_ in front of mode is optional.  M_ABS == ABS
		*/
		if( mode[0] == 'M'  &&  mode[1] == '_' )
			mode += 2;
		action( setclear, mode, *argv );
	}
}


#define	CASE(arg,str)	if( EQ(str, mode) ) {\
				switch( setclear ) {\
				case SET:\
					m_setmode( arg );\
					return;\
				case CLEAR:\
					m_clearmode( arg );\
					return;\
				}\
			}

static
action( setclear, mode, originalmode )
int	setclear;
char	*mode;
{
	CASE(M_ABS,"ABS");
	CASE(M_ACTIVATE,"ACTIVATE");
	CASE(M_AUTOEXPOSE,"AUTOEXPOSE");
	CASE(M_BACKGROUND,"BACKGROUND");
	CASE(M_DUPKEY,"DUPKEY");
	CASE(M_NOBUCKEY,"NOBUCKEY");
	CASE(M_NOINPUT,"NOINPUT");
	CASE(M_NOWRAP,"NOWRAP");
	CASE(M_OVERSTRIKE,"OVERSTRIKE");
	CASE(M_SNARFHARD,"SNARFHARD");
	CASE(M_SNARFLINES,"SNARFLINES");
	CASE(M_SNARFTABS,"SNARFTABS");
	CASE(M_STACK,"STACK");
	CASE(M_STANDOUT,"STANDOUT");
	CASE(M_WOB,"WOB");
	fprintf( stderr, "%s:  unrecognized mode: `%s'\n",
		pgm, originalmode );
}
