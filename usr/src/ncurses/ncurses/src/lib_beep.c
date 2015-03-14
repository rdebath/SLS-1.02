
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	beep.c
 *
 *	Routines beep() and flash()
 *
 */

#include "curses.h"
#include "curses.priv.h"
#include "terminfo.h"

/*
 *	beep()
 *
 *	Sound the current terminal's audible bell if it has one.   If not,
 *	flash the screen if possible.
 *
 */

int beep()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("beep() called");
#endif

	if (bell)
	    tputs(bell, 1, outc);
	else if (flash_screen)
	    tputs(flash_screen, 1, outc);
	
}

/*
 *	flash()
 *
 *	Flash the current terminal's screen if possible.   If not,
 *	sound the audible bell if one exists.
 *
 */

int flash()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("flash() called");
#endif

	if (flash_screen)
	    tputs(flash_screen, 1, outc);
	else if (bell)
	    tputs(bell, 1, outc);
}
