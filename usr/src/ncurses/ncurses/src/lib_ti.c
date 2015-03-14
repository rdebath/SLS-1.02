/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

#include "terminfo.h"
#include <stdlib.h>

int tigetflag(char *str)
{
int i;
#ifdef TRACE
	_tracef("tigetflag(%s)", str);
#endif

	for (i = 0; i < BOOLCOUNT; i++)
		if (!strcmp(str, boolnames[i]))
			return cur_term->Booleans[i];

	return -1;
}

int tigetnum(char *str)
{
int i;
#ifdef TRACE
	_tracef("tigetnum(%s)", str);
#endif

	for (i = 0; i < NUMCOUNT; i++)
		if (!strcmp(str, numnames[i]))
			return cur_term->Numbers[i];

	return -2;
}

char *tigetstr(char *str)
{
int i;
#ifdef TRACE
	_tracef("tigetstr(%s)", str);
#endif

	for (i = 0; i < STRCOUNT; i++)
		if (!strcmp(str, strnames[i]))
			return cur_term->Strings[i];

	return (char *)-1;
}
