
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	tputs.c
 *
 */

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "curses.h"
#include "curses.priv.h"
#include "terminfo.h"


int tputs(string, affcnt, outc)
char	*string;
int	affcnt;
int	(*outc)(char);
{
	float	number;
	int	baud = baudrate();
	char	null = '\0';
	int	i;

#ifdef TRACE
	if (_tracing)
	    _tracef("tputs(\"%s\", %d, %o) called", string, affcnt, outc);
#endif

	if (pad_char)
	    null = pad_char[0];

	while (*string)
	{
	    if (*string != '$')
		(*outc)(*string);
	    else
	    {
		string++;
		if (*string != '<')
		{
		    (*outc)('$');
		    (*outc)(*string);
		}
		else
		{

		    number = 0;
		    string++;

		    if ((!isdigit(*string) && *string != '.') || !strchr(string, '>')) {
			(*outc)('$');
			(*outc)('<');
			continue;
		    }
		    while (isdigit(*string))
		    {
			number = number * 10 + *string - '0';
			string++;
		    }

		    if (*string == '.')
		    {
			string++;
			if (isdigit(*string))
			{
			    number += (float) (*string - '0') / 10.;
			    string++;
			}
		    }

		    if (*string == '*')
		    {
			number *= affcnt;
			string++;
		    }

		    if (padding_baud_rate  &&  baud >= padding_baud_rate && !xon_xoff)
		    {
			number = ((baud / 10.) * number) / 1000.;
			
			for (i=0; i < number; i++)
			    (*outc)(null);
		    }

		} /* endelse (*string == '<') */
	    } /* endelse (*string == '$') */

	    if (*string == '\0')
		break;

	    string++;
	}
}


int 
_outc(char ch)
{
    putchar(ch);
}


int putp(char *string)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("putp(%s) called", string);
#endif
	tputs(string, 1, _outc);
}
