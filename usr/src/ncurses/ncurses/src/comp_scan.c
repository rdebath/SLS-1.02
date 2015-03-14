
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	comp_scan.c --- Lexical scanner for terminfo compiler.
 *
 */

#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>
#include "compiler.h"

#define iswhite(ch)	(ch == ' '  ||  ch == '\t')

static int	first_column;		/* See 'next_char()' below */

extern void panic_mode(char);

/*
 *	int
 *	get_token()
 *
 *	Scans the input for the next token, storing the specifics in the
 *	global structure 'curr_token' and returning one of the following:
 *
 *		NAMES		A line beginning in column 1.  'name'
 *				will be set to point to everything up to
 *				but not including the first comma on the line.
 *		BOOLEAN		An entry consisting of a name followed by
 *				a comma.  'name' will be set to point to the
 *				name of the capability.
 *		NUMBER		An entry of the form
 *					name#digits,
 *				'name' will be set to point to the capability
 *				name and 'valnumber' to the number given.
 *		STRING		An entry of the form
 *					name=characters,
 *				'name' is set to the capability name and
 *				'valstring' to the string of characters, with
 *				input translations done.
 *		CANCEL		An entry of the form
 *					name@,
 *				'name' is set to the capability name and
 *				'valnumber' to -1.
 *		EOF		The end of the file has been reached.
 *
 */

int
get_token()
{
long		number;
int		type;
int		ch;
static char	buffer[1024];
char		*ptr;
int		dot_flag = FALSE;

	while ((ch = next_char()) == '\n'  ||  iswhite(ch))
	    ;
	
	if (ch == EOF)
	    type = EOF;
	else
	{
	    if (ch == '.')
	    {
		dot_flag = TRUE;

		while ((ch = next_char()) == ' '  ||  ch == '\t')
		    ;
	    }

	    if (! isalnum(ch))
	    {
		 warning("Illegal character - '%c'", ch);
		 panic_mode(',');
	    }

	    ptr = buffer;
	    *(ptr++) = ch;

	    if (first_column)
	    {
		while ((ch = next_char()) != ',' && ch != '\n' && ch != EOF)
		    *(ptr++) = ch;
		
		if (ch == EOF)
		    err_abort("Premature EOF");
		else if (ch == '\n') {
		    warning("Newline in middle of terminal name");
		    panic_mode(',');
		}
		
		*ptr = '\0';
		curr_token.tk_name = buffer;
		type = NAMES;
	    }
	    else
	    {
		ch = next_char();
		while (isalnum(ch))
		{
		    *(ptr++) = ch;
		    ch = next_char();
		}

		*ptr++ = '\0';
		switch (ch)
		{
		    case ',':
			curr_token.tk_name = buffer;
			type = BOOLEAN;
			break;

		    case '@':
			if (next_char() != ',')
			    warning("Missing comma");
			curr_token.tk_name = buffer;
			type = CANCEL;
			break;

		    case '#':
			number = 0;
			while (isdigit(ch = next_char()))
			    number = number * 10 + ch - '0';
			if (ch != ',')
			    warning("Missing comma");
			curr_token.tk_name = buffer;
			curr_token.tk_valnumber = number;
			type = NUMBER;
			break;
		    
		    case '=':
			ch = trans_string(ptr);
			if (ch != ',')
			    warning("Missing comma");
			curr_token.tk_name = buffer;
			curr_token.tk_valstring = ptr;
			type = STRING;
			break;

		    default:
			/* just to get rid of the compiler warning */
			type = UNDEF;
			warning("Illegal character - '%x'", ch);
		}
	    } /* end else (first_column == FALSE) */
	} /* end else (ch != EOF) */

	if (dot_flag == TRUE)
	    DEBUG0(8, "Commented out ");

	if (debug_level >= 8)
	{
	    fprintf(stderr, "Token: ");
	    switch (type)
	    {
		case BOOLEAN:
		    fprintf(stderr, "Boolean;  name='%s'\n",
                                                          curr_token.tk_name);
		    break;
		
		case NUMBER:
		    fprintf(stderr, "Number; name='%s', value=%d\n",
				curr_token.tk_name, curr_token.tk_valnumber);
		    break;
		
		case STRING:
		    fprintf(stderr, "String; name='%s', value='%s'\n",
				curr_token.tk_name, curr_token.tk_valstring);
		    break;
		
		case CANCEL:
		    fprintf(stderr, "Cancel; name='%s'\n",
                                                          curr_token.tk_name);
		    break;
		
		case NAMES:

		    fprintf(stderr, "Names; value='%s'\n",
                                                          curr_token.tk_name);
		    break;

		case EOF:
		    fprintf(stderr, "End of file\n");
		    break;

		default:
		    warning("Bad token type");
	    }
	}

	if (dot_flag == TRUE)		/* if commented out, use the next one */
	    type = get_token();

	return(type);
}


/*
 *	char
 *	next_char()
 *
 *	Returns the next character in the input stream.  Comments and leading
 *	white space are stripped.  The global state variable 'firstcolumn' is
 *	set TRUE if the character returned is from the first column of the input
 * 	line.  The global variable curr_line is incremented for each new line.
 *	The global variable curr_file_pos is set to the file offset of the
 *	beginning of each line.
 *
 */

int	curr_column = -1;
char	line[1024];

char
next_char()
{
	char	*rtn_value;
	long	ftell();

	if (curr_column < 0  ||  curr_column > 1023  ||
	    line[curr_column] == '\0')
	{
	    do
	    {
		curr_file_pos = ftell(stdin);

		if ((rtn_value = fgets(line, 1024, stdin)) != NULL)
		    curr_line++;
	    } while (rtn_value != NULL  &&  line[0] == '#');

	    if (rtn_value == NULL)
		return (EOF);

	    curr_column = 0;
	    while (iswhite(line[curr_column]))
		curr_column++;
	}

	if (curr_column == 0  &&  line[0] != '\n')
	    first_column = TRUE;
	else
	    first_column = FALSE;
	
	return (line[curr_column++]);
}


void backspace()
{
	curr_column--;

	if (curr_column < 0)
	    syserr_abort("Backspaced off beginning of line");
}


/*
 *	reset_input()
 *
 *	Resets the input-reading routines.  Used after a seek has been done.
 *
 */

void reset_input()
{
	curr_column = -1;
}


/*
 *	char
 *	trans_string(ptr)
 *
 *	Reads characters using next_char() until encountering a comma, newline
 *	or end-of-file.  The returned value is the character which caused
 *	reading to stop.  The following translations are done on the input:
 *
 *		^X  goes to  ctrl-X (i.e. X & 037)
 *		{\E,\n,\r,\b,\t,\f}  go to
 *			{ESCAPE,newline,carriage-return,backspace,tab,formfeed}
 *		{\^,\\}  go to  {carat,backslash}
 *		\ddd (for ddd = up to three octal digits)  goes to the character ddd
 *
 *		\e == \E
 *		\0 == \200
 *
 */

char
trans_string(ptr)
char	*ptr;
{
	register int	count = 0;
	int		number;
	int		i;
        char		ch;

	while ((ch = next_char()) != ','  &&  ch != EOF)
	{
	    if (ch == '^')
	    {
		ch = next_char();
		if (ch == EOF)
		    err_abort("Premature EOF");

		if (! isprint(ch))
		{
		    warning("Illegal ^ character - '%c'", ch);
		}
		*(ptr++) = ch & 037;
	    }
	    else if (ch == '\\')
	    {
		ch = next_char();
		if (ch == EOF)
		    err_abort("Premature EOF");
		
		if (ch >= '0'  &&  ch <= '7')
		{
		    number = ch - '0';
		    for (i=0; i < 2; i++)
		    {
			ch = next_char();
			if (ch == EOF)
			    err_abort("Premature EOF");
			
			if (ch < '0'  ||  ch > '7')
			{
			    backspace();
			    break;
			}

			number = number * 8 + ch - '0';
		    }

		    if (number == 0)
			number = 0200;
		    *(ptr++) = (char) number;
		}
		else
		{
		    switch (ch)
		    {
			case 'E':
			case 'e':	*(ptr++) = '\033';	break;
			
			case 'l':
			case 'n':	*(ptr++) = '\n';	break;
			
			case 'r':	*(ptr++) = '\r';	break;
			
			/*case 'b':	*(ptr++) = '\008';	break;*/
			case 'b':	*(ptr++) = '\010';	break;

			case 's':	*(ptr++) = ' ';		break;
			
			case 'f':	*(ptr++) = '\014';	break;
			
			case 't':	*(ptr++) = '\t';	break;
			
			case '\\':	*(ptr++) = '\\';	break;
			
			case '^':	*(ptr++) = '^';		break;

			case ',':	*(ptr++) = ',';		break;

			case ':':	*(ptr++) = ':';		break;

			default:
			    warning("Illegal character in \\ sequence");
			    *(ptr++) = ch;
		    } /* endswitch (ch) */
		} /* endelse (ch < '0' ||  ch > '7') */
	    } /* end else if (ch == '\\') */
	    else
	    {
		*(ptr++) = ch;
	    }
	    
	    count ++;

	    if (count > 500)
		warning("Very long string found.  Missing comma?");
	} /* end while */

	*ptr = '\0';

	return(ch);
}

/*
 * Panic mode error recovery - skip everything until a "ch" is found.
 */
void panic_mode(char ch)
{
	int c;

	for (;;) {
		c = next_char();
		if (c == ch)
			return;
		if (c == EOF);
			return;
	}
}
