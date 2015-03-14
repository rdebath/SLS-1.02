/*  this routine was stolen from the CARL library.  -dll@ems.media.mit.edu */
/*
 * crack - put the command line to the vice.
 * 
 * This routine takes four arguments, the variables argc and argv from
 * the program's main procedure, a list of flags to recognize, and an
 * indication whether to ignore unknown flags or not.  
 * 
 * The action of crack is to read a command line laid out in the format:
 * 
 * % command [flags]... [files]...
 * 
 * where the flags are of the form -foption, that is, a minus, a character
 * and an optional argument, butted up to the character.  No space may
 * appear between any flag and its option.  The first command line
 * argument not preceeded by '-' is taken as the end of the flags and the
 * beginning of file names.
 * 
 * The flags argument to crack looks like this: "a|b|cd" for flags a b c
 * and d.  In this example, a and b take (optional!) options, as specified
 * by the trailing colon, c and d do not.  When crack scans a flag, it
 * returns the flag character after setting the external character pointer
 * arg_option to the option part.  It also sets arg_index to the index of
 * the argv variable scanned.  Crack returns NULL when it has scanned the
 * last flag.  The value of arg_index then points at the first 
 * argument after the last flag, which should be the first filename, if
 * any.  If there are no arguments, or no more arguments after reading 
 * the flags, arg_index == argc;
 * 
 * Flags may be concatenated, for instance, using the flags argument
 * given above: -cd will treat c and d as
 * flags.  -cdaoption also works.  However, tacking what you think is
 * a flag after another flag that takes an option will cause the flag to
 * be lost.  I.e., -ac will treat c as an option, not a flag.
 * 
 * When the ignore flag is zero,  flags scanned that are not in the flags
 * variable generate an error message and crack returns EOF.  If ignore is
 * nonzero, unknown flags are suppressed, that is, no flag is returned.
 * The purpose of ignoring flags is so that more than one part of a
 * program can scan the command line without having to know about the
 * flags of all the other parts.  For instance, where a program calculates
 * a sampling rate by one flag and a value in seconds in another, it must
 * search for the sampling rate first, then the time value.  Two calls to
 * crack() would be required, one to scan just for the flag setting sampling
 * rate, another to ignore the rate flag, but to set the time value based
 * on the sampling rate.
 * NOTE: WHEN MAKING MORE THAN ONE CALL TO crack() IN A PROGRAM, IT
 * IS NECESSARY TO RESET arg_index TO 0 FIRST.
 *
 * When ignoring unknown flags, if an unknown flag has an option
 * associated with it, the option is also ignored.  Care should be excercised
 * here because it may be possible that the associated "option" is really
 * more concatenated flags.  These, if any, are lost.  The rule is that,
 * when ignoring unknown flags, the first instance of an unknown flag
 * causes that flag and the rest of that argument to be discarded.  For
 * instance, if flags is set to "a:b:cd", and a command line:
 * "-zcdaoption" is supplied, c d and a would be ignored because they come
 * after z in the same argument.  The point is there is no way to disambiguate
 * flags from unknown options when ignoring flags, so concatenating options,
 * while nice, is problematical.
 */

#include <stdio.h>
#include <sysdep.h>  /* csound include */

int arg_index = 0;
char *arg_option;
char *pvcon = NULL;

char crack(argc, argv, flags, ign)
    int argc; char **argv; char *flags; int ign;
{
    char *pv, *flgp;
    while ((arg_index) < argc)
	{
	if (pvcon != NULL)
	    pv = pvcon;
	else
	    {
	    if (++arg_index >= argc) return(NULL); 
	    pv = argv[arg_index];
	    if (*pv != '-') 
		return(NULL);
	    }
	pv++;		/* skip '-' or prev. flag */
	if (*pv != NULL) 
	    {
	    if ((flgp=index(flags, *pv)) != NULL)
		{
		pvcon = pv;
		if (*(flgp+1) == '|') { arg_option = pv+1; pvcon = NULL; }
		return(*pv);
		}
	    else
		if (!ign)
		    {
		    fprintf(stderr, "%s: no such flag: %s\n", argv[0], pv);
		    return(EOF);
		    }
		else
		    pvcon = NULL;
	    }
	pvcon = NULL;
	}
    return(NULL);
    }

