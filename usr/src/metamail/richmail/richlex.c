/*-------------------------------------------------------------------------

  richlex.c - Lexical analysis routines for parsing richtext messages.
 
  Copyright (c) 1992 Rhys Weatherley

  Permission to use, copy, modify, and distribute this material
  for any purpose and without fee is hereby granted, provided
  that the above copyright notice and this permission notice
  appear in all copies, and that the name of Rhys Weatherley not be
  used in advertising or publicity pertaining to this
  material without specific, prior written permission.
  RHYS WEATHERLEY MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR
  SUITABILITY OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED
  "AS IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.

  Revision History:
  ================

   Version  DD/MM/YY  By  Description
   -------  --------  --  --------------------------------------
     1.0    31/01/92  RW  Original Version of richlex.c

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

-------------------------------------------------------------------------*/

#include <stdio.h>
#include "richlex.h"
#include <ctype.h>

int CorrectionEnabled = 1;	/* Zero if correction has been disabled */

#ifndef AMIGA
extern	int fgetc ();
extern	int fputc ();
#endif

int (*RichtextGetc) () = fgetc;	/* Function to call to get characters */
int (*RichtextPutc) () = fputc; /* Function to call to put characters */

#define	RGET(f)		((*RichtextGetc)(f))
#define	RPUT(c,f)	((*RichtextPutc)(c,f))

#define	MAX_STACK_SIZE	500
#define	MAX_FLUSH_SIZE	3
static	int	StackSize=0;
static	char	Stack[MAX_STACK_SIZE][MAX_TOKEN_SIZE];
static	char	NextToken[MAX_TOKEN_SIZE];
static	int	FlushStack=0;
static	int	FlushSize=0;
static	int	EndInpFile=0;

/*
 * Reset the richtext parsing mechanism.
 */
richtextreset()
{
    StackSize = 0;
    FlushStack = 0;
    FlushSize = 0;
    EndInpFile = 0;
}

/*
 * Find a match between NextToken and an element on the stack.
 * Returns the number of elements down from the top it is.
 * i.e. 0 if not on the stack, 1 if at the top, etc.
 */
static int richtextmatchup()
{
    int i = StackSize;
    while (i > 0 && i > (StackSize - MAX_FLUSH_SIZE)) {
	--i;
	if (!strcmp(NextToken,Stack[i]))
	    return(StackSize - i);
    }
    return(0);
}

/*
 * Determine if the current token is one of the singleton
 * richtext commands: <nl>, <lt>, <np>.
 */
static richtextsingle()
{
    return (!strcmp(NextToken,"nl") ||
	    !strcmp(NextToken,"lt") ||
	    !strcmp(NextToken,"np"));
}

/*
 * Get the next token from the input stream.  RICHTEXT_COMMAND
 * or RICHTEXT_NEG_COMMAND are returned if it is a richtext command.
 * e.g. "<cmd>" or "</cmd>".  The "token" buffer will receive the
 * name of the command (without <,> or /) if it is a command.  This
 * function will also truncate commands longer than MAX_TOKEN_SIZE - 1
 * characters and abort command parsing if white space is encountered,
 * so, for example, errors like "<bold hi kids</bold>" don't cause
 * problems: it will be corrected to "<bold>hi kids</bold>".
 */
richtextlex(file,token)
void *file;
char *token;
{
    int c,i,cmd;

    /* Perform any flushing of balancing commands that is necessary */
    if (FlushStack) {
	/* Flush out some extra closing commands */
	strcpy(token,Stack[StackSize - FlushSize + (--FlushStack)]);
	return(RICHTEXT_NEG_COMMAND);
    } else if (FlushSize) {
	/* Finished flushing: output the pending close command */
	StackSize -= FlushSize;
	if (StackSize > 0)
	    --StackSize; /* Remove the command that was being matched up */
	FlushSize = 0;
	strcpy(token,NextToken);
	if (EndInpFile)
	    return(EOF); /* The last flush was the end-of-file cleanup */
	else
	    return(RICHTEXT_NEG_COMMAND);
    }

    /* Fetch a new character or richtext command */
    for (;;) {		/* Loop so we can come back on ignored commands */
        c = RGET(file);
        if (c == '<') { 
	    /* Read a command token from the input file */
	    cmd = RICHTEXT_COMMAND;
	    if ((c = RGET(file)) == '/') {
	        cmd = RICHTEXT_NEG_COMMAND;
	        c = RGET(file);
	    }
            for (i = 0; i < (MAX_TOKEN_SIZE - 1) && c != '>'
	    		&& c != EOF && !isspace(c); ++i) {
                NextToken[i] = isupper(c) ? tolower(c) : c;
	        c = RGET(file);
            }
	    if (c != '>' && c != EOF && !isspace(c)) {
		/* We have a long command: skip the rest of it */
		while (c != '>' && c != EOF && !isspace(c))
		    c = RGET(file);
	    }
            if (c == EOF) {
		if (!StackSize)
	    	    return(EOF);
	        /* Flush the remaining commands at the end of the input file */
	        FlushSize = StackSize;
	        FlushStack = FlushSize;
	        EndInpFile = 1;
	        return(richtextlex(file,token)); /* Flush something out */
	    }
            NextToken[i] = '\0';

	    /* Check to see if we need to correct anything */
	    if (!CorrectionEnabled) {
		/* No correction to do: just skip the correction phase */
		strcpy(token,NextToken);
		return(cmd);
	    }
	    if (cmd == RICHTEXT_COMMAND) {
		/* Save the command on the stack if not a singleton command */
		if (!richtextsingle())
		    strcpy (Stack[StackSize++],NextToken);
	    }
	    else if (!(i = richtextmatchup()))
		continue;	/* No matchup - just drop it */
	    else if (i == 1)
		--StackSize;	/* Correct match at the stack top */
	    else {
		/* Flush some correction elements from the stack */
		FlushSize = i - 1;
		FlushStack = FlushSize;
		return(richtextlex(file,token));
	    }
	    strcpy(token,NextToken);
	    return(cmd);
        } else if (c == EOF && StackSize) {
	    /* Flush the remaining commands at the end of the input file */
	    FlushSize = StackSize;
	    FlushStack = FlushSize;
	    EndInpFile = 1;
	    return(richtextlex(file,token)); /* Flush something out */
	} else
	    return(c);
    }
}

/*
 * Output a string via "RichtextPutc".
 */
static richtextoutstr(str,outparam)
char *str;
void *outparam;
{
    while (*str) {
	RPUT(*str,outparam);
	++str;
    }
}

/*
 * Read the input stream, correct the richtext, and write the
 * results to the output stream.
 */
richtextcorrect(inparam,outparam)
void *inparam,*outparam;
{
    int c;
    char token[MAX_TOKEN_SIZE];
    while ((c = richtextlex(inparam,token)) != EOF) {
	if (c == RICHTEXT_COMMAND) {
	    RPUT('<',outparam);
	    richtextoutstr(token,outparam);
	    RPUT('>',outparam);
	} else if (c == RICHTEXT_NEG_COMMAND) {
	    RPUT('<',outparam);
	    RPUT('/',outparam);
	    richtextoutstr(token,outparam);
	    RPUT('>',outparam);
	} else
	    RPUT(c,outparam);
    }
}
