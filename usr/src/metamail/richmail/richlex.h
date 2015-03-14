/*-------------------------------------------------------------------------

  richlex.h - Lexical analysis routines for parsing richtext messages.
 
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
     1.0    31/01/92  RW  Original Version of richlex.h

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

-------------------------------------------------------------------------*/

#ifndef	__RICHTEXT_H__
#define	__RICHTEXT_H__

#ifdef	__cplusplus
extern "C" {
#endif

/*
 * Set the following variable to zero to disable the
 * correction of richtext commands.  The default
 * value is non-zero.
 */
extern	int CorrectionEnabled;

/*
 * Define the function to call to get characters from
 * the message.  The calling convention of this
 * function is: "int func (void *param)".  The default
 * value is "fgetc".  The function must return EOF
 * at the end of the messsage;
 */
extern	int (*RichtextGetc) ();

/*
 * Define the function to call to output characters from
 * richtextcorrect.  The calling convention of this
 * function is: "int func (int c,void *param)".  The default
 * value is "fputc".
 */
extern	int (*RichtextPutc) ();

/*
 * Define the maximum size of richtext command tokens.
 */
#define	MAX_TOKEN_SIZE		50

/*
 * Define the special token values that are returned by
 * the "richtextlex" function.  These values were chosen
 * to keep away from legal ASCII.
 */
#define	RICHTEXT_COMMAND	1000
#define	RICHTEXT_NEG_COMMAND	1001

/*
 * Reset the richtext parsing mechanism.
 */
extern	richtextreset();

/*
 * Get the next token from the input stream.  RICHTEXT_COMMAND
 * or RICHTEXT_NEG_COMMAND are returned if it is a richtext command.
 * e.g. "<cmd>" or "</cmd>".  The "token" buffer will receive the
 * name of the command (without <,> or /) if it is a command.  This
 * function will also truncate commands longer than MAX_TOKEN_SIZE - 1
 * characters and abort command parsing if white space is encountered,
 * so, for example, errors like "<bold hi kids</bold>" don't cause
 * problems: it will be corrected to "<bold>hi kids</bold>".
 * The "file" parameter is passed to the function pointed to by
 * "RichtextGetc" on each call.
 */
extern	richtextlex( /* void *file,char *token */ );

/*
 * Read the input stream, correct the richtext, and write the
 * results to the output stream.  "outparam" is passed to the
 * "RichtextPutc" function as the second argument, and "inparam"
 * is passed to "richtextlex" during parsing.
 */
extern	richtextcorrect( /* void *inparam,void *outparam */ );

#ifdef	__cplusplus
};
#endif

#endif	/* __RICHTEXT_H__ */
