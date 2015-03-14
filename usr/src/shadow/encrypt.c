/*
 * Copyright 1990, John F. Haugh II
 * All rights reserved.
 *
 * Permission is granted to copy and create derivative works for any
 * non-commercial purpose, provided this copyright notice is preserved
 * in all copies of source code, or included in human readable form
 * and conspicuously displayed on all copies of object code or
 * distribution media.
 */

#include <string.h>
#include "config.h"

#ifndef lint
static	char	sccsid[] = "@(#)encrypt.c	3.4	19:44:23	12/10/90";
#endif

extern	char	*crypt();

char *
pw_encrypt (clear, salt)
char	*clear;
char	*salt;
{
	static	char	cipher[32];
	static	int	count;
	char	newsalt[2];
	char	*cp;
	long	now;

	/*
	 * See if a new salt is needed and get a few random
	 * bits of information.  The amount of randomness is
	 * probably not all that crucial since the salt only
	 * serves to thwart a dictionary attack.
	 */

	if (salt == (char *) 0) {
		now = time ((long *) 0) + count++;
		now ^= clock ();
		now ^= getpid ();
		now = ((now >> 12) ^ (now)) & 07777;
		newsalt[0] = i64c ((now >> 6) & 077);
		newsalt[1] = i64c (now & 077);
		salt = newsalt;
	}
	cp = crypt (clear, salt);
	strcpy (cipher, cp);

#ifdef	DOUBLESIZE
	if (strlen (clear) > 8) {
		cp = crypt (clear + 8, salt);
		strcat (cipher, cp + 2);
	}
#endif
	return cipher;
}
