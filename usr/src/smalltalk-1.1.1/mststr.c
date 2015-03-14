
/***********************************************************************
 *
 * String Functions
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright (C) 1990, 1991 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 1, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 ***********************************************************************/


/*
 *    Change Log
 * ============================================================================
 * Author      Date       Change 
 *
 * sbyrne    27 Dec 88    created.
 */


#include <stdio.h>
#include "mststr.h"

#define	STRING_GRANULARITY	128

static void	reallocStrBase();

static char	*strBase = NULL, *strPtr;
static long	maxStrLen;


/*
 *	void initStrBuf()
 *
 * Description
 *
 *	Initializes the string buffer to accumulate new characters.
 *
 */
void initStrBuf()
{
  if (strBase == NULL) {
    reallocStrBase(0L, STRING_GRANULARITY);
  }
  
  strPtr = strBase;
}

/*
 *	char *curStrBuf()
 *
 * Description
 *
 *	Returns the currently accumulated string, as a C string.  Note that the
 *	actual string returned is not a unique string, and should be copied as
 *	soon as possible.
 *
 * Outputs
 *
 *	Pointer to NUL terminated C string that is the accumulated string.
 */
char *curStrBuf()
{
  addStrBufChar('\0');
  return (strBase);
}

/*
 *	void addStrBufChar(c)
 *
 * Description
 *
 *	Adds a character "c" to the string being accumulated.  The character can
 *	be any valid ASCII character.
 *
 * Inputs
 *
 *	c     : the character to add to the string.
 *
 */
void addStrBufChar(c)
char	c;
{
  if (strPtr - strBase  >= maxStrLen) {
    reallocStrBase(maxStrLen, STRING_GRANULARITY);
  }

  *strPtr++ = c;
}


/*
 *	static void reallocStrBase(len, delta)
 *
 * Description
 *
 *	Called to allocate a new string to be accumulated.  If there is an
 *	existing string, it is copied into the new string and then free;
 *	otherwise the new string is created.
 *
 * Inputs
 *
 *	len   : current length of current string, in bytes
 *	delta : increment to add to string length in bytes.  New string length
 *		is len+delta.
 *
 * Outputs
 *
 *	strBase, maxStrLen, and strPtr are all globals that are affected by
 *	this routine.
 */
static void reallocStrBase(len, delta)
long	len;
int	delta;
{
  char		*newStr;
  long		l;

  maxStrLen = len + delta;
  newStr = (char *)malloc(maxStrLen);

  if (strBase) {
    l = strPtr - strBase;
    strncpy(newStr, strBase, len);
    free(strBase);
  } else {
    l = 0L;
  }

  strBase = newStr;
  strPtr = strBase + l;
}


/*
 *	char *copyStr(str)
 *
 * Description
 *
 *	Returns a newly allocated copy of the string "str".
 *
 * Inputs
 *
 *	str   : NUL terminated C string to be copied.
 *
 * Outputs
 *
 *	Pointer to new NUL terminated C string that is a copy of "str".
 */
char *copyStr(str)
char	*str;
{
  char		*newStr;
  long		l;

  l = strlen(str) + 1;

  newStr = (char *)malloc(l);
  strncpy(newStr, str, l);

  return (newStr);
}
