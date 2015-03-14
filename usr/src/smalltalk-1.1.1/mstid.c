/***********************************************************************
 *
 * Identifier related functions
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
 * sbyrne    27 Dec 88    Created.
 */

#include "mststr.h"
#include "mstid.h"

/*
 *	void initIdent(c)
 *
 * Description
 *
 *	Prepares for scanning of an identifier
 *
 * Inputs
 *
 *	c     : Initial character of identifier
 *
 */
void initIdent(c)
char	c;
{
  initStrBuf();
  addStrBufChar(c);
}

/*
 *	void addIdentChar(c)
 *
 * Description
 *
 *	adds a character "c" to the identifier being accumulated.
 *
 * Inputs
 *
 *	c     : character to add.
 *
 */
void addIdentChar(c)
char	c;
{
  addStrBufChar(c);
}

/*
 *	char *doneIdent()
 *
 * Description
 *
 *	Called when the indentifier is complete, this function returns a C
 *	string (NUL terminated) that is the identifier.  Note that the returned
 *	value comes from a constant string, and should be copied by the caller
 *	as soon as possible.
 *
 * Outputs
 *
 *	Pointer to NUL terminated C string that is the identifier that has been
 *	accumulated.
 */
char *doneIdent()
{
  return (curStrBuf());
}
