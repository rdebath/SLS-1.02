/***********************************************************************
 *
 *	External definitions for the Lexer module.
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
 * sbyrne     3 Sep 89	  added getCurFileName
 *
 * sbyrne    14 May 89	  Created.
 *
 */

#ifndef __MSTLEX__
#define __MSTLEX__

typedef enum {
  unknownStreamType,
  fileStreamType,
  stringStreamType
#ifdef USE_READLINE
  , readlineStreamType
#endif /* USE_READLINE */
} StreamType;


#ifdef USE_READLINE
extern OOP			getCurReadline();
extern void			pushReadlineString();
#endif /* USE_READLINE */

extern OOP			getCurFileName(), getCurString();

extern void			pushUNIXFile(), pushSmalltalkString(), 
				popStream(), clearMethodStartPos();

extern int			getCurFilePos(), getMethodStartPos();

extern StreamType		getCurStreamType();

#endif /* __MSTLEX__ */
