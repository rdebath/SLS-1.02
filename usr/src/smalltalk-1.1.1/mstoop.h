/***********************************************************************
 *
 *	Object Table declarations.
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
 * sbyrne     8 Apr 90	  Changed oopFree to oopValid to better reflect the
 *			  semantics.
 *
 * sbyrne    13 Jan 89	  Created.
 *
 */


#ifndef __MSTOOP__
#define __MSTOOP__

#define INLINE_MACROS

/* The number of OOPs in the system.  This is exclusive of Character, True,
   False, and UndefinedObject (nil) oops, which are built-ins. */
#define OOP_TABLE_SIZE		(10240 * 8) /* for the nonce, then back to 4 */

#define NUM_CHAR_OBJECTS	256
#define CHAR_OBJECT_BASE	OOP_TABLE_SIZE
#define NUM_BUILTIN_OBJECTS	3
#define BUILTIN_OBJECT_BASE	(CHAR_OBJECT_BASE + NUM_CHAR_OBJECTS)

#define nilOOPIndex		(BUILTIN_OBJECT_BASE + 0)
#define trueOOPIndex		(BUILTIN_OBJECT_BASE + 1)
#define falseOOPIndex		(BUILTIN_OBJECT_BASE + 2)

#define TOTAL_OOP_TABLE_SLOTS \
  ( OOP_TABLE_SIZE + NUM_CHAR_OBJECTS + NUM_BUILTIN_OBJECTS )

/*
 * Given a number of bytes "x", return the number of 32 bit words
 * needed to represent that object, rounded up to the nearest 32 bit
 * word boundary.
 */
#define ROUNDED_WORDS(x) \
  (((x) + sizeof(long) - 1) / sizeof(long))

#define GCIsOn() \
  (gcState)

#define inToSpace(oop) \
  ((oop)->inSpace == toSpace)

#define inFromSpace(oop) \
  ((oop)->inSpace == fromSpace)

#define prepareToStore(destOOP, srcOOP) \
{ \
  if (inToSpace(destOOP) && isOOP(srcOOP) && inFromSpace(srcOOP)) { \
    moveOOP(srcOOP); \
  } \
}

#define maybeMoveOOPMac(oop) \
{ \
  if (!isInt(oop) && inFromSpace(oop)) { \
    moveOOP(oop); \
  } \
}

#define clearGCFlipFlagsMac() \
  gcFlipCounter = 0

#define oopAtMac(index) \
  ( &oopTable[index] )

#define oopAvailableMac(index) \
  ( oopTable[index].isFree )

#ifdef INLINE_MACROS

#define maybeMoveOOP	maybeMoveOOPMac
#define clearGCFlipFlags clearGCFlipFlagsMac
#define oopAt		oopAtMac
#define oopAvailable	oopAvailableMac

#else

extern void		maybeMoveOOP(), clearGCFlipFlags();
extern OOP		oopAt();
extern Boolean		oopAvailable();

#endif /* INLINE_MACROS */

typedef struct CharObjectStruct {
  OBJ_HEADER;
#ifdef BIG_ENDIAN
  Byte		charVal;
  Byte		dummy[3];	/* filler */
#else
  Byte		dummy[3];	/* filler */
  Byte		charVal;	/* probably not necessary to care about
				   ordering here */
#endif
} CharObject;

struct NilObjectStruct {
  OBJ_HEADER;
};

struct BooleanObjectStruct {
  OBJ_HEADER;
  OOP		booleanValue;
};

extern CharObject		charObjectTable[];
extern struct NilObjectStruct 	nilObject;
extern struct BooleanObjectStruct booleanObjects[];
extern OOP			freeOOPs;
extern int			numFreeOOPs;
extern char			toSpace, fromSpace;
extern Boolean			gcFlipped, gcState, gcMessage;
extern int			gcFlipCounter;

extern OOP			allocOOP(), charOOPAt(), findAnInstance();
extern void			initOOP(), setOOPAt(), swapObjects(), 
				fixupMetaclassObjects(), moveOOP(), gcOn(),
				setGCState(), gcFlip(), 
				setSpaceInfo();
extern Byte			charOOPValue();
extern Object			allocObj(), curSpaceAddr();
extern Boolean			oopIndexValid(), oopValid(), gcOff();
extern long			oopIndex();

extern struct OOPStruct	oopTable[];

#endif /* __MSTOOP__ */
