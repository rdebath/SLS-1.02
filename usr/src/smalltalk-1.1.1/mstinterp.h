/***********************************************************************
 *
 *	Byte Code interpreter declarations.
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
 * sbyrne     7 Jan 89	  Created.
 *
 */


#ifndef __MSTINTERP__
#define __MSTINTERP__

#define CONTEXT_STACK_SIZE		40 /* words/OOPS in a context */
#define NUM_PRIORITIES			8

/* These next three defines are the number of bits in a method header for
   the number of literals, the number of temporaries, and the number of
   arguments that the method takes.  If the representation is changed, these
   definitions need to be altered too */
#define NUM_LITERALS_BITS		6
#define NUM_TEMPS_BITS			5
#define NUM_ARGS_BITS			5

#define MAX_NUM_LITERALS		((1 << NUM_LITERALS_BITS) - 1)
#define MAX_NUM_TEMPS			((1 << NUM_TEMPS_BITS) - 1)
#define MAX_NUM_ARGS			((1 << NUM_ARGS_BITS) - 1)

/*
This is the organization of a method header.  The 1 bit in the high end of
the word indicates that this is an integer, so that the GC won't be tempted
to try to scan the contents of this field, and so we can do bitwise operations
on this value to extract component pieces.

   3                   2                   1 
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|1|.|.|.|.|flg| prim index    | #args   | #temps  |  #literals  |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

each of args temps literals and flags could have another bit

literals 7 0..6
temporarycount 5 7..11
args 5 12..16
primitiveIndex 8 17..24
flags 2 24-25
flags 0 -- use arguments as they are, ignore prim index
flags 1 -- return self
flags 2 -- return instance variable
flags 3 -- call the primitive indexed by primIndex
*/

typedef struct MethodHeaderStruct {
#ifdef BIG_ENDIAN
  unsigned	intMark			: 1; /* flag this as an Int */
  unsigned				: 5; /* unused */
  unsigned	headerFlag		: 2; /* numargs, prim self, etc. */
  unsigned	primitiveIndex		: 8; /* index of primitve, or 0 */
  unsigned	numArgs			: NUM_ARGS_BITS;
  unsigned	numTemps		: NUM_TEMPS_BITS;
  unsigned	numLiterals		: NUM_LITERALS_BITS;
#else
  unsigned	numLiterals		: NUM_LITERALS_BITS;
  unsigned	numTemps		: NUM_TEMPS_BITS;
  unsigned	numArgs			: NUM_ARGS_BITS;
  unsigned	primitiveIndex		: 8; /* index of primitve, or 0 */
  unsigned	headerFlag		: 2; /* numargs, prim self, etc. */
  unsigned				: 5; /* unused */
  unsigned	intMark			: 1; /* flag this as an Int */
#endif
} MethodHeader;

extern OOP			methodLiteralExt(), 
				finishExecutionEnvironment();
extern void			interpret(), sendMessage(), initInterpreter(),
				prepareExecutionEnvironment(), 
				storeMethodLiteralExt(), setFileStreamFile(),
				updateMethodCache(), initSignals(),
				storeMethodLiteralNoGC();
extern Boolean			equal();
extern long			hash();
extern MethodHeader		getMethodHeaderExt();

extern long			byteCodeCounter;
extern Boolean			executionTracing;
extern Boolean			makeCoreFile;

#endif /* __MSTINTERP__ */
