/***********************************************************************
 *
 *	Byte Code definitions.
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
 * sbyrne     2 Jan 89	  Created.
 *
 */


#ifndef __MSTBYTE__
#define __MSTBYTE__

/* ??? I don't like these being defines, but you can't do math on enums, and
 * you can't switch on variables.  I like the looks of these (lexically
 * speaking) but it does violate the guidelines about when to use defines.
 */

#define pushReceiverVariable	0
#define pushTemporaryVariable	16
#define pushLitConstant		32
#define pushLitVariable		64
#define popReceiverVariable	96
#define popTemporaryVariable	104
#define pushSpecial		112
#define returnIndexed		120
#define returnMethodStackTop	124
#define returnBlockStackTop	125
#define pushIndexed		128
#define storeIndexed		129
#define popStoreIndexed		130
#define sendSelector1ExtByte	131
#define sendSelector2ExtByte	132
#define sendSuper1ExtByte	133
#define sendSuper2ExtByte	134
#define popStackTop		135
#define dupStackTop		136
#define pushActiveContext	137
#define jumpShort		144
#define popJumpFalseShort	152
#define jumpLong		160
#define popJumpTrue		168
#define popJumpFalse		172
#define plusSpecial		176
#define minusSpecial		177
#define lessThanSpecial		178
#define greaterThanSpecial	179
#define lessEqualSpecial	180
#define greaterEqualSpecial	181
#define equalSpecial		182
#define notEqualSpecial		183
#define timesSpecial		184
#define divideSpecial		185
#define remainderSpecial	186
#define bitShiftColonSpecial	188
#define integerDivideSpecial	189
#define bitAndColonSpecial	190
#define bitOrColonSpecial	191
#define atColonSpecial		192
#define atColonPutColonSpecial	193
#define sizeSpecial		194
#define nextSpecial		195
#define nextPutColonSpecial	196
#define atEndSpecial		197
#define sameObjectSpecial	198
#define classSpecial		199
#define blockCopyColonSpecial	200
#define valueSpecial		201
#define valueColonSpecial	202
#define doColonSpecial		203
#define newSpecial		204
#define newColonSpecial		205
#define sendSelectorNoArg	208
#define sendSelector1Arg	224
#define sendSelector2Arg	240

#define receiverIndex		0
#define trueIndex		1
#define falseIndex		2
#define nilIndex		3
#define litMinusOneIndex	4
#define litZeroIndex		5
#define litOneIndex		6
#define litTwoIndex		7

#define receiverLocation	(0 << 6)
#define temporaryLocation	(1 << 6)
#define litConstLocation	(2 << 6)
#define litVarLocation		(3 << 6)

#define locationMask		(3 << 6)

typedef struct ByteCodeArray	*ByteCodes;

extern ByteCodes	getByteCodes(), saveByteCodeArray();

extern void		initByteCodes(),
			compileByte(), compileAndFreeByteCodes(),
  			restoreByteCodeArray(), freeByteCodes(),
  			copyByteCodes(), printByteCodeName(); 

extern	int		currentByteCodeLength(), isSimpleReturn();

#endif /* __MSTBYTE__ */
