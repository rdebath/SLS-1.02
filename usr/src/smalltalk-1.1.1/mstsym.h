/***********************************************************************
 *
 *	Symbol Table declarations
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
 * sbyrne    13 Jan 90	  Added thisContextSymbol.
 *
 * sbyrne     1 Jan 89	  Created.
 *
 */

#ifndef __MSTSYM__
#define __MSTSYM__

#define INITIAL_SYMBOL_TABLE_SIZE	521

/* Uncomment this to enable profiling of the symbol table */
/*#define symbol_table_profiling */

typedef enum {
  temporaryScope,
  receiverScope,
  poolScope,
  globalScope
} ScopeType;



typedef struct SymbolStruct {
  OBJ_HEADER;			/* I love inheritance */
  char		symString[1];
} *Symbol;

typedef struct SymbolEntryStruct {
  ScopeType	scope;
  OOP		symbol;
  int		varIndex;	/* index of receiver or temporary */
} *SymbolEntry;

extern SymbolEntry	findVariable();

extern OOP		atColonSymbol, atColonPutColonSymbol, sizeSymbol,
			nextSymbol, nextPutColonSymbol, atEndSymbol, 
  			classSymbol, blockCopyColonSymbol, valueSymbol,
			valueColonValueColonSymbol,
			valueColonValueColonValueColonSymbol,
 			valueWithArgumentsColonSymbol,
			valueColonSymbol, doColonSymbol, newSymbol,
			newColonSymbol, plusSymbol, minusSymbol,
			lessThanSymbol, greaterThanSymbol, lessEqualSymbol,
			greaterEqualSymbol, equalSymbol, notEqualSymbol,
			timesSymbol, divideSymbol, remainderSymbol,
			bitShiftColonSymbol, integerDivideSymbol,
			bitAndColonSymbol, bitOrColonSymbol, sameObjectSymbol,
  			notSameObjectSymbol,
  			whileTrueColonSymbol, whileFalseColonSymbol,
			ifTrueColonSymbol, ifFalseColonSymbol, 
			ifTrueColonIfFalseColonSymbol,
  			ifFalseColonIfTrueColonSymbol, andColonSymbol,
			orColonSymbol, selfSymbol, superSymbol, trueSymbol,
  			falseSymbol, nilSymbol, orSymbol, andSymbol,
			doesNotUnderstandColonSymbol, unknownSymbol,
			charSymbol, stringSymbol, stringOutSymbol, 
			symbolSymbol, intSymbol, longSymbol, doubleSymbol,
			voidSymbol, variadicSymbol, cObjectSymbol,
			smalltalkSymbol, symbolTable, thisContextSymbol,
                        byteArraySymbol;

extern OOP		internString(),
  			makeInstanceVariableArray(),
			makeClassVariableDictionary(),
			makePoolArray(), 
			internStringOOP();

extern char		*symbolAsString();

int			stringOOPLen();
void			printString();


#endif /* __MSTSYM__ */
