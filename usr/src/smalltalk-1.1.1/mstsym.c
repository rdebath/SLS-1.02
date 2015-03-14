/***********************************************************************
 *
 *	Symbol Table module.
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
 * sbyrne    16 May 90	  Changed usages of "entry" to "ent" to prevent
 *			  collisions with C compilers which have this
 *			  identifier as a reserved word.
 *
 * sbyrne    21 Apr 90	  Addded byteArraySymbol.
 *
 * sbyrne    13 Jan 90	  Added thisContextSymbol.
 *
 * sbyrne    19 Dec 89	  Rebuilt symbol table.  Used to use the main OOP table
 *			  as a symbol table, due to issues involving initial
 *			  bootstrapping of the system.  Now using open hash
 *			  table built of arrays and linked lists, so that no
 *			  special precautions need be taken by the GC system or
 *			  the image save/restore facility.
 *
 * sbyrne    25 Jul 89	  Changed undeclareName to take a parameter that
 *			  controls whether the frame index is decremented or
 *			  not.  It appears that each block gets its own,
 *			  non-shared temporaries/arguments, so that if the
 *			  block is used in a process, other blocks won't have
 *			  strange things happening to their arguments.
 *
 * sbyrne     5 Jan 89	  Created.
 *
 */

#include "mst.h"
#include "mstsym.h"
#include "mstoop.h"
#include "mstcomp.h"
#include "mstdict.h"
#include "msttree.h"
#include <stdio.h>
#include <ctype.h>
#ifdef HAS_ALLOCA_H
#include <alloca.h>
#endif

/* if defined, use the hash algorithm given in the dragon book (see usage for
   better reference. */
#define dragon_book

#define	max(x, y) \
  ( ((x) > (y)) ? (x) : (y) )

#define isSymbol(oop) \
   ( !isNil(oop) && (oopClass(oop) ==  symbolClass) )

typedef struct {
  OBJ_HEADER;
  OOP		nextLink;
  OOP		symbol;
} *SymLink;

OOP			andColonSymbol, atColonPutColonSymbol, atColonSymbol,
			atEndSymbol, bitAndColonSymbol, bitOrColonSymbol,
			bitShiftColonSymbol, blockCopyColonSymbol, classSymbol,
			divideSymbol, doColonSymbol, equalSymbol,
			greaterEqualSymbol, greaterThanSymbol,
			ifFalseColonIfTrueColonSymbol, ifFalseColonSymbol,
			ifTrueColonIfFalseColonSymbol, ifTrueColonSymbol,
			integerDivideSymbol, lessEqualSymbol, lessThanSymbol,
			minusSymbol, newColonSymbol, newSymbol,
			nextPutColonSymbol, nextSymbol, notEqualSymbol,
			notSameObjectSymbol, orColonSymbol, plusSymbol,
			remainderSymbol, sameObjectSymbol, sizeSymbol,
			thisContextSymbol,
			timesSymbol, valueColonSymbol,
			valueColonValueColonSymbol,
			valueColonValueColonValueColonSymbol,
 			valueWithArgumentsColonSymbol,
			valueSymbol,
			whileFalseColonSymbol, whileTrueColonSymbol, orSymbol,
			andSymbol, superSymbol, nilSymbol, trueSymbol,
			falseSymbol, selfSymbol,
			doesNotUnderstandColonSymbol, unknownSymbol,
			charSymbol, stringSymbol, stringOutSymbol, 
			symbolSymbol, intSymbol, longSymbol, doubleSymbol,
			voidSymbol, variadicSymbol, cObjectSymbol, 
			smalltalkSymbol, symbolTable, byteArraySymbol;

#ifdef symbol_table_profiling
int			adds = 0, reused = 0, reprobes = 0,
			hitsOn[OOP_TABLE_SIZE];
#endif /* symbol_table_profiling */

void 			printString();

static SymbolEntry	allocSymbolEntry();
static Symbol		makeNewSymbol();
static Boolean		isSameString(), isWhiteSpace();
unsigned long		hashString();
static void		declareName(), undeclareName(), parseVariableName();
static OOP		scanName(), internCountedString();
static int		instanceVariableIndex(), localVarIndex();

typedef struct SymbolListStruct *SymbolList;

struct SymbolListStruct {
  OOP		symbol;
  int		index;
  SymbolList	prevSymbol;
};
  
static SymbolList	symbolList;
static int		methodArguments, frameIndex, maxFrameIndex;


int getArgCount()
{
  return (methodArguments);
}

int getTempCount()
{
  return (maxFrameIndex - methodArguments);
}

void initArgCount()
{
  methodArguments = 0;
}

void initTempCount()
{
}

void declareArguments(args)
TreeNode args;
{
  symbolList = nil;
  frameIndex = 0;
  maxFrameIndex = 0;

  if (args->nodeType == unaryExprType) {
    return;
  } else if (args->nodeType == binaryExprType) {
    declareName(args->vExpr.expression->vList.name);
    methodArguments++;
  } else {
    for(args = args->vExpr.expression; args != nil; args = args->vList.next) {
      declareName(args->vList.value->vList.name);
      methodArguments++;
    }
  }
}
  
void declareTemporaries(temps)
TreeNode temps;
{
  for( ; temps != nil; temps = temps->vList.next) {
    declareName(temps->vList.name);
  }
}

void declareBlockArguments(args)
TreeNode args;
{
  for( ; args != nil; args = args->vList.next) {
    declareName(args->vList.name);
  }
}

static void declareName(name)
char	*name;
{
  SymbolList	newList;

  newList = (SymbolList)malloc(sizeof(struct SymbolListStruct));
  newList->symbol = internString(name);
  newList->index = frameIndex++;
  maxFrameIndex = max(maxFrameIndex, frameIndex);
  newList->prevSymbol = symbolList;
  symbolList = newList;
}

void undeclareArguments(args)
TreeNode args;
{
  if (args == nil) {
    return;
  }

  if (args->nodeType == unaryExprType) {
    return;
  } else if (args->nodeType == binaryExprType) {
    undeclareName(true);
  } else {
    for(args = args->vExpr.expression; args != nil; args = args->vList.next) {
      undeclareName(true);
    }
  }
}

void undeclareTemporaries(temps)
TreeNode temps;
{
  for( ; temps != nil; temps = temps->vList.next) {
    undeclareName(true);
  }
}

void undeclareBlockArguments(args)
TreeNode args;
{
  if (args == nil) {
    return;
  }

  for( ; args != nil; args = args->vList.next) {
    undeclareName(false);
  }
}

static void undeclareName(decrFrameIndex)
Boolean decrFrameIndex;
{
  SymbolList	oldList;

  oldList = symbolList;
  symbolList = symbolList->prevSymbol;
  free(oldList);
  if (decrFrameIndex) {
    frameIndex--;
  }
}

OOP findClassVariable(varName)
OOP	varName;
{
  OOP		classOOP, assocOOP, classVariableOOP;

  if (oopClass(thisClass) == behaviorClass
      || oopClass(thisClass) == classDescriptionClass) {
    /* classDescriptions and above don't have class or pool variables */
    /* ### this isn't quite the right test; we probably should be testing
       for if we have a classClass or some derivative of that */
    return (nilOOP);
  }

  for (classOOP = thisClass; !isNil(classOOP);
       classOOP = superClass(classOOP)) {
    if (oopClass(classOOP) == metaclassClass) {
      /* pretend that metaclasses have the class variables and shared
	 pools that their instance classes do */
      classVariableOOP = metaclassInstance(classOOP);
    } else {
      classVariableOOP = classOOP;
    }
    assocOOP =
      dictionaryAssociationAt(classVariableDictionary(classVariableOOP),
			      varName);
    if (!isNil(assocOOP)) {
      return (assocOOP);
    }

    assocOOP = findSharedPoolVariable(classVariableOOP, varName);
    if (!isNil(assocOOP)) {
      return (assocOOP);
    }
  }

  return (nilOOP);
}


SymbolEntry findVariable(varName)
char	*varName;
{
  OOP		varAssoc, symbol;
  int		index;

  symbol = internString(varName);
  index = localVarIndex(symbol);
  if (index >= 0) {
    return (allocSymbolEntry(temporaryScope, symbol, index));
  }

  index = instanceVariableIndex(symbol);
  if (index >= 0) {
    return (allocSymbolEntry(receiverScope, symbol, index));
  }

  varAssoc = findClassVariable(symbol);
  if (isNil(varAssoc)) {
    return (nil);
  }

  index = addForcedObject(varAssoc);

  return (allocSymbolEntry(globalScope, varAssoc, index));
}

static int instanceVariableIndex(symbol)
OOP	symbol;
{
  OOP		arrayOOP;
  int		index, numVars;


  if (oopClass(thisClass) == behaviorClass) {
    /* behaviors have no instance variables */
    return (-1);
  }

  arrayOOP = instanceVariableArray(thisClass);
  numVars = numOOPs(oopToObj(arrayOOP));

  for (index = 1; index <= numVars; index++) {
    if (arrayAt(arrayOOP, index) == symbol) {
      return (index-1);
    }
  }

  return (-1);
}

static int localVarIndex(symbol)
OOP	symbol;
{
  SymbolList	s;

  for (s = symbolList; s != nil && symbol != s->symbol; s = s->prevSymbol);

  if (s != nil) {		/* found one */
    return (s->index);
  } else {
    return (-1);
  }
}

static SymbolEntry allocSymbolEntry(scope, symbol, index)
ScopeType	scope;
OOP		symbol;
int		index;
{
  SymbolEntry	ent;

  ent = (SymbolEntry)malloc(sizeof(struct SymbolEntryStruct));
  ent->scope = scope;
  ent->symbol = symbol;
  ent->varIndex = index;
  return (ent);
}


void freeSymbolEntry(ent)
SymbolEntry ent;
{
  free(ent);
}


OOP makeInstanceVariableArray(superclassOOP, variableString)
OOP	superclassOOP;
Byte	*variableString;
{
  OOP		arrayOOP, superArrayOOP, name;
  int		index, numInstanceVars, superInstanceVars;
  Byte		*p;

  if (variableString == nil) {
    variableString = (Byte *)"";
  }


  if (isNil(superclassOOP)) {
    superArrayOOP = nilOOP;
    superInstanceVars = numInstanceVars = 0;
  } else {
    superArrayOOP = instanceVariableArray(superclassOOP);
    superInstanceVars = numInstanceVars = numOOPs(oopToObj(superArrayOOP));
  }

  for (p = variableString; *p; ) {
    /* skip intervening whitespace */
    name = scanName(&p);
    if (!isNil(name)) {
      numInstanceVars++;
    }
  }

  if (numInstanceVars == 0) {
    return (nilOOP);		/* no instances here */
  }

  arrayOOP = arrayNew(numInstanceVars);

  /* inherit variables from parent */
  for (index = 1; index <= superInstanceVars; index++) {
    arrayAtPut(arrayOOP, index, arrayAt(superArrayOOP, index));
  }

  /* now add our own variables */
  for (p = variableString; *p; index++) {
    /* skip intervening whitespace */
    name = scanName(&p);
    if (!isNil(name)) {
      arrayAtPut(arrayOOP, index, name);
    }
  }

  return (arrayOOP);
}

OOP makeClassVariableDictionary(classOOP, variableNames)
OOP	classOOP;
Byte	*variableNames;
{
  OOP		dictionaryOOP, name;
  int		len;
  Byte		*p;

  if (variableNames == nil) {
    variableNames = (Byte *)"";
  }

  dictionaryOOP = dictionaryNew();

  for (p = variableNames; *p; ) {
    name = scanName(&p);
    if (!isNil(name)) {
      /* ### error if already exists */
      dictionaryAtPut(dictionaryOOP, name, nilOOP);
    }
  }

  if (dictionarySize(dictionaryOOP) == 0) {
    dictionaryOOP = nilOOP;
  }

  return (dictionaryOOP);
}

OOP makePoolArray(classOOP, poolNames)
OOP	classOOP;
Byte 	*poolNames;
{
  OOP		pools, name;
  int		len, numPools, i;
  Byte		*p, *e;

  if (poolNames == nil) {
    poolNames = (Byte *)"";
  }
  
  /* count the number of new pool names */
  for (p = poolNames, numPools = 0; *p; ) {
    parseVariableName(&p, &e);
    if (p != e) {
      numPools++;
      p = e;
    }
  }

  pools = arrayNew(numPools);

  for (p = poolNames, i = 1; *p; i++) {
    name = scanName(&p);
    if (!isNil(name)) {
      /* ### error if already exists in parent?,
	 or if value isn't a dictionary */
      /* ### should I keep these as names?  or associations?  Should
	 I look up the names somewhere other than in the smalltalk
	 dictionary?  Need to check for undefineds? */
      arrayAtPut(pools, i, dictionaryAt(smalltalkDictionary, name));
    }
  }

  if (numPools == 0) {		/* ### maybe change this to leave empty array */
    pools = nilOOP;
  }

  return (pools);
}

/*
 *	static OOP scanName(pp)
 *
 * Description
 *
 *	Scan a variable name (letters and digits, initial letter), and return a
 *	symbol for it.
 *
 * Inputs
 *
 *	pp    : pointer to a pointer to the start of the string to be scanned.
 *		May be pointing at either whitespace or start of variable.  At
 *		end, points to first character after the parsed variable name,
 *		which may be NUL. 
 *
 * Outputs
 *
 *	Symbol for variable name, or nilOOP if none found.  pp points to first
 *	char after symbol name (if any).
 */
static OOP scanName(pp)
Byte	**pp;
{
  Byte		*end, *str;
  long		len;

  parseVariableName(pp, &end);
  len = end - *pp;
  if (len == 0) {
    return (nilOOP);
  }


  str = (Byte *)alloca(len + 1);
  strncpy(str, *pp, len);
  str[len] = '\0';

  *pp = end;

  return (internString(str));
}

static void parseVariableName(pp, endp)
Byte	**pp, **endp;
{
  register Byte	*p, *e;

  p = *pp;
  e = *endp;

  while (isWhiteSpace(*p)) {
    p++;
  }

  /* ### check for non-null here and not alnum; we've jammed on a bogus char
     and it's an error */

  /* variable name extends from p to e-1 */
  for (e = p; *e; e++) {
    if (!isalnum(*e)) {
      break;
    }
  }

  *pp = p;
  *endp = e;
}

static Boolean isWhiteSpace(c)
Byte	c;
{
   return (c == ' ' || c == '\t' || c == '\n' || c == '\f');
}

OOP internStringOOP(stringOOP)
OOP	stringOOP;
{
  int		len;

  len = stringOOPLen(stringOOP);
  return (internCountedString(stringOOPChars(stringOOP), len));
}

OOP internString(str)
char	*str;
{
  int		len;

  len = strlen(str);
  return (internCountedString(str, len));
}

static OOP internCountedString(str, len)
Byte	*str;
int	len;
{
  unsigned long	index;
  int		i;
  SymLink	link;
  Symbol	symbol;
  OOP		symbolOOP, linkOOP;

  index = (hashString(str, len) % numOOPs(oopToObj(symbolTable))) + 1;
  for (linkOOP = arrayAt(symbolTable, index); !isNil(linkOOP);
       linkOOP = link->nextLink) {
    link = (SymLink)oopToObj(linkOOP);
    if (isSameString(str, link->symbol, len)) {
      return (link->symbol);
    }
  }

  /* no match, have to add it to head of list */
  
  symbol = (Symbol)newInstanceWith(symbolClass, (long)len);
  strncpy(symbol->symString, str, len);
  symbolOOP = allocOOP(symbol);
  symbolOOP->emptyBytes = (4 - len) & 3;

  link = (SymLink)newInstance(symLinkClass);
  link->nextLink = arrayAt(symbolTable, index);
  maybeMoveOOP(link->nextLink);	/* make sure it's here with us */
  link->symbol = symbolOOP;
  arrayAtPut(symbolTable, index, allocOOP(link));
  return (symbolOOP);
}


static Symbol makeNewSymbol(str, len)
Byte	*str;
int	len;
{
  Symbol	symbol;

  symbol = (Symbol)newInstanceWith(symbolClass, (long)len);
  strncpy(symbol->symString, str, len);

  return (symbol);
}

#ifdef preserved_code /* Sun Oct  8 10:30:55 1989 */
/**/static Boolean isSymbol(oop)
/**/OOP	oop;
/**/{
/**/  if (isNil(oop)) {
/**/    return (false);
/**/  }
/**/
/**/  /* ??? are there subclasses of symbol ... do we need to worry about it? */
/**/  return (oop->object->objClass == symbolClass);
/**/}
#endif /* preserved_code Sun Oct  8 10:30:55 1989 */

static Boolean isSameString(str, oop, len)
char	*str;
OOP	oop;
int	len;
{
  if (stringOOPLen(oop) == len) {
    return (strncmp(str, ((Symbol)oopToObj(oop))->symString, len) == 0);
  }

  return (false);
}

int stringOOPLen(oop)
OOP	oop;
{
  return (oopSizeBytes(oop) - oop->emptyBytes);
}


unsigned long hashString(str, len)
char	*str;
int	len;
{
#ifdef dragon_book
/* from Dragon Book, p436 */
  unsigned long hashVal = 0, carry;

  for ( ; len > 0; str++, len--) {
    hashVal = (hashVal << 4) + (*str);
    if (carry = (hashVal & 0xf0000000)) {
      hashVal ^= (carry >> 24);
      hashVal ^= carry;
    }
  }

  return (hashVal);
#else
  long	result = 0L;

  for (; len > 0; str++, len--) {
    result = (result << 1) /* | (result < 0) */;
    result ^= *str;
  }

  return ((unsigned long)result);
#endif /* ! dragon_book */
}

void printSymbol(symbol)
OOP	symbol;
{
  if (isNil(symbol)) {
    printf(nilName);
  } else {
    printString(symbol);
  }
}

void printString(string)
OOP	string;
{
  int		len;

  len = stringOOPLen(string);
  fwrite(oopToObj(string)->data, sizeof(Byte), len, stdout);
  fflush(stdout);
}

/*
 *	char *symbolAsString(symbolOOP)
 *
 * Description
 *
 *	Given a symbol, this routine returns a C string that corresponds to the
 *	name of the symbol.  The returned value is a pointer to a static area,
 *	so if it's to be used for anything other than immediate output, the
 *	caller needs to make a copy of the retured string.
 *
 * Inputs
 *
 *	symbolOOP: 
 *		An OOP for a symbol
 *
 * Outputs
 *
 *	Pointer to a C string that contains the symbol name.
 */
char *symbolAsString(symbolOOP)
OOP	symbolOOP;
{
  static char	stringBuf[256];	/* probably large enough for most symbols */
  int		len;
  Symbol	symbol;

  symbol = (Symbol)oopToObj(symbolOOP);

  len = stringOOPLen(symbolOOP);
  if (len >= sizeof(stringBuf)) {
    errorf("symbol name too long: %d, max is %d", len, sizeof(stringBuf));
  }
  strncpy(stringBuf, symbol->symString, len);
  stringBuf[len] = '\0';
  return (stringBuf);
}

/*
 *	void printSymbols()
 *
 * Description
 *
 *	This routine is used for symbol table debugging only.
 *
 */
void printSymbolChain();	/* ### hack */
void printSymbols()
{
  unsigned long	i;
  
  for (i = 1; i <= numOOPs(oopToObj(symbolTable)); i++) {
    printf("Table[%d]:\n", i);
    printSymbolChain(i);
    printf("\n");
  }
}

void printSymbolChain(i)
unsigned long i;
{
  SymLink	link;
  OOP		linkOOP;
  for (linkOOP = arrayAt(symbolTable, i); !isNil(linkOOP);
       linkOOP = link->nextLink) {
    link = (SymLink)oopToObj(linkOOP);
    printf("%d:", linkOOP - oopTable); /* ### */
    printSymbol(link->symbol);
    printf("->");
  }
}


void printSymLink(symLinkOOP)
OOP	symLinkOOP;
{
  SymLink	link;

  link = (SymLink)oopToObj(symLinkOOP);

  printObject(link->symbol);
}

void initSymbols()
{
  andColonSymbol 	        = internString("and:");
  andSymbol			= internString("&");
  atColonPutColonSymbol         = internString("at:put:");
  atColonSymbol 	        = internString("at:");
  atEndSymbol 		        = internString("atEnd");
  bitAndColonSymbol 	        = internString("bitAnd:");
  bitOrColonSymbol 	        = internString("bitOr:");
  bitShiftColonSymbol 	        = internString("bitShift:");
  blockCopyColonSymbol 	        = internString("blockCopy:");
  byteArraySymbol		= internString("byteArray");
  charSymbol                    = internString("char");
  classSymbol 		        = internString("class");
  cObjectSymbol                 = internString("cObject");
  divideSymbol 		        = internString("/");
  doColonSymbol 	        = internString("do:");
  doesNotUnderstandColonSymbol  = internString("doesNotUnderstand:");
  doubleSymbol                  = internString("double");
  equalSymbol 		        = internString("=");
  falseSymbol			= internString("false");
  greaterEqualSymbol 	        = internString(">=");
  greaterThanSymbol 	        = internString(">");
  ifFalseColonIfTrueColonSymbol = internString("ifFalse:ifTrue:");
  ifFalseColonSymbol 	        = internString("ifFalse:");
  ifTrueColonIfFalseColonSymbol = internString("ifTrue:ifFalse:");
  ifTrueColonSymbol 	        = internString("ifTrue:");
  integerDivideSymbol 	        = internString("//");
  intSymbol                     = internString("int");
  lessEqualSymbol 	        = internString("<=");
  lessThanSymbol 	        = internString("<");
  longSymbol                    = internString("long");
  minusSymbol 		        = internString("-");
  newColonSymbol 	        = internString("new:");
  newSymbol 		        = internString("new");
  nextPutColonSymbol 	        = internString("nextPut:");
  nextSymbol 		        = internString("next");
  nilSymbol			= internString("nil");
  notEqualSymbol 	        = internString("~=");
  notSameObjectSymbol		= internString("~~");
  orColonSymbol 	        = internString("or:");
  orSymbol			= internString("|");
  plusSymbol 		        = internString("+");
  remainderSymbol 	        = internString("\\");
  sameObjectSymbol 	        = internString("==");
  selfSymbol			= internString("self");
  sizeSymbol 		        = internString("size");
  smalltalkSymbol		= internString("smalltalk");
  stringOutSymbol               = internString("stringOut");
  stringSymbol                  = internString("string");
  superSymbol			= internString("super");
  symbolSymbol                  = internString("symbol");
  thisContextSymbol		= internString("thisContext");
  timesSymbol 		        = internString("*");
  trueSymbol			= internString("true");
  unknownSymbol                 = internString("unknown");
  valueColonSymbol 	        = internString("value:");
  valueColonValueColonSymbol	= internString("value:value:");
  valueColonValueColonValueColonSymbol = internString("value:value:value:");
  valueSymbol 		        = internString("value");
  valueWithArgumentsColonSymbol = internString("valueWithArguments:");
  variadicSymbol                = internString("variadic");
  voidSymbol                    = internString("void");
  whileFalseColonSymbol	        = internString("whileFalse:");
  whileTrueColonSymbol 	        = internString("whileTrue:");
}
