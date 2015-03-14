/***********************************************************************
 *
 *	C - Smalltalk Interface module
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
 * sbyrne     4 Jun 89	  Added Smalltalk data conversion type.
 *
 * sbyrne    29 May 89	  Created.
 *
 */

/* Define this to enable initialization of the SunView hacks in the
   ./examples directory */
/* #define SUN_WIN_HACKS */

#include "mst.h"
#include "mstinterp.h"
#include "mstdict.h"
#include "mstoop.h"
#include "mstsym.h"

#define ARG_VEC_SIZE		20 /* 20 ints, 10 longs or ptrs, 5 dbls */


typedef enum {
  intAlign,
  longAlign,
  ptrAlign,
  doubleAlign
} AlignmentType;

typedef enum {			/* types for C parameters */
  unknownType,			/* when there is no type a priori */
  charType,
  stringType,
  stringOutType,		/* for things that modify string params */
  symbolType,
  byteArrayType,
  intType,
  longType,
  doubleType,
  voidType,			/* valid only as a return type */
  variadicType,			/* for parameters, this param is an array
				   to be interpreted as arguments.  Note that
				   only simple conversions are performed in
				   this case. */
  cObjectType,			/* a C object is being passed */
  smalltalkType			/* no conversion to-from C...C sees this
				   as "void *".  */
} CDataType;

typedef struct CFuncDescriptorStruct {
  OBJ_HEADER;
  OOP		cFunction;
  OOP		cFunctionName;
  OOP		returnType;
  OOP		numFixedArgs;
  OOP		argTypes[1];	/* variable length, really numFixedArgs long */
} *CFuncDescriptor;

typedef struct SymbolTypeMapStruct {
  OOP		*symbol;
  CDataType	type;
} SymbolTypeMap;

typedef struct StringInfoStruct {
  Byte		*cString;
  OOP		stringOOP;
  CDataType	returnedType;
} StringInfo;

typedef union CParamUnionUnion {
  int		intVal;
  long		longVal;
  voidPtr	ptrVal;
  double	doubleVal;
  int		valueVec[sizeof(double) / sizeof(int)];
} CParamUnion;

typedef struct 	CFuncInfoStruct {
  char		*funcName;
  void		(*funcAddr)();
} CFuncInfo;

void				defineCFunc();

static void 			pushObj(), callCFunction(),
				badType(), pushSmalltalkObj();
static CDataType 		getCType();
static CFuncDescriptor 		getCFuncDescriptor();
static OOP			classifyTypeSymbol();

static CFuncInfo		cFuncInfo[100], *cFuncIndex = cFuncInfo;
static int			cArgVec[ARG_VEC_SIZE];
static int			*cArg;
static StringInfo		stringInfo[ARG_VEC_SIZE], *sip;
/* printable names for corresponding C types */
static char			*cTypeName[] = {
  "void?",			/* unknownType */
  "char",			/* charType */
  "char *",			/* stringType */
  "char *",			/* stringType */
  "char *",			/* symbolType */
  "char *",			/* byteArrayType */
  "int",			/* intType */
  "long",			/* longType */
  "double",			/* doubleType */
  "void?",			/* voidType */
  "var args?",			/* variadicType */
  "void *",			/* cObjectType */
  "void *",			/* smalltalkType */
};

static SymbolTypeMap	symbolTypeMap[] = {
  &unknownSymbol, unknownType,
  &charSymbol, charType,
  &stringSymbol, stringType,
  &stringOutSymbol, stringOutType,
  &symbolSymbol, symbolType,
  &byteArraySymbol, byteArrayType,
  &intSymbol, intType,
  &longSymbol, longType,
  &doubleSymbol, doubleType,
  &voidSymbol, voidType,
  &variadicSymbol, variadicType,
  &cObjectSymbol, cObjectType,
  &smalltalkSymbol, smalltalkType,
  nil, unknownType
};

/* the arg vec pointer must be = 0 mod alignments[align] */
/* This is quite likely to be machine dependent.  Currently it is set up
 * to work correctly on sun2's, sun3's and sun4's */
static int 		alignments[] = {
  sizeof(int),			/* intType */
  sizeof(long),			/* longType */
  sizeof(voidPtr),		/* ptrType */
  DOUBLE_ALIGNMENT		/* doubleType */
};

static int		typeSizes[] = {
  sizeof(int),			/* intType */
  sizeof(long),			/* longType */
  sizeof(voidPtr),		/* ptrType */
  sizeof(double)		/* doubleType */
};

/*
 *	void marli(n)
 *
 * Description
 *
 *	Test/example C function.
 *
 * Inputs
 *
 *	n     : number of times to emit message.
 *
 */
void marli(n)
int n;
{
  int		i;

  for (i = 0; i < n; i++) {
    printf("Marli loves Steve!!!\n");
  }
}

void initCFuncs()
{
  extern void marli(), windowLoop();
  extern char *jeff(), *getAttrName();
  extern voidPtr *getAttrValue();
  extern void window_create();
  extern int system();
  extern char *getenv();

  defineCFunc("system", system);
  defineCFunc("getenv", getenv);
  defineCFunc("marli", marli);
#ifdef SUN_WIN_HACKS
  defineWindowFuncs();
#endif /* SUN_WIN_HACKS */

#ifdef notdefined
  defineCFunc("jeff", jeff);
  defineCFunc("getAttrName", getAttrName);
  defineCFunc("getAttrValue", getAttrValue);
#endif
}

void defineCFunc(funcName, funcAddr)
char	*funcName;
void	(*funcAddr)();
{
  cFuncIndex->funcName = funcName;
  cFuncIndex->funcAddr = funcAddr;
  cFuncIndex++;
}

void (*lookupFunction(funcName))()
char	*funcName;
{
  CFuncInfo	*fip;

  for (fip = cFuncInfo; fip < cFuncIndex; fip++) {
    if (strcmp(funcName, fip->funcName) == 0) {
      return (fip->funcAddr);
    }
  }
  return (nil);
}



/*
 *	void invokeCRoutine(numArgs, methodOOP)
 *
 * Description
 *
 *	Invokes a C routine.  The Smalltalk arguments have been popped off the
 *	Smalltalk stack when this routine returns.
 *
 * Inputs
 *
 *	numArgs: 
 *		
 *	methodOOP: 
 *		
 *
 */
void invokeCRoutine(numArgs, methodOOP)
long	numArgs;
OOP	methodOOP;
{
  CFuncDescriptor desc;
  CDataType	cType;
  OOP		oop; /* oopArgVec[32]; */
  Byte		*stringArg;
  int		i;

  cArg = cArgVec;
  
  desc = getCFuncDescriptor(methodOOP);

  sip = stringInfo;

  for (i = 0; i < numArgs; i++) {
    oop = stackAt(numArgs - i - 1);
    cType = getCType(desc, i);
    pushSmalltalkObj(oop, cType);
  }

  popNOOPs(numArgs);

  callCFunction(desc);

  /* Fixup all returned string variables */
  for ( ; sip-- != stringInfo; ) {
    if (sip->returnedType == stringOutType) {
      setOOPString(sip->stringOOP, sip->cString);
    }
    free(sip->cString);
  }
}

static CFuncDescriptor getCFuncDescriptor(methodOOP)
OOP	methodOOP;
{
  OOP		associationOOP, descOOP;

  associationOOP = methodLiteralExt(methodOOP, 0);
  descOOP = associationValue(associationOOP);
  return ((CFuncDescriptor)oopToObj(descOOP));
}

static CDataType getCType(desc, index)
CFuncDescriptor desc;
int	index;
{
  if (index < toInt(desc->numFixedArgs)) {
    return ((CDataType)toInt(desc->argTypes[index]));
  } else {
    return (unknownType);
  }
}

static void pushSmalltalkObj(oop, cType)
OOP	oop;
CDataType cType;
{
  OOP		class;
  int		i;
  CParamUnion	u;

  if (cArg - cArgVec >= ARG_VEC_SIZE) {
    errorf("Attempt to push more than %d ints; extra parameters ignored",
	   ARG_VEC_SIZE);
    return;
  }

  if (isInt(oop)) {
    class = integerClass;
  } else if (oop == trueOOP || oop == falseOOP) {
    class = booleanClass;
  } else {
    class = oopClass(oop);
  }

  if (cType == smalltalkType) {
    u.ptrVal = (voidPtr)oop;
    pushObj(&u, ptrAlign);
  } else if (class == integerClass) {
    if (cType == longType || cType == unknownType) {
      u.longVal = toInt(oop);
      pushObj(&u, longAlign);
    } else if (cType == intType || cType == charType) {
      u.intVal = toInt(oop);
      pushObj(&u, intAlign);
    } else {
      badType("Integer", cType);
    }
  } else if (class == booleanClass) {
    if (cType == intType || cType == charType || cType == unknownType) {
      u.intVal = (oop == trueOOP);
      pushObj(&u, intAlign);
    } else if (cType == longType) {
      u.longVal = (oop == trueOOP);
      pushObj(&u, longAlign);
    } else {
      badType("Boolean", cType);
    }
  } else if (class == charClass) {
    if (cType == charType || cType == unknownType) {
      u.intVal = charOOPValue(oop);
      pushObj(&u, intAlign);
    } else {
      badType("Character", cType);
    }
  } else if (class == stringClass) {
    if (cType == stringType || cType == stringOutType
	|| cType == unknownType) {
      if (sip - stringInfo >= ARG_VEC_SIZE) {
	errorf("Too many string arguments, max is %d.  Extra ignored",
	       ARG_VEC_SIZE);
      }
      sip->cString = toCString(oop);
      u.ptrVal = (voidPtr)sip->cString;
      sip->stringOOP = oop;
      sip->returnedType = cType;
      sip++;
      pushObj(&u, ptrAlign);
    } else {
      badType("String", cType);
    }
  } else if (class == symbolClass) {
    if (cType == symbolType || cType == stringType || cType == unknownType) {
      if (sip - stringInfo >= ARG_VEC_SIZE) {
	errorf("Too many string arguments, max is %d.  Extra ignored",
	       ARG_VEC_SIZE);
      }
      sip->cString = toCString(oop);
      u.ptrVal = (voidPtr)sip->cString;
      sip->stringOOP = oop;
      sip->returnedType = cType;
      sip++;
      pushObj(&u, ptrAlign);
    } else {
      badType("Symbol", cType);
    }
  } else if (class == byteArrayClass) {
    if (cType == byteArrayType || cType == unknownType) {
      if (sip - stringInfo >= ARG_VEC_SIZE) {
	errorf("Too many string arguments, max is %d.  Extra ignored",
	       ARG_VEC_SIZE);
      }
      sip->cString = toByteArray(oop);
      u.ptrVal = (voidPtr)sip->cString;
      sip->stringOOP = oop;
      sip->returnedType = cType;
      sip++;
      pushObj(&u, ptrAlign);
    } else {
      badType("ByteArray", cType);
    }
  } else if (class == floatClass) {
    if (cType == doubleType || cType == unknownType) {
      u.doubleVal = floatOOPValue(oop);
      pushObj(&u, doubleAlign);
    } else {
      badType("Float", cType);
    }
  } else if (class == cObjectClass) { 
    if (cType == cObjectType || cType == unknownType) {
      u.ptrVal = cObjectValue(oop);
      pushObj(&u, ptrAlign);
    } else {
      badType("CObject", cType);
    }
  } else if (class == undefinedObjectClass) {
    switch (cType) {
    case cObjectType:
    case stringType:
    case symbolType:
    case unknownType:
      u.ptrVal = nil;
      pushObj(&u, ptrAlign);
      break;

    default:
      badType("UndefinedObject", cType);
    }
  } else if (class == arrayClass) {
    for (i = 1; i <= numOOPs(oopToObj(oop)); i++) {
      pushSmalltalkObj(arrayAt(oop, i), unknownType);
    }
  }
  
}

static void pushObj(up, align)
CParamUnion *up;
AlignmentType align;
{
  int i, alignInts;

  alignInts = alignments[ENUM_INT(align)] / sizeof(int);

  /* Align the stack properly */
  if ((cArg - cArgVec) % alignInts) {
    cArg += alignInts - ((cArg - cArgVec) % alignInts);
  }
  
  for (i = 0; i < typeSizes[ENUM_INT(align)] / sizeof(int); i++) {
    if (cArg - cArgVec >= ARG_VEC_SIZE) {
      errorf("Too many parameters, max = %d.  Extra parameters ignored",
	     ARG_VEC_SIZE);
      return;
    }
    *cArg++ = up->valueVec[i];
  }
}

static void callCFunction(desc)
CFuncDescriptor desc;
{  
  int		intResult;
  long		longResult;
  double	doubleResult;
  int		(*cFunction)();
  CDataType	returnType;

  cFunction = (int (*)())cObjectValue(desc->cFunction);
  returnType = (CDataType)toInt(desc->returnType);

  switch (returnType) {
  case voidType:
    (*cFunction)(
      cArgVec[0],  cArgVec[1],  cArgVec[2],  cArgVec[3],
      cArgVec[4],  cArgVec[5],  cArgVec[6],  cArgVec[7],
      cArgVec[8],  cArgVec[9],  cArgVec[10], cArgVec[11],
      cArgVec[12], cArgVec[13], cArgVec[14], cArgVec[15],
      cArgVec[16], cArgVec[17], cArgVec[18], cArgVec[19]);
    break;
  case charType:
  case intType:
    intResult = (*cFunction)(
      cArgVec[0],  cArgVec[1],  cArgVec[2],  cArgVec[3],
      cArgVec[4],  cArgVec[5],  cArgVec[6],  cArgVec[7],
      cArgVec[8],  cArgVec[9],  cArgVec[10], cArgVec[11],
      cArgVec[12], cArgVec[13], cArgVec[14], cArgVec[15],
      cArgVec[16], cArgVec[17], cArgVec[18], cArgVec[19]);
    switch (returnType) {
    case intType: 
      setStackTop(fromInt((long)intResult));
      break;
    case charType:
      setStackTop(charOOPAt((Byte)intResult));
      break;
    }
    break;

  case longType:
  case stringType:
  case symbolType:
  case cObjectType:
  case smalltalkType:
    longResult = (*(long (*)())cFunction)(
      cArgVec[0],  cArgVec[1],  cArgVec[2],  cArgVec[3],
      cArgVec[4],  cArgVec[5],  cArgVec[6],  cArgVec[7],
      cArgVec[8],  cArgVec[9],  cArgVec[10], cArgVec[11],
      cArgVec[12], cArgVec[13], cArgVec[14], cArgVec[15],
      cArgVec[16], cArgVec[17], cArgVec[18], cArgVec[19]);
    switch (returnType) {
    case longType:
      setStackTop(fromInt(longResult));
      break;
    case stringType:
      if (longResult == 0) {
	setStackTop(nilOOP);
      } else {
	setStackTop(stringNew((char *)longResult));
      }
      break;
    case symbolType:
      if (longResult == 0) {
	setStackTop(nilOOP);
      } else {
	setStackTop(internString((char *)longResult));
      }
      break;
    case cObjectType:
      if (longResult == 0) {
	setStackTop(nilOOP);
      } else {
	setStackTop(cObjectNew((voidPtr)longResult));
      }
      break;
    case smalltalkType:
      setStackTop((OOP)longResult);
      break;
    }
    break;

  case doubleType:
    doubleResult = (*(double (*)())cFunction)(
      cArgVec[0],  cArgVec[1],  cArgVec[2],  cArgVec[3],
      cArgVec[4],  cArgVec[5],  cArgVec[6],  cArgVec[7],
      cArgVec[8],  cArgVec[9],  cArgVec[10], cArgVec[11],
      cArgVec[12], cArgVec[13], cArgVec[14], cArgVec[15],
      cArgVec[16], cArgVec[17], cArgVec[18], cArgVec[19]);
    setStackTop(floatNew(doubleResult));
    break;

  default:
    errorf("Invalid C function return type specified, index %d\n",
	   returnType);
    break;
  }

}

static void badType(smalltalkTypeName, cType)
char	*smalltalkTypeName;
CDataType cType;
{
  errorf("Attempt to pass a %s as a %s", smalltalkTypeName,
	 cTypeName[ENUM_INT(cType)]);
}


OOP makeDescriptor(funcNameOOP, returnTypeOOP, argsOOP)
OOP	funcNameOOP, returnTypeOOP, argsOOP;
{
  char		*funcName;
  void		(*funcAddr)();
  int		numArgs, i;
  CFuncDescriptor desc;

  funcName = (char *)toCString(funcNameOOP);
  funcAddr = lookupFunction(funcName);

  if (argsOOP == nilOOP) {
    numArgs = 0;
  } else {
    numArgs = numOOPs(oopToObj(argsOOP));
  }

  /*
   * since these are all either ints or new objects, I'm not moving the
   * oops
   */
  desc = (CFuncDescriptor)newInstanceWith(cFuncDescriptorClass, numArgs);
  desc->cFunction = cObjectNew(funcAddr);
  desc->cFunctionName = stringNew(funcName);
  desc->numFixedArgs = fromInt(numArgs);
  desc->returnType = classifyTypeSymbol(returnTypeOOP);
  for (i = 1; i <= numArgs; i++) {
    desc->argTypes[i - 1] = classifyTypeSymbol(arrayAt(argsOOP, i));
  }

  return (allocOOP(desc));
}

static OOP classifyTypeSymbol(symbolOOP)
OOP	symbolOOP;
{
  SymbolTypeMap	*sp;
  Byte		*symbolName;

  for (sp = symbolTypeMap; sp->symbol != nil; sp++) {
    if (*sp->symbol == symbolOOP) {
      return (fromInt(sp->type));
    }
  }

  symbolName = toCString(symbolOOP); /* yeah yeah...but they have the same
				        representation! */
  errorf("Unknown data type symbol: %s", symbolName);

  return (fromInt(unknownType));
}

/*
 *	void restoreCFuncDescriptor(cFuncDescOOP)
 *
 * Description
 *
 *	This routine is called during image loading to restore a C function
 *	descriptor pointer.  This is because between the time that the image
 *	was made and now, the executable image may have changed, so any
 *	reference to the C function address may be invalid.  We therefore just
 *	perform the function lookup again and use that value.
 *
 * Inputs
 *
 *	cFuncDescOOP: 
 *		A C function descriptor object to be adjusted.  Contains the
 *		name of the function to be looked up.
 *
 */
void restoreCFuncDescriptor(cFuncDescOOP)
OOP	cFuncDescOOP;
{
  CFuncDescriptor desc;
  void		(*funcAddr)();
  char		*funcName;

  desc = (CFuncDescriptor)oopToObj(cFuncDescOOP);
  funcName = (char *)toCString(desc->cFunctionName);
  funcAddr = lookupFunction(funcName);
  setCObjectValue(desc->cFunction, funcAddr);
}
