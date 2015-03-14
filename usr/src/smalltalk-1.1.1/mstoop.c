/***********************************************************************
 *
 *	Object Table maintenance module.
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
 * sbyrne     8 Apr 90	  Changed oopFree to oopValid to fix the bug with
 *			  someInstance losing after GC's due to objects that
 *			  have non-free OOP table entries, but point to freed
 *			  objects.
 *
 * sbyrne     7 Apr 90	  Increased mem space size to 4M.  This can be
 *			  decreased as necessary.
 *
 * sbyrne    24 Feb 90	  Update to change log: there are no longer any
 *			  explicitly allocated OOPs due to the new symbol table
 *			  structure; the comment below is now a noop.
 *
 * sbyrne    20 Sep 89	  Added oop table slot GC'ing.  I'm not dealing with
 *			  oop table slots that are explictly allocated; I
 *			  believe that most OOP slots are not explicitly chosen
 *			  and so not running the incremental reclaimer for that
 *			  case shouldn't hurt us.
 *
 * sbyrne    12 Sep 89	  Much of the garbage collector's operation depends on
 *			  the fact that only 1 flip will occur between any two
 *			  operations (such as a compilation, or a byte-code).
 *			  The code would be much more complex if this were not
 *			  the case, and I'm not sure that things would even be
 *			  possible if this were not the case.  Anyway, there is
 *			  code in this routine to check for that eventuality
 *			  and to halt the system if it occurs.
 *
 * sbyrne     6 Sep 89	  started implementing the garbage collector (YAY!!!)
 *
 * sbyrne    13 Jan 89	  Created.
 *
 */


#include <stdio.h>
#include "mst.h"
#include "mstoop.h"
#include "mstdict.h"
#include "mstsave.h"
#include "mstcomp.h"

/* Size of the object semi spaces, in bytes */
#define	K		1024

#ifndef atarist
#define MEM_SPACE_SIZE		(4 * K * K) 
#else
#define MEM_SPACE_SIZE		(1152 * K) 
#endif

#define oopReclaimFactor	4 /* how many oops to reclaim per oop alloc */
/* #define INLINE_MACROS */


/* Define this flag to turn on debugging code for OOP table management */
/* #define OOP_DEBUGGING */



#define alignSize(size) \
 ( ((size) + DOUBLE_ALIGNMENT - 1) & ~(DOUBLE_ALIGNMENT - 1) )

#define objSpace(obj) \
  ((char *)(obj) >= spaces[1].space)
 

typedef struct CompiledMethodStruct *Method;

extern Boolean		regressionTesting;

/* These are the real OOPS for nil, true, and false */
OOP			nilOOP, trueOOP, falseOOP;

/* The OOP table.  This contains a pointer to the object, and some flag
   bits indicating which semispace the pointed-to object lives in.  Some
   of the bits indicate the difference between the allocated length (stored 
   in the object itself, and the real length, for things like byte strings 
   that may not be an even multiple of 4 (== sizeof(void *)). */
struct OOPStruct	oopTable[TOTAL_OOP_TABLE_SLOTS];

/* This is the head of the free list.  The free list is maintained in the 
   oop table.  Each OOP on the free list has a bit indicating that it's free,
   a pointer to the next free OOP, and a pointer to the previous free OOP.
   when this points at NIL, we're out of space */
OOP			freeOOPs;
int			numFreeOOPs;

/* The indices of which is the current new space (toSpace) and which is
   the current old space (fromSpace).  At GC flip time, these two are 
   interchanged. */
char/*int*/		fromSpace, toSpace;

Boolean			gcFlipped, gcState, gcMessage;
int			gcFlipCounter;


/* This vector holds the storage for all the Character objects in the system.
   Since all character objects are unique, we pre-allocate space for 256 of
   them, and treat them as special built-ins when doing garbage collection.*/
CharObject		charObjectTable[NUM_CHAR_OBJECTS];

/* This is "nil" object in the system.  That is, the single instance of the
   UndefinedObject class, which is called "nil". */
struct NilObjectStruct	nilObject;

/* These represent the two boolean objects in the system, true and false.
   This is the object storage for those two objects. 
   false == &booleanObjects[0], true == &booleanObjects[1] */
struct BooleanObjectStruct booleanObjects[2];

struct memorySpaceStruct {
  char		*space;
  char		*allocPtr;
  char		*copyPtr;
  char		*scanPtr;
  unsigned long	size;
};

/* These two variables represent information about the semispaces.  spaces
   holds the information for each semispace (basically the pointer to the
   base of the space, and the pointers into it for allocation, copying, and
   scanning.  curSpace holds the address of one of the two semispace data
   structures, and is used by the garbage collector */
static struct memorySpaceStruct spaces[2];
static struct memorySpaceStruct *curSpace;

static Object		curObject;
static long		curObjectSize, copyQuota;
static double		copyRate;
static double		copyRateAdjustment = 1.50; /* copy 50% more than last time */
static long		oopTableIndex;

static Object		moveObject();
static void		initCharObject(), moveRootOOPs(),
			initSpace(), markBuiltinOOPs(), 
			copyReferencedObjects(), finishOOPScan(), clearOldOOPs();

#ifdef remove_soon /* Tue May 15 01:23:08 1990 */
/**/static char marks[TOTAL_OOP_TABLE_SLOTS];
/**/static void zeroMarks();
/**/static int traceOOPStuff = 0;
#endif /* remove_soon Tue May 15 01:23:08 1990 */
/*
 *	void initOOPTable()
 *
 * Description
 *
 *	Initialize the OOP table.  Initially, all the OOPs are on the OOP free
 *	list so that's just how we initialize them.  We do as much
 *	initialization as we can, but we're called before classses are
 *	defined, so things that have definite classes must wait until
 *	the classes are defined.
 *
 */
void initOOPTable()
{
  int		i;

#ifdef remove_soon /* Mon May 14 23:41:50 1990 */
/**/zeroMarks();
#endif /* remove_soon Mon May 14 23:41:50 1990 */

  numFreeOOPs = OOP_TABLE_SIZE;

  for (i = 0; i < OOP_TABLE_SIZE; i++) {
    oopTable[i].object = (Object)&oopTable[i+1];
    oopTable[i].isFree = 1;	/* setting this free bit */
  }
  oopTable[i-1].object = nil;

  freeOOPs = oopTable;

  nilOOP	= &oopTable[nilOOPIndex];
  trueOOP	= &oopTable[trueOOPIndex];
  falseOOP	= &oopTable[falseOOPIndex];

  nilOOP->isFree        = trueOOP->isFree	= falseOOP->isFree	= 0;
  nilOOP->emptyBytes    = trueOOP->emptyBytes	= falseOOP->emptyBytes	= 0;

  nilOOP->object	= (Object)&nilObject;
  nilObject.objSize	= ROUNDED_WORDS(sizeof(struct NilObjectStruct));

  trueOOP->object		= (Object)&booleanObjects[0];
  falseOOP->object		= (Object)&booleanObjects[1];
  booleanObjects[0].objSize	= ROUNDED_WORDS(sizeof(struct BooleanObjectStruct));
  booleanObjects[1].objSize	= ROUNDED_WORDS(sizeof(struct BooleanObjectStruct));
  booleanObjects[0].booleanValue= trueOOP;
  booleanObjects[1].booleanValue= falseOOP;
}


/*
 *	void initNil()
 *
 * Description
 *
 *	Initialize the "nil" object.
 *
 */
void initNil()
{
  nilObject.objClass	= undefinedObjectClass;
}

/*
 *	void initBooleans()
 *
 * Description
 *
 *	Initialize the two boolean objects, after their respective classes have
 *	been created.
 *
 */
void initBooleans()
{
  booleanObjects[0].objClass	= trueClass;
  booleanObjects[1].objClass	= falseClass;
}

/*
 *	void initCharTable()
 *
 * Description
 *
 *	Initialize the instances of the class Character, after that class has
 *	been created.
 *
 */
void initCharTable()
{
  int		i;

  for (i = 0; i < NUM_CHAR_OBJECTS; i++) {
    initCharObject(i);
    oopTable[i + CHAR_OBJECT_BASE].object = (Object)&charObjectTable[i];
    oopTable[i + CHAR_OBJECT_BASE].isFree = 0;
    oopTable[i + CHAR_OBJECT_BASE].emptyBytes = 0;
  }
}

/*
 *	static void initCharObject(i)
 *
 * Description
 *
 *	Initialize a single character object.
 *
 * Inputs
 *
 *	i     : The index of the character object, in the range 0..255.
 *
 */
static void initCharObject(i)
int	i;
{
  charObjectTable[i].objSize = ROUNDED_WORDS(sizeof(CharObject));
  charObjectTable[i].objClass = charClass;
  charObjectTable[i].charVal = i;
}

/*
 *	void fixupMetaclassObjects()
 *
 * Description
 *
 *	Called after the fundamental class hierarchy has been defined, this
 *	function goes through and fixes up all the objects in the oop table
 *	that don't have a objClass (objClass == nilOOP).  It's a
 *	chicken-and-egg problem: the metaclassClass doesn't yet exist when the
 *	hierarchy is put together, so after it's created, we have to go back
 *	and fix all the metaclasses that we created.
 *
 */
void fixupMetaclassObjects()
{
  int		i;

  for (i = 0; i < OOP_TABLE_SIZE; i++) {
    if (!oopTable[i].isFree && isNil(oopTable[i].object->objClass)) {
      oopTable[i].object->objClass = metaclassClass;
    }
  }
}

/*
 *	OOP findAnInstance(classOOP)
 *
 * Description
 *
 *	Finds and returns an instance of the class CLASSOOP.  Returns "nil" if
 *	there are no instances present.
 *
 * Inputs
 *
 *	classOOP: 
 *		OOP for a class for which to find an instance
 *
 * Outputs
 *
 *	The first instance of the given class in the OOP table.
 */
OOP findAnInstance(classOOP)
OOP	classOOP;
{
  int		i;

  for (i = 0; i < OOP_TABLE_SIZE; i++) {
    if (!oopTable[i].isFree && oopTable[i].object->objClass == classOOP) {
      return (&oopTable[i]);
    }
  }

  return (nilOOP);
}

/*
 *	long oopIndex(oop)
 *
 * Description
 *
 *	Returns the index within the OOP table of the given OOP.
 *
 * Inputs
 *
 *	oop   : OOP to return index of
 *
 * Outputs
 *
 *	Returned index in the OOP table, in range 0..TOTAL_OOP_TABLE_SLOTS.
 */
long oopIndex(oop)
OOP	oop;
{
  return (oop - oopTable);
}

/*
 *	Boolean oopIndexValid(index)
 *
 * Description
 *
 *	Checks to see if index represents a valid OOP.
 *
 * Inputs
 *
 *	index : a long index into the OOP table, apparently 1 based due to
 *		being called from Smalltalk via a primitive.
 *
 * Outputs
 *
 *	True if the index represents a valid OOP table element, false
 *	otherwise.
 */
Boolean oopIndexValid(index)
long	index;
{
  return (index >= 1 && index <= TOTAL_OOP_TABLE_SLOTS);
}

#ifndef INLINE_MACROS

OOP oopAt(index)
long	index;
{
  return (oopAtMac(index));
}

#endif  /* INLINE_MACROS */

void swapObjects(oop1, oop2)
OOP	oop1, oop2;
{
  struct OOPStruct tempOOP;

  tempOOP = *oop2;
  *oop2 = *oop1;
  *oop1 = tempOOP;
}

OOP charOOPAt(c)
Byte	c;
{
  return (&oopTable[c + CHAR_OBJECT_BASE]);
}

Byte charOOPValue(charOOP)
OOP	charOOP;
{
  return (charOOP - &oopTable[CHAR_OBJECT_BASE]);
}

void printObject(oop)
OOP	oop;
{
  if (isInt(oop)) {
    printf("%d", toInt(oop));
  } else if (isNil(oop)) {
    printf("nil");
  } else if (oop == trueOOP) {
    printf("true");
  } else if (oop == falseOOP) {
    printf("false");
  } else if (oopClass(oop) == charClass) {
    printf("$%c", charOOPValue(oop));
  } else if (oopClass(oop) == floatClass) {
    printf("%#g", floatOOPValue(oop));
  } else if (oopClass(oop) == symbolClass) {
    printf("#"); printSymbol(oop);
  } else if (oopClass(oop) == stringClass) {
    /* ### have to quote embedded quote chars */
    printf("'");
    printString(oop);
    printf("'");
  } else {
    printOOPConstructor(oop);
  }
}

Boolean oopValid(oop)
OOP	oop;
{
  return (!oop->isFree && (oop->oddMark || oop->evenMark));
}

#ifndef INLINE_MACROS

Boolean oopAvailable(index)
long	index;
{
  return (oopAvailableMac(index);
}

#endif /* INLINE_MACROS */

/*
 *	OOP allocOOP(obj)
 *
 * Description
 *
 *	Given an object OBJ, this routine allocates an OOP table slot for it
 *	and returns it.  It marks the OOP so that it indicates the object is in
 *	new space, and that the oop has been referenced on this pass (to keep
 *	the OOP table reaper from reclaiming this OOP).
 *
 * Inputs
 *
 *	obj   : Object that the new OOP should point to.
 *
 * Outputs
 *
 *	An OOP, which is the address of an element in the OOP table.
 */
OOP allocOOP(obj)
Object	obj;
{
  OOP		oop;
  int		i;

  for (i = 0; i < oopReclaimFactor; i++) {
    if (oopTableIndex >= OOP_TABLE_SIZE) {
      break;
    }
    oop = &oopTable[oopTableIndex++];
    if (!oop->isFree) {
      if (!oop->evenMark && !oop->oddMark) {
#ifdef remove_soon /* Mon May 14 23:42:47 1990 */
/**/if (traceOOPStuff) {
/**/printf("freeing %d\n", oopTableIndex - 1);
/**/}
#endif /* remove_soon Mon May 14 23:42:47 1990 */
#ifdef remove_soon /* Tue May 15 02:23:58 1990 */
/**/    if (marks[oopTableIndex - 1]) {
/**/      printf("freeling link %d\n", oopTableIndex -1 );
/**/    }
#endif /* remove_soon Tue May 15 02:23:58 1990 */
	/* we've found a dead one...add it to the free list */
	numFreeOOPs++;
	oop->object = (Object)freeOOPs;
	freeOOPs = oop;
	freeOOPs->isFree = true;
      } else if (toSpace) {
#ifdef remove_soon /* Mon May 14 23:42:54 1990 */
/**/if (oop == &oopTable[100]) {	/* ### */
/**/printf("clearing 101 even\n");
/**/}
#endif /* remove_soon Mon May 14 23:42:54 1990 */
	oop->evenMark = 0;
#ifdef remove_soon /* Tue May 15 02:23:47 1990 */
/**/    if (marks[oopTableIndex - 1]) {
/**/      printf("even marking %d\n", oopTableIndex -1 );
/**/    }
#endif /* remove_soon Tue May 15 02:23:47 1990 */
      } else {
#ifdef remove_soon /* Mon May 14 23:42:59 1990 */
/**/if (oop == &oopTable[100]) {	/* ### */
/**/printf("clearing 101 odd\n");
/**/}
#endif /* remove_soon Mon May 14 23:42:59 1990 */
	oop->oddMark = 0;
#ifdef remove_soon /* Tue May 15 02:23:31 1990 */
/**/    if (marks[oopTableIndex - 1]) {
/**/      printf("odd marking %d\n", oopTableIndex -1);
/**/    }
#endif /* remove_soon Tue May 15 02:23:31 1990 */
      }
#ifdef remove_soon /* Mon May 14 23:42:19 1990 */
/**/if (traceOOPStuff) 
/**/{ int i;
/**/
/**/  i = oop - oopTable;
/**/if (marks[i] != 0) {
/**/  printf("AllocOOP: oop %d value %d even %d odd %d\n", i, marks[i],
/**/	 oop->evenMark, oop->oddMark);
/**/}
/**/marks[i] = 2;			/* mark as freed */
/**/}
#endif /* remove_soon Mon May 14 23:42:19 1990 */
    }
  }

  oop = freeOOPs;

  numFreeOOPs--;

  if (oop == nil) {
    errorf("Ran out of OOP Table slots!!!");
    exit(1);
    /* ### this needs to be fixed */
  }

  if (!oop->isFree) {
    errorf("Allocating allocated OOP!!!");
    exit(0);
  }

  freeOOPs = (OOP)oop->object;

  if (objSpace(obj) != toSpace) {
    obj = moveObject(obj);
  }

#ifdef remove_soon /* Mon May 14 23:43:09 1990 */
/**/if (oop == &oopTable[100] || oop == &oopTable[123]) {	/* ### */
/**/printf("allocating %d\n", oop - oopTable);
/**/debug();
/**/}
#endif /* remove_soon Mon May 14 23:43:09 1990 */

  oop->object = obj;
  oop->isFree = false;
  oop->emptyBytes = 0;
  oop->inSpace = toSpace;
  oop->oddMark = toSpace;
  oop->evenMark = !toSpace;

#ifdef remove_soon /* Mon May 14 23:42:32 1990 */
/**/{ extern int okToPrint;
/**/if (traceOOPStuff && okToPrint) {
/**/  static Boolean dumped = false;
/**/  if (!dumped) {
/**/    OOP		o;
/**/    dumped = true;
/**/    for (o = oopTable; o < oop; o++) {
/**/      printf("Object %d is ", o - oopTable); printObject(o);
/**/      printf("\n");
/**/    }
/**/  }
/**/if (oop - oopTable > 23000) {
/**/  printf("Allocated %d = ", oop - oopTable); printObject(oop); 
/**/  printf("\n");
/**/}
/**/}
/**/}
#endif /* remove_soon Mon May 14 23:42:32 1990 */
#ifdef remove_soon /* Tue May 15 02:23:14 1990 */
/**/
/**/  if (oopClass(oop) == symLinkClass) {
/**/    marks[oop - oopTable] = 1;
/**/    printf("Allocating link %d: ", oop-oopTable);
/**/    printSymLink(oop);
/**/    printf("\n");
/**/  }
#endif /* remove_soon Tue May 15 02:23:14 1990 */

  return (oop);
}


/*
 *	void setOOPObject(oop, object)
 *
 * Description
 *
 *	Sets the object of OOP to be OBJECT.  Makes sure that the object is in
 *	new space before it assigns it to OOP.
 *
 * Inputs
 *
 *	oop   : an OOP table entry to be assigned into
 *	object: an object that the OOP should point to.
 *
 */
void setOOPObject(oop, object)
OOP	oop;
Object	object;
{
  if (objSpace(object) != toSpace) {
    object = moveObject(object);
  }
  oop->object = object;
  oop->inSpace = toSpace;
}

/*
 *	void initMem()
 *
 * Description
 *
 *	Initialize the memory allocator.  Both semispaces are allocated, and
 *	the various garbage collection flags are set to their initial values.
 *
 */
void initMem()
{
  int		i;

  for (i = 0; i < 2; i++) {
    spaces[i].space = (char *)malloc(MEM_SPACE_SIZE);
    if (spaces[i].space == NULL) {
      printf("Malloc failure; you're out of paging/swapping space\n");
      exit(1);
    }
    initSpace(&spaces[i]);
  }

  curSpace = &spaces[0];
  toSpace = 0;
  fromSpace = !toSpace;
  gcFlipped = false;
  gcState = false;
  gcMessage = true;
  copyRate = 0.0;		/* don't copy anything until first flip */
  copyQuota = 0;
  oopTableIndex = 0;
  markBuiltinOOPs();
  clearGCFlipFlags();
}

Object curSpaceAddr()
{
  return ((Object)curSpace->space);
}

void setSpaceInfo(size)
long	size;
{
  curSpace->copyPtr = curSpace->scanPtr = curSpace->space + size;
  curSpace->size -= size;
}

#ifndef INLINE_MACROS

void clearGCFlipFlags()
{
  clearGCFlipFlagsMac();
}

#endif /* INLINE_MACROS */

/*
 *	Object allocObj(size)
 *
 * Description
 *
 *	Allocate and return space for an object of SIZE bytes.  This basically
 *	means moving the allocation pointer for the current space down by SIZE
 *	bytes, and, if there isn't enough space left, flipping the garbage
 *	collector to switch semispaces.  The space is merely allocated; it is
 *	not initialized.
 *
 * Inputs
 *
 *	size  : size in bytes of the object to allocate.  This will be rounded
 *		by this routine up to a suitable boundary, typically to a 4
 *		byte boundary.
 *
 * Outputs
 *
 *	Address of the newly allocated object.
 */
Object allocObj(size)
long	size;
{
  size = alignSize(size);

  copyReferencedObjects((long)(size * copyRate));
  curSpace->allocPtr -= size;
  if (curSpace->allocPtr <= curSpace->copyPtr) {
    gcFlip();
    curSpace->allocPtr -= size;
  }
    
  return ((Object)curSpace->allocPtr);
}


/*
 *	static void copyReferencedObjects(numBytes)
 *
 * Description
 *
 *	This is the heart of the garbage collector.  It is told that NUMBYTES
 *	have been allocated, and it sweeps through a proportional number of
 *	objects that have been copied into new space, looking for references to
 *	objects that are still in old space.  It has to special case
 *	CompiledMethod objects due to their unusual structure.  It uses global
 *	variables to preserve its state so that it can sweep through only as
 *	many objects as it wants, and resume sweeping on the next allocation
 *	where it left off.
 *
 * Inputs
 *
 *	numBytes: 
 *		The number of bytes that have been allocated.  Used as a
 *		parameter that controls how much space to sweep on this
 *		iteration.
 *
 */
static void copyReferencedObjects(numBytes)
long	numBytes;
{
  Object	result, object;
  OOP		curClass;
  InstanceSpec	instanceSpec;
  int		stepSize, i;
  Method	method;

  copyQuota += numBytes;
  while (curSpace->scanPtr < curSpace->copyPtr
	 && copyQuota > 0) { /* there's more to scan */
    if (curObject == nil) {
      /* if there is no current object, start off with the object's class */
      object = (Object)curSpace->scanPtr;
      curClass = object->objClass;
      maybeMoveOOP(curClass);
      
      if (curClass == compiledMethodClass) {
	/* Compiled methods have to be dealt with specially since they
	 * have a structure that's unlike a regular Smalltalk object:
	 * it has two fixed instance variables (description and 
	 * methodHeader), a variable number of literals, and then
	 * a bunch of bytecodes, which must be skipped over */
	stepSize = object->objSize * sizeof(OOP);
	method = (Method)object;
	maybeMoveOOP(method->descriptor);
	if (method->header.headerFlag == 0 || method->header.headerFlag == 3) {
	  for (i = 0; i < method->header.numLiterals; i++) {
	    maybeMoveOOP(method->literals[i]);
	  }
	}

	curSpace->scanPtr += stepSize;
	copyQuota -= stepSize;
      } else if (!classIsPointers(curClass)) {
	/* nothing to scan, just skip over it */
	stepSize = object->objSize * sizeof(OOP);
	curSpace->scanPtr += stepSize;
	copyQuota -= stepSize;
      } else {
	/* we've got an object with sub structure, so we set the object
	 * size pointer to 2 words less than the object size (ignore
	 * the header; we've already copied the class) and set the
	 * scan pointer to the first word of the object.  We then continue
	 * to go through the normal scanning procedure (fall out the
	 * bottom of the if and go back to the top of the loop again)
	 */
	curObject = object;
	curObjectSize = numOOPs(curObject) * sizeof(OOP);
	curSpace->scanPtr = (char *)curObject->data;
      }

    } else {
      /* we're part way through scanning an object, continue to scan... */
      if (curObjectSize <= 0) {
	curObject = nil;
      } else {
	maybeMoveOOP(*(OOP *)curSpace->scanPtr);
	stepSize = sizeof(OOP);
	curSpace->scanPtr += stepSize;
	curObjectSize -= stepSize;
	copyQuota -= stepSize;
      }
    }
  }

  if (copyQuota < 0) {
    copyQuota = 0;
  }
}

/*
 *	Boolean gcOff()
 *
 * Description
 *
 *	Turns off the garbage collector.  Returns the previous on/off state.
 *
 * Outputs
 *
 *	Previous state of the garbage collector (on or off).
 */
Boolean gcOff()
{
  Boolean	oldGCState;

  oldGCState = gcState;
  gcState = false;
  return (oldGCState);
}

/*
 *	void gcOn()
 *
 * Description
 *
 *	Turns on the garbage collector.
 *
 */
void gcOn()
{
  gcState = true;
}

/*
 *	void setGCState(state)
 *
 * Description
 *
 *	Set the garbage collector flag to the specified state (either on or
 *	off).
 *
 * Inputs
 *
 *	state : Boolean, true => gc on.
 *
 */
void setGCState(state)
Boolean	state;
{
  gcState = state;
}


/*
 *	void fullGC()
 *
 * Description
 *
 *	Perform a complete garbage collection of the system.  This basically
 *	copies all the remaining referenced objects to new space, flips old and
 *	new space, copies the root set to the now new space, and then copies
 *	all the referenced objects to the now new space.
 *
 */
void fullGC()
{
  if (!gcState) {
    /* can't do a full GC with gc turned off! */
    return;
  }


  copyReferencedObjects(0x7FFFFFFF); /* copy what we can */
  gcFlip();
  copyReferencedObjects(0x7FFFFFFF);
  clearOldOOPs();
}





/*
 *	static void finishOOPScan()
 *
 * Description
 *
 *	Scans through the OOP table, finishing the incremental OOP table
 *	scanning that goes on as OOPs are allocated.
 *
 */
static void finishOOPScan()
{
  OOP		oop;
  int		i;

  for (i = oopTableIndex; i < OOP_TABLE_SIZE; i++) {
    oop = &oopTable[i];
    if (!oop->isFree) {
      if (!oop->evenMark && !oop->oddMark) {
	/* we've found a dead one...add it to the free list */
	numFreeOOPs++;
	oop->object = (Object)freeOOPs;
	freeOOPs = oop;
	freeOOPs->isFree = true;
/*	printf("Freed %d\n", i); */
      } else if (toSpace) {
	oop->evenMark = 0;
      } else {
	oop->oddMark = 0;
      }
    }
  }
}

/*
 *	static void clearOldOOPs()
 *
 * Description
 *
 *	Scans through the OOP table, removing OOPS that have died.  Only
 *	called at the end of a full GC to remove any stragglers.
 *
 */
static void clearOldOOPs()
{
  OOP		oop;
  int		i;

  for (i = oopTableIndex; i < OOP_TABLE_SIZE; i++) {
    oop = &oopTable[i];
    if (!oop->isFree) {
      if ((toSpace && !oop->oddMark) || (!toSpace && !oop->evenMark)) {
	/* we've found a dead one...add it to the free list */
	numFreeOOPs++;
	oop->object = (Object)freeOOPs;
	freeOOPs = oop;
	freeOOPs->isFree = true;
/*	printf("Freed %d\n", i); */
      } else if (toSpace) {
	oop->evenMark = 0;
      } else {
	oop->oddMark = 0;
      }
    }
  }
}

/*
 *	void gcFlip()
 *
 * Description
 *
 *	Switches the garbage collector's notion of which space is "new" space
 *	and which is "old" space.  Readjusts the garbage collection parameters
 *	based on things like the allocation to copying ratio.  Copies the root
 *	set to new space.
 *
 */
void gcFlip()
{
  long		oldCopySize, oldNewSize;

  if (!gcState) {
    errorf("Attempted to do a gcFlip with garbage collector off!");
    exit(1);
  }

  if (gcFlipCounter >= 1) {
    errorf("Attempted to do a gcFlip too soon after a gcFlip!");
    exit(1);
  }


#ifdef OOP_DEBUGGING
  printf("%d free oops = %.2f%%, scanner was at %d/%d\n", numFreeOOPs,
       100.0 * numFreeOOPs / OOP_TABLE_SIZE, oopTableIndex, OOP_TABLE_SIZE);
#endif

  if (gcMessage && !regressionTesting) {
    /* print the first part of this message before we finish scanning 
     * oop table for live ones, so that the delay caused by this scanning
     * is apparent.
     */
    printf("\"GC flipping "); fflush(stdout);
  }

  finishOOPScan();		/* make sure we're done */

  oldCopySize = curSpace->copyPtr - curSpace->space;
  oldNewSize = curSpace->space + MEM_SPACE_SIZE - curSpace->allocPtr;

if (oldCopySize == 0) {  /* ### Experimental */
  oldCopySize = oldNewSize;
}


  copyRate = ((double)oldCopySize) / oldNewSize;
  copyRate *= copyRateAdjustment;

  toSpace = !toSpace;
  fromSpace = !fromSpace;
  curSpace = &spaces[toSpace];
  curObject = nil;
  copyQuota = 0;
  oopTableIndex = 0;
  initSpace(curSpace);

#ifdef remove_soon /* Mon May 14 23:41:55 1990 */
/**/zeroMarks();
#endif /* remove_soon Mon May 14 23:41:55 1990 */

  /* note the use of quotation marks around the printed message.  The
     idea here was to make them appear as Smalltalk comments, so that 
     generated output could be fed to another Smalltalk without harm. */
  if (gcMessage && !regressionTesting) {
    printf("to space %d...", toSpace); fflush(stdout);
    printf("copied space = %.1f%%...", oldCopySize * 100.0 / MEM_SPACE_SIZE);
  }
  moveRootOOPs();
  if (gcMessage && !regressionTesting) {
    printf("done\"\n");
  }
  gcFlipped = true;
}

/*
 *	static void moveRootOOPs()
 *
 * Description
 *
 *	Copies the root objects from old space to new space.  All of the root
 *	objects are those that are mentioned in the set of objects that are
 *	known specially by the interpreter, those that are being used by the
 *	interpreter, and some information about compilation state.  Also, the
 *	built-in oops (Characters, nil, true, false) are marked as being in new
 *	space so that they won't ever be moved.
 *
 */
static void moveRootOOPs()
{
  OOP		**oopPtr, oop;
  int		i;

  markBuiltinOOPs();

  /* copy objects that have global pointers */
  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
    oop = **oopPtr;
    maybeMoveOOP(oop);		/* use the maybe form here so that we don't
				 * accidentally move builtins, which have
				 * already been marked as being in the new
				 * space
				 */
  }

  moveProcessorRegisters();
  copyCompileContext();
}

/*
 *	static void markBuiltinOOPs()
 *
 * Description
 *
 *	Marks all of the builtin OOPS (nil, true, false, and the Characters) as
 *	being in the current new space.
 *
 */
static void markBuiltinOOPs()
{
  int		i;

  for (i = OOP_TABLE_SIZE; i < TOTAL_OOP_TABLE_SLOTS; i++) {
    oopTable[i].inSpace = toSpace;
    oopTable[i].evenMark = oopTable[i].oddMark = 1;
  }
}

/*
 *	static void initSpace(space)
 *
 * Description
 *
 *	Initializes the allocation and copying pointers for semispace SPACE.
 *
 * Inputs
 *
 *	space : Semispace index number.
 *
 */
static void initSpace(space)
struct memorySpaceStruct *space;
{
  space->copyPtr = space->scanPtr = space->space;
  space->size = MEM_SPACE_SIZE;
  space->allocPtr = space->space + space->size;
}


#ifndef INLINE_MACROS

/*
 *	void maybeMoveOOP(oop)
 *
 * Description
 *
 *	Move OOP to new space if it's not already there.
 *
 * Inputs
 *
 *	oop   : OOP to be examined, and, if it's in old space, moved to new
 *		space.
 *
 */
void maybeMoveOOP(oop)
OOP	oop;
{
  maybeMoveOOPMac(oop);
}

#endif /* INLINE_MACROS */


/*
 *	void moveOOP(oop)
 *
 * Description
 *
 *	Moves an OOP from old space to new space unconditionally.  Basically
 *	marks the OOP as being in the current new space, copies the object that
 *	the oop points to to new space, and sets the even/odd flags to keep the
 *	OOP table garbage collector from reaping this OOP.
 *
 * Inputs
 *
 *	oop   : OOP to be moved.  Should always be in OLD space.
 *
 */
void moveOOP(oop)
OOP	oop;
{
  Object	object;

  object = oopToObj(oop);
  oop->inSpace = toSpace;
  oop->object = moveObject(object);
#ifdef remove_soon /* Mon May 14 23:42:39 1990 */
/**/if (traceOOPStuff) 
/**/{ int i;
/**/
/**/  i = oop - oopTable;
/**/if (marks[i] != 0) {
/**/  printf("moveOOP: oop %d has a mark value of %d\n", i, marks[i]);
/**/}
/**/marks[i] = 1;
/**/}
#endif /* remove_soon Mon May 14 23:42:39 1990 */
  if (toSpace) {
    oop->oddMark = 1;
  } else {
    oop->evenMark = 1;
  }
}

/*
 *	static Object moveObject(object)
 *
 * Description
 *
 *	Copies OBJECT from old space to new space.  Adjusts the garbage
 *	collectors pointers to indicate that the object has been added to new
 *	space so that the scanner will see it.
 *
 * Inputs
 *
 *	object: Object to be moved to new space.
 *
 */
static Object moveObject(object)
Object	object;
{
  int		size;

  size = object->objSize * sizeof(OOP);
  memcpy(curSpace->copyPtr, object, size);
  object = (Object)curSpace->copyPtr;
  curSpace->copyPtr += size;
  curSpace->size -= size;
  if (curSpace->copyPtr >= curSpace->allocPtr) {
    errorf("Garbage collector failed...ran out of room while copying!!!");
    exit(0);
  }

  return (object);
}

/*
 *	void printFreeList()
 *
 * Description
 *
 *	Debugging support routine.  Prints the free list.  Meant only to be
 *	called from a debugger.
 *
 */
void printFreeList()
{
  OOP		oop;
  for (oop = freeOOPs; oop != nil; oop = (OOP)oop->object) {
    printf("oop %x\n", oop);
    printf("oop isfree %d\n", oop->isFree);
  }
}

/*
 *	debug()
 *
 * Description
 *
 *	Used for debugging.  You set a breakpoint in the debug routine in the
 *	debugger, and have code call it when you want it to stop.  Performs no
 *	action normally.
 *
 */
debug()
{
}


#ifdef remove_soon /* Mon May 14 23:42:01 1990 */
/**/static void zeroMarks()
/**/{
/**/  char	*p;
/**/
/**/  for (p = marks; p < &marks[TOTAL_OOP_TABLE_SLOTS]; ) {
/**/    *p++ = 0;
/**/  }
/**/}
#endif /* remove_soon Mon May 14 23:42:01 1990 */
