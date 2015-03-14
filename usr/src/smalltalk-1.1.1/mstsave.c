/***********************************************************************
 *
 *	Binary image save/restore.
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
 * sbyrne    17 Apr 90	  Fixing binary save to save only to the maximum used
 *			  OOP slot, instead of saving the entire OOP table.
 *			  This should improve load time and decrease disk
 *			  storage requirements.
 *
 * sbyrne    11 Feb 90	  Changed the header to record the size of the oop
 *			  table, since trying to load back into a system with a
 *			  different sized oop table loses bigtime.
 *
 * sbyrne     5 Apr 89	  modified to reflect change in classes: now their name
 *			  is a Smalltalk string; before, it was a C string that
 *			  had to be saved specially.
 *
 * sbyrne     4 Mar 89	  Created.
 *
 */


#include <stdio.h>
#include "mst.h"
#include "mstsave.h"
#include "mstcomp.h"
#include "mstinterp.h"
#include "mstdict.h"
#include "mstsym.h"
#include "mstoop.h"		/* indirectly defines oopAt for sym tab prof */
#include "mstmain.h"
#ifdef HAS_ALLOCA_H
#include <alloca.h>
#endif
#include <stdio.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN		1024 /* max length of a file and path */
#endif

#define fromBeginning		0 /* symbolic name for file offset modifier */

/* convert to a relative offset from start of OOP table */
#define toRelative(obj) \
  ( (OOP)((long)(obj) - (long)oopTable) )

/* convert from relative offset to actual oop table address */
#define fromRelative(obj) \
  ( (OOP)((long)(obj) + (long)oopTable) )

/* round "x" up to the next 4 byte boundary */
#define roundUpWord(x) \
  ( ((x) + 3) & ~3 )

/*
 * The binary image file has the following format:
 *
 *	header
 *	complete oop table
 *	global oop variable data
 *	objects and non-oop object data
 *	char object data
 *	nil object
 *	boolean objects
 */


typedef struct SaveFileHeaderStruct {
  long		version;	/* the Smalltalk version that made this dump */
  long		objectDataSize;	/* size of object data section in bytes */
  long		oopTableSize;	/* size of the oop table at dump */
} SaveFileHeader;

typedef struct OOPVectorStruct {
  Object	base;		/* base of the storage */
  Object	ptr;		/* the current object */
} OOPVector;


OOP		*globalOOPs[] = {
  &andColonSymbol,
  &atColonPutColonSymbol,
  &atColonSymbol,
  &atEndSymbol,
  &bitAndColonSymbol,
  &bitOrColonSymbol,
  &bitShiftColonSymbol,
  &blockCopyColonSymbol,
  &classSymbol,
  &divideSymbol,
  &doColonSymbol,
  &equalSymbol,
  &greaterEqualSymbol,
  &greaterThanSymbol,
  &ifFalseColonIfTrueColonSymbol,
  &ifFalseColonSymbol,
  &ifTrueColonIfFalseColonSymbol,
  &ifTrueColonSymbol,
  &integerDivideSymbol,
  &lessEqualSymbol,
  &lessThanSymbol,
  &minusSymbol,
  &newColonSymbol,
  &newSymbol,
  &nextPutColonSymbol,
  &nextSymbol,
  &notEqualSymbol,
  &notSameObjectSymbol,
  &orColonSymbol,
  &plusSymbol,
  &remainderSymbol,
  &sameObjectSymbol,
  &sizeSymbol,
  &thisContextSymbol,
  &timesSymbol,
  &valueColonSymbol,
  &valueColonValueColonSymbol,
  &valueColonValueColonValueColonSymbol,
  &valueWithArgumentsColonSymbol,
  &valueSymbol,
  &whileFalseColonSymbol,
  &whileTrueColonSymbol,
  &orSymbol,
  &andSymbol,
  &superSymbol,
  &nilSymbol,
  &trueSymbol,
  &falseSymbol,
  &selfSymbol,
  &doesNotUnderstandColonSymbol,
  &unknownSymbol,
  &charSymbol,
  &stringSymbol,
  &stringOutSymbol, 
  &symbolSymbol,
  &intSymbol,
  &longSymbol,
  &doubleSymbol,
  &voidSymbol,
  &variadicSymbol,
  &cObjectSymbol,
  &smalltalkSymbol,
  &byteArraySymbol,
  &objectClass,
  &magnitudeClass,
  &charClass,
  &timeClass,
  &numberClass,
  &floatClass,
  &integerClass,
  &lookupKeyClass,
  &associationClass,
  &linkClass,
  &processClass,
  &symLinkClass,
  &collectionClass,
  &sequenceableCollectionClass,
  &linkedListClass,
  &semaphoreClass,
  &arrayedCollectionClass,
  &arrayClass,
  &stringClass,
  &symbolClass,
  &byteArrayClass,
  &compiledMethodClass,
  &intervalClass,
  &orderedCollectionClass,
  &sortedCollectionClass,
  &bagClass,
  &mappedCollectionClass,
  &setClass,
  &dictionaryClass,
  &identityDictionaryClass,
  &systemDictionaryClass,
  &undefinedObjectClass,
  &booleanClass,
  &falseClass,
  &trueClass,
  &processorSchedulerClass,
  &delayClass,
  &sharedQueueClass,
  &behaviorClass,
  &classDescriptionClass,
  &classClass,
  &metaclassClass,
  &smalltalkDictionary,
  &messageClass,
  &methodContextClass,
  &blockContextClass,
  &streamClass,
  &positionableStreamClass,
  &readStreamClass,
  &writeStreamClass,
  &readWriteStreamClass,
  &cObjectClass,
  &fileStreamClass,
  &memoryClass,
  &byteMemoryClass,
  &wordMemoryClass,
  &randomClass,
  &cFuncDescriptorClass,
  &tokenStreamClass,
  &methodInfoClass,
  &fileSegmentClass,
  &nilOOP,
  &trueOOP,
  &falseOOP,
  &processorOOP,
  &symbolTable,
  nil
};


static void	skipOverHeader(), saveObject(), fixupObject(),
		fixupMethodObject(), restoreObject(), restoreMethodObject(),
		saveOOPTable(), fixupAllOOPs(), fixupOOP(),
		/* fixupFreeOOP(), */restoreAllOOPs(), restoreOOP(), 
		/*restoreFreeOOP(), */loadOOPTable(), loadNormalOOPs(),
		loadCharOOPs(), loadSpecialOOPs(), saveGlobalOOPs(),
		loadGlobalOOPs(), fixupClassObject(), restoreInstanceVars(),
		restoreClassObject(), saveFileVersion(),
		loadFileVersion(), fixupInstanceVars(), fixupOOPInstanceVars();

static int	saveNormalOOPs(), saveAllObjects();


/* This variable contains the OOP slot index of the highest non-free OOP,
 * excluding the built-in ones (i.e., it will always be < OOP_TABLE_SIZE).
 * This is used for optimizing the size of the saved image, and minimizing
 * the load time when restoring the system. */
static int	maxUsedOOPSlot = 0;


Boolean saveToFile(fileName)
char	*fileName;
{
  FILE		*imageFile;
  long		objectDataSize;
  Boolean	oldGCState;

  fullGC();			/* make sure that the world is compact if
				   possible */

  oldGCState = gcOff();

#ifdef BINARY_MODE_NEEDED
  imageFile = fopen(fileName, "wb");
#else
  imageFile = fopen(fileName, "w");
#endif
  if (imageFile == NULL) {
    errorf("Couldn't open file %s", fileName);
    return (false);
  }

  skipOverHeader(imageFile);
  saveOOPTable(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After saving oopt table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  saveGlobalOOPs(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After global oop table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  objectDataSize = saveAllObjects(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After saving all objects table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  skipToHeader(imageFile);
  saveFileVersion(imageFile, objectDataSize);

  fclose(imageFile);

  setGCState(oldGCState);
  return (true);
} 


static void skipOverHeader(imageFile)
FILE	*imageFile;
{
  unsigned long	pos;

  fseek(imageFile, sizeof(SaveFileHeader), fromBeginning);
}

/*
 *	static void saveOOPTable(imageFile)
 *
 * Description
 *
 *	Writes the OOP table out to the image file.  We need to make all
 *	of the object pointers relative, including free OOP table slots, and
 *	we use a parallel vector containing file offsets for the objects that
 *	we developed during saving of the objects themselves as the fixup
 *	table.
 *
 * Inputs
 *
 *	imageFile: 
 *		A stdio FILE to be written to.  It must be positioned
 *		correctly before this routine is called.
 *
 */
static void saveOOPTable(imageFile)
FILE	*imageFile;
{
  fixupAllOOPs();

#ifdef OOP_TABLE_TRACE
printf("there are %d free oops out of %d oops, leaving %d\n",
       numFreeOOPs, OOP_TABLE_SIZE, OOP_TABLE_SIZE - numFreeOOPs);
printf("max used is %d\n", maxUsedOOPSlot);
#endif /* OOP_TABLE_TRACE */

  /* save up to the max oop slot in use */
  fwrite(oopTable, sizeof(struct OOPStruct), maxUsedOOPSlot + 1, imageFile);

  /* then save the constant ones at the end */
  fwrite(&oopTable[OOP_TABLE_SIZE], sizeof(struct OOPStruct),
	 TOTAL_OOP_TABLE_SLOTS - OOP_TABLE_SIZE, imageFile);

  restoreAllOOPs();
}


static void fixupAllOOPs()
{
  int		i;

  maxUsedOOPSlot = 0;

  for (i = 0; i < TOTAL_OOP_TABLE_SLOTS; i++) {
    fixupOOP(i);
  }
}

static void fixupOOP(i)
int	i;
{
  OOP		oop;

  oop = oopAt(i);
#ifdef old_code /* Tue Apr 17 22:48:45 1990 */
/**/  if (oop->isFree) {
/**/    fixupFreeOOP(oop);
/**/  } else {
#endif /* old_code Tue Apr 17 22:48:45 1990 */
  if (!oop->isFree) {
    if (i < OOP_TABLE_SIZE) {
      maxUsedOOPSlot = i;
    }
    oop->object = (Object)toRelative(oop->object);
  }
}

#ifdef old_code /* Tue Apr 17 22:49:42 1990 */
/**/
/**/static void fixupFreeOOP(oop)
/**/OOP	oop;
/**/{
/**/  if (oop->object == nil) {
/**/    oop->object = (Object)-1L;
/**/  } else {
/**/    oop->object = (Object)toRelative(oop->object);
/**/  }
/**/
/**/  oop->isFree = 0;
/**/  if (oop->prevFree == (long)nil) {
/**/    oop->prevFree = -1L;
/**/  } else {
/**/    oop->prevFree = (long)toRelative(oop->prevFree);
/**/  }
/**/
/**/  oop->isFree = 1;
/**/}
#endif /* old_code Tue Apr 17 22:49:42 1990 */


static void restoreAllOOPs()
{
  int		i;

  for (i = 0; i < TOTAL_OOP_TABLE_SLOTS; i++) {
    restoreOOP(i);
  }
}



static void restoreOOP(i)
int	i;
{
  OOP		oop;

  oop = oopAt(i);
#ifdef old_code /* Tue Apr 17 22:50:19 1990 */
/**/  if (oop->isFree) {
/**/    restoreFreeOOP(oop);
/**/  } else {
#endif /* old_code Tue Apr 17 22:50:19 1990 */
  if (!oop->isFree) {
    oop->object = (Object)fromRelative(oop->object);
  }
}

#ifdef old_code /* Tue Apr 17 22:50:39 1990 */
/**/static void restoreFreeOOP(oop)
/**/OOP	oop;
/**/{
/**/  if (oop->object == (Object)-1L) {
/**/    oop->object = nil;
/**/  } else {
/**/    oop->object = (Object)fromRelative(oop->object);
/**/  }
/**/
/**/  if (oop->prevFree == -1L) {
/**/    oop->prevFree = (long)nil;
/**/  } else {
/**/    oop->isFree = 0;
/**/    oop->prevFree = (long)fromRelative(oop->prevFree);
/**/  }
/**/
/**/  oop->isFree = 1;
/**/}
#endif /* old_code Tue Apr 17 22:50:39 1990 */


static void saveGlobalOOPs(imageFile)
FILE	*imageFile;
{
  OOP		**oopPtr, oop;

  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
    oop = toRelative(**oopPtr);
    fwrite(&oop, sizeof(OOP), 1, imageFile);
  }
}


static int saveAllObjects(imageFile)
FILE	*imageFile;
{
  long		objectStart, objectEnd;
  int		i;
  OOP		oop;

  objectStart = ftell(imageFile);
  for (i = 0; i < OOP_TABLE_SIZE; i++) {
    oop = oopAt(i);
    if (oopValid(oop)) {
      saveObject(imageFile, oop);
    }
  }

  objectEnd = ftell(imageFile);

  /* dump out the character objects, nil, true, and false */
  for (i = OOP_TABLE_SIZE; i < TOTAL_OOP_TABLE_SLOTS; i++) {
    saveObject(imageFile, oopAt(i));
  }

  return (objectEnd - objectStart);
}

static void saveObject(imageFile, oop)
FILE	*imageFile;
OOP	oop;
{
  Object	object;
  int		numFixed, numIndexed;
  Boolean	hasPointers;

  object = oopToObj(oop);
  hasPointers = isPointers(oop);
  numFixed = oopFixedFields(oop);
  numIndexed = numIndexableFields(oop);

  fixupObject(oop, hasPointers, numFixed, numIndexed);
  fwrite(object, sizeof(OOP), object->objSize, imageFile);
  restoreObject(oop, hasPointers, numFixed, numIndexed);
}

static void fixupObject(oop, hasPointers, numFixed, numIndexed)
OOP	oop;
Boolean	hasPointers;
int	numFixed, numIndexed;
{
  int		i;
  Object	object;
  OOP		instOOP, classOOP;

  classOOP = oopClass(oop);

  if (classOOP == compiledMethodClass) {
    fixupMethodObject(oop);
  } else {
    if (hasPointers) {
      for (i = 1; i <= numFixed + numIndexed; i++) {
	instOOP = instVarAt(oop, i);
	if (!isInt(instOOP)) {
	  instVarAtPut(oop, i, toRelative(instOOP));
	}
      }
    }
  }

  object = oopToObj(oop);
  object->objClass = toRelative(object->objClass);
}

static void fixupMethodObject(oop)
OOP	oop;
{
  MethodHeader	header;
  int		i;
  OOP		literalOOP, descriptorOOP;

  descriptorOOP = getMethodDescriptor(oop);
  if (!isInt(descriptorOOP)) {
    setMethodDescriptor(oop, toRelative(descriptorOOP));
  }
  header = getMethodHeaderExt(oop);
  if (header.headerFlag == 1 || header.headerFlag == 2) {
    /* these have no method literals to fix up, so we ignore them */
    return;
  }

  for (i = 0; i < header.numLiterals; i++) {
    literalOOP = methodLiteralExt(oop, i);
    if (!isInt(literalOOP)) {
      storeMethodLiteralNoGC(oop, i, toRelative(literalOOP));
    }
  }
}

static void restoreObject(oop, hasPointers, numFixed, numIndexed)
OOP	oop;
Boolean	hasPointers;
int	numFixed, numIndexed;
{
  Object	object;

  object = oopToObj(oop);

  object->objClass = fromRelative(object->objClass);

  restoreInstanceVars(oop, hasPointers, numFixed, numIndexed);
}

static void restoreInstanceVars(oop, hasPointers, numFixed, numIndexed)
OOP	oop;
Boolean	hasPointers;
int	numFixed, numIndexed;
{
  register int	i;
  OOP		instOOP, classOOP;

  classOOP = oopClass(oop);
  if (classOOP == compiledMethodClass) {
    restoreMethodObject(oop);
  } else {
    if (hasPointers) {
      for (i = 1; i <= numFixed + numIndexed; i++) {
	instOOP = instVarAt(oop, i);
	if (!isInt(instOOP)) {
	  instVarAtPut(oop, i, fromRelative(instOOP));
	}
      }
      if (classOOP == cFuncDescriptorClass) {
	restoreCFuncDescriptor(oop); /* in mstcint.c */
      }
    }
  }
}

static void restoreMethodObject(oop)
OOP	oop;
{
  MethodHeader	header;
  int		i;
  OOP		literalOOP, descriptorOOP;

  descriptorOOP = getMethodDescriptor(oop);
  if (!isInt(descriptorOOP)) {
    setMethodDescriptor(oop, fromRelative(descriptorOOP));
  }

  header = getMethodHeaderExt(oop);
  if (header.headerFlag == 1 || header.headerFlag == 2) {
    /* these have no method literals to fix up, so we ignore them */
    return;
  }

  for (i = 0; i < header.numLiterals; i++) {
    literalOOP = methodLiteralExt(oop, i);
    if (!isInt(literalOOP)) {
      storeMethodLiteralNoGC(oop, i, fromRelative(literalOOP));
    }
  }
}


skipToHeader(imageFile)
FILE	*imageFile;
{
  rewind(imageFile);
}

static void saveFileVersion(imageFile, objectDataSize)
FILE	*imageFile;
long	objectDataSize;
{
  SaveFileHeader header;

  header.version = sysVersMajor * 1000000 + sysVersMinor * 1000 + sysVersEdit;
  header.objectDataSize = objectDataSize;
  header.oopTableSize = maxUsedOOPSlot + 1; /* n slots, numbered 0..n-1 */

  fwrite(&header, sizeof(SaveFileHeader), 1, imageFile);
}

static void skipToOOPTable(imageFile)
FILE	*imageFile;
{
  rewind(imageFile);
}


/***********************************************************************
 *
 *	Binary loading routines.
 *
 ***********************************************************************/


Boolean loadFromFile(fileName)
char	*fileName;
{
  FILE		*imageFile;
  SaveFileHeader header;
  OOPVector	ov;
  Boolean	oldGCState;
  char		fullImageName[MAXPATHLEN];

  oldGCState = gcOff();

  findImageFile(fileName, fullImageName);
#ifdef BINARY_MODE_NEEDED
  imageFile = fopen(fullImageName, "rb");
#else
  imageFile = fopen(fullImageName, "r");
#endif
  if (imageFile == NULL) {
    return (false);
  }

  loadFileVersion(imageFile, &header);
  if (header.version != (sysVersMajor * 1000000 + sysVersMinor * 1000
			 + sysVersEdit)) {
    /* version mismatch; this image file is invalid */
    return (false);
  }
  
#ifdef old_code /* Tue Apr 17 22:25:33 1990 */
/**/  if (header.oopTableSize != OOP_TABLE_SIZE) {
/**/    return (false);
/**/  }
#endif /* old_code Tue Apr 17 22:25:33 1990 */

  loadOOPTable(imageFile, header.oopTableSize);

  loadGlobalOOPs(imageFile);

  loadNormalOOPs(imageFile, header.objectDataSize, &ov);
  loadCharOOPs(imageFile);
  loadSpecialOOPs(imageFile);

  fixupInstanceVars(&ov);

  fclose(imageFile);

  setGCState(oldGCState);

  return (true);
}

static void loadFileVersion(imageFile, headerp)
FILE	*imageFile;
SaveFileHeader *headerp;
{
  fread(headerp, sizeof(SaveFileHeader), 1, imageFile);
}

static void loadOOPTable(imageFile, oldSlotsUsed)
FILE	*imageFile;
long 	oldSlotsUsed;
{
  long		i;
  OOP		oop;

/*  fread(oopTable, sizeof(struct OOPStruct), TOTAL_OOP_TABLE_SLOTS, imageFile);*/

  /* load in the valid OOP slots from previous dump */
  fread(oopTable, sizeof(struct OOPStruct), oldSlotsUsed, imageFile);
  
  /* mark the remaining ones as available */
  for (i = oldSlotsUsed; i < OOP_TABLE_SIZE; i++) {
    oop = oopAt(i);
    oop->isFree = true;
  }


  /* read in the constant stuff at the end */
  fread(&oopTable[OOP_TABLE_SIZE], sizeof(struct OOPStruct),
	TOTAL_OOP_TABLE_SLOTS - OOP_TABLE_SIZE, imageFile);

  /* the fixup gets handled by load normal oops */
}


static void loadGlobalOOPs(imageFile)
FILE	*imageFile;
{
  OOP		**oopPtr, oop, *oopVec;
  long		numGlobalOOPs;

  for (numGlobalOOPs = 0, oopPtr = globalOOPs; *oopPtr;
       oopPtr++, numGlobalOOPs++);

  oopVec = (OOP *)alloca(numGlobalOOPs * sizeof(OOP));
  fread(oopVec, sizeof(OOP), numGlobalOOPs, imageFile);

  for (oopPtr = globalOOPs; *oopPtr; oopPtr++, oopVec++) {
    **oopPtr = fromRelative(*oopVec);
  }
#ifdef old_code /* Wed Apr 26 21:15:38 1989 */ /* <<<== what dedication...3 days before my wedding -- SBB */
/**/  /* ??? this could be sped up by using alloca, doing one fread, and then
/**/     iterating through the array */
/**/  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
/**/    fread(&oop, sizeof(OOP), 1, imageFile);
/**/    **oopPtr = fromRelative(oop);
/**/  }
#endif /* old_code Wed Apr 26 21:15:38 1989 */
}

static void loadNormalOOPs(imageFile, objectDataSize, ovp) 
FILE	*imageFile;
long	objectDataSize;
OOPVector *ovp;
{
  register long	i;
  OOP		oop, prevFreeOOP;
  Object	object, objPtr;

  objPtr = curSpaceAddr();

  ovp->base = objPtr;

  prevFreeOOP = nil;

  fread(objPtr, 1, objectDataSize, imageFile);

  freeOOPs = nil;
  numFreeOOPs = 0;
  for (i = 0; i < OOP_TABLE_SIZE; i++) {
    oop = oopAt(i);
    if (oopValid(oop)) {
      object = objPtr;
      oop->object = object;
      /* ### should probably make this setting symbolic/abstracted */
      oop->inSpace = toSpace;
      oop->oddMark = toSpace;
      oop->evenMark = !toSpace;
      object->objClass = fromRelative(object->objClass);
      objPtr = (Object)((long *)object + object->objSize);
    } else if (oop->isFree) {	/* ignore non-free but non-valid...they'll get handled
				   naturally in due time. */
      numFreeOOPs++;
      if (prevFreeOOP) {	/* forward chain the oop free list */
	prevFreeOOP->object = (Object)oop;
      }
      if (freeOOPs == nil) {	/* free list points at first free oop in
				   OOP table */
	freeOOPs = oop;
      }
      prevFreeOOP = oop;
    }
  }

  if (prevFreeOOP) {
    prevFreeOOP->object = nil;	/* terminate the free list nicely */
  }

  setSpaceInfo(objectDataSize);
  ovp->ptr = objPtr;
}

static void loadCharOOPs(imageFile)
FILE	*imageFile;
{
  int		i;

  fread(charObjectTable, sizeof(CharObject), NUM_CHAR_OBJECTS, imageFile);

  for (i = 0; i < NUM_CHAR_OBJECTS; i++) {
    oopTable[i + CHAR_OBJECT_BASE].object = (Object)&charObjectTable[i];
    oopTable[i + CHAR_OBJECT_BASE].inSpace = toSpace;
    charObjectTable[i].objClass = fromRelative(charObjectTable[i].objClass);
  }
}

static void loadSpecialOOPs(imageFile)
FILE	*imageFile;
{
  fread(&nilObject, sizeof(struct NilObjectStruct), 1, imageFile);
  nilOOP->object = (Object)&nilObject;
  nilOOP->object->objClass = fromRelative(nilOOP->object->objClass);
  nilOOP->inSpace = toSpace;

  fread(booleanObjects, sizeof(struct BooleanObjectStruct), 2, imageFile);
  trueOOP->object = (Object)&booleanObjects[0];
  falseOOP->object = (Object)&booleanObjects[1];
  trueOOP->object->objClass = fromRelative(trueOOP->object->objClass);
  falseOOP->object->objClass = fromRelative(falseOOP->object->objClass);
  trueOOP->inSpace = falseOOP->inSpace = toSpace;
}

static void fixupInstanceVars(ovp)
OOPVector *ovp;
{
  int		i;
  OOP		oop;

  for (i = 0; i < TOTAL_OOP_TABLE_SLOTS; i++) {
    oop = oopAt(i);
    if (oopValid(oop)) {
      fixupOOPInstanceVars(oop, ovp);
    }
  }
}

static void fixupOOPInstanceVars(oop, ovp)
OOP	oop;
OOPVector *ovp;
{
  int		numFixed, numIndexed;
  Boolean	hasPointers;

  hasPointers = isPointers(oop);
  numFixed = oopFixedFields(oop);
  numIndexed = numIndexableFields(oop);
  restoreInstanceVars(oop, hasPointers, numFixed, numIndexed);
}


