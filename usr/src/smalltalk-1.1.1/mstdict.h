/***********************************************************************
 *
 *	Dictionary Support Module Definitions.
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
 * sbyrne    13 Jan 89	  Created.
 *
 */


#ifndef __MSTDICT__
#define __MSTDICT__

/* ### YUK YUK YUK -- I hate to do this, but ... I need access to this
   structure for the binary save/restore code and I'd rather not have to
   write a bunch of accessors just to get on the air.  Fix this asap. */

typedef struct InstanceSpecStruct {
#ifdef BIG_ENDIAN
  unsigned	intMark			: 1;
  unsigned	isPointers 		: 1;
  unsigned	isWords			: 1;
  unsigned	isIndexable		: 1;
  unsigned	numFixedFields		: 28;
#else
  unsigned	numFixedFields		: 28;
  unsigned	isIndexable		: 1;
  unsigned	isWords			: 1;
  unsigned	isPointers 		: 1;
  unsigned	intMark			: 1;
#endif
} InstanceSpec;

/* Note the use of inheritance in C structure definitions here */
#define BEHAVIOR_HEADER \
  OBJ_HEADER; \
  OOP		superClass; \
  OOP		subClasses; \
  OOP		methodDictionary; \
  InstanceSpec	instanceSpec

typedef struct BehaviorStruct {
  BEHAVIOR_HEADER;
} *Behavior;

#define CLASS_DESCRIPTION_HEADER \
  BEHAVIOR_HEADER; \
  OOP		name; \
  OOP		comment; \
  OOP		instanceVariables; \
  OOP		category


typedef struct ClassDescriptionStruct {
  CLASS_DESCRIPTION_HEADER;
} *ClassDescription;


typedef struct ClassStruct {
  CLASS_DESCRIPTION_HEADER;
  OOP		classVariables;	/* dictionary of varName, storage */
  OOP		sharedPools;
} *Class;

typedef struct MetaclassStruct {
  CLASS_DESCRIPTION_HEADER;
  OOP		instanceClass;
} *Metaclass;

extern OOP		objectClass, magnitudeClass, charClass, timeClass,
			dateClass,
			numberClass, floatClass, integerClass, lookupKeyClass,
			associationClass, linkClass, processClass,
			symLinkClass, collectionClass,
			sequenceableCollectionClass, linkedListClass,
			semaphoreClass,
			arrayedCollectionClass,	arrayClass, stringClass,
			symbolClass, byteArrayClass, compiledMethodClass,
			intervalClass, orderedCollectionClass,
			sortedCollectionClass, bagClass, mappedCollectionClass,
			setClass, dictionaryClass, 
			systemDictionaryClass,
			identityDictionaryClass, undefinedObjectClass,
			booleanClass, falseClass, trueClass, 
			processorSchedulerClass, delayClass, sharedQueueClass,
			behaviorClass,
			classDescriptionClass, classClass, metaclassClass,
			smalltalkDictionary, messageClass, methodContextClass,
			blockContextClass,
			streamClass, positionableStreamClass, readStreamClass,
			writeStreamClass, readWriteStreamClass, cObjectClass,
			fileStreamClass, memoryClass,
			byteMemoryClass, wordMemoryClass, randomClass,
			cFuncDescriptorClass, tokenStreamClass,
			methodInfoClass, fileSegmentClass,
  			processorOOP;

extern OOP		associationNew(), dictionaryNew(), associationValue(),
			superClass(), validClassMethodDictionary(),
			classVariableDictionary(), findSharedPoolVariable(),
			dictionaryAt(), dictionaryAtPut(), dictionaryAdd(),
			arrayAt(), floatNew(), arrayNew(), stringNew(),
			indexOOP(), instVarAt(), indexStringOOP(),
			classMethodDictionary(), findClass(),
			dictionaryAssociationAt(), findClassMethod(),
			messageNewArgs(), messageSelector(), messageArgs(),
  			getClassSymbol(), dictionaryCopy(),
			instanceVariableArray(), sharedPoolDictionary(),
			instantiateOOPWith(), metaclassInstance(),
			cObjectNew();

extern Object		newInstanceWith(), newInstance(), instantiate(),
  			instantiateWith();

extern int		numIndexableFields(), dictionarySize(),
			oopFixedFields();

extern Boolean		isIndexable(), isAKindOf(), checkIndexableBoundsOf(),
  			checkBoundsOf(), instVarAtPut(), indexOOPPut(),
			isPointers(), isAClass(), isAMetaclass(), 
			classIsPointers();

extern Byte		*stringOOPChars(), *toCString(), *toByteArray();

extern void		indexStringOOPPut(),
			printAssocationKey(), printOOPConstructor(),
			addClassSymbol(), initDictionary(), initSTDIOObjects(),
			setOOPString(), setCObjectValue(), setComment();
extern voidPtr		cObjectValue();

extern double		floatOOPValue();
extern void		arrayAtPut();

#endif /* __MSTDICT__ */
