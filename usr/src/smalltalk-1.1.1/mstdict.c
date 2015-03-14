/***********************************************************************
 *
 *	Dictionary Support Module.
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
 * sbb	     12 Sep 91	  Changed to support edit numbers in version string.
 *
 * sbyrne    21 Apr 90	  Added toByteArray.
 *
 * sbyrne     7 Jan 90	  Added more commentary to classes, added new global
 *			  Smalltalk variable: Bigendian, which allows code 
 *			  to be conditional based on the architecture type.
 *
 * sbyrne     7 Sep 89	  Started adding garbage collection support.
 *
 * sbyrne    29 May 89	  Added the memory classes.  Added the FileStream about
 *			  a week ago.
 *
 * sbyrne    29 Apr 89	  Author changed from single to married.
 *
 * sbyrne     5 Apr 89	  Restructured Class and Metaclass creation.  Is now
 *			  table driven, and metaclasses are created containing
 *			  the proper information.
 *
 * sbyrne    29 Mar 89	  Removed MethodDictionary as a separate type; it is an
 *			  IdentityDictionary. 
 *
 * sbyrne    11 Mar 89	  Smalltalk is now an instance of SystemDictionary.
 *
 * sbyrne    13 Jan 89	  Created.
 *
 */


#include "mst.h"
#include "mstdict.h"
#include "mstoop.h"
#include "mstinterp.h"
#include "mststr.h"
#include "mstsym.h"
#include "mstmain.h"
#include <stdio.h>

#define INITIAL_DICTIONARY_SIZE		32 /* chosen at random */


/***********************************************************************
 *
 *	Below are the structural definitions for several of the important
 *	objects present in the Smalltalk system.  Their C representation
 *	corresponds exactly with their Smalltalk representation.
 *
 ***********************************************************************/

typedef struct DictionaryStruct {
  OBJ_HEADER;
  OOP		tally;		/* really, an int */
  OOP		assoc[1];	/* variable sized array of associations */
  /* Other, indexable fields that are the associations for this dictionary */
} *Dictionary;

typedef struct IdentityDictionaryStruct {
  OBJ_HEADER;
  OOP		tally;		/* really, an int */
  OOP		values;		/* an Array */
  OOP		keys[1];	/* variable sized array of OOPS (symbols) */
} *IdentityDictionary;

typedef struct AssociationStruct {
  OBJ_HEADER;
  OOP		key;
  OOP		value;
} *Association;

typedef struct ArrayStruct {
  OBJ_HEADER;
  OOP		elements[1];	/* elements of the array */
} *Array;

typedef struct FloatObjectStruct {
  OBJ_HEADER;
  double	value;
} *FloatObject;

typedef struct StringStruct {
  OBJ_HEADER;
  char		chars[1];
} *String;

typedef struct ByteArrayStruct {
  OBJ_HEADER;
  Byte		bytes[1];
} *ByteArray;

typedef struct MessageStruct {
  OBJ_HEADER;
  OOP		selector;
  OOP		args;
} *Message;

typedef struct ClassInfoStruct {
  OOP		*classVar;
  OOP		*superClassPtr;
  Boolean	isPointers;
  Boolean	isWords;
  Boolean	isIndexable;
  char		numFixedFields;
  char		*name;
  char		*instVarNames;
  char		*classVarNames;
  char		*sharedPoolNames;
  char		*comment;
} ClassInfo;

/* Primary class variables. These variables hold the class objects for
   all of the builtin classes in the system */
OOP			objectClass, magnitudeClass, charClass, timeClass,
			dateClass,
			numberClass, floatClass, integerClass, lookupKeyClass,
			associationClass, linkClass, processClass,
			symLinkClass, collectionClass,
			sequenceableCollectionClass, linkedListClass,
			semaphoreClass,
			arrayedCollectionClass, arrayClass, stringClass,
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
			writeStreamClass, readWriteStreamClass,
			cObjectClass, fileStreamClass, memoryClass,
			byteMemoryClass, wordMemoryClass, randomClass,
			cFuncDescriptorClass, tokenStreamClass,
			methodInfoClass, fileSegmentClass,
			processorOOP;

void 			setAssociationValue();

static Dictionary	growDictionary();
static IdentityDictionary growIdentityDictionary();
static InstanceSpec	classInstanceSpec();
static OOP		identityDictionaryNew(), systemDictionaryNew(),
			newClass(), newMetaclass();
static void		initSmalltalkDictionary(), addSmalltalk(),
			printOOPClassName(), printClassName(),
			createClassesPass1(), createClassesPass2(),
			addSubClass(), addSTDIOObject();
static int		oopNumFields();

/* The class definition structure.  From this structure, the initial set of
   Smalltalk classes are defined.  Note that the comment field is largely
   superfluous, thanks to the comment: primitive and the universal use
   of the class and class comment declarations throughout the Smalltalk
   method definition files.  In any dispute, the comment definition in the
   ".st" file wins. */

static ClassInfo classInfo[] = {
  { &objectClass,		nil,
      true,	true,	false,	0,
      "Object",		nil,	nil,    "Smalltalk CFunctionDescs",
      "I am the root of the Smalltalk class system. \n\
All classes in the system are subclasses of me." },

  { &magnitudeClass,		&objectClass,
      true,	true,	false,	0,
      "Magnitude",	nil,	nil,	nil,
      nil },

  { &messageClass,		&objectClass,
      true,	true,	false,	2,
      "Message",	"selector args",	nil,	nil,
      nil },

  { &charClass,			&magnitudeClass,
      false,	true,	true,	0, /* really has 1 indexed var */
      "Character",	nil,	nil,	nil,
      "My instances represent the 256 characters of the character set.  I provide\n\
messages to translate between integers and character objects, and provide \n\
names for some of the common unprintable characters." },

  { &timeClass,			&magnitudeClass,
      true,	true,	false,	1,
      "Time",		"seconds",	nil,	nil,
      nil },

  { &dateClass,			&magnitudeClass,
      true,	true,	false,	1,
      "Date",		"days",	nil,	nil,
      nil },

  { &numberClass,		&magnitudeClass,
      true,	true,	false,	0,
      "Number",		nil,	nil,	nil,
      nil },

  { &floatClass,		&numberClass,
      false,	true,	false,	0,	/* really 2, but we're variable sized*/
      "Float",		nil,	nil,	nil,
      nil },

  { &integerClass,	       	&numberClass,
      false,	true,	false,	0,
      "Integer",	nil,	nil,	nil,
      nil },

  { &lookupKeyClass,		&magnitudeClass,
      true,	true,	false,	0,
      "LookupKey",	nil,	nil,	nil,
      nil },

  { &associationClass,		&lookupKeyClass,
      true,	true,	false,	2,
      "Association",	"key value",	nil,	nil,
      nil },

  { &linkClass,			&objectClass,
      true,	true,	false,	1,
      "Link",	        "nextLink",	nil,	nil,
      nil },

  { &processClass,		&linkClass,
      true,	true,	false,	3,
      "Process",	"suspendedContext priority myList",	nil,	nil,
      nil },

  { &symLinkClass,		&linkClass,
      true,	true,	false,	1,
      "SymLink",	"symbol",	nil,	nil,
      nil },

  { &collectionClass,		&objectClass,
      true,	true,	false,	0,
      "Collection",	nil,	nil,	nil,
      nil },

  { &sequenceableCollectionClass,	&collectionClass,
      true,	true,	true,	0,
      "SequenceableCollection",	nil,	nil,	nil,
      nil },

  { &linkedListClass,		&sequenceableCollectionClass,
      true,	true,	false,	2,
      "LinkedList",	"firstLink lastLink",	nil,	nil,
      nil },

  { &semaphoreClass,		&linkedListClass,
      true,	true,	false,	1,
      "Semaphore",	"signals",	nil,	nil,
      nil },

  { &arrayedCollectionClass,	&sequenceableCollectionClass,
      true,	true,	true,	0,
      "ArrayedCollection",	nil,	nil,	nil,
      nil },

  { &arrayClass,		&arrayedCollectionClass,
      true,	true,	true,	0,
      "Array",  	nil,	nil,	nil,
      nil },

  { &stringClass,		&arrayedCollectionClass,
      false,	false,	true,	0,
      "String",		nil,	nil,	nil,
      nil },

  { &symbolClass,		&stringClass,
      false,	false,	true,	0,
      "Symbol",		nil,	nil,	nil,
      nil },

  { &byteArrayClass,		&arrayedCollectionClass,
      false,	false,	true,	0,
      "ByteArray",	nil,	nil,	nil,
      nil },

  { &compiledMethodClass,	&arrayedCollectionClass,
      false,	false,	true,	2, /* leave this this way */
      "CompiledMethod",	"descriptor methodHeader",	nil,	nil, 
      "I represent methods that have been compiled.  I can recompile \n\
methods from their source code, I can invoke Emacs to edit the source code \n\
for one of my instances, and I know how to access components of my \n\
instances." },

  { &intervalClass,	&sequenceableCollectionClass,
      true,	true,	false,	3,
      "Interval",	"start stop step", nil,	nil,
      "My instances represent ranges of objects, typically Magnitude type\n\
objects.  I provide iteration/enumeration messages for producing all the\n\
members that my instance represents." },

  { &orderedCollectionClass,	&sequenceableCollectionClass,
      true,	true,	true,	2,
      "OrderedCollection",	"firstIndex lastIndex",	nil,	nil,
      nil },

  { &sortedCollectionClass,	&orderedCollectionClass,
      true,	true,	true,	1,
      "SortedCollection",	"sortBlock",	nil,	nil,
      "I am a collection of objects, stored and accessed according to some\n\
sorting criteria.  I store things using a bubble sort.  My instances have a \n\
comparison block associated with them; this block takes two arguments and\n\
is a predicate which returns true if the first argument should be sorted \n\
earlier than the second.  The default block is [ :a :b | a <= b ], but I\n\
will accept any block that conforms to the above criteria." },

  { &bagClass,	&collectionClass,
      true,	true,	false,	1,
      "Bag",		"contents",	nil,	nil,
      "My instances are unordered collections of objects.  You can think\n\
of me as a set with a memory; that is, if the same object is added to me\n\
twice, then I will report that that element has been stored twice." },

  { &mappedCollectionClass,	&collectionClass,
      true,	true,	false,	2,
      "MappedCollection",	"domain map",	nil,	nil,
      nil },

  { &setClass,	&collectionClass,
      true,	true,	true,	1,
      "Set",		"tally",	nil,	nil,
      "I am the typical set object; I can store any objects uniquely.  I\n\
use the = operator to determine duplication of objects." },

  { &dictionaryClass,	&setClass,
      true,	true,	true,	0,
      "Dictionary",	nil,	nil,	nil,
      "I implement a dictionary, which is an object that is indexed by \n\
unique objects (typcially instances of Symbol), and associates another \n\
object with that index.  I use the equality operator = to determine \n\
equality of indices." },

  { &identityDictionaryClass,		&dictionaryClass,
      true,	true,	true,	1,
      "IdentityDictionary",	"values",	nil,	nil,
      "I am similar to dictionary, except that my representation is \n\
different, and I use the object identity comparision message == to \n\
determine equivalence of indices." },

  /* MUST have the same structure as dictionary; they're used interchangeably
   * within the C portion of the system */
  { &systemDictionaryClass,		&dictionaryClass,
      true,	true,	true,	0, 
      "SystemDictionary",	nil,	nil,	nil,
      nil },

  { &streamClass,		&objectClass,
      true,	true,	false,	0,
      "Stream",		nil,	nil,	nil,
      nil },

  { &tokenStreamClass,		&streamClass,
      true,	true,	false,	1,
      "TokenStream",		"charStream",	nil,	nil,
      "I am not a typical part of the Smalltalk kernel class hierarchy.\n\
I operate on a stream of characters and return distinct \n\
(whitespace-delimited) groups of characters." },

  { &positionableStreamClass,	&streamClass,
      true,	true,	false,	4,
      "PositionableStream",	"collection ptr endPtr access",	nil,	nil,
      nil },

  { &readStreamClass,		&positionableStreamClass,
      true,	true,	false,	0,
      "ReadStream",	nil,	nil,	nil,
      nil },

  { &writeStreamClass,		&positionableStreamClass,
      true,	true,	false,	1,
      "WriteStream",	"maxSize",	nil,	nil,
      nil },

  { &readWriteStreamClass,	&writeStreamClass,
      true,	true,	false,	0,
      "ReadWriteStream",	nil,	nil,	nil,
      nil },

  { &fileStreamClass,		&readWriteStreamClass,
      true,	true,	false,	2,
      "FileStream",	"file name",		nil,	nil,
      "My instances are what conventional programmers think of as files.\n\
My instance creation methods accept the name of a disk file (or any named \n\
file object, such as /dev/rmt0 on UNIX or MTA0: on VMS)." },

  { &randomClass,		&streamClass,
      true,	true,	false,	1,
      "Random",		"seed",		nil,	nil,
      nil },

  { &undefinedObjectClass,		&objectClass,
      true,	true,	false,	0,
      "UndefinedObject",	nil,	nil,	nil,
      "I have the questionable distinction of being a class with only one\n\
instance, which is the object \"nil\".  I suspect that I should be sent\n\
messages when errors occur, but currently I am not." },

  { &booleanClass,		&objectClass,
      true,	true,	false,	0,
      "Boolean",	nil,	nil,	nil,
      nil },

  { &falseClass,		&booleanClass,
      true,	true,	false,	1,
      "False",		"truthValue",	nil,	nil, /* ### what's the inst var name in ST-80? */
      nil },

  { &trueClass,		&booleanClass,
      true,	true,	false,	1,
      "True",		"truthValue",	nil,	nil, /* ### what's the inst var name in ST-80? */
      nil },

  { &processorSchedulerClass,	&objectClass,
      true,	true,	false,	2,
      "ProcessorScheduler",	"processLists activeProcess",	nil,	nil,
      nil },

  { &delayClass,	&objectClass,
      true,	true,	false,	0, /* ### wrong */
      "Delay",	nil,	nil,	nil,
      nil },

  { &sharedQueueClass,	&objectClass,
      true,	true,	false,	3, 
      "SharedQueue",	"queueSem valueReady queue",	nil,	nil,
      nil },

  /* Change this, classDescription, or Class, and you must change 
   * the implementaion of newMetaclass some */
  { &behaviorClass,		&objectClass,
      true,	true,	false,	4,
      "Behavior",	"superClass subClasses methodDictionary instanceSpec",
      nil,	nil,
      nil },

  { &classDescriptionClass,		&behaviorClass,
      true,	true,	false,	4,
      "ClassDescription",	"name comment instanceVariables category",
      nil,	nil,
      nil },

  { &classClass,		&classDescriptionClass,
      true,	true,	false,	2,
      "Class",	"classVariables sharedPools",	nil,	nil,
      nil },

  { &metaclassClass,		&classDescriptionClass,
      true,	true,	false,	1,
      "Metaclass",	"instanceClass",	nil,	nil,
      nil },

  { &methodContextClass,		&objectClass,
      true,	true,	true,	7,
      "MethodContext",	"sender ip sp method block selector receiver",	nil,	nil,
      nil },

  { &blockContextClass,		&objectClass,
      true,	true,	true,	7,
      "BlockContext",	"caller ip sp numArgs initialIP selector home",	nil,
     nil,
      nil },

/***********************************************************************
 *
 *	End of Standard Smalltalk Class definitions.  The definitions below are
 *	specific to GNU Smalltalk.
 *
 ***********************************************************************/

  { &cObjectClass,		&objectClass,
      false,	true,	true,	0,
      "CObject",	nil,	nil,	nil,
      "I am not part of the standard Smalltalk kernel class hierarchy.\n\
My instances contain values that are not interpreted by the Smalltalk \n\
system; they frequently hold \"pointers\" to data outside of the Smalltalk\n\
environment.  The C callout mechanism allows my instances to be transformed\n\
into their corresponding C values for use in external routines." },

  { &cFuncDescriptorClass,	&objectClass,
      true,	true,	true,	4,
      "CFunctionDescriptor",	"cFunction cFunctionName returnType numFixedArgs",
      nil,	nil,
      nil },

  { &memoryClass,		&objectClass,
      false,	true,	true,	0,
      "Memory",		nil,	nil,	nil,
      nil },

  { &byteMemoryClass,		&memoryClass,
      false,	false,	true,	0,
      "ByteMemory",	nil,	nil,	nil,
      nil },

  { &wordMemoryClass,		&memoryClass,
      false,	true,	true,	0,
      "WordMemory",	nil,	nil,	nil,
      nil },

  { &methodInfoClass,		&objectClass,
      true,	true,	false,	2,
      "MethodInfo",	"sourceCode category",	nil,	nil,
      nil },

  { &fileSegmentClass,		&objectClass,
      true,	true,	false,	3,
      "FileSegment",	"fileName startPos length",	nil,	nil,
      nil },

  { nil }

/* Smalltalk classes not defined:
   Fraction
   SmallInteger, LargeInteger
   Bitmap, DisplayBitmap, RunArray
   Text
   FileDirectory, FilePage (probably never will be defined)
   Point, Rectangle, BitBlt, CharacterScanner, Pen 
   DisplayObject hierarchy
 */

};


/*
 *	initDictionary()
 *
 * Description
 *
 *	Creates the kernel classes of the Smalltalk system.  Operates in two
 *	passes: pass1 creates the class objects, but they're not completely
 *	initialized.  pass2 finishes the initialization process.  The garbage
 *	collector can NOT run during this time.
 *
 */
void initDictionary()
{
  Metaclass	objectMetaclass; /* the metaclass of class Object */

  createClassesPass1();

  initCharTable();		/* we can do this now that char class def'd */
  initNil();
  initBooleans();

  initSmalltalkDictionary();

  createClassesPass2();

  initSTDIOObjects();
}

static void createClassesPass1()
{
  ClassInfo	*ci;
  OOP		parentClassOOP;

  /* Because all of the classes in classInfo are in the root set, we
   * never need to validate them */
  for (ci = classInfo; ci->classVar; ci++) {
    if (ci->superClassPtr == nil) {
      parentClassOOP = (OOP)nil;
    } else {
      parentClassOOP = *ci->superClassPtr;
    }
      
    *ci->classVar = newClass(parentClassOOP, ci->isPointers, ci->isWords,
			     ci->isIndexable, ci->numFixedFields);
  }
}

/* runs before GC turned on */
static void  createClassesPass2()
{
  ClassInfo	*ci;
  OOP		classOOP, superClassOOP;
  Class		class, superClass;
  long		index;

  /* Because all of the classes in classInfo are in the root set, we
   * never need to validate them */
  for (ci = classInfo; ci->classVar; ci++) {
    classOOP = *ci->classVar;
    class = (Class)oopToObj(classOOP);
    class->name = internString(ci->name);
    addSmalltalk(ci->name, classOOP);
    class->methodDictionary = nilOOP;
    index = toInt(class->subClasses);
    if (classOOP == classClass) {
      /*
       * Object class being a subclass of Class is not an apparent link,
       * and so the index which is the number of subclasses of the class
       * is off by one.  We correct that here.
       */
      index++;
    }
    class->subClasses = arrayNew(index);
    if (index > 0) {
      arrayAtPut(class->subClasses, 1, fromInt(index));
    }
    if (classOOP == classClass) {
      /*
       * we don't want the meta class to have a subclass if we're special
       * casing Object class, so back off the number of sub classes for
       * the meta class.
       */
      index--;
    }
    if (classOOP == objectClass) { /* is this Object? */
      /* nilOOP wasn't available during pass1, but now it is */
      class->superClass = nilOOP;
    } else {
      /* hack the parent's subclass array */
      superClassOOP = class->superClass;
      addSubClass(superClassOOP, classOOP);
      if (classOOP == classClass) {
	/* here's where we patch in Object class is-a-subclass-of Class */
	superClass = (Class)oopToObj(oopClass(objectClass));
	superClass->superClass = classOOP;
	addSubClass(classOOP, oopClass(objectClass));
      }
    }
    class->objClass = newMetaclass(classOOP, index);
    class->instanceVariables =
      makeInstanceVariableArray(class->superClass, ci->instVarNames);
    class->classVariables = makeClassVariableDictionary(class->superClass,
							ci->classVarNames);
    class->sharedPools = makePoolArray(class->superClass, ci->sharedPoolNames);
    if (ci->comment) {
      class->comment = stringNew(ci->comment);
    } else {
      class->comment = nilOOP;	/* mark for later use */
    }

    class->category = nilOOP;	/* not used yet. */
  }
}

/* runs before GC turned on */
static OOP newMetaclass(classOOP, numSubClasses)
OOP	classOOP;
int	numSubClasses;
{
  OOP		superClassOOP, metaclassOOP;
  int		index;
  Metaclass	metaclass;

  metaclass = (Metaclass)newInstance(metaclassClass);
  metaclassOOP = allocOOP(metaclass);
  superClassOOP = superClass(classOOP);

  if (classOOP == objectClass) {
    /* Object case: make this be Class to close the circularity */
    metaclass->superClass = classClass;
  } else {
    metaclass->superClass = oopClass(superClassOOP);
    addSubClass(metaclass->superClass, metaclassOOP);
  }

  /* the specifications here should match what a class should have: instance
     variable names, the right number of instance variables, etc.  We could
     take three passes, and use the instance variable spec for classes once
     it's established, but it's easier to create them here by hand */
  metaclass->name = nilOOP;
  metaclass->comment = nilOOP;
  metaclass->instanceVariables = 
      makeInstanceVariableArray(nilOOP, 
"superClass subClasses methodDictionary instanceSpec \
name comment instanceVariables category \
classVariables sharedPools");

  metaclass->category = nilOOP;
  metaclass->subClasses = arrayNew(numSubClasses);
  if (numSubClasses > 0) {
    arrayAtPut(metaclass->subClasses, 1, fromInt(numSubClasses));
  }
  metaclass->methodDictionary = nilOOP;
  metaclass->instanceSpec.intMark = 1;
  metaclass->instanceSpec.isPointers = 1;
  metaclass->instanceSpec.isWords = 1;
  metaclass->instanceSpec.isIndexable = 0;
  metaclass->instanceSpec.numFixedFields = 
    (sizeof(struct ClassStruct) - sizeof(ObjectHeader))/sizeof(OOP);

  metaclass->instanceClass = classOOP;

  return (metaclassOOP);
}

static void addSubClass(superClassOOP, subClassOOP)
OOP	superClassOOP, subClassOOP;
{
  ClassDescription superClass;
  int		index;

  superClass = (ClassDescription)oopToObj(superClassOOP);

  if (numOOPs(oopToObj(superClass->subClasses)) > 0) {
    index = toInt(arrayAt(superClass->subClasses, 1));
    arrayAtPut(superClass->subClasses, 1, fromInt(index - 1));
    arrayAtPut(superClass->subClasses, index, subClassOOP);
  } else {
    errorf("Attempt to add subclass to zero sized class");
  }
}

/*
 *	static void initSmalltalkDictionary()
 *
 * Description
 *
 *	This creates the SystemDictionary called Smalltalk and initializes some
 *	of the variables in it.
 *
 */
static void initSmalltalkDictionary()
{
  OOP		cFunctionDescsDictionary;
  char	    	fullVersionString[200];

  symbolTable = arrayNew(INITIAL_SYMBOL_TABLE_SIZE);

  smalltalkDictionary = systemDictionaryNew();
  addSmalltalk("Smalltalk",		smalltalkDictionary);
  cFunctionDescsDictionary = dictionaryNew();
  addSmalltalk("CFunctionDescs",	cFunctionDescsDictionary);

  sprintf(fullVersionString, "Smalltalk version %s", versionString);
  addSmalltalk("Version", stringNew(fullVersionString));

#ifdef BIG_ENDIAN
  addSmalltalk("Bigendian", trueOOP);
#else
  addSmalltalk("Bigendian", falseOOP);
#endif

  addSmalltalk("KernelInitialized", falseOOP);

  addSmalltalk("SymbolTable", symbolTable);

  initProcessSystem();

  addSmalltalk("Processor",		processorOOP);
}

static void addSmalltalk(globalName, globalValue)
char	*globalName;
OOP	globalValue;
{
  dictionaryAtPut(smalltalkDictionary, internString(globalName), globalValue);
}


OOP findClass(classNameOOP)
OOP	classNameOOP;
{
  return (dictionaryAt(smalltalkDictionary, classNameOOP));
}

void initSTDIOObjects()
{
  addSTDIOObject(stdin, "stdin");
  addSTDIOObject(stdout, "stdout");
  addSTDIOObject(stderr, "stderr");
}

static void addSTDIOObject(file, fileObjectName)
FILE	*file;
char	*fileObjectName;
{
  OOP		fileOOP, fileStreamOOP;

  fileOOP = cObjectNew(file);
  fileStreamOOP = allocOOP(instantiate(fileStreamClass));
  setFileStreamFile(fileStreamOOP, fileOOP, stringNew(fileObjectName));

  addSmalltalk(fileObjectName, fileStreamOOP);
}


/* runs before GC turned on */
static OOP newClass(superClassOOP, isPointers, isWords, isIndexable,
		    numFixedFields)
OOP	superClassOOP;
Boolean	isPointers, isWords, isIndexable;
int	numFixedFields;
{
  Class		class, superClass;
  InstanceSpec	superInstanceSpec;

  if (superClassOOP != (OOP)nil) {
    /* adjust the number of instance variables to account for inheritance */
    superInstanceSpec = classInstanceSpec(superClassOOP);
    numFixedFields += superInstanceSpec.numFixedFields;
    superClass = (Class)oopToObj(superClassOOP);
    superClass->subClasses = fromInt(toInt(superClass->subClasses) + 1);
  }

  class			= (Class)allocObj(sizeof(struct ClassStruct));
  class->objSize	= ROUNDED_WORDS(sizeof(struct ClassStruct));
  class->objClass	= nil;
  class->superClass			= superClassOOP;
  class->instanceSpec.intMark		= 1;
  class->instanceSpec.isPointers	= isPointers;
  class->instanceSpec.isWords		= isWords;
  class->instanceSpec.isIndexable	= isIndexable;
  class->instanceSpec.numFixedFields	= numFixedFields;
  class->subClasses			= fromInt(0);

  return (allocOOP(class));
}


void setComment(classDescOOP, commentOOP)
OOP	classDescOOP, commentOOP;
{
    Class	class;

    class = (Class)oopToObj(classDescOOP);
    class->comment = commentOOP;
}


void printOOPConstructor(oop)
OOP	oop;
{
  InstanceSpec	instanceSpec;
  OOP		classOOP;
  Class		class;

  if (isAMetaclass(oop)) {
    classOOP = findAnInstance(oop);
    if (isNil(classOOP)) {
      printf("<name unknown>");		/* we're a nameless class */
    } else {
      printClassName(classOOP);
    }
    printf(" class");
    return;
  }

  if (isAClass(oop)) {
    printClassName(oop);
    return;
  }

  printOOPClassName(oop);

  classOOP = oopClass(oop);
  instanceSpec = classInstanceSpec(classOOP);
  if (instanceSpec.isIndexable) {
    printf(" new: %d ", numIndexableFields(oop));
  } else {
    printf(" new ");
  }

  /* ### still need to print the initialization for instance variables */
  if (regressionTesting) {
    printf("\"<%#x>\"", 0);
  } else {
    printf("\"<%#x>\"", oop);
  }
}

Boolean isAMetaclass(oop)
OOP	oop;
{
  if (isInt(oop)) {
    return (false);
  }

  return (oopClass(oop) == metaclassClass);
}

Boolean isAClass(oop)
OOP	oop;
{
  OOP		classOOP;

  if (isInt(oop)) {
    return (false);
  }

  classOOP = oopClass(oop);
  return (oopClass(classOOP) == metaclassClass);
}

static void printOOPClassName(oop)
OOP	oop;
{
  OOP		classOOP;
  Class		class;

  if (isInt(oop)) {
    classOOP = integerClass;
  } else {
    classOOP = oopClass(oop);
  }

  printClassName(classOOP);
}


static void printClassName(classOOP)
OOP	classOOP;
{
  Class		class;

  class = (Class)oopToObj(classOOP);
  if (isNil(class->name)) {
    printf("<no class name>");
  } else {
    printString(class->name);
  }
}

OOP getClassSymbol(classOOP)
OOP	classOOP;
{
  Class		class;

  class = (Class)oopToObj(classOOP);
  return (class->name);
  /* this is the case when we have a metaclass,
     ??? I don't think that this is right, but I don't know what else to do
     here */
}


/*
 *	OOP metaclassInstance(metaclassOOP)
 *
 * Description
 *
 *	Returns the class that is the sole instance of the meta class
 *	"metaclassOOP".
 *
 * Inputs
 *
 *	metaclassOOP: 
 *		An OOP that should be a meta class.
 *
 * Outputs
 *
 *	The class that's the sole instance of "metaclassOOP".
 */
OOP metaclassInstance(metaclassOOP)
OOP	metaclassOOP;
{
  return (((Metaclass)oopToObj(metaclassOOP))->instanceClass);
}

/*
 *	OOP validClassMethodDictionary(classOOP)
 *
 * Description
 *
 *	Gets the method dictionary associated with "classOOP", and returns it.
 *	If the methodDictionary associated with "classOOP" is nil, one is
 *	created and installed into that class.
 *
 * Inputs
 *
 *	classOOP: 
 *		Class to get the method dictionary of.
 *
 * Outputs
 *
 *	A non-nil object of type MethodDictionary.
 */
OOP validClassMethodDictionary(classOOP)
OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);
  if (isNil(class->methodDictionary)) {
    class->methodDictionary = identityDictionaryNew();
  }

  return (class->methodDictionary);
}

OOP classMethodDictionary(classOOP)
OOP	classOOP;
{
  Class		class;

  class = (Class)oopToObj(classOOP);
  return (class->methodDictionary);
}

OOP classVariableDictionary(classOOP)
OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);
  return (class->classVariables);
}

OOP instanceVariableArray(classOOP)
OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class  objects */
  class = (Class)oopToObj(classOOP);
  return (class->instanceVariables);
}

OOP sharedPoolDictionary(classOOP)
OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);
  return (class->sharedPools);
}


OOP findSharedPoolVariable(classOOP, symbol)
OOP	classOOP, symbol;
{
  OOP		assocOOP, poolDictionaryOOP;
  Class		class;
  int		numPools, i;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);

  /* ??? shared pools are currently represented as arrays, from the book
     I conjecture that their shared pools are implemented as sets. */
  numPools = numOOPs(oopToObj(class->sharedPools));
  for (i = 0; i < numPools; i++) {
    poolDictionaryOOP = arrayAt(class->sharedPools, i+1);
    assocOOP = dictionaryAssociationAt(poolDictionaryOOP, symbol);
    if (!isNil(assocOOP)) {
      return (assocOOP);
    }
  }

  return (nilOOP);
}

/*
 *	Boolean isAKindOf(memberOOP, classOOP)
 *
 * Description
 *
 *	Checks to see if "memberOOP" is a subclass of "classOOP", returning
 *	true if it is.
 *
 * Inputs
 *
 *	memberOOP: 
 *		A class OOP that's to be checked for (sub)class membership.
 *	classOOP: 
 *		A class OOP that's the conjectured (super)class.
 *
 * Outputs
 *
 *	True if "memberOOP" is a (sub)class of "classOOP".
 */
Boolean isAKindOf(memberOOP, classOOP)
OOP	memberOOP, classOOP;
{
  for ( ; !isNil(memberOOP); memberOOP = superClass(memberOOP)) {
    if (memberOOP == classOOP) {
      return (true);
    }
  }
  
  return (false);
}

/*
 *	OOP superClass(classOOP)
 *
 * Description
 *
 *	Given an OOP for a class, this routine returns the superclass OOP for
 *	that class.  Note: this is NOT the metaclass, this is the parent class.
 *
 * Inputs
 *
 *	classOOP: 
 *		OOP that references a class.
 *
 * Outputs
 *
 *	Superclass of "classOOP".  A class OOP or nil OOP.
 */
OOP superClass(classOOP)
OOP	classOOP;
{
  return (((Class)oopToObj(classOOP))->superClass);
}

OOP findClassMethod(classOOP, selector)
OOP	classOOP, selector;
{
  Class		class;
  IdentityDictionary methodDictionary;
  OOP		methodDictionaryOOP;
  int		index;

  class = (Class)oopToObj(classOOP);
  methodDictionaryOOP = class->methodDictionary;
  if (isNil(methodDictionaryOOP)) {
    return (nilOOP);
  }

  index = identityDictionaryFindKeyOrNil(methodDictionaryOOP, selector);
  methodDictionary = (IdentityDictionary)oopToObj(methodDictionaryOOP);

  return (arrayAt(methodDictionary->values, index+1));
}

static OOP identityDictionaryNew()
{
  IdentityDictionary identityDictionary;

  identityDictionary =
    (IdentityDictionary)instantiateWith(identityDictionaryClass,
				       INITIAL_DICTIONARY_SIZE);
  identityDictionary->tally = fromInt(0);
  identityDictionary->values = arrayNew(INITIAL_DICTIONARY_SIZE);

  return (allocOOP(identityDictionary));
}

OOP identityDictionaryAtPut(identityDictionaryOOP, keyOOP, valueOOP)
OOP	identityDictionaryOOP, keyOOP, valueOOP;
{
  IdentityDictionary identityDictionary;
  Array		valuesArray;
  long		index;
  
  index = identityDictionaryFindKeyOrNil(identityDictionaryOOP, keyOOP);
  identityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);

  if (isNil(identityDictionary->keys[index])) {
    identityDictionary->tally = incrInt(identityDictionary->tally);
  }
  prepareToStore(identityDictionaryOOP, keyOOP);
  identityDictionary->keys[index] = keyOOP;
  valuesArray = (Array)oopToObj(identityDictionary->values);
  prepareToStore(identityDictionary->values, valueOOP);
  valuesArray->elements[index] = valueOOP;

  return (keyOOP);
}

static IdentityDictionary growIdentityDictionary(identityDictionaryOOP)
OOP	identityDictionaryOOP;
{
  IdentityDictionary oldIdentityDictionary, identityDictionary;
  Array		values, oldValues;
  OOP		key, valuesOOP, oldValuesOOP, oldOOP;
  long		oldNumFields, numFields, i, index;

  oldIdentityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);
  oldNumFields = numOOPs(oldIdentityDictionary) - OBJ_HEADER_SIZE_WORDS;

  numFields = oldNumFields * 2;

  identityDictionary =
    (IdentityDictionary)instantiateWith(identityDictionaryClass, numFields);
  maybeMoveOOP(identityDictionaryOOP); /* make sure that it's valid */
  oldIdentityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);
  identityDictionary->tally = oldIdentityDictionary->tally;
  setOOPObject(identityDictionaryOOP, identityDictionary);
  identityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);

  oldValuesOOP = oldIdentityDictionary->values;
  maybeMoveOOP(oldValuesOOP);	/* ### not sure that this is necessary */
  oldValues = (Array)oopToObj(oldValuesOOP);
  valuesOOP = arrayNew(numFields);
  values = (Array)oopToObj(valuesOOP);
  identityDictionary->values = valuesOOP;

  /* rehash all associations from old dictionary into new one */
  for (i = 0; i < oldNumFields; i++) {
    key = oldIdentityDictionary->keys[i];
    if (!isNil(key)) {
      index = identityDictionaryFindKeyOrNil(identityDictionaryOOP, key);
      maybeMoveOOP(key);
      identityDictionary->keys[index] = key;
      oldOOP = oldValues->elements[i];
      maybeMoveOOP(oldOOP);
      values->elements[index] = oldOOP;
    }
  }

  maybeMoveOOP(identityDictionary->values);
  maybeMoveOOP(identityDictionaryOOP);
  return (identityDictionary);
}

int identityDictionaryFindKeyOrNil(identityDictionaryOOP, keyOOP)
OOP	identityDictionaryOOP, keyOOP;
{
  IdentityDictionary identityDictionary;
  register long	index, count;
  long		numFields;
  
  identityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);
  for ( ; ; ) {
/* ### WRONG WRONG WRONG ### this is not accounting for the instance
   variables*/
    numFields = numOOPs(identityDictionary) - OBJ_HEADER_SIZE_WORDS;
    index = hash(keyOOP);
    index %= numFields;
    count = numFields;

    /* linear reprobe -- it is simple and guaranteed */
    for ( ; count > 0; index = (index + 1) % numFields, count--) {
      if (isNil(identityDictionary->keys[index])) {
	return (index);
      }

      if (identityDictionary->keys[index] == keyOOP) {
	return (index);
      }
    }

    /*
     * If we get to here, the dictionary is full, but we haven't found
     * the element that we're looking for.  Since we either return the
     * index of the element being sought, or the index of a nil element,
     * and the dictionary was full so that there was no nil element, we
     * grow the dictionary and scan it again.  We're guaranteed to exit
     * this loop via a return after at most two iterations.
     */
    identityDictionary = growIdentityDictionary(identityDictionaryOOP);
  }
}

/*
 *	pid(id)
 *
 * Description
 *
 *	Debug support routine.  Prints out the keys of an IdentityDictionary.
 *
 * Inputs
 *
 *	id    : an IdentityDictionary
 *
 */
pid(id)
IdentityDictionary id;
{
  int i;

  for (i = 0; i < toInt(id->tally); i++) {
    printf("%d: "); printObject(id->keys[i]); printf("\n");
  }
}

static OOP systemDictionaryNew()
{
  OOP		dictionaryOOP;
  Dictionary	dictionary;

  /* ^super new! */
  dictionaryOOP = dictionaryNew();
  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  dictionary->objClass = systemDictionaryClass;
  return (dictionaryOOP);
}

OOP dictionaryNew()
{
  Dictionary	dictionary;

  dictionary = (Dictionary)instantiateWith(dictionaryClass,
					   INITIAL_DICTIONARY_SIZE);
  dictionary->tally = fromInt(0);

  return (allocOOP(dictionary));
}


int dictionarySize(dictionaryOOP)
OOP	dictionaryOOP;
{
  Dictionary	dictionary;

  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  return (toInt(dictionary->tally));
}


OOP dictionaryAtPut(dictionaryOOP, keyOOP, valueOOP)
OOP	dictionaryOOP, keyOOP, valueOOP;
{
  OOP		associationOOP;

  associationOOP = associationNew(keyOOP, valueOOP);
  return (dictionaryAdd(dictionaryOOP, associationOOP));
}

OOP dictionaryAdd(dictionaryOOP, associationOOP)
OOP	dictionaryOOP,  associationOOP;
{
  long		index, size;
  Association	association;
  Dictionary	dictionary;
  OOP		value;

  association = (Association)oopToObj(associationOOP);
  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  if (toInt(dictionary->tally) >= numOOPs(dictionary)-1) {
    dictionary = growDictionary(dictionaryOOP);
  }

  index = findKeyOrNil(dictionaryOOP, association->key);
  if (isNil(dictionary->assoc[index])) {
    prepareToStore(dictionaryOOP, associationOOP);
    dictionary->tally = incrInt(dictionary->tally);
    dictionary->assoc[index] = associationOOP;
  } else {
    value = associationValue(associationOOP);
    associationOOP = dictionary->assoc[index];
    setAssociationValue(associationOOP, value);
  }

  return (associationOOP);
}

/*
 *	static Dictionary growDictionary(dictionaryOOP)
 *
 * Description
 *
 *	Called when a dictionary becomes full, this routine replaces the
 *	dictionary instance that "dictionaryOOP" is pointing to with a new,
 *	larger dictionary, and returns this new dictionary as its value.
 *
 * Inputs
 *
 *	dictionaryOOP: 
 *		Object pointer to the dictionary that's to be expanded
 *
 * Outputs
 *
 *	New dictionary, with all of the old elements rehashed into it. 
 */
static Dictionary growDictionary(dictionaryOOP)
OOP	dictionaryOOP;
{
  Dictionary	oldDictionary, dictionary;
  long		oldNumFields, numFields, i, index;
  OOP		associationOOP;
  Association	association;


  oldDictionary = (Dictionary)oopToObj(dictionaryOOP);
  oldNumFields = numOOPs(oldDictionary) - 1;

  numFields = oldNumFields * 2;

  dictionary = (Dictionary)instantiateWith(oopClass(dictionaryOOP), numFields);
  dictionary->tally = oldDictionary->tally;
  maybeMoveOOP(dictionaryOOP);	/* make sure old dictionary is valid */
  oldDictionary = (Dictionary)oopToObj(dictionaryOOP);
  setOOPObject(dictionaryOOP, dictionary);

  /* rehash all associations from old dictionary into new one */
  for (i = 0; i < oldNumFields; i++) {
    if (!isNil(oldDictionary->assoc[i])) {
      associationOOP = oldDictionary->assoc[i];
      association = (Association)oopToObj(associationOOP);
      index = findKeyOrNil(dictionaryOOP, association->key);
      dictionary->assoc[index] = associationOOP;
      maybeMoveOOP(associationOOP);
    }
  }

  maybeMoveOOP(dictionaryOOP);
  return (dictionary);
}

/*
 *	OOP dictionaryCopy(dictionaryOOP)
 *
 * Description
 *
 *	Create and return an exact copy of "dictionaryOOP", which is a normal
 *	dictionary object.  This is a "shallow copy"; all the associations in
 *	the dictionary are the exact same ones that are in the original
 *	dictionary.  If passed nil, returns nil.
 *
 * Inputs
 *
 *	dictionaryOOP: 
 *		A dictionary object that a copy is to be made of.
 *
 * Outputs
 *
 *	An exact copy of the dictionary that we were passed.
 */
OOP dictionaryCopy(dictionaryOOP)
OOP	dictionaryOOP;
{
  Dictionary	oldDictionary, dictionary;
  long		numFields, i;

  if (isNil(dictionaryOOP)) {
    return (nilOOP);
  }

  oldDictionary = (Dictionary)oopToObj(dictionaryOOP);
  numFields = numOOPs(oldDictionary) - 1;

  /* ??? we may want to create a copy object routine that just mallocs and
     copies the contents verbatim; this routine would then be just a call to
     that routine. */
  dictionary = (Dictionary)instantiateWith(dictionaryClass, numFields);
  memcpy(dictionary, oldDictionary, oldDictionary->objSize << 2);
  for (i = 0; i < numFields; i++) {
    maybeMoveOOP(dictionary->assoc[i]);
  }

  return (allocOOP(dictionary));
}

OOP dictionaryAt(dictionaryOOP, keyOOP)
OOP	dictionaryOOP, keyOOP;
{
  OOP		assocOOP;

  assocOOP = dictionaryAssociationAt(dictionaryOOP, keyOOP);

  if (isNil(assocOOP)) {
    return (nilOOP);
  } else {
    return (associationValue(assocOOP));
  }
}

OOP dictionaryAssociationAt(dictionaryOOP, keyOOP)
OOP	dictionaryOOP, keyOOP;
{
  long		index;
  Dictionary	dictionary; 

  if (isNil(dictionaryOOP)) {
    return (nilOOP);
  }

  index = findKeyOrNil(dictionaryOOP, keyOOP);
  dictionary = (Dictionary)dictionaryOOP->object;

  return (dictionary->assoc[index]);
}

int findKeyOrNil(dictionaryOOP, keyOOP)
OOP	dictionaryOOP, keyOOP;
{
  long		index, numFields;
  Dictionary	dictionary; 
  OOP		associationOOP;
  Association	association;

  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  numFields = numOOPs(dictionary) - 1;
  index = hash(keyOOP);
  index %= numFields;

  /* linear reprobe -- it is simple and guaranteed */
  for ( ; ; index = (index + 1) % numFields) {
    if (isNil(dictionary->assoc[index])) {
      return (index);
    }

    associationOOP = dictionary->assoc[index];
    association = (Association)associationOOP->object;

    if (equal(association->key, keyOOP)) {
      return (index);
    }
  }
}

OOP associationNew(key, value)
OOP	key, value;
{
  Association	association;

  association = (Association)newInstance(associationClass);
  maybeMoveOOP(key);
  maybeMoveOOP(value);
  association->key = key;
  association->value = value;

  return (allocOOP(association));
}

OOP associationValue(associationOOP)
OOP	associationOOP;
{
  return (((Association)oopToObj(associationOOP))->value);
}

void setAssociationValue(associationOOP, value)
OOP	associationOOP, value;
{
  prepareToStore(associationOOP, value);
  ((Association)oopToObj(associationOOP))->value = value;
}

void printAssociationKey(associationOOP)
OOP	associationOOP;
{
  Association	association;

  association = (Association)oopToObj(associationOOP);
  if (oopClass(association->key) != symbolClass) {
    printf("<unprintable key type>");
  } else {
    printSymbol(association->key);
  }
}

/*
 *	OOP instantiateOOPWith(classOOP, numIndexFields)
 *
 * Description
 *
 *	Returns an OOP for a newly allocated instance of "classOOP", with
 *	"numIndexFields" fields.  The OOP is adjusted to reflect any
 *	variance in size (such as a string that's shorter than a word boundary.
 *
 * Inputs
 *
 *	classOOP: 
 *		An OOP for the class to create the instance of.
 *	numIndexFields: 
 *		The number of index fields to create in the instance.  Must be
 *		>= 0.
 *
 * Outputs
 *
 *	A new OOP that holds the newly allocated instance, with possible
 *	correction for size.
 */
OOP instantiateOOPWith(classOOP, numIndexFields)
OOP	classOOP;
long	numIndexFields;
{
  Object	object;
  OOP		oop;
  InstanceSpec	instanceSpec;

  object = instantiateWith(classOOP, numIndexFields);
  oop = allocOOP(object);
  instanceSpec = classInstanceSpec(classOOP);
  if (!instanceSpec.isWords) {
    oop->emptyBytes = (4 - numIndexFields) & 3;
  }

  return (oop);
}

/*
 *	Object instantiateWith(classOOP, numIndexFields)
 *
 * Description
 *
 *	Returns a new, initialized instance with indexable fields.  If the
 *	instance contains pointers, they are initialized to nilOOP, else they
 *	are set to real zero.
 *
 * Inputs
 *
 *	classOOP: 
 *		Class to make an instance of.  An OOP.
 *	numIndexFields: 
 *		The number if indexed instance variables this instance is to
 *		have, possibly zero.  A long.
 *
 * Outputs
 *
 *	New instance with initialized, indexed instance variables.
 */
Object instantiateWith(classOOP, numIndexFields)
OOP	classOOP;
long	numIndexFields;
{
  Object	instance;
  InstanceSpec	instanceSpec;
  long		numBytes, OOPCount;
  register long i;
  register OOP	*oopPtr;
  
  instance = newInstanceWith(classOOP, numIndexFields);
  instanceSpec = classInstanceSpec(classOOP);
  if (instanceSpec.isPointers) {
    for (i = 0, oopPtr = instance->data,
	 OOPCount = instanceSpec.numFixedFields + numIndexFields;
	 i < OOPCount; i++) {
      *oopPtr++ = nilOOP;
    }
  } else {
    numBytes = instanceSpec.numFixedFields + numIndexFields;
    if (instanceSpec.isWords) {
      numBytes <<= 2;
    }
    bzero(instance->data, numBytes);
  }
  return (instance);
}

/*
 *	Object instantiate(classOOP)
 *
 * Description
 *
 *	Create and return a new instance of class "classOOP".  "classOOP" must
 *	be a class with no indexable fields.  The named instance variables of
 *	the new instance are initialized to nilObj, since fixed-field-only 
 *	objects can only have pointers.
 *
 * Inputs
 *
 *	classOOP: 
 *		An OOP for the class to create the instance of.
 *
 * Outputs
 *
 *	The new instance, with its fields initialized.
 */
Object instantiate(classOOP)
OOP	classOOP;
{
  Object	instance;
  InstanceSpec	instanceSpec;
  int		i;

  instance = newInstance(classOOP);
  instanceSpec = classInstanceSpec(classOOP);
  if (!instanceSpec.isPointers) {
    errorf("Class with non-pointer instance spec passed to instantiate");
  }

  for (i = 0; i < instanceSpec.numFixedFields; i++) {
    instance->data[i] = nilOOP;
  }
  return (instance);
}

Object newInstanceWith(classOOP, numIndexFields)
OOP	classOOP;
long	numIndexFields;
{
  Object	instance;
  int		numBytes;
  InstanceSpec	instanceSpec;

  numBytes = instanceSize(classOOP);
  instanceSpec = classInstanceSpec(classOOP);
  if (instanceSpec.isWords) {
    numIndexFields <<= 2;
  }
  numBytes += numIndexFields;
  numBytes = ROUNDED_WORDS(numBytes) << 2;
  instance = (Object)allocObj(numBytes);
  instance->objSize = numBytes >> 2;
  instance->objClass = classOOP;
  maybeMoveOOP(classOOP);
  return (instance);
}


/*
 *	Object newInstance(classOOP)
 *
 * Description
 *
 *	Creates a new instance of class "classOOP".  The space is allocated,
 *	the class and size fields of the class are filled in, and the instance
 *	is returned.  Its fields are NOT INITIALIZED.  "classOOP" must
 *	represent a class with no indexable fields.
 *
 * Inputs
 *
 *	classOOP: 
 *		OOP for the class that the new instance is to be an instance
 *		of.
 *
 * Outputs
 *
 *	The new instance, with objSize and objClass filled in.
 */
Object newInstance(classOOP)
OOP	classOOP;
{
  Object	instance;
  int		numBytes;

  numBytes = instanceSize(classOOP);
  instance = (Object)allocObj(numBytes);
  instance->objSize = numBytes >> 2;
  instance->objClass = classOOP;
  maybeMoveOOP(classOOP);
  return (instance);
}

/*
 *	int oopSizeBytes(oop)
 *
 * Description
 *
 *	Returns the size of object in bytes, exclusive of the size of the
 *	object header.
 *
 * Inputs
 *
 *	oop   : An OOP to return the size of
 *
 * Outputs
 *
 *	As in the description above.
 */
int oopSizeBytes(oop)
OOP	oop;
{
  return ((oop->object->objSize << 2) - sizeof(ObjectHeader));
}

int instanceSize(classOOP)
OOP	classOOP;
{
  int		numBytes;
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(classOOP);
  numBytes = instanceSpec.numFixedFields;
  if (instanceSpec.isPointers | instanceSpec.isWords) {
    numBytes <<= 2;
  }

  return (numBytes + sizeof(ObjectHeader));
}

Boolean isIndexable(classOOP)
OOP	classOOP;
{
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(classOOP);
  return (instanceSpec.isIndexable);
}

static InstanceSpec classInstanceSpec(classOOP)
OOP	classOOP;
{
  Class		class;

  class = (Class)oopToObj(classOOP);
  return (class->instanceSpec);
}

Boolean checkIndexableBoundsOf(oop, index)
OOP	oop;
int	index;
{
  if (isInt(oop)) {
    return (false);
  }

  return (index >= 1 && index <= numIndexableFields(oop));
}

Boolean checkBoundsOf(oop, index)
OOP	oop;
int	index;
{
  if (isInt(oop)) {
    return (false);
  }

  return (index >= 1 && index <= oopNumFields(oop));
}

Boolean classIsPointers(classOOP)
OOP	classOOP;
{
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(classOOP);
  return (instanceSpec.isPointers);
}

Boolean isPointers(oop)
OOP	oop;
{
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(oopClass(oop));
  return (instanceSpec.isPointers);
}

int oopFixedFields(oop)
OOP	oop;
{
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(oopClass(oop));
  if (instanceSpec.isWords) {
    return (instanceSpec.numFixedFields);
  } else {
    return (instanceSpec.numFixedFields * sizeof(OOP));
  }
}

static int oopNumFields(oop)
OOP	oop;
{
  Object	object;
  InstanceSpec	instanceSpec;
  int		numFields;

  object = oopToObj(oop);
  instanceSpec = classInstanceSpec(oopClass(oop));

  numFields = (object->objSize << 2) - sizeof(ObjectHeader);
  if (instanceSpec.isWords) {
    numFields >>= 2;
  } else {			/* must be bytes */
    numFields -= oop->emptyBytes;
  }

  return (numFields);
}

OOP indexOOP(oop, index)
OOP	oop;
int	index;
{
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(oopClass(oop));

  if (instanceSpec.isPointers) {
    index += instanceSpec.numFixedFields;
    return (oopToObj(oop)->data[index-1]);
  } else if (instanceSpec.isWords) {
    index += instanceSpec.numFixedFields;
    return (fromInt( ((long *)oopToObj(oop)->data)[index-1] ));
  } else {
    index += instanceSpec.numFixedFields * sizeof(OOP);
    return (fromInt( ((Byte *)oopToObj(oop)->data)[index-1] ));
  }
}

Boolean indexOOPPut(oop, index, value)
OOP	oop, value;
int	index;
{
  InstanceSpec	instanceSpec;
  unsigned long	valueInt;

  instanceSpec = classInstanceSpec(oopClass(oop));
  index += oopFixedFields(oop);

  if (instanceSpec.isPointers) {
    prepareToStore(oop, value);
    oopToObj(oop)->data[index-1] = value;
  } else if (instanceSpec.isWords) {
    valueInt = toInt(value);
    ((long *)oopToObj(oop)->data)[index-1] = valueInt;
  } else {
    valueInt = toInt(value);
    if (valueInt >= 256) {
      return (false);
    }
    ((Byte *)oopToObj(oop)->data)[index-1] = (Byte)valueInt;
  }

  return (true);
}

OOP indexStringOOP(oop, index)
OOP	oop;
int	index;
{
  InstanceSpec	instanceSpec;

  /* ??? I'm presuming that we have a string here */

  instanceSpec = classInstanceSpec(oopClass(oop));
  index += instanceSpec.numFixedFields;

  return (charOOPAt( ((Byte *)oopToObj(oop)->data)[index-1] ));
}

void indexStringOOPPut(oop, index, value)
OOP	oop, value;
int	index;
{
  InstanceSpec	instanceSpec;
  unsigned long	valueInt;

  /* ??? I'm presuming that we have a string oop here */

  instanceSpec = classInstanceSpec(oopClass(oop));
  index += instanceSpec.numFixedFields;

  ((Byte *)oopToObj(oop)->data)[index-1] = charOOPValue(value);
}

OOP stringNew(s)
char	*s;
{
  String	string;
  int		len;
  OOP		stringOOP;

  len = strlen(s);
  string = (String)newInstanceWith(stringClass, len);
  strncpy(string->chars, s, len);

  stringOOP = allocOOP(string);
  stringOOP->emptyBytes = (4 - len) & 3;

  return (stringOOP);
}

void setOOPString(stringOOP, s)
OOP	stringOOP;
char	*s;
{
  String	string;
  long		len;

  len = strlen(s);
  string = (String)newInstanceWith(stringClass, len);
  strncpy(string->chars, s, len);

  setOOPObject(stringOOP, string);
  stringOOP->emptyBytes = (4 - len) & 3;
}

Byte *stringOOPChars(stringOOP)
OOP	stringOOP;
{
  String	string;

  string = (String)oopToObj(stringOOP);
  return ((Byte *)string->chars);
}

Byte *toCString(stringOOP)
OOP	stringOOP;
{
  Byte		*result, *chars;
  int		len;
  String	string;

  string = (String)oopToObj(stringOOP);
  len = oopNumFields(stringOOP);
  result = (Byte *)malloc(len + 1);
  strncpy(result, string->chars, len);
  result[len] = '\0';

  return (result);
}

Byte *toByteArray(byteArrayOOP)
OOP	byteArrayOOP;
{
  Byte		*result;
  int		len;
  ByteArray	byteArray;

  byteArray = (ByteArray)oopToObj(byteArrayOOP);
  len = oopNumFields(byteArrayOOP);
  result = (Byte *)malloc(len);
  memcpy(result, byteArray->bytes, len);

  return (result);
}

OOP instVarAt(oop, index)
OOP	oop;
int	index;
{
  InstanceSpec	instanceSpec;

  instanceSpec = classInstanceSpec(oopClass(oop));

  if (instanceSpec.isPointers) {
    return (oopToObj(oop)->data[index-1]);
  } else if (instanceSpec.isWords) {
    return (fromInt( ((long *)oopToObj(oop)->data)[index-1] ));
  } else {
    return (fromInt( ((Byte *)oopToObj(oop)->data)[index-1] ));
  }
}

Boolean instVarAtPut(oop, index, value)
OOP	oop, value;
int	index;
{
  InstanceSpec	instanceSpec;
  unsigned long	valueInt;

  instanceSpec = classInstanceSpec(oopClass(oop));

  if (instanceSpec.isPointers) {
    if (GCIsOn()) {
      prepareToStore(oop, value);
    }
    oopToObj(oop)->data[index-1] = value;
  } else if (instanceSpec.isWords) {
    valueInt = toInt(value);
    ((long *)oopToObj(oop)->data)[index-1] = valueInt;
  } else {
    valueInt = toInt(value);
    if (valueInt >= 256) {
      return (false);
    }
    ((Byte *)oopToObj(oop)->data)[index-1] = (Byte)valueInt;
  }

  return (true);
}

int numIndexableFields(oop)
OOP	oop;
{
  if (isInt(oop)) {
    return (0);
  }

  return (oopNumFields(oop) - oopFixedFields(oop));
}

OOP arrayNew(numElts)
int	numElts;
{
  return (allocOOP(instantiateWith(arrayClass, numElts)));
}

OOP arrayAt(arrayOOP, index)
OOP	arrayOOP;
int	index;
{
  return ( ((Array)oopToObj(arrayOOP))->elements[index-1]);
}

void arrayAtPut(arrayOOP, index, value)
OOP	arrayOOP, value;
int	index;
{
  prepareToStore(arrayOOP, value);
  ((Array)oopToObj(arrayOOP))->elements[index-1] = value;
}

OOP floatNew(f)
double	f;
{
  FloatObject	floatObject;

  /*
   * ### Seems like this can lose on architectures where floats need
   * to be aligned...there are no guarantees that the float data
   * is aligned to an 8 byte boundary, so the store could lose.
   */
  floatObject = (FloatObject)newInstanceWith(floatClass, 2);
  floatObject->value = f;
  
  return (allocOOP(floatObject));
}

double floatOOPValue(floatOOP)
OOP	floatOOP;
{
  Object obj;
  union {
    long l[2];
    double d;
  } hack;

  if (DOUBLE_ALIGNMENT > sizeof(long)) {
    /* we may not be aligned properly...fetch things out the hard way */
    obj = oopToObj(floatOOP);
    hack.l[0] = (long)obj->data[0];
    hack.l[1] = (long)obj->data[1];
    return (hack.d);
  } else {
    return (((FloatObject)oopToObj(floatOOP))->value);
  }
}

OOP messageNewArgs(selectorOOP, argsArray)
OOP	selectorOOP, argsArray;
{
  Message	message;

  message = (Message)newInstance(messageClass);
  maybeMoveOOP(selectorOOP);
  message->selector = selectorOOP;
  maybeMoveOOP(argsArray);
  message->args = argsArray;

  return (allocOOP(message));
}

OOP messageSelector(messageOOP)
OOP	messageOOP;
{
  Message	message;

  message = (Message)oopToObj(messageOOP);
  return (message->selector);
}

OOP messageArgs(messageOOP)
OOP	messageOOP;
{
  Message	message;

  message = (Message)oopToObj(messageOOP);
  return (message->args);
}


OOP cObjectNew(cObjPtr)
voidPtr	*cObjPtr;
{
  Object	cObject;

  cObject = (Object)newInstanceWith(cObjectClass, 1);
  cObject->data[0] = (OOP)cObjPtr;
  
  return (allocOOP(cObject));
}

voidPtr cObjectValue(cObjOOP)
OOP	cObjOOP;
{
  Object	cObject;

  cObject = oopToObj(cObjOOP);
  return ((voidPtr)cObject->data[0]);
}

void setCObjectValue(cObjOOP, value)
OOP	cObjOOP;
voidPtr	value;
{
  Object	cObject;

  cObject = oopToObj(cObjOOP);
  cObject->data[0] = (OOP)value;
}
