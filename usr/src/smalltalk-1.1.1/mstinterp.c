/***********************************************************************
 *
 *	Byte Code Interpreter Module.
 *
 *	Interprets the compiled bytecodes of a method.
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
 * sbb	     12 Sep 91	  Fixed some process/gc related lossages.  Poor use of
 *			  prepareToStore.
 *
 * sbyrne    20 May 90	  Improved error handling when error: or
 *			  doesNotUnderstand: occurs.  Also, added ^C handling
 *			  to abort execution.
 *
 * sbyrne    24 Apr 90	  Improved error handling for fopen/popen primitives.
 *
 * sbyrne    20 Apr 90	  Make fileIn not close the stream that it's reading
 *			  from; this is taken care of by the caller, and causes
 *			  very strange behavior if we try to close it twice!
 *
 * sbyrne     7 Apr 90	  Added declaration tracing primitive.
 *
 * sbyrne     7 Apr 90	  Fixed fileIn: to check for existence of the file
 *			  before trying to open it.  Returns failure if the
 *			  file cannot be accessed.
 *
 * sbyrne    25 Mar 90	  Minor change for AtariSt: decrease size of ASYNC
 *			  queue size.
 *
 * sbyrne    19 Dec 89	  Added suport for primitive filein (for use with
 *			  autoload --
 *			                  "12 gauge autoloader"
 *			                  A. Swartzenegger
 *			                  The Terminator)
 *
 * sbyrne     2 Sep 89	  Process primitives in and working...starting to
 *			  switch to compiled methods with descriptor instance
 *			  variable in addition to header.
 *
 * sbyrne     9 Aug 89	  Conversion completed.  Performance now 40k
 *			  bytecodes/sec; was 43k bytecodes/sec.
 *
 * sbyrne    18 Jul 89	  Began conversion from stack based method contexts and
 *			  blocks to more traditional method contexts and
 *			  blocks.  This change was done 1) to make call in from
 *			  C easier, 2) to make processs possible (they could
 *			  have been implemented using stack based contexts, but
 *			  somewhat space-wastefully), and 3) to conform with
 *			  the more traditional definition method contexts and
 *			  block contexts.
 *
 * sbyrne    26 May 89	  added method cache!  Why didn't I spend the 1/2 hour
 *			  sooner?
 *
 * sbyrne     7 Jan 89	  Created.
 *
 */


#include "mst.h"
#include "mstinterp.h"
#include "mstdict.h"
#include "mstsym.h"
#include "mstoop.h"
#include "mstsave.h"
#include "mstcomp.h"
#include "mstcint.h"
#include "mstsysdep.h"
#include <math.h>
#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>

#define	METHOD_CACHE_SIZE		(1 << 11) /* 2048, mostly random choice */
#ifdef atarist
#define ASYNC_QUEUE_SIZE		16 /* still way too much */
#else
#define ASYNC_QUEUE_SIZE		100 /* way too much */
#endif

/* Don't enable this...it doesn't really work properly with the new GC, since
   using local register variables defeats the ability to copy the root set
   when a GC flip occurs. */
/*#define LOCAL_REGS */

/* Enabling this means that some accessors for method object pieces, such
   as instance variables or literals, are implemented as routines, instead
   of being in-line code via macros */
/* #define ACCESSOR_DEBUGGING */

#ifdef LOCAL_REGS
#define exportSP()	*spAddr = sp
#define exportIP()	*ipAddr = ip
#define exportRegs()	{ exportSP(); exportIP(); }
#define importSP()	sp = *spAddr
#define importIP()	ip = *ipAddr
#define importRegs()	{ importSP(); importIP(); }
#else
#define exportSP()
#define exportIP()
#define exportRegs()
#define importSP()
#define importIP()
#define importRegs()
#endif /* LOCAL_REGS */

#ifndef ACCESSOR_DEBUGGING

#define receiverVariableInternal(receiver, index) \
  (!inBounds(receiver, index) && errorf("Index out of bounds %d", index), \
    oopToObj(receiver)->data[index])

#define getStackReceiverInternal(numArgs) \
  (stackAt(numArgs))

#define methodTemporaryInternal(index) \
  (temporaries[index])


#define methodLiteralInternal(methodOOP, index) \
  (((Method)oopToObj(methodOOP))->literals[index])

#define methodVariableInternal(methodOOP, index) \
  (associationValue(((Method)oopToObj(methodOOP))->literals[index]))

#define getMethodByteCodesInternal(methodOOP) \
  (isNil(methodOOP) ? (Byte *)nil \
   : ((Byte *)&((Method)oopToObj(methodOOP))->literals[((Method)oopToObj(methodOOP))->header.numLiterals]))


#define getMethodHeaderInternal(methodOOP) \
  (((Method)oopToObj(methodOOP))->header)

#define getMethodClassInternal(methodOOP) \
  (associationValue(((Method)oopToObj(methodOOP))->literals[((Method)oopToObj(methodOOP))->header.numLiterals - 1]))


#define storeReceiverVariableInternal(receiver, index, oop) \
{  \
  OOP __storeRecVarOOP = (oop); \
  if (!inBounds(receiver, index)) { \
    errorf("Index out of bounds %d", index); \
  } \
  prepareToStore(receiver, __storeRecVarOOP); \
  oopToObj(receiver)->data[index] = __storeRecVarOOP; \
}

#define storeMethodTemporaryInternal(index, oop) \
{ \
  OOP __storeMethTempOOP = (oop); \
  prepareToStore(thisContextOOP, __storeMethTempOOP); \
  temporaries[index] = __storeMethTempOOP; \
}

#define storeMethodVariableInternal(methodOOP, index, oop) \
  setAssociationValue(((Method)oopToObj(methodOOP))->literals[index], oop)

#define storeMethodLiteralInternal(methodOOP, index, oop) \
{ \
  OOP __storeMethLitOOP = (oop); \
  prepareToStore(methodOOP, __storeMethLitOOP); \
  ((Method)oopToObj(methodOOP))->literals[index] = __storeMethLitOOP; \
}

#define inBoundsInternal(oop, index) \
  ((index) >= 0 && (index) < numOOPs(oopToObj(oop)))

#define isBlockContextInternal(contextOOP) \
  (oopClass(contextOOP) == blockContextClass)


#define receiverVariable		receiverVariableInternal
#define getStackReceiver		getStackReceiverInternal
#define methodTemporary			methodTemporaryInternal
#define methodVariable			methodVariableInternal
#define getMethodByteCodes		getMethodByteCodesInternal
#define getMethodClass			getMethodClassInternal
#define storeReceiverVariable		storeReceiverVariableInternal
#define storeMethodTemporary		storeMethodTemporaryInternal
#define storeMethodVariable		storeMethodVariableInternal
#define inBounds			inBoundsInternal
#define isBlockContext			isBlockContextInternal

#define methodLiteral			methodLiteralInternal
#define getMethodHeader			getMethodHeaderInternal
#define storeMethodLiteral		storeMethodLiteralInternal
#endif /* !ACCESSOR_DEBUGGING */



typedef enum {
  openFilePrim,
  closeFilePrim,
  getCharPrim,
  putCharPrim,
  seekPrim,
  tellPrim,
  eofPrim,
  popenFilePrim,
  sizePrim
} filePrimitiveTypes;

typedef struct FileStreamStruct {
  OOP		collection;
  OOP		ptr;
  OOP		endPtr;
  OOP		maxSize;
  OOP		file;
  OOP		name;
} *FileStream;

typedef struct CompiledMethodStruct *Method;

typedef struct MethodContextStruct {
  OBJ_HEADER;
  OOP		sender;
  OOP		ipOffset;	/* an integer byte index into method */
  OOP		spOffset;	/* an integer index into cur context stack */
  OOP		method;		/* the method that we're executing */
  OOP		hasBlock;	/* nil or not nil */
  OOP		selector;	/* the selector that invoked this method */
  OOP		receiver;	/* the Receiver OOP */
  OOP		contextStack[CONTEXT_STACK_SIZE];
} *MethodContext;

typedef struct BlockContextStruct {
  OBJ_HEADER;
  OOP		caller;
  OOP		ipOffset;	/* an integer byte index into method */
  OOP		spOffset;	/* an integer index into cur context stack */
  OOP		numArgs;	/* number of arguments we have */
  OOP		initialIP;	/* initial value of IP (an offset) */
  OOP		selector;	/* the selector that invoked this block */
  OOP		home;		/* the home context */
  OOP		contextStack[CONTEXT_STACK_SIZE];
} *BlockContext;

typedef struct SemaphoreStruct {
  OBJ_HEADER;
  OOP		firstLink;
  OOP		lastLink;
  OOP		signals;
} *Semaphore;

typedef struct ProcessStruct {
  OBJ_HEADER;
  OOP		nextLink;
  OOP		suspendedContext;
  OOP		priority;
  OOP		myList;
} *Process;

typedef struct ProcessorSchedulerStruct {
  OBJ_HEADER;
  OOP		processLists;
  OOP		activeProcess;
} *ProcessorScheduler;

long			byteCodeCounter;
long			cacheHits = 0;
long			cacheMisses = 0;

/* If this is true, for each byte code that is executed, the byte index
 * within the current CompiledMethod and a decoded interpretation of
 * the byte code is printed on standard output. */
Boolean			executionTracing;

/* When this is true, and an interrupt occurs (such as SIGSEGV), Smalltalk
 * will terminate itself by making a core dump (normally it does not
 * terminate in this manner). */
Boolean			makeCoreFile = false;


Byte			*ip, **ipAddr = &ip;
OOP			*sp, **spAddr = &sp;
OOP			thisMethod;

int			collide[METHOD_CACHE_SIZE];

#ifdef countingByteCodes
static long		byteCodes[256];
static long		primitives[256];
#endif

static OOP		methodCacheSelectors    [METHOD_CACHE_SIZE];
static OOP		primitiveCacheSelectors [METHOD_CACHE_SIZE];
static OOP		methodCacheClasses      [METHOD_CACHE_SIZE];
static OOP		primitiveCacheClasses   [METHOD_CACHE_SIZE];
static OOP		methodCacheMethods      [METHOD_CACHE_SIZE];
static int		primitiveCachePrimitives[METHOD_CACHE_SIZE];

static OOP		queuedAsyncSignals	[ASYNC_QUEUE_SIZE]; 
static int		asyncQueueIndex;
static OOP		switchToProcess; /* non-nil when proc switch wanted */

static OOP		*temporaries;	/* points into method or block context
					 * to start of arguments and
					 * temporaries */
static OOP		self;
static OOP		thisContextOOP;
static Boolean		inInterpreter = false;
static int		exceptFlag;

/* When true, this causes the byte code interpeter to immediately act
 * as if it saw a stream af method return bytecodes, until it finally exits.
 */
static Boolean		abortExecution = false;

/* When this is true, it means that the system is executing external C code,
 * which can be used by the ^C handler to know whether it longjmp to the
 * end of the C callout primitive in executePrimitiveOperation. */
static Boolean		inCCode = false;


/* Used to handle the case when the user types a ^C while executing callout
 * code */
static jmp_buf cCalloutJmpBuf;

/* when this flag is on and execution tracing is in effect, the top
 * of the stack is printed as well as the byte code */
static Boolean		verboseExecTracing = false;


#ifdef ACCESSOR_DEBUGGING
static OOP		methodTemporary(), receiverVariable(),
  			methodVariable(), getMethodClass(),
			getStackReceiver(), methodLiteral();
static void		storeMethodTemporary(), storeReceiverVariable(),
  			storeMethodVariable(), storeMethodLiteral();
static Boolean		inBounds(), isBlockContext();
static Byte		*getMethodByteCodes();
static MethodHeader	getMethodHeader();
#endif /* ACCESSOR_DEBUGGING */
static OOP		findMethod(), createMethodContext(), 
			getMethodContext(), getActiveProcess(),
			getProcessLists(), highestPriorityProcess(),
			removeFirstLink(), semaphoreNew();
static void		returnWithValue(), 
			sendBlockValue(),
			showBacktrace(),
			invalidateMethodCache(), methodHasBlockContext(),
			sleepProcess(), resumeProcess(), activateProcess(),
			changeProcessContext(), addLastLink(),
			suspendActiveProcess(), moveSemaphoreOOPs();
static Boolean		executePrimitiveOperation(),
			noParentContext(), isEmpty(), isRealOOP();
static char		*selectorAsString();
static signalType	interruptHandler(), stopExecuting();

static OOP		*mathSelectors[16] = {
  &plusSymbol,			/* 0  + */
  &minusSymbol,			/* 1  - */
  &lessThanSymbol,		/* 2  < */
  &greaterThanSymbol,		/* 3  > */
  &lessEqualSymbol,		/* 4  <= */
  &greaterEqualSymbol,		/* 5  >= */
  &equalSymbol,			/* 6  = */
  &notEqualSymbol,		/* 7  ~= */
  &timesSymbol,			/* 8  * */
  &divideSymbol,		/* 9  / */
  &remainderSymbol,		/* 10 \\ */
  &plusSymbol,			/* 11 @, not implemented */
  &bitShiftColonSymbol,		/* 12 bitShift: */
  &integerDivideSymbol,		/* 13 // */
  &bitAndColonSymbol,		/* 14 bitAnd: */
  &bitOrColonSymbol		/* 15 bitOr: */
};

struct SpecialSelectorStruct {
  OOP		*selector;
  int		args;
} specialMessages[16] = {
  &atColonSymbol,		1,
  &atColonPutColonSymbol,	2,
  &sizeSymbol,			0,
  &nextSymbol,			0,
  &nextPutColonSymbol,		1,
  &atEndSymbol,			0,
  &sameObjectSymbol,		1,
  &classSymbol,			0,
  &blockCopyColonSymbol,	1,
  &valueSymbol,			0,
  &valueColonSymbol,		1,
  &doColonSymbol,		1,
  &newSymbol,			0,
  &newColonSymbol,		1,
  &nilSymbol,			0, /* unimplemented selector */
  &nilSymbol,			0  /* unimplemented selector */
};

/*

### This is from the old stack based context days...update it to reflect
reality!

+-----------------------------------+
| receiver (self)		    |
+-----------------------------------+
| args				    |
+-----------------------------------+
| ...				    |
+-----------------------------------+
| temps				    |
+-----------------------------------+
| ...				    |
+-----------------------------------+
| saved ip of caller (relative)	    | FP, SP on interpreter entry
+-----------------------------------+
| saved method of caller            |
+-----------------------------------+
| saved temp pointer of caller	    |
+-----------------------------------+
| saved frame pointer of caller (?) |
+-----------------------------------+
| isBlock (boolean)                 |
+-----------------------------------+
| method context pointer	    |
+-----------------------------------+
|                                   | SP (after saving state)

 */


/*
 Interpretation of the virtual machine byte codes

0-15 push receiver variable 	0000iiii
16-31 push temporary location	0001iiii
32-63 push literal constant	001iiiii
64-95 push literal variable	010iiiii
96-103 pop & store rec var	01100iii
104-111 pop & store temp loc	01101iii
112-119 push indexed		01110iii receiver true false nil -1 0 1 2
120-123 return indexed		011110ii receiver true false nil
124-125 return st top from	0111110i message, block
126-127 unused			0111111i
128	push indir		10000000 jjkkkkkk (receiver var, temp loc,
						   lit const, lit var)
						   [jj] #kkkkkk
129	store indir		10000001 jjkkkkkk (rv, tl, illegal, lv)
130	pop & store indir	10000010 jjkkkkkk (like store indir)
131	send lit selector	10000011 jjjkkkkk sel #kkkkk with jjj args
132	send lit selector	10000100 jjjjjjjj kkkkkkkk (as 131)
133	send lit sel to super	10000101 jjjkkkkk as 131
134	send lit to super	10000110 jjjjjjjj kkkkkkkk like 132
135	pop stack top		10000111
136	duplicate stack top	10001000
137	push active context	10001001
138-143	unused
144-151	jmp iii+1		10010iii
152-159	pop & jmp false iii+1	10011iii
160-167	jmp (iii-4)*256+jjjjjjjj10100iii jjjjjjjj
168-171 pop & jmp on true	101010ii jjjjjjjj ii*256+jjjjjjjj
172-175 pop & jmp on false	101011ii jjjjjjjj like 168
176-191 send arith message	1011iiii
192-207	send special message	1100iiii
208-223 send lit sel #iiii	1101iiii with no arguments
224-239 send lit sel #iiii	1110iiii with 1 argument
240-255 send lit sel #iiii	1111iiii with 2 arguments
*/

/*
 *
 * How the interpreter works:
 *  1) The interpreter expects to be called in an environment where there
 *     already exists a well-defined method context.  The instruction pointer,
 *     stored in the global variable "ip", and the stack pointer, stored in the
 *     global variable "sp", should be set up to point into the current
 *     method and MethodContext.  Other global variables, such as "thisMethod",
 *     "self", "temporaries", etc. should also be setup.  See the routine
 *     prepareExecutionEnvironment for details.
 *  2) The interpreter checks to see if any change in its state is required,
 *     such as switching to a new process, dealing with an asynchronous signal
 *     which is not yet implemented, and printing out the byte codes that are 
 *     being executed, if that was requested by the user.
 *  3) After that, the byte code that ip points to is fetched and decoded.
 *     Some byte codes perform jumps, which are performed by merely adjusting
 *     the value of ip.  Some are message sends, which are described in
 *     more detail below.  Some instructions require more than one byte code
 *     to perform their work; ip is advanced as needed and the extension
 *     byte codes are fetched.
 *  4) After dispatching the byte code, the interpreter loops around to
 *     execute another byte code.  If ip has changed to point to nil, it is
 *     a signal that the execution of the method is over, and the interpreter
 *     returns to its caller.
 *
 * Note that the interpreter is not called recursively to implement message
 * sends.  Rather the state of the interpreter is saved away in the currently
 * executing context, and a new context is created and the global variables
 * such as ip, sp, and temporaries are initialized accordingly.
 *
 * When a message send occurs, the sendMessage routine is invoked.  It 
 * determines the class of the receiver, and checks to see if it already has
 * cached the method definition for the given selector and receiver class.
 * If so, that method is used, and if not, the receiver's method dictionary
 * is searched for a method with the proper selector.  If it's not found in
 * that method dictionary, the method dictionary of the classes parent is
 * examined, and on up the hierarchy, until a matching selector is found.
 *
 * If no selector is found, the receiver is sent a #doesNotUnderstand: message
 * to indicate that a matching method could not be found.
 *
 * If a method is found, it is examined for some special cases.  The special
 * cases are primitive return of self, return of an instance variable, or
 * execution of a primitive method definition.  This latter operation is
 * performed by the executePrimitiveOperation routine.  If the execution
 * of this primitive interpreter fails, the normal message send operation
 * is performed.
 *
 * If the found method is not one of the special cases, or if it is a 
 * primitive that failed to execute, a "normal" message send is performed.
 * This basically entails saving away what state the interpreter has, such
 * as the values of ip, and sp, being careful to save their relative locations
 * and not their physical addresses, because one or more garbage collections
 * could occur before the method context is returned to, and the absolute
 * pointers would be invalid.
 *
 * The sendMessage routine then creates a new MethodContext object, makes
 * its parent be the currently executing MethodContext, and sets up
 * the interpreters global variables to reference the new method and
 * new MethodContext.  Once those variables are set, sendMessage returns
 * to the interpreter, which cheerfully begins executing the new method,
 * totally unaware that the method that it was executing has changed.
 *
 * When a method returns, the method that called it is used to restore the
 * interpreter's global variables to the state that they were in before
 * the called method was called.  The values of ip and sp are restored to
 * their absolute address values, and the other global state variables
 * are restored accordingly.  When after the state has been restored, the
 * interpreter continues execution, again totally oblivious to the fact
 * that it's not running the same method it was on its previous byte code.
 *
 * Global state
 * The following variables constitute the interpreter's state:
 * ip -- the real memory address of the next byte code to be executed.
 * sp -- the real memory address of the stack that's stored in the currently
 *       executing block or method context.
 * thisMethod -- a CompiledMethod that is the currently executing method.
 * thisContextOOP -- a BlockContext or MethodContext that indicates the
 *                   context that the interpreter is currently running in.
 * temporaries -- physical address of the base of the method temporary
 *                variables.  Typically a small number of bytes (multiple of 4
 *                since it points to OOPs) lower than sp.
 * self -- an OOP that is the current receiver of the current message.
 * 
 * Note about the interpreter:
 * As an experiment, I unrolled the case statement somewhat into separate
 * case arms for each byte code.  The intention was to increase performance.
 * I haven't measured to see whether it makes a difference or not.
 *
 * The local regs concept was pre-GC.  By caching the values of IP and SP
 * in local register variables, I hoped to increase performance.  I only
 * needed to export the variables when I was calling out to routines that
 * might change them.  However, the garbage collector may run at any time,
 * and the values of IP and SP point to things in the root set and so will
 * change on a GC flip.  I'm leaving the code to deal with them as local 
 * registers in but conditionally compiled out until I can figure out a
 * clever way to make them be registers again, or give up on the idea totally.
 */

void interpret()
{
  Byte		ival, ival2, ival3, *savedIP;
  OOP		returnedValue, *savedSP, methodContextOOP, tempOOP;
  BlockContext	blockContext;
  int		i;
  IntState	oldSigMask;
#ifdef LOCAL_REGS
  register OOP	*sp;
  register Byte	*ip;
#endif /* LOCAL_REGS */

  importSP();
  importIP();

  inInterpreter = true;

  exceptFlag = executionTracing;

  for (; ip; ) {		/* when IP is nil, return to caller */
    clearGCFlipFlags();

    if (exceptFlag) {
      if (abortExecution) {
	goto abortMethod;	/* ugh! */
      }
      if (asyncQueueIndex) {	/* deal with any async signals  */
	oldSigMask = disableInterrupts(); /* block out everything! */
	for (i = 0; i < asyncQueueIndex; i++) {
	  /* ### this is not right...async signals must not allocate storage */
	  errorf("### Fix asyncSignal handling");
	  syncSignal(queuedAsyncSignals[i]);
	}
	asyncQueueIndex = 0;
	enableInterrupts(oldSigMask);
      }
      if (!isNil(switchToProcess)) {
	exportRegs();
	changeProcessContext(switchToProcess);
	importRegs();
	/* make sure to validate the IP again */
	continue;
      }
      if (executionTracing) {
	printf("%5d:\t", relativeByteIndex(ip, thisMethod));
	printByteCodeName(ip, relativeByteIndex(ip, thisMethod),
			  ((Method)oopToObj(thisMethod))->literals);
	printf("\n");
	if (verboseExecTracing) {
	  printf("\t  --> ");
	  printObject(stackTop());
	  printf("\n");
	}
      }
      exceptFlag = executionTracing;
    }
      

    byteCodeCounter++;
#ifdef countingByteCodes
    byteCodes[*ip]++;
#endif /* countingByteCodes */

    /* Note: some of the case arms are expanded out to literal cases,
       instead of case0: case1: ... pushOOP(receiverVariable(self, ival&15))
       this is an experiment to try to improve performance of the byte code
       interpreter throughout the system. */
    switch(ival = *ip++) {
    case  0:	pushOOP(receiverVariable(self, 0));	break;
    case  1:	pushOOP(receiverVariable(self, 1));	break;
    case  2:	pushOOP(receiverVariable(self, 2));	break;
    case  3:	pushOOP(receiverVariable(self, 3));	break;
    case  4:	pushOOP(receiverVariable(self, 4));	break;
    case  5:	pushOOP(receiverVariable(self, 5));	break;
    case  6:	pushOOP(receiverVariable(self, 6));	break;
    case  7:	pushOOP(receiverVariable(self, 7));	break;
    case  8:	pushOOP(receiverVariable(self, 8));	break;
    case  9:	pushOOP(receiverVariable(self, 9));	break;
    case 10:	pushOOP(receiverVariable(self, 10));	break;
    case 11:	pushOOP(receiverVariable(self, 11));	break;
    case 12:	pushOOP(receiverVariable(self, 12));	break;
    case 13:	pushOOP(receiverVariable(self, 13));	break;
    case 14:	pushOOP(receiverVariable(self, 14));	break;
    case 15:	pushOOP(receiverVariable(self, 15));	break;

    case 16:	pushOOP(methodTemporary(0));	break;
    case 17:	pushOOP(methodTemporary(1));	break;
    case 18:	pushOOP(methodTemporary(2));	break;
    case 19:	pushOOP(methodTemporary(3));	break;
    case 20:	pushOOP(methodTemporary(4));	break;
    case 21:	pushOOP(methodTemporary(5));	break;
    case 22:	pushOOP(methodTemporary(6));	break;
    case 23:	pushOOP(methodTemporary(7));	break;
    case 24:	pushOOP(methodTemporary(8));	break;
    case 25:	pushOOP(methodTemporary(9));	break;
    case 26:	pushOOP(methodTemporary(10));	break;
    case 27:	pushOOP(methodTemporary(11));	break;
    case 28:	pushOOP(methodTemporary(12));	break;
    case 29:	pushOOP(methodTemporary(13));	break;
    case 30:	pushOOP(methodTemporary(14));	break;
    case 31:	pushOOP(methodTemporary(15));	break;

    case 32:	pushOOP(methodLiteral(thisMethod, 0));	break;
    case 33:	pushOOP(methodLiteral(thisMethod, 1));	break;
    case 34:	pushOOP(methodLiteral(thisMethod, 2));	break;
    case 35:	pushOOP(methodLiteral(thisMethod, 3));	break;
    case 36:	pushOOP(methodLiteral(thisMethod, 4));	break;
    case 37:	pushOOP(methodLiteral(thisMethod, 5));	break;
    case 38:	pushOOP(methodLiteral(thisMethod, 6));	break;
    case 39:	pushOOP(methodLiteral(thisMethod, 7));	break;
    case 40:	pushOOP(methodLiteral(thisMethod, 8));	break;
    case 41:	pushOOP(methodLiteral(thisMethod, 9));	break;
    case 42:	pushOOP(methodLiteral(thisMethod, 10));	break;
    case 43:	pushOOP(methodLiteral(thisMethod, 11));	break;
    case 44:	pushOOP(methodLiteral(thisMethod, 12));	break;
    case 45:	pushOOP(methodLiteral(thisMethod, 13));	break;
    case 46:	pushOOP(methodLiteral(thisMethod, 14));	break;
    case 47:	pushOOP(methodLiteral(thisMethod, 15));	break;
    case 48:	pushOOP(methodLiteral(thisMethod, 16));	break;
    case 49:	pushOOP(methodLiteral(thisMethod, 17));	break;
    case 50:	pushOOP(methodLiteral(thisMethod, 18));	break;
    case 51:	pushOOP(methodLiteral(thisMethod, 19));	break;
    case 52:	pushOOP(methodLiteral(thisMethod, 20));	break;
    case 53:	pushOOP(methodLiteral(thisMethod, 21));	break;
    case 54:	pushOOP(methodLiteral(thisMethod, 22));	break;
    case 55:	pushOOP(methodLiteral(thisMethod, 23));	break;
    case 56:	pushOOP(methodLiteral(thisMethod, 24));	break;
    case 57:	pushOOP(methodLiteral(thisMethod, 25));	break;
    case 58:	pushOOP(methodLiteral(thisMethod, 26));	break;
    case 59:	pushOOP(methodLiteral(thisMethod, 27));	break;
    case 60:	pushOOP(methodLiteral(thisMethod, 28));	break;
    case 61:	pushOOP(methodLiteral(thisMethod, 29));	break;
    case 62:	pushOOP(methodLiteral(thisMethod, 30));	break;
    case 63:	pushOOP(methodLiteral(thisMethod, 31));	break;

    case 64:	pushOOP(methodVariable(thisMethod, 0));	break;
    case 65:	pushOOP(methodVariable(thisMethod, 1));	break;
    case 66:	pushOOP(methodVariable(thisMethod, 2));	break;
    case 67:	pushOOP(methodVariable(thisMethod, 3));	break;
    case 68:	pushOOP(methodVariable(thisMethod, 4));	break;
    case 69:	pushOOP(methodVariable(thisMethod, 5));	break;
    case 70:	pushOOP(methodVariable(thisMethod, 6));	break;
    case 71:	pushOOP(methodVariable(thisMethod, 7));	break;
    case 72:	pushOOP(methodVariable(thisMethod, 8));	break;
    case 73:	pushOOP(methodVariable(thisMethod, 9));	break;
    case 74:	pushOOP(methodVariable(thisMethod, 10));	break;
    case 75:	pushOOP(methodVariable(thisMethod, 11));	break;
    case 76:	pushOOP(methodVariable(thisMethod, 12));	break;
    case 77:	pushOOP(methodVariable(thisMethod, 13));	break;
    case 78:	pushOOP(methodVariable(thisMethod, 14));	break;
    case 79:	pushOOP(methodVariable(thisMethod, 15));	break;
    case 80:	pushOOP(methodVariable(thisMethod, 16));	break;
    case 81:	pushOOP(methodVariable(thisMethod, 17));	break;
    case 82:	pushOOP(methodVariable(thisMethod, 18));	break;
    case 83:	pushOOP(methodVariable(thisMethod, 19));	break;
    case 84:	pushOOP(methodVariable(thisMethod, 20));	break;
    case 85:	pushOOP(methodVariable(thisMethod, 21));	break;
    case 86:	pushOOP(methodVariable(thisMethod, 22));	break;
    case 87:	pushOOP(methodVariable(thisMethod, 23));	break;
    case 88:	pushOOP(methodVariable(thisMethod, 24));	break;
    case 89:	pushOOP(methodVariable(thisMethod, 25));	break;
    case 90:	pushOOP(methodVariable(thisMethod, 26));	break;
    case 91:	pushOOP(methodVariable(thisMethod, 27));	break;
    case 92:	pushOOP(methodVariable(thisMethod, 28));	break;
    case 93:	pushOOP(methodVariable(thisMethod, 29));	break;
    case 94:	pushOOP(methodVariable(thisMethod, 30));	break;
    case 95:	pushOOP(methodVariable(thisMethod, 31));	break;

    case  96:	storeReceiverVariable(self, 0, popOOP());	break;
    case  97:	storeReceiverVariable(self, 1, popOOP());	break;
    case  98:	storeReceiverVariable(self, 2, popOOP());	break;
    case  99:	storeReceiverVariable(self, 3, popOOP());	break;
    case 100:	storeReceiverVariable(self, 4, popOOP());	break;
    case 101:	storeReceiverVariable(self, 5, popOOP());	break;
    case 102:	storeReceiverVariable(self, 6, popOOP());	break;
    case 103:	storeReceiverVariable(self, 7, popOOP());	break;

    case 104:	storeMethodTemporary(0, popOOP());	break;
    case 105:	storeMethodTemporary(1, popOOP());	break;
    case 106:	storeMethodTemporary(2, popOOP());	break;
    case 107:	storeMethodTemporary(3, popOOP());	break;
    case 108:	storeMethodTemporary(4, popOOP());	break;
    case 109:	storeMethodTemporary(5, popOOP());	break;
    case 110:	storeMethodTemporary(6, popOOP());	break;
    case 111:	storeMethodTemporary(7, popOOP());	break;

    case 112: uncheckedPushOOP(self);		break;
    case 113: uncheckedPushOOP(trueOOP);	break;
    case 114: uncheckedPushOOP(falseOOP); 	break;
    case 115: uncheckedPushOOP(nilOOP); 	break;
    case 116: pushInt(-1);			break;
    case 117: pushInt(0);			break;
    case 118: pushInt(1);			break;
    case 119: pushInt(2);			break;

    case 120: case 121: case 122: case 123:
      switch (ival & 3) {
      case 0: uncheckedPushOOP(self);   	break;
      case 1: uncheckedPushOOP(trueOOP);	break;
      case 2: uncheckedPushOOP(falseOOP); 	break;
      case 3: uncheckedPushOOP(nilOOP); 	break;
      }

      /* fall through */

    case 124:			/* return stack top from method */
abortMethod:			/* here if ^C is seen to abort things */
      returnedValue = popOOP();

      if (isBlockContext(thisContextOOP)) {
	/*
	 * We're executing in a block context and an explicit return is
	 * encountered.  This means that we are to return from the caller of
	 * the method that created the block context, no matter how many
	 * levels of message sending are between where we currently are and
	 * our parent method context.
	 */
	blockContext = (BlockContext)oopToObj(thisContextOOP);
	methodContextOOP = blockContext->home;
	if (noParentContext(methodContextOOP)) {
	  /* ### this should send a message to Object of some kind */
	  errorf("Block returning to non-existent method context");
	  return;
	}
      } else {
	methodContextOOP = thisContextOOP;
      }

      returnWithValue(returnedValue, methodContextOOP);
      importRegs();		/* don't need to export these */
      break;

    case 125:			/* return stack top from block to caller */
      returnedValue = popOOP();
      returnWithValue(returnedValue, thisContextOOP);
      importRegs();
      break;

/* 126, 127 unused by blue book, allocating 127 for debugger's
   breakpoint (not yet implemented) */

    case 128:
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	pushOOP(receiverVariable(self, ival2 & 63));
	break;
      case 1:
	pushOOP(methodTemporary(ival2 & 63));
	break;
      case 2:
	pushOOP(methodLiteral(thisMethod, ival2 & 63));
	break;
      case 3:
	pushOOP(methodVariable(thisMethod, ival2 & 63));
	break;
      }
      break;

    case 129:
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	storeReceiverVariable(self, ival2 & 63, stackTop());
	break;
      case 1:
	storeMethodTemporary(ival2 & 63, stackTop());
	break;
      case 2:
	errorf("Attempt to store into a method constant");
	break;
      case 3:
	storeMethodVariable(thisMethod, ival2 & 63, stackTop());
      }
      break;

    case 130:
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	storeReceiverVariable(self, ival2 & 63, popOOP());
	break;
      case 1:
	storeMethodTemporary(ival2 & 63, popOOP());
	break;
      case 2:
	errorf("Attempt to store into a method constant");
	break;
      case 3:
	storeMethodVariable(thisMethod, ival2 & 63, popOOP());
      }
      break;

    case 131:			/* send selector y (xxxyyyyy), x args */
      ival2 = *ip++;
      /* ### Send message knows the number of arguments that are being
	 passed.  We could easily adjust the stack pointer here by doing
	 some kind of popNOOPs.  The only trouble is what happens when
	 the number of args doesn't agree with what the method is expecting,
	 and we have to generate an error.  Also, if we don't export the sp
	 here, we'll have to pass this as a parameter and sendMessage will
	 have to export it anyway.  The cost of an export or import is
	 about 1 or 2 instructions, so it may be cheap enough to just do
	 in the places that we need to to it */
      exportRegs();		/* ### can this be removed? */
      sendMessage(methodLiteral(thisMethod, ival2 & 31), ival2 >> 5, false);
      importRegs();
      break;

    case 132:			/* send selector y (xxxxxxxx,yyyyyyyy) x args*/
      ival2 = *ip++;		/* the number of args */
      ival3 = *ip++;		/* the selector */
      exportRegs();
      sendMessage(methodLiteral(thisMethod, ival3), ival2, false);
      importRegs();
      break;

    case 133:			/* send super selector y (xxxyyyyy), x args*/
      ival2 = *ip++;
      exportRegs();
      sendMessage(methodLiteral(thisMethod, ival2 & 31), ival2 >> 5, true);
      importRegs();
      break;

    case 134:			/* send super y (xxxxxxxx,yyyyyyyy) x args */
      ival2 = *ip++;		/* the number of args */
      ival3 = *ip++;		/* the selector */
      exportRegs();
      sendMessage(methodLiteral(thisMethod, ival3), ival2, true);
      importRegs();
      break;

    case 135:
      popOOP();
      break;

    case 136:
      tempOOP = stackTop();
      pushOOP(tempOOP);
      break;

    case 137: 			/* push active context */
      pushOOP(thisContextOOP);
      break;

    case 144: case 145: case 146: case 147:
    case 148: case 149: case 150: case 151:
      ip += (ival & 7) + 1;	/* jump forward 1 to 8 bytes */
      break;

    case 152: case 153: case 154: case 155:
    case 156: case 157: case 158: case 159:
      if (popOOP() == falseOOP) { /* jump forward if false 1 to 8 bytes */
	ip += (ival & 7) + 1;
      }
      break;

    case 160: case 161: case 162: case 163:
    case 164: case 165: case 166: case 167:
      ival2 = *ip++;		/* jump forward or back */
      ip += (((ival & 7) - 4) << 8) + ival2;
      break;

    case 168: case 169: case 170: case 171:
      ival2 = *ip++;
      if (popOOP() == trueOOP) {
	ip += ((ival & 3) << 8) + ival2;
      }
      break;

    case 172: case 173: case 174: case 175:
      ival2 = *ip++;
      if (popOOP() == falseOOP) {
	ip += ((ival & 3) << 8) + ival2;
      }
      break;

    case 176: case 177: case 178: case 179:
    case 180: case 181: case 182: case 183:
    case 184: case 185: case 186: case 187:
    case 188: case 189: case 190: case 191:
				/* send math message */
      exportRegs();
      sendMessage(*mathSelectors[ival & 15], 1, false);
      importRegs();
      break;

    case 192: case 193: case 194: case 195:
    case 196: case 197: case 198: case 199:
    case 200: case 201: case 202: case 203:
    case 204: case 205: case 206: case 207:
				/* send special message */
      exportRegs();
      sendMessage(*specialMessages[ival & 15].selector,
		  specialMessages[ival & 15].args, false);
      importRegs();
      break;

    case 208: case 209: case 210: case 211:
    case 212: case 213: case 214: case 215:
    case 216: case 217: case 218: case 219:
    case 220: case 221: case 222: case 223:
				/* send selector no args */
      exportRegs();
      sendMessage(methodLiteral(thisMethod, ival & 15), 0, false);
      importRegs();
      break;

    case 224: case 225: case 226: case 227:
    case 228: case 229: case 230: case 231:
    case 232: case 233: case 234: case 235:
    case 236: case 237: case 238: case 239:
				/* send selector 1 arg */
      exportRegs();
      sendMessage(methodLiteral(thisMethod, ival & 15), 1, false);
      importRegs();
      break;

    case 240: case 241: case 242: case 243:
    case 244: case 245: case 246: case 247:
    case 248: case 249: case 250: case 251:
    case 252: case 253: case 254: case 255:
				/* send selector 2 args */
      exportRegs();
      sendMessage(methodLiteral(thisMethod, ival & 15), 2, false);
      importRegs();
      break;

    default:
      errorf("Illegal byte code %d executed\n", ival);
      break;
    }
  }
  inInterpreter = false;

  exportRegs();
}

static void changeProcessContext(newProcess)
OOP	newProcess;
{
  MethodContext thisContext, methodContext;
  OOP		processOOP, methodContextOOP;
  Process	process;
  ProcessorScheduler processor;
  
  switchToProcess = nilOOP;
  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* save old context information */
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
    /* leave sp pointing to receiver, which is replaced on return with value*/
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
  }

  processOOP = getActiveProcess();
  process = (Process)oopToObj(processOOP);
  prepareToStore(processOOP, thisContextOOP);
  process->suspendedContext = thisContextOOP;

  processor = (ProcessorScheduler)oopToObj(processorOOP);
  prepareToStore(processorOOP, newProcess);
  processor->activeProcess = newProcess;
  
  process = (Process)oopToObj(newProcess);

  thisContextOOP = process->suspendedContext;
  /* ### should this be block context? */
  thisContext = (MethodContext)oopToObj(thisContextOOP);

  methodContextOOP = getMethodContext(thisContextOOP);

  methodContext = (MethodContext)oopToObj(methodContextOOP);
  thisMethod = methodContext->method;
  ip = toInt(thisContext->ipOffset) + getMethodByteCodes(thisMethod);
  sp = thisContext->contextStack + toInt(thisContext->spOffset);

  /* temporaries and self live in the method, not in the block */
  temporaries = methodContext->contextStack;
  self = methodContext->receiver;
}


/*
 *	static Boolean noParentContext(methodContextOOP)
 *
 * Description
 *
 *	Returns true if there is no parent context for "methodContextOOP".
 *	This occurs when the method context has been returned from, but it had
 *	created a block context during its execution and so it was not
 *	deallocated when it returned.  Now some block context is trying to
 *	return from that method context, but where to return to is undefined.
 *
 * Inputs
 *
 *	methodContextOOP: 
 *		An OOP that is the method context to be examined.
 *
 * Outputs
 *
 *	True if the current method has no parent, false otherwise.
 */
static Boolean noParentContext(methodContextOOP)
OOP methodContextOOP;
{
  MethodContext methodContext;

  methodContext = (MethodContext)oopToObj(methodContextOOP);

  return (isNil(methodContext->sender));
}

/*
 *	static OOP getMethodContext(contextOOP)
 *
 * Description
 *
 *	Returns the method context for either a block context or a method
 *	context. 
 *
 * Inputs
 *
 *	contextOOP: Block or Method context OOP
 *		
 *
 * Outputs
 *
 *	Method context for CONTEXTOOP.
 */
static OOP getMethodContext(contextOOP)
OOP	contextOOP;
{
  BlockContext blockContext;

  if (isBlockContext(contextOOP)) {
    blockContext = (BlockContext)oopToObj(contextOOP);
    return (blockContext->home);
  } else {
    return (contextOOP);
  }
}

static OOP allocMethodContext()
{
  MethodContext methodContext;

  methodContext = (MethodContext)instantiateWith(methodContextClass,
						 CONTEXT_STACK_SIZE);
  return (allocOOP(methodContext));
}

static OOP allocBlockContext()
{
  BlockContext blockContext;

  blockContext = (BlockContext)instantiateWith(blockContextClass,
					       CONTEXT_STACK_SIZE);
  return (allocOOP(blockContext));
}


#ifdef ACCESSOR_DEBUGGING
/*
 *	static Boolean isBlockContext(contextOOP)
 *
 * Description
 *
 *	Returns true if "contextOOP" is a block context.
 *
 * Inputs
 *
 *	contextOOP: 
 *		an OOP for a context that is to be checked.
 *
 * Outputs
 *
 *	True if it's a block context, false otherwise.
 */
static Boolean isBlockContext(contextOOP)
OOP	contextOOP;
{
  return (oopClass(contextOOP) == blockContextClass);
}
#endif /* ACCESSOR_DEBUGGING */


/*
 * on entry to this routine, the stack should have the receiver and the
 * arguments pushed on the stack.  We need to get a new context,
 * setup things like the IP, SP, and Temporary pointers, and then
 * return.   Note that this routine DOES NOT invoke the interpreter; it merely
 * sets up a new context so that calling (or, more typically, returning to) the
 * interpreter will operate properly.  This kind of sending is for normal
 * messages only.  Things like sending a "value" message to a block context are
 * handled by primitives which do similar things, but they use information from
 * the block and method contexts that we don't have available (or need) here.
 */

void sendMessage(sendSelector, sendArgs, sendToSuper)
OOP	sendSelector;
int	sendArgs;
Boolean	sendToSuper;
{
  OOP		methodOOP, receiver, methodClass, receiverClass,
		argsArray, newContextOOP;
  MethodContext thisContext, newContext;
  MethodHeader	header;
  int		i;
  long		hashIndex;

  if (!sendToSuper) {
    receiver = getStackReceiver(sendArgs);
    if (isInt(receiver)) {
      receiverClass = integerClass;
    } else {
      receiverClass = oopClass(receiver);
    }
  } else {
    methodClass = getMethodClass(thisMethod);
    receiverClass = superClass(methodClass);
    receiver = self;
  }

  /* hash the selector and the class of the receiver together using XOR.
   * Since both are pointers to long word aligned quantities, shift over
   * by 2 bits to remove the useless low order zeros */
  hashIndex = ((long)sendSelector ^ (long)receiverClass) >> 4;
  hashIndex &= (METHOD_CACHE_SIZE - 1);


  if (methodCacheSelectors[hashIndex] == sendSelector
      && methodCacheClasses[hashIndex] == receiverClass) {
    /* :-) CACHE HIT!!! (-: */
    methodOOP = methodCacheMethods[hashIndex];
    cacheHits++;
  } else {
    /* :-( cache miss )-: */
    methodOOP = findMethod(receiverClass, sendSelector, &methodClass);
    if (isNil(methodOOP)) {
      argsArray = arrayNew(sendArgs);
      for (i = 0; i < sendArgs; i++) {
	arrayAtPut(argsArray, i+1, stackAt(sendArgs-i-1));
      }
      popNOOPs(sendArgs);
      pushOOP(messageNewArgs(sendSelector, argsArray));
      sendMessage(doesNotUnderstandColonSymbol, 1, false);
      return;
    }
    methodCacheSelectors[hashIndex] = sendSelector;
    methodCacheClasses[hashIndex] = receiverClass;
    methodCacheMethods[hashIndex] = methodOOP;
    collide[hashIndex]++;
    cacheMisses++;
  }

  header = getMethodHeader(methodOOP);
  if (header.numArgs != sendArgs) {
    errorf("invalid number of arguments %d, expecting %d", sendArgs,
	   header.numArgs);
    return;
  }

  if (header.headerFlag != 0) {
    switch (header.headerFlag) {
    case 1:			/* return self */
      if (sendArgs != 0) {
	errorf("method returns primitive self, but has args!!!");
	return;
      }

      /* self is already on the stack...so we leave it */
      return;

    case 2:			/* return instance variable */
      if (sendArgs != 0) {
	errorf("method returns primitive instance variable, but has args!!!");
	return;
      }
      /* replace receiver with the returned instance variable */
      setStackTop(receiverVariable(receiver, header.numTemps));
      return;

    case 3:			/* send primitive */
      if (!executePrimitiveOperation(header.primitiveIndex, sendArgs,
				     methodOOP)) {
	return;
      }
      /* primitive failed.  Invoke the normal method */
      break;
    }
  }

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* save old context information */
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
    /* leave sp pointing to receiver, which is replaced on return with value*/
    thisContext->spOffset = fromInt(sp - sendArgs - thisContext->contextStack);
  }

  /* prepare the new state */
  newContextOOP = allocMethodContext();
  newContext = (MethodContext)oopToObj(newContextOOP);
  newContext->sender = thisContextOOP;
  maybeMoveOOP(methodOOP);
  newContext->method = methodOOP;
  newContext->hasBlock = nilOOP;	/* becomes non-nil when a block is created */

  /* copy self and sendArgs arguments into new context */
  maybeMoveOOP(sendSelector);
  newContext->selector = sendSelector;
  maybeMoveOOP(receiver);
  newContext->receiver = receiver;
  memcpy(newContext->contextStack, &sp[-sendArgs+1], (sendArgs) * sizeof(OOP));
  for (i = 0; i < sendArgs; i++) {
    maybeMoveOOP(newContext->contextStack[i]);
  }

  sp = &newContext->contextStack[sendArgs + header.numTemps - 1];
				/* 1 before the actual start of stack */

  thisMethod = methodOOP;
  thisContextOOP = newContextOOP;
  
  temporaries = newContext->contextStack;
  self = newContext->receiver;
  ip = getMethodByteCodes(thisMethod);
  /* ### fix getmethodbytecodes to check for actual byte codes in method */
}


/*
 *	static void returnWithValue(returnedValue, returnContext)
 *
 * Description
 *
 *	Return from context "returnContext" with value "returnedValue".  Note
 *	that this context may not be the current context.  If returnContext
 *	is not a block context, then we need to carefully unwind the
 *	"method call stack".  Here carefully means that we examine each
 *	context.  If it's a block context then we cannot deallocate it.  If
 *	it's a method context, and if during its execution it did not create a
 *	block context, then we can deallocate it.  Otherwise, we need to mark
 *	it as returned (set the sender to nilOOP) and continue up the call
 *	chain until we reach returnContext.
 *
 * Inputs
 *
 *	returnedValue: 
 *		Value to be put on the stack in the sender's context.
 *	returnContext: 
 *		The context to return from, an OOP.  This may not be the
 *		current context.
 *
 */
static void returnWithValue(returnedValue, returnContext)
OOP	returnedValue, returnContext;
{
  MethodContext	oldContext, thisContext, methodContext;
  OOP			oldContextOOP, methodContextOOP;

  while (thisContextOOP != returnContext) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    if (isBlockContext(thisContextOOP)) {
      thisContextOOP = ((BlockContext)thisContext)->caller;
    } else {
      oldContextOOP = thisContextOOP;
      thisContextOOP = thisContext->sender; /* ### what if sender is nil? */
      if (!isNil(thisContext->hasBlock)) {
	/* This context created a block.  Since we don't know who is holding
	   the block, we must presume that it is global.  Since any blocks
	   created by this method can reference arguments and temporaries
	   of this method, we must keep the method context around, but mark
	   it as non-returnable so that attempts to return from it to an
	   undefined place will lose. */
	thisContext->sender = nilOOP;
      }
    }
  }

  /* when we're here, we've deallocated any intervening contexts, and now
     we need to restore the state of the world as it was before we were called.
     Our caller has set the stack pointer to where we should place the
     return value, so all we need do is restore the interpreter's state and
     we're set. */
  /* ??? Geez, this feels clumsy.  We could have merged the "pop context"
     code below with the while loop above, using a do...while, but I wonder
     if, over the long haul, the code for popping the final context will
     be a special case and so will need separate code.  */
  oldContextOOP = thisContextOOP;
  thisContext = (MethodContext)oopToObj(thisContextOOP);
  thisContextOOP = thisContext->sender;

  if (!isBlockContext(oldContextOOP)) {
    if (!isNil(thisContext->hasBlock)) {
      /* mark it so block can't return from method */
      thisContext->sender = nilOOP;
    }
  }

  maybeMoveOOP(thisContextOOP);
  
  thisContext = (MethodContext)oopToObj(thisContextOOP);

  methodContextOOP = getMethodContext(thisContextOOP);
  if (methodContextOOP != thisContextOOP) { /* if we're a block */
    maybeMoveOOP(methodContextOOP); /* validate containing method */
  }
  methodContext = (MethodContext)oopToObj(methodContextOOP);
  thisMethod = methodContext->method;
  maybeMoveOOP(thisMethod);
  ip = toInt(thisContext->ipOffset) + getMethodByteCodes(thisMethod);
  sp = thisContext->contextStack + toInt(thisContext->spOffset);

  /* temporaries and self live in the method, not in the block */
  temporaries = methodContext->contextStack;
  self = methodContext->receiver;
  maybeMoveOOP(self);

  maybeMoveOOP(returnedValue);

  setStackTop(returnedValue);
}



/***********************************************************************
 *
 *	Simple Method Object Accessors
 *
 ***********************************************************************/

#ifdef ACCESSOR_DEBUGGING

static OOP receiverVariable(receiver, index)
OOP	receiver;
int	index;
{
  if (!inBounds(receiver, index)) {
    errorf("Index out of bounds %d", index);
  }
  return (oopToObj(receiver)->data[index]);
}

static OOP getStackReceiver(numArgs)
int	numArgs;
{
  /* this is correct: numArgs == 0 means that there's just the receiver
     on the stack, at 0.  numArgs = 1 means that at location 0 is the arg,
     location 1 is the receiver. */
  return (stackAt(numArgs));
}

static OOP methodTemporary(index)
int	index;
{
  return (temporaries[index]);
}

static OOP methodLiteral(methodOOP, index)
OOP	methodOOP;
int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  /* ### check for in bounds with index */
  return (method->literals[index]);
}

static OOP methodVariable(methodOOP, index)
OOP	methodOOP;
int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  return (associationValue(method->literals[index]));
}

static Byte *getMethodByteCodes(methodOOP)
OOP	methodOOP;
{
  Method	method;

  if (isNil(methodOOP)) {
    return (nil);
  }

  method = (Method)oopToObj(methodOOP);

  /* skip the header and the number of literals to find the start of the
     byte codes */
  return ((Byte *)&method->literals[method->header.numLiterals]);
}

static MethodHeader getMethodHeader(methodOOP)
OOP	methodOOP;
{
  Method	method;

  method = (Method)oopToObj(methodOOP);
  return (method->header);
}

/*
 *	static OOP getMethodClass(method)
 *
 * Description
 *
 *	This is called when a method contains a send to "super".  The compiler
 *	is supposed to notice a send to "super", and make sure that the last
 *	literal of a method is an association between the symbol for the
 *	class of the method and the class of the method itself.  This routine
 *	returns the class of the method itself using this association.
 *
 * Inputs
 *
 *	method: An OOP that represents a method.
 *
 * Outputs
 *
 *	An OOP for the class of the method.
 */
static OOP getMethodClass(methodOOP)
OOP	methodOOP;
{
  Method	method;
  OOP		associationOOP;

  method = (Method)oopToObj(methodOOP);
  associationOOP = method->literals[method->header.numLiterals - 1];
  return (associationValue(associationOOP));
}

/***********************************************************************
 *
 *	Simple Method Object Storing routines.
 *
 ***********************************************************************/


static void storeReceiverVariable(receiver, index, oop)
OOP	receiver, oop;
int	index;
{
  if (!inBounds(receiver, index)) {
    errorf("Index out of bounds %d", index);
  }
  prepareToStore(receiver, oop);
  oopToObj(receiver)->data[index] = oop;
}

static void storeMethodTemporary(index, oop)
int	index;
OOP	oop;
{
  prepareToStore(thisContextOOP, oop);
  temporaries[index] = oop;
}

static void storeMethodVariable(methodOOP, index, oop)
OOP	methodOOP, oop;
int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  setAssociationValue(method->literals[index], oop);
}

static void storeMethodLiteral(methodOOP, index, oop)
OOP	methodOOP, oop;
int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  prepareToStore(methodOOP, oop);
  method->literals[index] = oop;
}

static Boolean inBounds(oop, index)
OOP	oop;
int	index;
{
  Object	obj = oopToObj(oop);

  return (index >= 0 && index < numOOPs(obj));
}
#endif /* ACCESSOR_DEBUGGING */

MethodHeader getMethodHeaderExt(methodOOP)
OOP	methodOOP;
{
  return (getMethodHeader(methodOOP));
}

void storeMethodLiteralExt(methodOOP, index, oop)
OOP	methodOOP, oop;
int	index;
{
  storeMethodLiteral(methodOOP, index, oop);
}

/*
 *	void storeMethodLiteralNoGC(methodOOP, index, oop)
 *
 * Description
 *
 *	This routine exists primarily for the binary save/restore code.  Rather
 *	than adding a test of the garbage collector's state to a very busy
 *	routine, it's better to create a a clone that doesn't do the prepare to
 *	store.  If this routine were more complicated, it would make sense to
 *	do the test in storeMethodLiteral (ala instVarAtPut).
 *
 * Inputs
 *
 *	methodOOP: 
 *		A method OOP to set the literal of.
 *	index : the zero-based index of the literal to set
 *	oop   : the OOP to store into the method's literal table.
 *
 */
void storeMethodLiteralNoGC(methodOOP, index, oop)
OOP	methodOOP, oop;
int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  method->literals[index] = oop;
}

/*
 *	OOP methodLiteralExt(methodOOP, index)
 *
 * Description
 *
 *	External accessor routine.  Returns a literal from the given method.
 *
 * Inputs
 *
 *	methodOOP: 
 *		A CompiledMethod OOP.
 *	index : An index into the literals of the method.
 *
 * Outputs
 *
 *	The literal at index in the CompiledMethod.
 */
OOP methodLiteralExt(methodOOP, index)
OOP	methodOOP;
int	index;
{
  return (methodLiteral(methodOOP, index));
}

/*
 *	Boolean equal(oop1, oop2)
 *
 * Description
 *
 *	Internal definition of equality.  Returns true if "oop1" and "oop2" are
 *	the same object, false if they are not, and false and an error if they
 *	are not the same and not both Symbols.
 *
 * Inputs
 *
 *	oop1  : An OOP to be compared, typically a Symbol.
 *	oop2  : An OOP to be compared, typically a Symbol.
 *
 * Outputs
 *
 *	True if the two objects are the same object, false if not, and an error
 *	message if they are not the same and not both symbols.
 */
Boolean equal(oop1, oop2)
OOP	oop1, oop2;
{
  if (oop1 == oop2) {
    /* no brain case (ha ha ha) */
    return (true);
  }

  if (isClass(oop1, symbolClass) && isClass(oop2, symbolClass)) {
    return (false);
  }

  errorf("Internal #= called with invalid object types\n");
  return (false);
}

/*
 *	long hash(oop)
 *
 * Description
 *
 *	Internal hash function.  Currently defined only for symbols, but may be
 *	extended as needed for other objects.  The definition of the hash
 *	function used here must be the same as that defined in Smalltalk
 *	methods.
 *
 * Inputs
 *
 *	oop   : An OOP to be hashed.
 *
 * Outputs
 *
 *	Hash value of the OOP, or 0 and an error message if the OOP does not
 *	have a defined has value (that this routine knows how to compute).
 */
long hash(oop)
OOP	oop;
{
  if (!isInt(oop) && oopClass(oop) == symbolClass) {
    return (oopIndex(oop));
  }

  errorf("Internal #hash called with invalid object type\n");
  return (0);
}

/*
 *	static Boolean executePrimitiveOperation(primitive, numArgs, methodOOP)
 *
 * Description
 *
 *	This routine provides the definitions of all of the primitive methods
 *	in the GNU Smalltalk system.  It normally removes the arguments to the
 *	primitive methods from the stack, but if the primitive fails, the
 *	arguments are put back onto the stack and this routine returns false,
 *	indicating failure to invoke the primitive.
 *
 * Inputs
 *
 *	primitive: 
 *		A C int that indicates the number of the primitive to invoke.
 *		Must be > 0.
 *	numArgs: 
 *		The number of arguments that the primitive has.
 *	methodOOP: 
 *		The OOP for the currently executing method.  This allows
 *		primitives to poke around in the method itself, to get at
 *		pieces that they need.  Normally, this is only used by the C
 *		callout routine to get at the compiled-in descriptor for the
 *		called C function.
 *
 * Outputs
 *
 *	True if the execution of the primitive operation succeeded, false if it
 *	failed for some reason.
 */
static Boolean executePrimitiveOperation(primitive, numArgs, methodOOP)
int	primitive, numArgs;
OOP	methodOOP;
{
  Boolean	failed, atEof;
  OOP		oop, oop1, oop2, oop3, oop4, oopVec[4], classOOP, fileOOP,
		blockContextOOP;
  long		arg1, arg2, arg3;
  double	farg1, farg2, fdummy;
  int		i, ch;
  BlockContext	blockContext;
  Byte		*fileName, *fileMode;
  FILE		*file;
  FileStream	fileStream;
  Semaphore	sem;
#if !defined(USG)
  struct timeval tv;
#else
  time_t tv;
#endif
  struct stat	statBuf;
#ifdef LOCAL_REGS
  register OOP	*sp;
#endif /* LOCAL_REGS */

  importSP();

#ifdef countingByteCodes
  primitives[primitive]++;
#endif

  failed = true;
  switch (primitive) {
  case  1: case  2: case  3: case  4:
  case  5: case  6: case  7: case  8:
  case  9: case 10: case 11: case 12:
  case 13: case 14: case 15: case 16:
  case 17:
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1) && isInt(oop2)) {
      failed = false;
      arg1 = toInt(oop1);
      arg2 = toInt(oop2);
      /* ??? make this faster by not pushing and popping */

      switch(primitive) {
      case 1:	pushInt(arg1 + arg2);		break;
      case 2:	pushInt(arg1 - arg2);		break;
      case 3:	pushBoolean(arg1 < arg2);	break;
      case 4:	pushBoolean(arg1 > arg2);	break;
      case 5:	pushBoolean(arg1 <= arg2);	break;
      case 6:	pushBoolean(arg1 >= arg2);	break;
      case 7:	pushBoolean(arg1 == arg2);	break;
      case 8:	pushBoolean(arg1 != arg2);	break;
      case 9:	pushInt(arg1 * arg2);		break; /* ### overflow? */
      case 10:
	if (arg2 != 0 /*&& (arg1 % arg2) == 0*/) { /* ### fix this when coercing goes in */
	  pushInt(arg1 / arg2);
	} else {
	  failed = true;
	}
	break;
      case 11:
	if (arg2 != 0) {
	  if ((arg1 ^ arg2) < 0) {
	    /* ??? help...is there a better way to do this? */
	    pushInt(arg1 - ((arg1 - (arg2-1)) / arg2) * arg2);
	  } else {
	    pushInt(arg1 % arg2);
	  }
	} else {
	  failed = true;
	}
	break;
      case 12:
	if (arg2 != 0) {
	  if ((arg1 ^ arg2) < 0) { /* differing signs => negative result */
	    pushInt((arg1 - (arg2-1)) / arg2);
	  } else {
	    pushInt(arg1 / arg2);
	  }
	} else {
	  failed = true;
	}
	break;
      case 13:
	if (arg2 != 0) {
	  pushInt(arg1 / arg2);
	} else {
	  failed = true;
	}
	break;
      case 14:	pushInt(arg1 & arg2);	  	break;
      case 15:	pushInt(arg1 | arg2);		break;
      case 16:	pushInt(arg1 ^ arg2);		break;
      case 17:
	/* ??? check for overflow */
	if (arg2 >= 0) {
	  pushInt(arg1 << arg2);
	} else {
	  pushInt(arg1 >> -arg2);
	}
	break;
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 40:
    oop1 = popOOP();
    if (isInt(oop1)) {
      pushOOP(floatNew((double)toInt(oop1)));
      failed = false;
    }

    if (failed) {
      unPop(1);
    }
    break;

  case 41: case 42: case 43: case 44:
  case 45: case 46: case 47: case 48:
  case 49: case 50:
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop1, floatClass) && isClass(oop2, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      farg2 = floatOOPValue(oop2);
      switch (primitive) {
      case 41:	pushOOP(floatNew(farg1 + farg2));	break;
      case 42:	pushOOP(floatNew(farg1 - farg2));	break;
      case 43:	pushBoolean(farg1 < farg2); 		break;
      case 44:	pushBoolean(farg1 > farg2);		break;
      case 45:	pushBoolean(farg1 <= farg2);		break;
      case 46:	pushBoolean(farg1 >= farg2);		break;
      case 47:	pushBoolean(farg1 == farg2);		break;
      case 48:	pushBoolean(farg1 != farg2);		break;
      case 49:	pushOOP(floatNew(farg1 * farg2));	break;
      case 50:
	if (farg2 != 0.0) {
	  pushOOP(floatNew(farg1 / farg2));
	} else {
	  failed = true;
	}
	break;
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 51:			/* Float truncated */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      pushInt((long)floatOOPValue(oop1));
    } else {
      unPop(1);
    }
    break;

  case 52:			/* Float fractionPart */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      if (farg1 < 0.0) {
	farg1 = -farg1;
      }
      pushOOP(floatNew(modf(farg1, &fdummy)));
    } else {
      unPop(1);
    }
    break;

  case 53:			/* Float exponent */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      if (farg1 == 0.0) {
	arg1 = 1;
      } else {
	frexp(floatOOPValue(oop1), (int *)&arg1);
      }
      pushInt(arg1-1);
    } else {
      unPop(1);
    }
    break;

  case 54:			/* Float timesTwoPower: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop1, floatClass) && isInt(oop2)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      arg2 = toInt(oop2);
#ifdef SUNOS40
      pushOOP(floatNew(scalbn(farg1, arg2)));
#else
      pushOOP(floatNew(ldexp(farg1, arg2)));
#endif
    } else {
      unPop(2);
    }
    break;

  case 60:			/* Object at:, Object basicAt: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	failed = false;
	pushOOP(indexOOP(oop1, arg2));
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 61:			/* Object at:put:, Object basicAt:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	if (indexOOPPut(oop1, arg2, oop3)) {
	  failed = false;
	  pushOOP(oop3);
	}
      }
    }

    if (failed) {
      unPop(3);
    }
    break;

  case 62:			/* Object basicSize; Object size; String size;
				   ArrayedCollection size */
    oop1 = popOOP();
    pushOOP(fromInt(numIndexableFields(oop1)));
    failed = false;
    break;

  case 63:			/* String at:; String basicAt: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	pushOOP(indexStringOOP(oop1, arg2));
	failed = false;
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 64:			/* String basicAt:put:; String at:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();

    if (isInt(oop2) && isClass(oop3, charClass)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	indexStringOOPPut(oop1, arg2, oop3);
	failed = false;
	pushOOP(oop3);
      }
    }

    if (failed) {
      unPop(3);
    }
    break;

  case 68:			/* CompiledMethod objectAt: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop1, compiledMethodClass) && isInt(oop2)) {
      arg2 = toInt(oop2);
      if (validMethodIndex(oop1, arg2)) {
	failed = false;
	pushOOP(compiledMethodAt(oop1, arg2));
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 69:			/* CompiledMethod objectAt:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isClass(oop1, compiledMethodClass) && isInt(oop2)) {
      arg2 = toInt(oop2);
      if (validMethodIndex(oop1, arg2)) {
	failed = false;
	compiledMethodAtPut(oop1, arg2, oop3);
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 70:			/* Behavior basicNew; Behavior new;
				   Interval class new */
    oop1 = popOOP();
    if (isOOP(oop1)) {
      if (!isIndexable(oop1)) {
	pushOOP(allocOOP(instantiate(oop1)));
	failed = false;
      }
    }

    if (failed) {
      unPop(1);
    }
    break;


  case 71:			/* Behavior new:; Behavior basicNew: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isOOP(oop1) && isInt(oop2)) {
      if (isIndexable(oop1)) {
	arg2 = toInt(oop2);
	pushOOP(instantiateOOPWith(oop1, arg2));
	failed = false;
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 72:			/* Object become: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isOOP(oop1) && isOOP(oop2)) {
      swapObjects(oop1, oop2);
      pushOOP(oop1);
      failed = false;
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 73:			/* Object instVarAt: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkBoundsOf(oop1, arg2)) {
	failed = false;
	pushOOP(instVarAt(oop1, arg2));
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 74:			/* Object instVarAt:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkBoundsOf(oop1, arg2)) {
	if (instVarAtPut(oop1, arg2, oop3)) {
	  failed = false;
	}
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 75:			/* Object asOop; Object hash; Symbol hash */
    oop1 = popOOP();
    if (isOOP(oop1)) {
      failed = false;
      pushInt(oopIndex(oop1));
    }

    if (failed) {
      unPop(1);
    }
    break;

  case 76:			/* SmallInteger asObject;
				   SmallInteger asObjectNoFail */
    oop1 = popOOP();
    if (isInt(oop1)) {
      arg1 = toInt(oop1);
      if (oopIndexValid(arg1)) {
	failed = false;
	pushOOP(oopAt(arg1-1));
      }
    }

    if (failed) {
      unPop(1);
    }
    break;

  case 77:			/* Behavior someInstance */
    oop1 = popOOP(); 
    for (oop = oopTable; oop < &oopTable[TOTAL_OOP_TABLE_SLOTS]; oop++) {
      if (oopValid(oop) && oop1 == oopClass(oop)) {
	pushOOP(oop);
	failed = false;
	break;
      }
    }

    if (failed) {
      unPop(1);
    }
    break;

  case 78:			/* Object nextInstance */
    oop1 = popOOP();
    if (!isInt(oop1)) {
      classOOP = oopClass(oop1);
      for (oop = oop1 + 1; oop < &oopTable[TOTAL_OOP_TABLE_SLOTS]; oop++) {
	if (oopValid(oop) && classOOP == oopClass(oop)) {
	  failed = false;
	  pushOOP(oop);
	  break;
	}
      }
    }

    if (failed) {
      unPop(1);
    }
    break;

  case 79:			/* CompiledMethod class newMethod:header: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop3) && isInt(oop2)) {
      failed = false;
      arg3 = toInt(oop3);
      arg2 = toInt(oop2);
      pushOOP(methodNewOOP(arg2, arg3));
    }
      
    if (failed) {
      unPop(3);
    }
    break;

  case 80:			/* ContextPart blockCopy: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      failed = false;
      arg2 = toInt(oop2);
      blockContextOOP = allocBlockContext();
      blockContext = (BlockContext)oopToObj(blockContextOOP);
      blockContext->home = getMethodContext(oop1);
      maybeMoveOOP(blockContext->home);
      blockContext->numArgs = oop2;
      methodHasBlockContext(blockContext->home);
      /* the +2 here is to skip over the jump byte codes that follow the
	 invocation of blockCopy, so that the ipIndex points to the first
	 byte code of the block. */
      blockContext->initialIP = fromInt(relativeByteIndex(ip, thisMethod) + 2);
      if (oopClass(blockContext->home) != methodContextClass) {
	errorf("Block's home is not a MethodContext!!!\n");
      }
      pushOOP(blockContextOOP);
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 81:			/* BlockContext value
				   BlockContext value:
				   BlockContext value:value:
				   BlockContext value:value:value: */
    exportSP();
    sendBlockValue(numArgs);	/* ### check number of args for agreement! */
    importSP();

    failed = false;
    break;

  case 82:			/* BlockContext valueWithArguments: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isClass(oop2, arrayClass)) {
      failed = false;
      numArgs = numIndexableFields(oop2);
      for (i = 1; i <= numArgs; i++) {
	pushOOP(arrayAt(oop2, i));
      }
      exportSP();
      sendBlockValue(numArgs);
      importSP();
    }

    if (failed) {
      unPop(1);
    }
    break;

  case 83:			/* Object perform:
				   Object perform:with:
				   Object perform:with:with:
				   Object perform:with:with:with: */
    failed = false;
    /* pop off the arguments (if any) */
    for (i = 0; i < numArgs - 1; i++) {
      oopVec[i] = popOOP();
    }
    oop1 = popOOP();		/* the selector */
    /* push the args back onto the stack */
    for (; --i >= 0; ) {
      pushOOP(oopVec[i]);
    }
    exportSP();
    sendMessage(oop1, numArgs - 1, false);
    importSP();
    break;

  case 84:			/* Object perform:withArguments: */

    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop2, arrayClass)) {
      failed = false;
      numArgs = numIndexableFields(oop2);
      for (i = 1; i <= numArgs; i++) {
	pushOOP(arrayAt(oop2, i));
      }
      exportSP();
      sendMessage(oop1, numArgs, false);
      importSP();
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 85:			/* Semaphore signal */
    oop1 = stackTop();
    if (isClass(oop1, semaphoreClass)) {
      failed = false;
      syncSignal(oop1);
    }

    break;

  case 86:			/* Semaphore wait */
    oop1 = stackTop();
    if (isClass(oop1, semaphoreClass)) {
      failed = false;
      sem = (Semaphore)oopToObj(oop1);
      if (toInt(sem->signals) > 0) {	/* no waiting here */
	sem->signals = decrInt(sem->signals);
      } else {			/* have to suspend */
	addLastLink(oop1, getActiveProcess());
	suspendActiveProcess();
      }
    }

    break;

  case 87:			/* Process resume */
    resumeProcess(stackTop());
    failed = false;
    break;

  case 88:			/* Process suspend */
    oop1 = popOOP();
    if (oop1 == getActiveProcess()) {
      failed = false;
      pushOOP(nilOOP);		/* this is our return value */
      suspendActiveProcess();
    }

    if (failed) {
      unPop(1);
    }
    break;


  case 98:			/* Time class secondClock
				 *  -- note: this primitive has different
				 *     semantics from those defined in the
				 *     book.  This primitive returns the
				 *     seconds since Jan 1, 1970 00:00:00
				 *     instead of Jan 1,1901.
				 */
    popOOP();
    failed = false;
#if !defined(USG)
    gettimeofday(&tv, nil);
    pushInt(tv.tv_sec);
#else
    (void) time(&tv);
    pushInt(tv);
#endif
    break;

  case 99:			/* Time class millisecondClock
				 * -- Note: the semantics of this primitive
				 *    are different than those described in
				 *    the book.  This primitive returns the
				 *    number of milliseconds since midnight
				 *    today. */
    popOOP();
    failed = false;
#if !defined(USG)
    gettimeofday(&tv, nil);
    pushInt((tv.tv_sec % (24*60*60)) * 1000 + tv.tv_usec / 1000);
#else
    (void) time(&tv);
    pushInt((tv % (24*60*60)) * 1000);
#endif
    break;

  case 110:			/* Object ==, Character = */
    oop2 = popOOP();
    oop1 = popOOP();
    pushBoolean(oop1 == oop2);
    failed = false;
    break;

  case 111:			/* Object class */
    oop1 = popOOP();
    /* ??? is this called with ints? */
    if (isInt(oop1)) {
      pushOOP(integerClass);
    } else {
      pushOOP(oopClass(oop1));
    }
    failed = false;
    break;

  case 113:			/* quitPrimitive */
    exit(0);
    break;

  case 128:			/* Dictionary at: */
    oop2 = popOOP();
    oop1 = popOOP();
    failed = false;
    pushOOP(dictionaryAt(oop1, oop2));
    break;

  case 129:			/* Dictionary at: put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    failed = false;
    dictionaryAtPut(oop1, oop2, oop3);
    pushOOP(oop3);
    break;

  case 130:			/* doesNotUnderstand: message */
    oop2 = popOOP();
    oop1 = popOOP();
    printObject(oop1);
    printf(" did not understand selector '");
    printSymbol(messageSelector(oop2));
    printf("'\n\n");
    showBacktrace();
    stopExecuting(0);
    failed = false;
    break;

  case 131:			/* error: message */
    oop2 = popOOP();		/* error string */
    oop1 = stackTop();		/* the receiver */
    printObject(oop1);
    printf(" error: ");
    printString(oop2);
    printf("\n\n");
    showBacktrace();
    stopExecuting(0);
    failed = false;
    break;
    
  case 132:			/* Character class value: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (arg2 >= 0 && arg2 <= 255) {
	failed = false;
	pushOOP(charOOPAt(arg2));
      }
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 133:			/* Character asciiValue */
    oop1 = popOOP();
    pushOOP(fromInt(charOOPValue(oop1)));
    failed = false;
    break;

  case 134:			/* Symbol class intern: aString */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop2, stringClass)) {
      failed = false;
      pushOOP(internStringOOP(oop2));
    }
    if (failed) {
      unPop(2);
    }
    break;

  case 135:			/* Dictionary new */
    popOOP();			/* ignore receiver */
    pushOOP(dictionaryNew());
    failed = false;
    break;

  case 136:			/* ByteMemory at: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      failed = false;
      arg2 = toInt(oop2);
      pushInt(*(Byte *)arg2);
    }

    if (failed) {
      unPop(2);
    }
    break;
    
  case 137:			/* ByteMemory at:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    if (isInt(oop2) && isInt(oop3)) {
      arg1 = toInt(oop2);
      arg2 = toInt(oop3);
      if (arg2 >= 0 && arg2 <= 255) {
	failed = false;
	*(Byte *)arg1 = arg2;
      }
    }

    if (failed) {
      unPop(2);
    }
    break;
    
  case 138:			/* Memory addressOfOOP: oop */
    oop2 = popOOP();
    oop1 = popOOP();
    if (!isInt(oop2)) {
      failed = false;
      pushInt((long)oop2);
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 139:			/* Memory addressOf: oop */
    oop2 = popOOP();
    oop1 = popOOP();
    if (!isInt(oop2)) {
      failed = false;
      pushInt((long)oopToObj(oop2));
    }

    if (failed) {
      unPop(2);
    }
    break;

  case 140:			/* SystemDictionary backtrace */
    showBacktrace();
    failed = false;
    break;


  case 141:			/* SystemDictionary executionTrace: aBoolean */
    oop1 = popOOP();
    if (oop1 == trueOOP) {
      executionTracing = true;
    } else {
      executionTracing = false;
    }
    exceptFlag = true;
    failed = false;
    break;

  case 142:			/* SystemDictionary declarationTrace: aBoolean */
    oop1 = popOOP();
    if (oop1 == trueOOP) {
      declareTracing = true;
    } else {
      declareTracing = false;
    }
    failed = false;
    break;

  case 143:			/* ClassDescription comment: aString */
    oop2 = popOOP();
    oop1 = stackTop();
    setComment(oop1, oop2);
    failed = false;
    break;

  case 150:			/* methodsFor: category */
    setCompilationCategory(popOOP());
    setCompilationClass(stackTop());
    failed = false;
    break;


  case 160:			/* exp */
  case 161:			/* ln */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      switch (primitive) {
      case 160: pushOOP(floatNew(exp(farg1)));	break;
      case 161: pushOOP(floatNew(log(farg1)));	break;
      }
    }
    if (failed) {
      unPop(1);
    }
    break;

  /* case 162:			/* log: aNumber -- base aNumber log */
  /* case 163:			/* floorLog: radix -- integer floor operation */

  case 164:			/* raisedTo: aNumber -- receiver ** aNumber */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop1, floatClass) && isClass(oop2, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      farg2 = floatOOPValue(oop2);
      pushOOP(floatNew(pow(farg1, farg2)));
    }
    if (failed) {
      unPop(2);
    }
    break;


  /* >>>>>> HOLE 165 <<<<<< */

  case 166:			/* sqrt -- floating result */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      pushOOP(floatNew(sqrt(farg1)));
    }
    if (failed) {
      unPop(1);
    }
    break;

  /* >>>>>> 167: HOLE <<<<<< */

  case 168:			/* ceiling */
  case 169:			/* floor */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      switch (primitive) {
      case 168: pushInt((long)ceil(farg1));	break;
      case 169: pushInt((long)floor(farg1));	break;
      }
    }
    if (failed) {
      unPop(1);
    }
    break;


  case 171:			/* truncateTo: aNumber the next multiple of aNumber nearest the receiver towards zero */
  case 172:			/* rounded -- integer nearest the receiver */
  case 173:			/* roundTo: aNumber -- multiple of aNumber nearest self */
  case 174:			/* degreesToRadians */
  case 175:			/* radiansToDegrees */

  case 176:			/* sin */
  case 177:			/* cos */
  case 178:			/* tan */
  case 179:			/* arcSin */
  case 180:			/* arcCos */
  case 181:			/* arcTan */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      failed = false;
      farg1 = floatOOPValue(oop1);
      switch (primitive) {
      case 176: pushOOP(floatNew(sin(farg1)));	break;
      case 177: pushOOP(floatNew(cos(farg1)));	break;
      case 178: pushOOP(floatNew(tan(farg1)));	break;
      case 179: pushOOP(floatNew(asin(farg1)));	break;
      case 180: pushOOP(floatNew(acos(farg1)));	break;
      case 181: pushOOP(floatNew(atan(farg1)));	break;
      }
    }
    if (failed) {
      unPop(1);
    }
    break;

  case 230:			/* SystemDictionary monitor: aBoolean */
    oop1 = popOOP();
    failed = false;
#ifdef USE_MONCONTROL    
    if (oop1 == trueOOP) {
      moncontrol(1);
    } else {
      moncontrol(0);
    }
#endif /* USE_MONCONTROL */
    break;

  case 231:			/* SystemDictionary gcMessage: aBoolean */
    oop1 = popOOP();
    failed = false;
    gcMessage = (oop1 == trueOOP);
    break;

  case 232:			/* SystemDictionary debug */
    failed = false;		/* used to allow dbx to stop based on st exec. */
    debug();
    break;

  case 233:			/* SystemDictionary verboseTrace: aBoolean */
    oop1 = popOOP();
    failed = false;
    verboseExecTracing = (oop1 == trueOOP);
    break;
    
  case 240:			/* SysFile class openFile: filename for: read-or-write */
    break;

  case 241:			/* SysFile close */
    break;

  case 242:			/* SysFile next */
    break;

  case 243:			/* SysFile nextPut: aCharOrByte */
    break;

  case 244:			/* SysFile atEnd */
    break;

  case 245:			/* SysFile position: anInteger */
    break;

  case 246:			/* SysFile size */
    break;

  case 247:			/* FileStream fileIn */
    oop1 = stackTop();
    fileStream = (FileStream)oopToObj(oop1);
    fileOOP = fileStream->file;
    file = (FILE *)cObjectValue(fileOOP);
    fileName = toCString(fileStream->name);
    if (access(fileName, 4) == 0) {
      failed = false;
      exportSP();
      initLexer(false);
      pushUNIXFile(file, fileName);
      yyparse();
      popStream(false);		/* we didn't open it, so we don't close it */
      importSP();
    }
    free(fileName);
    break;

  case 249:			/* Behavior makeDescriptorFor: funcNameString
				            returning: returnTypeSymbol
					    withArgs: argsArray */
    
    oop4 = popOOP();
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop2, stringClass) && isClass(oop3, symbolClass)
	&& (isClass(oop4, arrayClass)
	    || isClass(oop4, undefinedObjectClass))) {
      failed = false;
      pushOOP(makeDescriptor(oop2, oop3, oop4));
    }

    if (failed) {
      unPop(4);
    }
    break;

  case 250:			/* Object snapshot */
    failed = false;
    saveToFile(defaultImageName);
    break;

  case 251:			/* Object snapshot: aString */
    oop2 = popOOP();
    if (isClass(oop2, stringClass)) {
      failed = false;
      fileName = toCString(oop2);
      saveToFile(fileName);
      free(fileName);
    }

    if (failed) {
      unPop(1);
    }
    break;
    
  case 252:			/* Object basicPrint */
    printf("Object: ");
    printObject(stackTop());
    printf("\n");
    failed = false;
    break;

  case 253:			/* Behavior compileString: aString */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isClass(oop2, stringClass)) {
      failed = false;
      exportSP();
      initLexer(true);
      pushSmalltalkString(oop2);
      setCompilationClass(oop1);
      yyparse();
      popStream(false);		/* don't close a String! */
      importSP();
    }
    if (failed) {
      unPop(1);
    }
    break;

  case 254:			/* IO primitive, variadic */
    for (i = numArgs; --i >= 0; ) {
      oopVec[i] = popOOP();
    }
    oop1 = stackTop();
    if (isInt(oopVec[0])) {
      failed = false;
      arg1 = toInt(oopVec[0]);
      if (arg1 == ENUM_INT(openFilePrim) || arg1 == ENUM_INT(popenFilePrim)) {
	/* open: fileName[1] mode: mode[2] or popen: command[1] dir: direction[2] */
	fileName = toCString(oopVec[1]);
	fileMode = toCString(oopVec[2]);
	if (arg1 == ENUM_INT(openFilePrim)) {
	  file = fopen((char *)fileName, (char *)fileMode);
	} else {
	  file = popen((char *)fileName, (char *)fileMode);
	}
	if (file == NULL) {
	  errorf("Failed to open %s named '%s'",
		 (ENUM_INT(openFilePrim) == arg1) ? "file" : "pipe",
		 fileName);
	  free(fileName);
	  free(fileMode);
	  failed = true;
	  break;
	}
	  
	fileOOP = cObjectNew(file);
	setFileStreamFile(oop1, fileOOP, oopVec[1]);
	free(fileName);
	free(fileMode);
      } else {
	fileStream = (FileStream)oopToObj(oop1);
	fileOOP = fileStream->file;
	file = (FILE *)cObjectValue(fileOOP);
	switch (arg1) {

	case closeFilePrim:	/* FileStream close */
	  fclose(file);
	  break;
	
	case getCharPrim:	/* FileStream next */
	  /* ### handle returning eof here */
	  ch = getc(file);
	  setStackTop(charOOPAt(ch));
	  break;
	  
	case putCharPrim:	/* FileStream nextPut: aChar */
	  if (isClass(oopVec[1], charClass)) {
	    ch = charOOPValue(oopVec[1]);
	    fputc(ch, file);
	  } else {
	    failed = true;
	  }
	  break;

	case seekPrim:		/* FileStream position: position */
	  fseek(file, toInt(oopVec[1]), 0);
	  break;

	case tellPrim:		/* FileStream position */
	  setStackTop(fromInt(ftell(file)));
	  break;

	case eofPrim:		/* FileStream atEnd */ 
	  popOOP();		/* remove self */
	  ch = fgetc(file);
	  atEof = feof(file);
	  pushBoolean(atEof);
	  ungetc(ch, file);
	  break;

	case sizePrim:
	  if (fstat(fileno(file), &statBuf)) {
	    failed = true;
	  } else {
	    setStackTop(fromInt(statBuf.st_size));
	  }
	  break;
	}
      }
    }

    if (failed) {
      unPop(numArgs);
    }
    break;

  case 255:			/* C callout primitive */
    inInterpreter = false;
    exportSP();
    inCCode = true;
    if (setjmp(cCalloutJmpBuf) == 0) {
      invokeCRoutine(numArgs, methodOOP);
    }
    inCCode = false;
    importSP();
    inInterpreter = true;
    failed = false;
    break;

  default:
    errorf("Unhandled primitive operation %d", primitive);
  }

  exportSP();
  return (failed);

}

/*
These are the primitives as defined in the Blue Book.  The ones with numbers
but without stars are those which are not implemented.

* 1 +
* 2 -
* 3 <
* 4 >
* 5 <=
* 6 >=
* 7 =
* 8 ~=
* 9 *
* 10 /
* 11 \\
* 12 //
* 13 quo:
* 14 bitAnd:
* 15 bitOr:
* 16 bitXor:
* 17 bitShift:

* 40 Smallinteger asFloat
* 41 Float +
* 42 Float -
* 43 Float >
* 44 Float <
* 45 Float <=
* 46 Float >=
* 47 Float =
* 48 Float ~=
* 49 Float *
* 50 Float /
* 51 Float truncated
* 52 Float fractionPart
* 53 Float exponent
* 54 Float timesTwoPower:

* 60 Object at:
   Object basicAt:
* 61 Object basicAt:put:
   Object at:put:
* 62 Object basicSize
   Object size
   String size
   ArrayedCollection size
* 63 String at:
   String basicAt:
* 64 String basicAt:put:
   String at:put:

* 70 Behavior basicNew
   Behavior new
   Interval class new
* 71 Behavior new:
   Behavior basicNew:
* 72 Object become:
* 73 Object instVarAt:
* 74 Object instVarAt:put:
* 75 Object asOop
   Object hash
   Symbol hash
* 76 SmallInteger asObject
   SmallInteger asObjectNoFail
* 77 Behavior someInstance
* 78 Object nextInstance
79 CompiledMethod class newMethod:header:
* 80 ContextPart blockCopy:
* 81 BlockContext value:value:value:
   BlockContext value:
   BlockContext value:value:
* 82 BlockContext valueWithArguments:
* 83 Object perform:with:with:with:
   Object perform:with:
   Object perform:with:with:
   Object perform:
* 84 Object perform:withArguments:

105 ByteArray primReplaceFrom:to:with:startingAt:
    ByteArray replaceFrom:to:withString:startingAt:
    String replaceFrom:to:withByteArray:startingAt:
    String primReplaceFrom:to:with:startingAt:

* 110 Character =
    Object ==
* 111 Object class

*/

static OOP getActiveProcess()
{
  ProcessorScheduler processor;

  if (!isNil(switchToProcess)) {
    return (switchToProcess);
  } else {
    processor = (ProcessorScheduler)oopToObj(processorOOP);
    return (processor->activeProcess);
  }
}

static void addLastLink(semaphoreOOP, processOOP)
OOP	semaphoreOOP, processOOP;
{
  Semaphore	sem;
  Process	process, lastProcess;
  OOP		lastProcessOOP;

  prepareToStore(processOOP, semaphoreOOP);
  process = (Process)oopToObj(processOOP);
  process->myList = semaphoreOOP;
  process->nextLink = nilOOP;

  sem = (Semaphore)oopToObj(semaphoreOOP);
  if (isNil(sem->lastLink)) {
    prepareToStore(semaphoreOOP, processOOP);
    sem = (Semaphore)oopToObj(semaphoreOOP);
    sem->firstLink = sem->lastLink = processOOP;
  } else {
    lastProcessOOP = sem->lastLink;
    prepareToStore(lastProcessOOP, processOOP);
    lastProcess = (Process)oopToObj(lastProcessOOP);
    lastProcess->nextLink = processOOP;
    prepareToStore(semaphoreOOP, processOOP);
    sem = (Semaphore)oopToObj(semaphoreOOP);
    sem->lastLink = processOOP;
  }
}

syncSignal(semaphoreOOP)
OOP	semaphoreOOP;
{
  Semaphore sem;

  sem = (Semaphore)oopToObj(semaphoreOOP);
  if (isEmpty(semaphoreOOP)) {	/* nobody waiting */
    sem->signals = incrInt(sem->signals);
  } else {
    resumeProcess(removeFirstLink(semaphoreOOP));
  }
}

static OOP removeFirstLink(semaphoreOOP)
OOP	semaphoreOOP;
{
  Semaphore	sem;
  Process	process;
  OOP		processOOP;

  sem = (Semaphore)oopToObj(semaphoreOOP);
  processOOP = sem->firstLink;
  process = (Process)oopToObj(processOOP);
  prepareToStore(semaphoreOOP, process->nextLink);
  sem->firstLink = process->nextLink;
  if (isNil(sem->firstLink)) {
    sem->lastLink = nilOOP;
  }

  process->nextLink = nilOOP;
  process->myList = nilOOP;

  return (processOOP);
}

static void resumeProcess(processOOP)
OOP	processOOP;
{
  OOP		activeOOP;
  Process	process, active;

  activeOOP = getActiveProcess();
  active = (Process)oopToObj(activeOOP);
  process = (Process)oopToObj(processOOP);

  if (toInt(process->priority) > toInt(active->priority)) {
    /*
     * we're resuming a process with a higher priority, so sleep the
     * current one and activate the new one
     */
    sleepProcess(activeOOP);
    activateProcess(processOOP);
  } else {
    /* this process isn't higher than the active one */
    sleepProcess(processOOP);
  }
}

static void activateProcess(processOOP)
OOP	processOOP;
{
  switchToProcess = processOOP;
  exceptFlag = true;
}

static void sleepProcess(processOOP)
OOP	processOOP;
{
  Process	process;
  int		priority;
  OOP		processLists;
  OOP		processList;

  process = (Process)oopToObj(processOOP);
  priority = toInt(process->priority);
  processLists = getProcessLists();
  processList = arrayAt(processLists, priority);

  /* add process to end of priority queue */
  addLastLink(processList, processOOP);
}


static void suspendActiveProcess()
{
  activateProcess(highestPriorityProcess());
}


static OOP highestPriorityProcess()
{
  OOP		processLists, processList;
  int		priority;

  processLists = getProcessLists();
  priority = numOOPs(oopToObj(processLists));
  for (; priority > 0 ; priority--) {
    processList = arrayAt(processLists, priority);
    if (!isEmpty(processList)) {
      return (removeFirstLink(processList));
    }
  }

  errorf("No Runnable process!!!");
  exit(0);
}

static Boolean isEmpty(processListOOP)
OOP	processListOOP;
{
  Semaphore	processList;

  processList = (Semaphore)oopToObj(processListOOP);
  return (isNil(processList->firstLink));
}

static OOP getProcessLists()
{
  ProcessorScheduler processor;

  processor = (ProcessorScheduler)oopToObj(processorOOP);
  return (processor->processLists);
}

static OOP semaphoreNew()
{
  Semaphore	sem;

  sem = (Semaphore)instantiate(semaphoreClass);
  sem->signals = fromInt(0);

  return (allocOOP(sem));
}


/*
 *	static void methodHasBlockContext(methodOOP)
 *
 * Description
 *
 *	Marks a method context has having created a block context.
 *
 * Inputs
 *
 *	methodOOP: MethodContext OOP to be marked
 *		
 *
 */
static void methodHasBlockContext(methodOOP)
OOP	methodOOP;
{
  MethodContext methodContext;

  methodContext = (MethodContext)oopToObj(methodOOP);
  /* Since trueOOP is in the root set, we don't have to prepare to store it */
  methodContext->hasBlock = trueOOP;
}

void setFileStreamFile(fileStreamOOP, fileOOP, fileNameOOP)
OOP	fileStreamOOP, fileOOP, fileNameOOP;
{
  FileStream	fileStream;

  fileStream = (FileStream)oopToObj(fileStreamOOP);
  prepareToStore(fileStreamOOP, fileOOP);
  fileStream->file = fileOOP;
  prepareToStore(fileStreamOOP, fileNameOOP);
  fileStream->name = fileNameOOP;
}


/*
 *	static void sendBlockValue(numArgs)
 *
 * Description
 *
 *	This is the equivalent of sendMessage, but is for blocks.  The block
 *	context that is to the the receiver of the "value" message should be
 *	"numArgs" into the stack.  Temporaries come from the block's method
 *	context, as does self.  IP is set to the proper
 *	place within the block's method's byte codes, and SP is set to the top
 *	of the arguments in the block context, which have been copied out of
 *	the caller's context.
 *
 * Inputs
 *
 *	numArgs: 
 *		The number of arguments sent to the block.
 *
 */
static void sendBlockValue(numArgs)
int	numArgs;
{
  OOP		selector, blockContextOOP, methodContextOOP;
  BlockContext	blockContext;
  MethodContext thisContext, methodContext;
  int		i;

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* save old context information */
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
    /* leave sp pointing to receiver, which is replaced on return with value*/
    thisContext->spOffset = fromInt(sp - numArgs - thisContext->contextStack);
  }

  /* prepare the new state */
  blockContextOOP = stackAt(numArgs);
  maybeMoveOOP(blockContextOOP); /* make sure we're alive */

  blockContext = (BlockContext)oopToObj(blockContextOOP);
  maybeMoveOOP(thisContextOOP);	/* ### not sure if this is needed*/
  blockContext->caller = thisContextOOP;
  switch (numArgs) {
  case 0:	blockContext->selector = valueSymbol; break;
  case 1:	blockContext->selector = valueColonSymbol; break;
  case 2:	blockContext->selector = valueColonValueColonSymbol; break;
  case 3:	blockContext->selector = valueColonValueColonValueColonSymbol; break;
  default:	blockContext->selector = valueWithArgumentsColonSymbol; break;
  }

  /* home is set when the block is created */

  /* copy numArgs arguments into new context */
  memcpy(blockContext->contextStack, &sp[-numArgs+1],
	 (numArgs) * sizeof(OOP));
  for (i = 0; i < numArgs; i++) {
    maybeMoveOOP(blockContext->contextStack[i]);
  }

  sp = &blockContext->contextStack[numArgs-1]; /* start of stack-1 */

  thisContextOOP = blockContextOOP;

  methodContextOOP = blockContext->home;
  methodContext = (MethodContext)oopToObj(methodContextOOP);
  ip = toInt(blockContext->initialIP) + getMethodByteCodes(methodContext->method);
  thisMethod = methodContext->method;
  maybeMoveOOP(thisMethod);
  temporaries = methodContext->contextStack;
  self = methodContext->receiver;
  maybeMoveOOP(self);
}

/*
 *	static char *selectorAsString(selector)
 *
 * Description
 *
 *	Converts a selector to a C string object
 *
 * Inputs
 *
 *	selector: A OOP for the selector, a Symbol.
 *		
 *
 * Outputs
 *
 *	C string that corresponds to the selector's printed name.
 */
static char *selectorAsString(selector)
OOP	selector;
{
  return (symbolAsString(selector));
}

/*
 *	static OOP findMethod(receiverClass, selector)
 *
 * Description
 *
 *	Scans the methods of "receiverClass" and all its super classes for one
 *	with selector "selector".  It returns the method if it found, otherwise
 *	nil is returned.
 *
 * Inputs
 *
 *	receiverClass:
 *		The class to begin the search in.  This is normally called from
 *		the message sending code, so that's why this parameter is
 *		called receiverClass.
 *	selector:
 *		The selector for the method that is being sought.
 *	methodClassPtr:
 *		The class that the method was eventually found in.  Passed
 *		by reference and set when returning.
 *
 * Outputs
 *
 *	Method for "selector", or nilOOP if not found.  "methodClassPtr" is
 *	returned as a by-reference parameter.
 */
static OOP findMethod(receiverClass, selector, methodClassPtr)
OOP	receiverClass, selector, *methodClassPtr;
{
  OOP		classOOP, methodOOP;

  for (classOOP = receiverClass; !isNil(classOOP);
       classOOP = superClass(classOOP)) {
    methodOOP = findClassMethod(classOOP, selector);
    if (!isNil(methodOOP)) {
      *methodClassPtr = classOOP;
      return (methodOOP);
    }
  }

  *methodClassPtr = undefinedObjectClass; /* probably not used */
  return (nilOOP);
}

/* runs before GC turned on */
void initProcessSystem()
{
  OOP		processLists;
  int		i;
  ProcessorScheduler processor;
  Process	initialProcess;
  OOP		initialProcessOOP;
  

  processLists = arrayNew(NUM_PRIORITIES);

  for (i = 1; i <= NUM_PRIORITIES; i++) {
    arrayAtPut(processLists, i, semaphoreNew()); /* ### should be linked list */
  }

  initialProcess = (Process)instantiate(processClass);
  initialProcess->priority = fromInt(4);
  initialProcessOOP = allocOOP(initialProcess);


  processor = (ProcessorScheduler)instantiate(processorSchedulerClass);
  processor->processLists = processLists;
  processor->activeProcess = initialProcessOOP;

  processorOOP = allocOOP(processor);
}


void initInterpreter()
{
  thisContextOOP = nilOOP;
  asyncQueueIndex = 0;
  switchToProcess = nilOOP;
}

void prepareExecutionEnvironment()
{
  MethodContext thisContext, newContext;
  OOP		newContextOOP;

  abortExecution = false;

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* save old context information */
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
    /* leave sp pointing to receiver, which is replaced on return with value*/
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
  }

  /* now make a dummy context to run with */
  newContextOOP = allocMethodContext();
  newContext = (MethodContext)oopToObj(newContextOOP);
  ip = nil;
  maybeMoveOOP(thisContextOOP);
  newContext->sender = thisContextOOP;
  thisMethod = newContext->method = nilOOP;
  newContext->selector = nilOOP; /* no real selector invoked us */
  newContext->receiver = nilOOP; /* make self be real (well, nil) */
  sp = newContext->contextStack - 1;

  temporaries = newContext->contextStack;
  self = nilOOP;

  thisContextOOP = newContextOOP;

  invalidateMethodCache();
#ifdef countingByteCodes 
  initByteCodeCounter();
#endif
}

OOP finishExecutionEnvironment()
{
  MethodContext oldContext, thisContext;
  OOP		oldContextOOP, returnedValue;
  
  returnedValue = stackTop();
  oldContextOOP = thisContextOOP;
  oldContext = (MethodContext)oopToObj(oldContextOOP);
  thisContextOOP = oldContext->sender;

  if (!isNil(thisContextOOP)) {
    maybeMoveOOP(thisContextOOP);
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* restore old context information */
    thisMethod = thisContext->method;
    maybeMoveOOP(thisMethod);
    temporaries = thisContext->contextStack;
    self = thisContext->receiver;
    maybeMoveOOP(self);
    ip = toInt(thisContext->ipOffset) + getMethodByteCodes(thisMethod);
    sp = thisContext->contextStack + toInt(thisContext->spOffset);
  }
  return (returnedValue);
}

static void invalidateMethodCache()
{
  register int	i;

  cacheHits = cacheMisses = 0;

  for (i = 0; i < METHOD_CACHE_SIZE; i++) {
    methodCacheSelectors[i] = primitiveCacheSelectors[i] = nilOOP;
    methodCacheClasses[i] = primitiveCacheClasses[i] = nilOOP;
    methodCacheMethods[i] = nilOOP;
    collide[0] = primitiveCachePrimitives[i] = 0;
  }
}

void updateMethodCache(selectorOOP, classOOP, methodOOP)
OOP	selectorOOP, classOOP, methodOOP;
{
  long		hashIndex;

  hashIndex = ((long)selectorOOP ^ (long)classOOP) >> 4;
  hashIndex &= (METHOD_CACHE_SIZE - 1);
  if (methodCacheSelectors[hashIndex] == selectorOOP &&
      methodCacheClasses[hashIndex] == classOOP) {
    methodCacheMethods[hashIndex] = methodOOP;
  }
}

#ifdef countingByteCodes
initByteCodeCounter()
{
  int i;

  for (i = 0; i < 256; i++) {
    primitives[i] = byteCodes[i] = 0;
  }
}

printByteCodeCounts()
{
  int i;

  for (i = 0; i < 256; i++) {
    if (byteCodes[i]) {
      printf("Byte code %d = %d\n", i, byteCodes[i]);
    }
  }

  printf("\n---> primitives:\n");
  for (i = 0; i < 256; i++) {
    if (primitives[i]) {
      printf("Primitive %d = %d\n", i, primitives[i]);
    }
  }

}
#endif

relativeByteIndex(bp, methodOOP)
Byte	*bp;
OOP	methodOOP;
{
  return (bp - getMethodByteCodes(methodOOP));
}

/*
 *	void moveProcessorRegisters()
 *
 * Description
 *
 *	Part of the GC flip, copy the root set process.  This ensures that the
 *	processor registers are pointing to objects in new space.  The term
 *	"processor registers" refers here to interpreter variables like ip, sp,
 *	temporaries, etc.
 *
 */
void moveProcessorRegisters()
{
  MethodContext thisContext;	/* may be block context, but doesn't matter */
  MethodContext	methodContext;
  int		spOffset, ipOffset;
  OOP		methodContextOOP;

  invalidateMethodCache();

  thisContext = (MethodContext)oopToObj(thisContextOOP);
  spOffset = sp - thisContext->contextStack;
  ipOffset = relativeByteIndex(ip, thisMethod);

  maybeMoveOOP(thisContextOOP);
  maybeMoveOOP(thisMethod);

  ip = ipOffset + getMethodByteCodes(thisMethod);

  thisContext = (MethodContext)oopToObj(thisContextOOP);
  sp = thisContext->contextStack + spOffset;

  methodContextOOP = getMethodContext(thisContextOOP);
  maybeMoveOOP(methodContextOOP);
  methodContext = (MethodContext)oopToObj(methodContextOOP);

  temporaries = methodContext->contextStack;
/*** just as a test...I don't remember why the comment says self remains valid,
 *** because in my tests, it's pointing to old-space data...for long running
 *** executions, this could be disasterous 
 */
  /* self remains valid */
maybeMoveOOP(self);

  moveSemaphoreOOPs();
}

/*
 *	static void moveSemaphoreOOPs()
 *
 * Description
 *
 *	This routine doesn't really do anything yet.  It's intended purpose is
 *	to be called during the root set copying part of a GC flip to copy any
 *	asynchronous semaphores.  However, the async semaphore representation
 *	is likely not to be in terms of Smalltalk objects for a variety of
 *	reasons, so the need for this routine may never materialize.
 *
 */
static void moveSemaphoreOOPs()
{
  int		i;
  IntState	oldSigMask;

  oldSigMask = disableInterrupts(); /* block out everything! */
  /* ### this needs to be changed; async signals shouldn't be oops! */
  for (i = 0; i < asyncQueueIndex; i++) {
    moveOOP(queuedAsyncSignals[i]);
  }
  enableInterrupts(oldSigMask);
}

/*
 *	void initSignals()
 *
 * Description
 *
 *	Trap the signals that we care about, basically SIGBUG and SIGSEGV.
 *	These are sent to the back trace routine so we can at least have some
 *	idea of where we were when we died.
 *
 */
void initSignals()
{
  signal(SIGBUS, interruptHandler);
  signal(SIGSEGV, interruptHandler);

  signal(SIGINT, stopExecuting);
}


/*
 *	static signalType stopExecuting(sig)
 *
 * Description
 *
 *	Sets flags so that the interpreter starts returning immediately
 *	from whatever byte codes it's executing.  It returns via normal method
 *	returns, so that the world is in a consistent state when it's done.
 *
 * Inputs
 *
 *	sig   : signal that caused the interrupt (typically ^C), or 0, which
 *		comes from a call from within the system.
 *
 */
static signalType stopExecuting(sig)
{
  if (sig) {
    printf("\nInterrupt!\n");
  }

  abortExecution = true;
  exceptFlag = true;
  if (inCCode) {
    longjmp(cCalloutJmpBuf, 1);	/* throw out from C code */
  }
}


/*
 *	static signalType interruptHandler(sig)
 *
 * Description
 *
 *	Called to handle serious problems, such as segmentation violation.
 *	Tries to show a method invocation backtrace if possibly, otherwise
 *	tries to show where the system was in the file it was procesing when
 *	the error occurred.
 *
 * Inputs
 *
 *	sig   : Signal number, an integer
 *
 * Outputs
 *
 *	not used.  Always exits from Smalltalk.
 */
static signalType interruptHandler(sig)
int	sig;
{
  switch (sig) {
  case SIGBUS:
    errorf("Bus Error");
    break;

  case SIGSEGV:
    errorf("Segmentation violation");
    break;
    
  default:
    errorf("Unknown signal caught: %d", sig);
  }

  if (makeCoreFile) {
    kill(getpid(), SIGQUIT);	/* die with a core dump */
  }

  if (inInterpreter) {
    showBacktrace();
  } else {
    errorf("Not in interpreter!!");
  }

  exit(1);
}

static void showBacktrace()
{
  OOP		context, receiver, receiverClass;
  MethodContext methodContext;

  for (context = thisContextOOP; !isNil(context);
       context = methodContext->sender) {
    if (!isRealOOP(context)) {
      printf("Context stack corrupted!\n");
      break;
    }
    methodContext = (MethodContext)oopToObj(context);
    if (!isRealOOP(methodContext->selector)) {
      printf("Context stack corrupted!\n");
      break;
    }
      
    receiver = methodContext->receiver;
    if (isInt(receiver)) {
      receiverClass = integerClass;
    } else {
      if (!isRealOOP(receiver)) {
	printf("Context stack corrupted!\n");
	break;
      }
      receiverClass = oopClass(receiver);
    }
    printObject(receiverClass);
    printf(">>");
    printObject(methodContext->selector);
    printf("\n");
  }

}

static Boolean isRealOOP(oop)
OOP oop;
{
  return (oop >= oopTable && oop < &oopTable[TOTAL_OOP_TABLE_SLOTS]);
}
