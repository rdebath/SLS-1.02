"Smalltalk built in methods.  These are read in by the system initially, to 
prepare the execution environment."

"======================================================================
|
| Copyright (C) 1990, 1991 Free Software Foundation, Inc.
| Written by Steve Byrne.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 1, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"

"
|     Change Log
| ============================================================================
| Author       Date       Change 
| sbyrne     20 Apr 90	  Added SystemDictionary debug to help out with
|			  DBX level debugging.
|
| sbyrne     15 Apr 90	  Added sqrt primitive (I'm sure this was here before)
|                         must have got lost during breaking out from builtins.
|
| sbyrne      7 Apr 90	  Added declare tracing primitive.
|
| sbyrne     13 Jan 90	  Experimental Class self-definition.
|
| sbyrne     19 Dec 89	  Added filein primitive.
|
| sbyrne     12 Aug 89	  Added process and semaphore builtins.
|
| sbyrne      6 Feb 89	  Created.
|

"


!Integer methodsFor: 'built ins'!
+ arg
    <primitive: 1>
    ^self retry: #+ coercing: arg
!

- arg
    <primitive: 2>
    ^self retry: #- coercing: arg
!

< arg
    <primitive: 3>
    ^self retry: #< coercing: arg
!

> arg
    <primitive: 4>
    ^self retry: #> coercing: arg
!

<= arg
    <primitive: 5>
    ^self retry: #<= coercing: arg
!

>= arg
    <primitive: 6>
    ^self retry: #>= coercing: arg
!

= arg
    <primitive: 7>
    ^self retry: #= coercing: arg
!

~= arg
    <primitive: 8>
    ^self retry: #~= coercing: arg
!

* arg
    <primitive: 9>
    ^self retry: #* coercing: arg
!

/ arg
    <primitive: 10>
    ^self retry: #/ coercing: arg
!

\\ arg
    <primitive: 11>
    ^self retry: #\\ coercing: arg
!

// arg
    <primitive: 12>
    ^self retry: #// coercing: arg
!

quo: arg
    <primitive: 13>
    ^self retry: #quo: coercing: arg
!

bitAnd: arg
    <primitive: 14>
!

bitOr: arg
    <primitive: 15>
!

bitXor: arg
    <primitive: 16>
!

bitShift: arg
    <primitive: 17>
!

asFloat
    <primitive: 40>
!

asObject
    <primitive: 76>
    self primitiveFailed
!

asObjectNoFail
    <primitive: 76>

!!



!Float methodsFor: 'built ins'!

+ arg
    <primitive: 41>
    ^self retry: #+ coercing: arg
!

- arg
    <primitive: 42>
    ^self retry: #- coercing: arg
!

< arg
    <primitive: 43>
    ^self retry: #< coercing: arg
!

> arg
    <primitive: 44>
    ^self retry: #> coercing: arg
!

<= arg
    <primitive: 45>
    ^self retry: #<= coercing: arg
!

>= arg
    <primitive: 46>
    ^self retry: #>= coercing: arg
!

= arg
    <primitive: 47>
    ^self retry: #= coercing: arg
!

~= arg
    <primitive: 48>
    ^self retry: #~= coercing: arg
!

* arg
    <primitive: 49>
    ^self retry: #* coercing: arg
!

/ arg
    <primitive: 50>
    ^self retry: #/ coercing: arg
!

truncated
    <primitive: 51>
!

fractionPart
    <primitive: 52>
!

exponent
    <primitive: 53>
!

timesTwoPower: arg
    <primitive: 54>
!

exp
    <primitive: 160>
!

ln
    <primitive: 161>
!

raisedTo: aNumber
    <primitive: 164>
    ^self retry: #raisedTo: coercing: aNumber
!

sqrt
    <primitive: 166>
    ^self error: 'Primitive Sqrt failed!!!'
!

ceiling
    <primitive: 168>
!

floor
    <primitive: 169>
!

sin
    <primitive: 176>
!

cos
    <primitive: 177>
!

tan
    <primitive: 178>
!

arcSin
    <primitive: 179>
!

arcCos
    <primitive: 180>
!

arcTan
    <primitive: 181>
!!



!Object methodsFor: 'built ins'!

at: index
    <primitive: 60>
!

basicAt: index
    <primitive: 60>
!

at: index put: value
    <primitive: 61>
!

basicAt: index put: value
    <primitive: 61>
!

size
    <primitive: 62>
!

basicSize
    <primitive: 62>
!

become: otherObject
    <primitive: 72>
!

instVarAt: index
    <primitive: 73>
    ^self primitiveFailed
!

instVarAt: index put: value
    <primitive: 74>
    ^self primitiveFailed
!

asOop
    <primitive: 75>
!

hash
    <primitive: 75>
!

nextInstance
    <primitive: 78>
    ^nil
!

perform: selector
    <primitive: 83>
!

perform: selector with: arg1
    <primitive: 83>
!

perform: selector with: arg1 with: arg2
    <primitive: 83>
!

perform: selector with: arg1 with: arg2 with: arg3
    <primitive: 83>
!

perform: selector withArguments: argumentsArray
    <primitive: 84>
!

== arg
    <primitive: 110>
!

= arg
    "The equality test is by default the same as that for equal objects"
    <primitive: 110>
!

class
    <primitive: 111>
    self primitiveFailed
!

doesNotUnderstand: message
    <primitive: 130>
!

error: message
    <primitive: 131>
!

basicPrint
    <primitive: 252>
!

"### look these messages up to be sure"

primitiveFailed
    self error: 'primitive operation failed'
!

shouldNotImplement
    self error: 'should not implement'
!

subclassResponsibility
    self error: 'the method is the responsibility of the subclass'
!

notYetImplemented
    self error: 'not yet implemented'

!!



!SystemDictionary methodsFor: 'builtins'!

quitPrimitive
    <primitive: 113>
    self primitiveFailed
!

monitor: aBoolean
    <primitive: 230>
    self primitiveFailed
!


backtrace
    "Prints the method invocation stack backtrace, as an aid to debugging"
    <primitive: 140>
    self primitiveFailed
!

executionTrace: aBoolean
    <primitive: 141>
    self primitiveFailed
!

declarationTrace: aBoolean
    <primitive: 142>
    self primitiveFailed
!

snapshot
    <primitive: 250>
!

snapshot: aString
    <primitive: 251>
!

gcMessage: aBoolean
    <primitive: 231>
!

debug				"for DBX.  Set breakpoint in debug() and 
				 invoke this primitive near where you want
				 to stop"
    <primitive: 232>
!

verboseTrace: aBoolean
    <primitive: 233>
!!



!Behavior methodsFor: 'built ins'!

new
    <primitive: 70>
!

basicNew
    <primitive: 70>
!

new: numInstanceVariables
    <primitive: 71>
!

basicNew: numInstanceVariables
    <primitive: 71>
!

someInstance
    <primitive: 77>
    ^nil			"return nil on failure"
!

makeDescriptorFor: funcNameString 
  returning: returnTypeSymbol 
  withArgs: argsArray
    <primitive: 249>
    ^self primitiveFailed
!

compileString: aString
    <primitive: 253>
    ^self primitiveFailed
!!



!CompiledMethod class methodsFor: 'built ins'!

newMethod: numBytecodes header: anInteger
    <primitive: 79>
    ^self primitiveFailed
!!



!CompiledMethod methodsFor: 'built ins'!

objectAt: index
    <primitive: 68>
    ^self primitiveFailed
!

objectAt: index put: value
    <primitive: 69>
    ^self primitiveFailed
!!



!MethodContext methodsFor: 'built ins'!

" Note: the name for this class in the book is 'ContextPart' "

blockCopy: block
    <primitive: 80>

!!

!BlockContext methodsFor: 'built ins'!

blockCopy: block
    <primitive: 80>
!

value
    <primitive: 81>
!

value: arg1
    <primitive: 81>
!

value: arg1 value: arg2
    <primitive: 81>
!

value: arg1 value: arg2 value: arg3
    <primitive: 81>
!

valueWithArguments: argArray
    <primitive: 82>

!!


!ArrayedCollection methodsFor: 'built ins'!

size
    <primitive: 62>

!!


!String methodsFor: 'built ins'!

size
    <primitive: 62>
!

at: index
    <primitive: 63>
!

basicAt: index
    <primitive: 63>
!

at: index put: value
    <primitive: 64>
!

basicAt: index put: value
    <primitive: 64>

!!



!Symbol class methodsFor: 'built ins'!

intern: aString
    <primitive: 134>
    ^self error: 'Attempted to intern non-string'
!!



!Symbol methodsFor: 'built ins'!

hash
    <primitive: 75>

!!



!Character class methodsFor: 'built ins'!

value: anInteger
    "Returns the character object corresponding to anInteger.  Error if
     anInteger is not an integer, or not in 0..255."
     <primitive: 132>
    ^self error: 'invalid argument type or integer out of range'
!!



!Character methodsFor: 'built ins'!

= char
    "Boolean return value; true if the characters are equal"
    <primitive: 110>
!

asciiValue
    "Returns the integer value corresponding to self"
    <primitive: 133>
!!



!Dictionary class methodsFor: 'built ins'!

new
   <primitive: 135>
!!


!Dictionary methodsFor: 'built ins'!

at: key
    <primitive: 128>
!

at: key put: value
    <primitive: 129>

!!



!FileStream methodsFor: 'built ins'!

fileOp: ioFuncIndex
    <primitive: 254>
    ^self error: 'FileStream primitive operation failed'
!

fileOp: ioFuncIndex with: arg1
    <primitive: 254>
    ^self error: 'FileStream primitive operation failed'
!

fileOp: ioFuncIndex with: arg1 with: arg2
    <primitive: 254>
    ^self error: 'FileStream primitive operation failed'
!

fileIn
    <primitive: 247>
    ^self error: 'fileIn failed!!!'
!!



!Memory class methodsFor: 'basic'!

addressOfOOP: anObject
    "Returns the address of the OOP for anObject"
    <primitive: 138>
!

addressOf: anObject
    "Returns the address of the actual object that anObject references"
    <primitive: 139>
!!



!ByteMemory class methodsFor: 'basic'!

at: address
    "Returns the byte at address as an integer"
    <primitive: 136>
!

at: address put: value
    "Sets the byte at ADDRESS (an integer) to be VALUE (INTEGER 0..255)"
    <primitive: 137>

!!



!Time class methodsFor: 'builtins'!

secondClock
    <primitive: 98>
    ^self primitiveFailed
!

millisecondClock
    <primitive: 99>
   ^self primitiveFailed
!!



!Process methodsFor: 'builtins'!

resume
    <primitive: 87>
   ^self primitiveFailed
!

suspend
    <primitive: 88>
   ^self primitiveFailed
!!



!Semaphore methodsFor: 'builtins'!

"communication"

signal
    <primitive: 85>
    ^self primitiveFailed
!

wait
    <primitive: 86>
    ^self primitiveFailed
!!



!ClassDescription methodsFor: 'builtins'!

comment: aString
    <primitive: 143>
    "This method is present so that comment declarations can always work, even
     before the real method is defined."

!!



!Class methodsFor: 'builtins'!

"These are stubs...they will be replaced with the appropriate class
from Class.st.  These allow for Smalltalk type class declarations
of the built-in classes, so that they may be edited and modified.  This
mostly present to allow for future enhancement in which the Smalltalk source
files take a more active role in the definition of the system, and the 
C definition of the classes diminishes in importance."

subclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    ^nil
!

variableSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    ^nil
!

variableWordSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    ^nil
!

variableByteSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    ^nil	
!!
