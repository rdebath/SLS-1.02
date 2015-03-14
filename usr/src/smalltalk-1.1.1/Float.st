"======================================================================
|
|   Float Method Definitions
|
 ======================================================================"


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
| sbb	     12 Sep 91	  Fixed storeOn: to not be recursive
|
| sbyrne     15 Apr 90	  Added asFloat...I could have sworn this was already
|                         here...must have been lost in an edit.
|
| sbyrne     25 Apr 89	  created.
|
"



Number variableWordSubclass: #Float "not really a variable class"
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Float comment: 
'My instances represent floating point numbers that have 64 bits of 
precision (well, less than that in precision; they are precisely the same
as C''s "double" datatype).  Besides the standard numerical operations,
I provide transcendental operations too.'  !

!Float class methodsFor: 'basic'!

pi
    "Returns the value of pi"
    ^3.14159265358979323846264338327950288
!!



!Float methodsFor: 'arithmetic'!

// aNumber
    "Return the integer quotient of dividing the receiver by aNumber with
    truncation towards negative infinity."
    ^(self / aNumber) floor
!

\\ aNumber
    "Return the integer remainder of dividing the receiver by aNumber with
    truncation towards negative infinity."
    ^(self - ((self / aNumber) integerPart * aNumber)) floor
!

integerPart
    ^self - self fractionPart
!!



!Float methodsFor: 'coercing methods'!

coerce: aNumber
    ^aNumber asFloat
!

generality
    ^4
!

asFloat
    "Just defined for completeness."
    ^self
!

hash
    "Returns the hash value for the receiver (a Float).  If it's representable
    as an integer, we use that value, since it's likely to be much more random,
    but if not, we use the exponent.  Another approach that may be used in 
    the future is to scale the number so that it's within the integer range if
    it's not normally and then return the integer part"
    | expt |
    expt _ self exponent.
    expt >= 0 & (expt < 31)
    	ifTrue: [ ^self truncated ]
	ifFalse: [ ^expt ]
!!



!Float methodsFor: 'copying'!

shallowCopy
    ^self
!

deepCopy
    ^self
!!




!Float methodsFor: 'printing'!

printOn: aStream
    | num exp |
    (self exponent between: -51 and: 51)
    	ifTrue: [ self printFloatOn: aStream ]
	ifFalse: [ exp _ 0.
	    	   num _ self.
		   num abs < 1.0
		      ifTrue: [ [ num abs < 1.0 ] whileTrue:
				    [ num _ num * 10.0.
				      exp _ exp - 1 ] ]
		      ifFalse: [ [num abs >= 10.0] whileTrue:
		      	    	    [ num _ num / 10.0.
				      exp _ exp + 1 ] ].
    	    	   num printFloatOn: aStream.
		   aStream nextPut: $e.
		   exp printOn: aStream ].
!!



!Float methodsFor: 'storing'!

storeOn: aStream
    self printOn: aStream
!!



!Float methodsFor: 'private'!

printFloatOn: aStream
    | num str |
    num _ self.
    num < 0 ifTrue:
    	[ aStream nextPut: $-.
	  num _ num negated. ].
    num printIntegerPartOn: aStream.
    aStream nextPut: $..
    num printFracPartOn: aStream.
!

printIntegerPartOn: aStream
    | num |
    num _ self integerPart.
    (num revDigitsBase: 10.0) reverseDo:
    	[ :char | aStream nextPut: char ]
!

printFracPartOn: aStream
    | num count zeroCount digit |
    num _ self fractionPart.
    num = 0.0 ifTrue: [ ^aStream nextPut: $0 ].
    zeroCount _ count _ 0.

    " produce the digits, up to a maximum of 15, suppressing trailing
      zeros "
    [ (num ~= 0.0) & (count < 15) ] whileTrue:
    	[ num _ num * 10.0.
	  digit _ num floor.
	  digit = 0		"for trailing zero suppression"
	    ifTrue: [ zeroCount _ zeroCount + 1 ]
	    ifFalse: [ aStream next: zeroCount put: $0.
    	    	       aStream nextPut: (Character digitValue: digit).
	    	       zeroCount _ 0 ].
	  num _ num fractionPart.
	  count _ count + 1 ].

    zeroCount = 15 ifTrue: [ aStream nextPut: $0 ]
!

"AARRGGHH!  Why can't I share this code with integer?  Grrr...."
revDigitsBase: b    
    | str num |
    str _ WriteStream on: (String new: 1).
    self = 0
    	ifTrue: [ str nextPut: $0 ]
	ifFalse: [
    	    num _ self.
	    [ num = 0 ] whileFalse:
	    	[ str nextPut: (Character digitValue: num \\ b).
		  num _ (num / b) integerPart ] ].
    ^str contents

!!

