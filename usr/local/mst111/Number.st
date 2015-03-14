"======================================================================
|
|   Number Method Definitions
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
| sbyrne     19 Sep 89	  Converted to use real category strings.
|
| sbyrne     25 Apr 89	  created.
|
"

Magnitude subclass: #Number
	  instanceVariableNames: ''
	  classVariableNames: ''
	  poolDictionaries: ''
	  category: nil.

Number comment: 
'I am an abstract class that provides operations on numbers, both floating
point and integer.  I provide some generic predicates, and supply the 
implicit type coercing code for binary operations.' !

!Number methodsFor: 'converting'!

degreesToRadians
    ^self / 57.295779513082320876846364344191
!

radiansToDegrees
    ^self * 57.295779513082320876846364344191
!

coerce: aNumber
    self subclassResponsibility
!

generality
    self subclassResponsibility
!

retry: aSymbol coercing: aNumber
    | selfGen aNumGen |
    aSymbol == #=
    	ifTrue: [ (aNumber isKindOf: Number) ifFalse: [ ^false ] ].
    selfGen _ self generality.
    aNumGen _ aNumber generality.
    selfGen > aNumGen
    	ifTrue: [ ^self perform: aSymbol with: (self coerce: aNumber) ].
    selfGen < aNumGen
    	ifTrue: [ ^(aNumber coerce: self) perform: aSymbol with: aNumber ].
    self error: 'retry:coercing: called with arguments of the same generality' 
!!



!Number methodsFor: 'arithmetic'!

+ aNumber
    self subclassResponsibility
!

- aNumber
    self subclassResponsibility
!

* aNumber
    self subclassResponsibility
!

/ aNumber
    self subclassResponsibility
!

// aNumber
    self subclassResponsibility
!

\\ aNumber
    self subclassResponsibility
!

quo: aNumber
    "Return the integer quotient of dividing the receiver by aNumber with
    truncation towards zero."
    ^(self / aNumber) truncated
!

rem: aNumber
    "Return the integer remainder of dividing the receiver by aNumber with
    truncation towards zero."
    ^(self - ((self / aNumber) truncated * aNumber)) truncated
!!



!Number methodsFor: 'truncation and round off'!

truncated
    ^self subclassResponsibility
!

truncateTo: aNumber
    ^(self / aNumber) truncated * aNumber
!

rounded
    "Returns the integer nearest the receiver"
    ^(self + 0.5) truncated
!

roundTo: aNumber
    ^(self / aNumber) rounded * aNumber
!!



!Number methodsFor: 'testing'!
negative
    ^self < 0
!

positive
    ^self >= 0
!

strictlyPositive
    ^self > 0
!

sign
    "Returns the sign of the receiver."
    self < 0 ifTrue: [ ^-1 ].
    self > 0 ifTrue: [ ^1 ].
    ^0
!

even
    "Returns true if self is divisible by 2"
    ^self truncated even
!

odd
    "Returns true if self is not divisible by 2"
    ^self truncated odd
!!



!Number methodsFor: 'misc math'!

squared
    ^self * self
!

abs
    self > 0 ifTrue: [ ^self ] ifFalse: [ ^self negated ]
!

negated
    ^0 - self
!

sin
    ^self asFloat sin
!

cos
    ^self asFloat cos
!

tan
    ^self asFloat tan
!

arcSin
    ^self asFloat arcSin
!

arcCos
    ^self asFloat arcCos
!

arcTan
    ^self asFloat arcTan
!

sqrt
    ^self asFloat sqrt
!

exp
    ^self asFloat exp
!

ln
    ^self asFloat ln
!

log: aNumber
    "return log base aNumber of the receiver"
    ^self ln / aNumber ln
!

floorLog: radix
    ^(self log: radix) floor
!

raisedToInteger: anInteger
    "Return self raised to anInteger power"
    | result |
    result _ self coerce: 1.
    anInteger timesRepeat: [ result _ result * self ].
    ^result
!!



!Number methodsFor: 'Interval iterators'!

to: stop
    ^Interval from: self to: stop
!

to: stop by: step
    ^Interval from: self to: stop by: step
!

to: stop by: step do: aBlock
    | i |
    i _ self.
    step > 0
    	ifTrue: [
    	    [ i <= stop ]
	    	whileTrue: [ aBlock value: i.
		    	     i _ i + step ]
    	]
	ifFalse: [
    	    [ i >= stop ]
	    	whileTrue: [ aBlock value: i.
		    	     i _ i + step ]
	]
    " ??? What's the return value? "
!

to: stop do: aBlock
    ^self to: stop by: 1 do: aBlock
!!

