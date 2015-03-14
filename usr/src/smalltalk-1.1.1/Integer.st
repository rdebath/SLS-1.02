"======================================================================
|
|   Integer Method Definitions
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
| sbb	     12 Sep 91	  Fixed gcd: to return result instead of self.
|
| sbyrne     25 Apr 90	  Fixed (oh...happy birthday, Integer.st!) bitInvert.
|			  After fixing the lexer to be pickier about integer
|			  literals that were too large to be represented as
|			  Smalltalk literals, the previous code (which xored
|			  with 7fffffff) broke, so as a hack I computed two's
|			  complement and subtracted one.
|
| sbyrne     25 Apr 89	  created.
|
"

Number variableWordSubclass: #Integer "### Not really a variable word subclass"
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Integer comment:
'I am the integer class of the GNU Smalltalk system.  My instances can 
represent 31 bit integers and are as efficient as possible.' !

!Integer methodsFor: 'Misc math operators'!

hash
    ^self
!!



!Integer methodsFor: 'Other iterators'!

timesRepeat: aBlock
    | i |
    i _ 1.
    [ i <= self ] whileTrue: [ aBlock value.
    	    	    	       i _ i + 1 ]
!!



!Integer methodsFor: 'bit operators'!

bitAt: index
    ^(self bitShift: index negated) bitAnd: 1
!

bitInvert
    "Return the 1's complement of the bits of the receiver"
    ^self negated - 1		"compute 2's complement then remove 1"
!


allMask: anInteger
    "True if all bits in anInteger are 1 in the receiver"
    ^(self bitAnd: anInteger) = anInteger
!

anyMask: anInteger
    "True if any 1 bits in anInteger are 1 in the receiver"
    ^(self bitAnd: anInteger) ~= 0
!

noMask: anInteger
    "True if no 1 bits in anInteger are 1 in the receiver"
    ^(self bitAnd: anInteger) = 0
!

highBit
    "Return the index of the highest order 1 bit of the receiver"
    self = 0 ifTrue: [ ^-1 ].	"??? I don't know what the right value is"
    30 to: 1 step: -1 do:
    	[ :i | (self bitAnd: (1 bitShift: i)) ~= 0 ifTrue: [ ^i ] ]
!!



!Integer methodsFor: 'Math methods'!

factorial
    self < 2 ifTrue: [ ^1 ]
    	     ifFalse: [ ^self * (self - 1) factorial ]
!

gcd: anInteger
    | selfInteger temp |
    "Return the greatest common divisor (Euclid's algorithm)"
    selfInteger _ self.
    [ anInteger ~= 0 ]
    	whileTrue: [ temp _ selfInteger \\ anInteger.
	    	     selfInteger _ anInteger.
		     anInteger _ temp. ].
    ^selfInteger
!

lcm: anInteger
    ^(self * anInteger) abs // (self gcd: anInteger)
!

even
    ^(self bitAnd: 1) = 0
!

odd
    ^(self bitAnd: 1) ~= 0
!!



!Integer methodsFor: 'Coercion methods (heh heh heh)'!

asCharacter
    "Return self as an ascii character"
    (self <= 255 and: [ self >= 0])
    	ifTrue: [ ^Character value: self ]
	ifFalse: [ ^self error: 'Integer not convertible to character' ]
!

coerce: aNumber
    ^aNumber truncated
!

generality
    ^1
!

ceiling
    ^self
!

floor
    ^self
!

truncated
    ^self
!

rounded
    ^self
!!




!Integer methodsFor: 'copying'!

shallowCopy
    ^self
!

deepCopy
    ^self
!!




!Integer methodsFor: 'printing'!

printOn: aStream base: b
    aStream nextPutAll: (self radix: b)
!

radix: baseInteger
    ^self signedStringBase: baseInteger showRadix: true
!

printOn: aStream
    (self signedStringBase: 10 showRadix: false) printOn: aStream
!!



!Integer methodsFor: 'storing'!

storeOn: aStream
    self printOn: aStream		"they print and store the same"
!!



!Integer methodsFor: 'not implemented'!

asFraction
    self notYetImplemented
!!



!Integer methodsFor: 'private'!

signedStringBase: baseInteger showRadix: showRadix
    | str revString string sign len num i |
    self < 0
    	ifTrue: [ sign _ true.
	    	  num _ self negated ]
	ifFalse: [ sign _ false.
	    	   num _ self ].
    revString _ num revDigitsBase: baseInteger.
    str _ WriteStream on: (String new: 1).
    showRadix ifTrue:
    	[ baseInteger printOn: str.
	  str nextPut: $r ].
    sign ifTrue: [ str nextPut: $- ].
    revString reverseDo:
    	[ :char | str nextPut: char ].
    ^str contents
!

revDigitsBase: b    
    | str num |
    str _ WriteStream on: (String new: 1).
    self = 0
    	ifTrue: [ str nextPut: $0 ]
	ifFalse: [
    	    num _ self.
	    [ num = 0 ] whileFalse:
	    	[ str nextPut: (Character digitValue: num \\ b).
		  num _ num // b ] ].
    ^str contents

!!

