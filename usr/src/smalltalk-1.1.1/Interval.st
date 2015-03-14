"======================================================================
|
|   Interval Method Definitions
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
| sbyrne     25 Apr 89	  created.
|
"

SequenceableCollection subclass: #Interval
		       instanceVariableNames: 'start stop step'
		       classVariableNames: ''
		       poolDictionaries: ''
		       category: nil.

Interval comment: 
'My instances represent ranges of objects, typically Magnitude type
objects.  I provide iteration/enumeration messages for producing all the
members that my instance represents.' !

!Interval class methodsFor: 'instance creation'!

from: startInteger to: stopInteger by: stepInteger
    | int |
    ^self new initializeFrom: startInteger to: stopInteger by: stepInteger
!

from: startInteger to: stopInteger    
    ^self from: startInteger to: stopInteger by: 1

!!



!Interval methodsFor: 'basic'!

" Note to the reader: these two methods (do: and collect:) are implemented
in this expanded way, instead of just making the whileTrue: condition be a
block that's conditionally assigned based on the sign of step and then
invoking a single whileTrue: loop.  However, in this form, the optimizer
can optimize these while loops into direct byte codes and not have to bother
with expensive (relatively) block and method context creation and sending
messages to blocks. "

do: aBlock
    | i |
    i _ start.
    step > 0
    	ifTrue: [
    	    [ i <= stop ] whileTrue:
    	    	[ aBlock value: i.
    	    	  i _ i + step ]
    	]
    	ifFalse: [
	    [ i >= stop ] whileTrue:
    	    	[ aBlock value: i.
    	    	  i _ i + step ]
    	]
!

collect: aBlock
    | i result j |
    result _ self species new: self size.
    i _ 1.
    j _ start.
    step > 0
    	ifTrue: [
	    [ j <= stop ]
	    	whileTrue:
		    [ result at: i put: (aBlock value: j).
		    	j _ j + step.
			i _ i + 1 ]
    	]
	ifFalse: [
	    [ j >= stop ]
	    	whileTrue:
		    [ result at: i put: (aBlock value: j).
		      j _ j + step.
		      i _ i + 1 ]
    	]
!    

size
    step > 0
    	ifTrue: [
	    stop >= start ifTrue: [ ^(stop - start) // step + 1 ]
	    	    	  ifFalse: [ ^0 ]
	]
	ifFalse: [
    	    start >= stop ifTrue: [ ^(start - stop) // step + 1 ]
	    	    	  ifFalse: [ ^0 ]
	]
!

species
    ^Array
!


at: index
    (index >= 1 and: [index <= self size])
    	ifTrue: [ ^start + (step * (index - 1)) ]
	ifFalse: [ self error: 'subscript out of bounds' ]
!

at: index put: anObject
    self error: 'you cannot store into an Interval'
!

add: newObject
    self error: 'elements cannot be add3ed to an Interval'
!

remove: newObject
    self error: 'elements canot be removed from an Interval'
!!



!Interval methodsFor: 'testing'!

= anInterval
    ^(start = anInterval start) &
	(stop = anInterval stop) &
	(step = anInterval step)
!

hash
    ^(start bitXor: stop) bitXor: step
!!



!Interval methodsFor: 'printing'!

printOn: aStream
    aStream nextPutAll: self classNameString.
    aStream nextPut: Character space.
    start printOn: aStream.
    aStream nextPut: $,.
    stop printOn: aStream.
    aStream nextPut: $,.
    step printOn: aStream
!!



!Interval methodsFor: 'storing'!

storeOn: aStream
    aStream nextPut: $(.
    aStream nextPutAll: self classNameString.
    aStream nextPutAll: ' from: '.
    start storeOn: aStream.
    aStream nextPutAll: ' to: '.
    stop storeOn: aStream.
    aStream nextPutAll: ' by: '.
    step storeOn: aStream.
    aStream nextPut: $)
!!



!Interval methodsFor: 'private methods'!

initializeFrom: startInteger to: stopInteger by: stepInteger
    start _ startInteger.
    stop _ stopInteger.
    step _ stepInteger
!

start
    ^start
!

stop
    ^stop
!

step
    ^step
!!

