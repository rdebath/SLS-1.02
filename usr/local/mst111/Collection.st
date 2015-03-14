"======================================================================
|
|   Collection Method Definitions
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
| sbb	     12 Sep 91	  Changed printOn: to recursively call printOn:
|
| sbb	     12 Sep 91	  Fix detect: method to return the value
|
| sbyrne     25 Apr 89	  created.
|
"

Object subclass: #Collection
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Collection comment: 
'I am an abstract class.  My instances are collections of objects.  My
subclasses may place some restrictions or add some definitions to how
the objects are stored or organized; I say nothing about this.  I merely
provide some object creation and access routines for general collections
of objects.' !


!Collection class methodsFor: 'instance creation'!

with: anObject
    ^self new add: anObject; yourself
!

with: firstObject with: secondObject
    ^self new add: firstObject; add: secondObject; yourself
!

with: firstObject with: secondObject with: thirdObject
    ^self new add: firstObject; add: secondObject; add: thirdObject; yourself
!

with: firstObject with: secondObject with: thirdObject with: fourthObject
    ^self new add: firstObject; add: secondObject; add: thirdObject;
    	add: fourthObject; yourself
!!



!Collection methodsFor: 'Adding to a collection'!

add: newObject
    self subclassResponsibility
!

addAll: aCollection
    aCollection do: [ :element | self add: element ].
    ^aCollection
!!



!Collection methodsFor: 'Removing from a collection'!

remove: oldObject ifAbsent: anExceptionBlock
    self subclassResponsibility
!

remove: oldObject
    self remove: oldObject
         ifAbsent: [ ^self error: 'Failed to remove object' ].
    ^oldObject
!

removeAll: aCollection
    " ??? we're supposed to report an error if an object can't be removed
      properly.  I've elected to let remove: report the error.  Also, it's
      not clear whether we're supposed to remove all occurrences of the
      members of aCollection from the receiver, or only the first."
    aCollection do: [ :element | self remove: element ].
    ^aCollection
!!



!Collection methodsFor: 'testing collections'!

includes: anObject
    self do: [ :element | anObject = element ifTrue: [ ^true ] ].
    ^false
!

isEmpty
    ^self size == 0
!

occurrencesOf: anObject
    | count |
    count _ 0.
    self do: [ :element | anObject == element ifTrue: [ count _ count + 1 ] ].
    ^count
!

size
    | count |
    count _ 0.
    self do: [ :element | count _ count + 1].
    ^count
!!



!Collection methodsFor: 'enumerating the elements of a collection'!

do: aBlock
    self subclassResponsibility
!

select: aBlock
    | newCollection |
    " ### I don't know if this is the right way to create a new collection
      in all cases...I suspect that it is not..."
    newCollection _ self species new.
    self do: [ :element | (aBlock value: element)
    	    	    	    ifTrue: [ newCollection add: element ]
	     ].
    ^newCollection
!

reject: aBlock
    | newCollection |
    " ### I don't know if this is the right way to create a new collection
      in all cases...I suspect that it is not..."
    newCollection _ self species new.
    self do: [ :element | (aBlock value: element)
    	    	    	    ifFalse: [ newCollection add: element ]
	     ].
    ^newCollection
!

collect: aBlock
    | newCollection |
    " ### I don't know if this is the right way to create a new collection
      in all cases...I suspect that it is not..."
    newCollection _ self species new.
    self do: [ :element | newCollection add: (aBlock value: element) ].
    ^newCollection
!

detect: aBlock ifNone: exceptionBlock
    self do: [ :element | (aBlock value: element) ifTrue: [ ^element ] ].
    exceptionBlock value    
!
    
detect: aBlock
    ^self detect: aBlock ifNone: [ ^self error: 'Collection detect: failed']
!

inject: thisValue into: binaryBlock
    self do: [ :element | thisValue _ binaryBlock value: thisValue
    	    	    	    	    	    	  value: element ].
    ^thisValue
!!



!Collection methodsFor: 'converting'!

asBag
    | aBag |
    aBag _ Bag new.
    self do: [ :element | aBag add: element ].
    ^aBag
!

asSet
    | aSet |
    aSet _ Set new: self size.
    self do: [ :element | aSet add: element ].
    ^aSet
!

asOrderedCollection
    | anOrderedCollection |
    anOrderedCollection _ OrderedCollection new: self size.
    self do: [ :element | anOrderedCollection add: element ].
    ^anOrderedCollection
!

asSortedCollection
    | aSortedCollection |
    aSortedCollection _ SortedCollection new.
    self do: [ :element | aSortedCollection add: element ].
    ^aSortedCollection
!

asSortedCollection: aBlock
    | aSortedCollection |
    aSortedCollection _ SortedCollection sortBlock: aBlock.
    self do: [ :element | aSortedCollection add: element ].
    ^aSortedCollection
!!



!Collection methodsFor: 'printing'!

printOn: aStream
    | firstTime |
    firstTime _ true.
    aStream nextPutAll: self classNameString.
    aStream nextPutAll: ' ('.
    self do:
    	[ :element | firstTime ifTrue: [ firstTime _ false ]
	   	    	        ifFalse: [ aStream nextPut: Character space ].
		      element printOn: aStream ].
    aStream nextPut: $)
!!



!Collection methodsFor: 'storing'!

store: aStream
    | noElements |
    aStream nextPut: $(.
    aStream nextPutAll: self classNameString.
    aStream nextPutAll: ' new'.
    noElements _ true.
    self do:
    	[ :element | aStream nextPutAll: ' add: '.
    	    	     element storeOn: aStream.
		     aStream nextPut: $;.
		     noElements _ false ].
    noElements ifFalse: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!!

