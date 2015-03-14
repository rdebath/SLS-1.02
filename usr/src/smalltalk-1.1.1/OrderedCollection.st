"======================================================================
|
|   OrderedCollection Method Definitions
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
| sbyrne     19 Sep 89	  Converted to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

SequenceableCollection variableSubclass: #OrderedCollection
		       instanceVariableNames: 'firstIndex lastIndex'
		       classVariableNames: ''
		       poolDictionaries: ''
		       category: nil !

OrderedCollection comment: 
'My instances represent ordered collections of arbitrary typed objects which
are not directly accessible by an index.  They can be accessed indirectly
through an index, and can be manipulated by adding to the end or based
on content (such as add:after:)' !

!OrderedCollection class methodsFor: 'instance creation'!

new: anInteger
    ^(super new: anInteger) initIndices
!

new
    ^self new: 16

!!



!OrderedCollection methodsFor: 'accessing'!
at: index
    index _ index + firstIndex - 1.
    (index >= firstIndex and: [ index <= lastIndex ])
    	ifTrue: [ ^self basicAt: index ]
	ifFalse: [ self error: 'index out of bounds for ordered collection' ]
!

at: index put: anObject
    index _ index + firstIndex - 1.
    (index >= firstIndex and: [ index <= lastIndex ])
    	ifTrue: [ ^self basicAt: index put: anObject ]
	ifFalse: [ self error: 'index out of bounds for ordered collection' ]
!    

after: oldObject
    "Return the element after oldObject.  Error if oldObject not found or
    if no following object is available"
    firstIndex to: lastIndex do:
    	[ :index |		"should we use '=' or '==' here?"
	    (self basicAt: index) = oldObject
	    	ifTrue: [
		    index < lastIndex
		    	ifTrue: [ ^self basicAt: index + 1 ]
			ifFalse: [ ^self error: 'no following object' ] ]
    	].
    self error: 'object not found'
!

before: oldObject
    "Return the element after oldObject.  Error if oldObject not found or
    if no following object is available"
    firstIndex to: lastIndex do:
    	[ :index |		"should we use '=' or '==' here?"
	    (self basicAt: index) = oldObject
	    	ifTrue: [
		    index > firstIndex
		    	ifTrue: [ ^self basicAt: index - 1 ]
			ifFalse: [ ^self error: 'no preceding object' ] ]
    	].
    self error: 'object not found'
!

copyEmpty
    ^self species new: self basicSize
!

size
    ^lastIndex - firstIndex + 1
!!



!OrderedCollection methodsFor: 'adding'!

add: anObject
    ^self addLast: anObject
!

add: newObject after: oldObject
    firstIndex to: lastIndex do:
    	[ :i | (self basicAt: i) = oldObject
	       	 ifTrue: [ self at: i + 1 insertObject: newObject.
    	       	    	   ^newObject ] ].
    self error: 'object not found in collection'
!

add: newObject before: oldObject
    firstIndex to: lastIndex do:
    	[ :i | (self basicAt: i) = oldObject
	       	 ifTrue: [ self at: i - 1 insertObject: newObject.
    	       	    	   ^newObject ] ].
    self error: 'object not found in collection'
!


addAllFirst: anOrderedCollection
    anOrderedCollection reverseDo:
    	[ :element | self addFirst: element ].
    ^anOrderedCollection
!
    
addAllLast: anOrderedCollection
    anOrderedCollection do:
    	[ :element | self addLast: element ].
    ^anOrderedCollection
!
    

addFirst: newObject
    firstIndex = 1
    	ifTrue: [ self growFirst ].
    firstIndex _ firstIndex - 1.
    ^self basicAt: firstIndex put: newObject
!
    
addLast: newObject
    lastIndex = self basicSize
    	ifTrue: [ self growLast ].
    lastIndex _ lastIndex + 1.
    ^self basicAt: lastIndex put: newObject
!!



!OrderedCollection methodsFor: 'removing'!
removeFirst
    lastIndex < firstIndex
    	ifTrue: 
    	    [ ^self error: 'attempted to remove from an empty collection' ].
    firstIndex _ firstIndex + 1.
    ^self basicAt: firstIndex - 1
!

removeLast
    lastIndex < firstIndex
    	ifTrue: 
    	    [ ^self error: 'attempted to remove from an empty collection' ].
    lastIndex _ lastIndex - 1.
    ^self basicAt: lastIndex + 1
!!



!OrderedCollection methodsFor: 'private methods'!

initIndices
    firstIndex _ self basicSize // 2 max: 1.
    lastIndex _ firstIndex - 1
!

firstIndex: first lastIndex: last
    firstIndex _ first.
    lastIndex _ last
!

growFirst
    "Make growSize room at the start of the ordered collection, and copy
    all the elements of the old collection into the new one starting
    at the value that growSize returned."
    | newOrderedCollection delta |
    delta _ self growSize.
    newOrderedCollection _ self growTo: self basicSize + delta.
    firstIndex to: lastIndex do:
    	[ :index | newOrderedCollection basicAt: delta + index - 1
	    	    	    	    	put: (self basicAt: index) ].
    newOrderedCollection firstIndex: delta + firstIndex - 1
    	    	    	 lastIndex: delta + lastIndex - 1.
    ^self become: newOrderedCollection    
!

growLast
    "Make growSize room at the end of the ordered collection, and copy
    all the elements of the old collection into the new one starting
    at firstIndex."
    | newOrderedCollection |
    newOrderedCollection _ self growTo: self basicSize + self growSize.
    firstIndex to: lastIndex do:
    	[ :index | newOrderedCollection basicAt: index
	    	    	    	    	put: (self basicAt: index) ].
    newOrderedCollection firstIndex: firstIndex
    	    	    	 lastIndex: lastIndex.
    ^self become: newOrderedCollection    
!

grow
    "Make growSize room in the collection, putting the old contents in the
    middle."
    | oldSize newSize newFirstIndex newOrderedCollection |
    oldSize _ self basicSize.
    newSize _ oldSize + self growSize.
    newOrderedCollection _ self growTo: newSize.
    newFirstIndex _ newSize - oldSize // 2 max: 1.
    1 to: self size do:
    	[ :i | newOrderedCollection basicAt: i + newFirstIndex - 1
	    	    	    	    put: (self basicAt: i + firstIndex - 1) ].
    newOrderedCollection firstIndex: newFirstIndex
    	    	    	 lastIndex: newFirstIndex + self size - 1.
    ^self become: newOrderedCollection
!

growTo: anInteger
    ^self species new: anInteger
!    

growSize
    ^10				"number out of a hat"
!

at: index insertObject: anObject
    lastIndex = self basicSize ifTrue: [ self growLast ].
    lastIndex to: index by: -1 do:
    	[ :i | self basicAt: i + 1
	    	    put: (self basicAt: i) ].
    self basicAt: index put: anObject.
    ^anObject
!!

