"======================================================================
|
|   SortedCollection Method Definitions
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
| sbyrne     19 Sep 89	  Changed to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

OrderedCollection variableSubclass: #SortedCollection
		  instanceVariableNames: 'sortBlock'
		  classVariableNames: ''
		  poolDictionaries: ''
		  category: nil.

SortedCollection comment:
'I am a collection of objects, stored and accessed according to some
sorting criteria.  I store things using a bubble sort.  My instances have a
comparison block associated with them; this block takes two arguments and
is a predicate which returns true if the first argument should be sorted
earlier than the second.  The default block is [ :a :b | a <= b ], but I
will accept any block that conforms to the above criteria.' !

!SortedCollection class methodsFor: 'instance creation'!

new
    ^self sortBlock: [ :a :b | a <= b ]
!

sortBlock: aSortBlock
    ^super new setSortBlock: aSortBlock

!!



!SortedCollection methodsFor: 'basic'!

addFirst: anObject
    self shouldNotImplement
!

addLast: anObject
    self shouldNotImplement
!

at: index put: anObject
    self shouldNotImplement
!

add: newObject after: oldObject
    self shouldNotImplement
!

add: newObject before: oldObject
    self shouldNotImplement
!

add: anObject
    "Add anObject into the collection at the proper place using bubble sort."
    "### not real happy with the way this is coded"
    super addFirst: anObject.
    firstIndex + 1 to: lastIndex do:
    	[ :i | (sortBlock value: (self basicAt: i)
	    	    	  value: anObject)
    	    	  ifTrue: [ self basicAt: i - 1 put: (self basicAt: i) ]
		  ifFalse: [ self basicAt: i - 1 put: anObject.
		    	     ^anObject ] ].
    self basicAt: lastIndex put: anObject.
    ^anObject
!!



!SortedCollection methodsFor: 'instance protocol'!
sortBlock
    ^sortBlock
!

sortBlock: aSortBlock
    "Change the sort criteria for a sorted collection, resort the elements of 
    the collection, and return it."
    | newSortedCollection |
    newSortedCollection _ SortedCollection sortBlock: aSortBlock.
    self do: [ :element | newSortedCollection add: element ].
    ^self become: newSortedCollection
!!



!SortedCollection methodsFor: 'enumerating'!

collect: aBlock
    | newOrderedCollection |
    newOrderedCollection _ self copyEmpty.
    self do: [ :element | newOrderedCollection add: (aBlock value: element) ].
    ^newOrderedCollection
!!



!SortedCollection methodsFor: 'copying'!

copyEmpty
    ^(super copyEmpty) setSortBlock: sortBlock
!!



!SortedCollection methodsFor: 'private methods'!
setSortBlock: aSortBlock
    sortBlock _ aSortBlock
!

growTo: anInteger
    | newCollection |
    newCollection _ super growTo: anInteger.
    newCollection setSortBlock: sortBlock.
    ^newCollection
!!
