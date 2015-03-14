"======================================================================
|
|   SequenceableCollection Method Definitions
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
| sbb	     12 Sep 91	  Fixed indexOfSubCollection.
|
| sbyrne     19 Sep 89	  Converted to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

Collection variableSubclass: #SequenceableCollection
	   instanceVariableNames: ''
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: nil.

SequenceableCollection comment: 
'My instances represent collections of objects that are ordered.  I provide
some access and manipulation methods.' !


!SequenceableCollection methodsFor: 'basic'!

atAll: aCollection put: anObject
    aCollection do: [ :index | self at: index put: anObject ]
!

atAllPut: anObject
    1 to: self size do: [ :index | self at: index put: anObject ]
!

first
    ^self at: 1
!

last
    self size < 1 ifTrue: [ ^self error: 'last not defined with no elements' ].
    ^self at: self size
!

indexOf: anElement ifAbsent: exceptionBlock
    1 to: self size do: [ :index | (self at: index) = anElement
    	    	    	    	     ifTrue: [ ^index ] ].
    ^exceptionBlock value
!

indexOf: anElement
    ^self indexOf: anElement ifAbsent: [ ^0 ]
!!



!SequenceableCollection methodsFor: 'nonpublic methods'!

matchSubCollection: aSubCollection startingAt: anIndex
    2 to: aSubCollection size do:
    	[ :index | (self at: anIndex + index - 1) ~= (aSubCollection at: index)
	    	       ifTrue: [ ^false ]
	].
    ^true
!

indexOfSubCollection: aSubCollection startingAt: anIndex
    ifAbsent: exceptionBlock
    | selfSize subSize |
    subSize  _ aSubCollection size.
    selfSize _ self size.
    anIndex + subSize - 1 <= selfSize ifTrue:
	[ anIndex to: selfSize - subSize + 1 do:
	      [:index | (self at: index) = (aSubCollection at: 1)
			    ifTrue: [(self matchSubCollection: aSubCollection
					   startingAt: index)
					 ifTrue: [^index]
					 ]
			    ]
	      ].
    ^exceptionBlock value
!

indexOfSubCollection: aSubCollection startingAt: anIndex
    ^self indexOfSubCollection: aSubCollection startingAt: anIndex
    	ifAbsent: [ ^0 ]
!

replaceFrom: start to: stop with: replacementCollection startingAt: repStart
    (self == replacementCollection and: [ repStart ~= 1 ])
    	ifTrue: [ ^self error: 'replaceFrom:to:with:startingAt: called for 
in-place replacement, but starting index was not 1' ].
    1 to: stop - start + 1 do:
    	[ :index |
	  self at: (start + index - 1)
    	       put: (replacementCollection at: (repStart + index - 1)) ]
!

replaceFrom: start to: stop with: replacementCollection
    stop - start + 1 ~= replacementCollection size
    	ifTrue: [ ^self error: 'replacement range does not equal size of
replacement collection' ].
    self replaceFrom: start to: stop with: replacementCollection startingAt: 1
!!



!SequenceableCollection methodsFor: 'copying SequenceableCollections'!

, aSequenceableCollection
    | newCollection |
    newCollection _ self species new: (self size + aSequenceableCollection size).
    newCollection replaceFrom: 1 to: self size with: self.
    newCollection replaceFrom: (self size) + 1
    	    	  to: self size + aSequenceableCollection size
		  with: aSequenceableCollection.
    ^newCollection
!

copyFrom: start to: stop
    | newCollection len |
    len _ stop - start + 1.
    newCollection _ self species new: len.
    newCollection replaceFrom: 1 to: len with: self startingAt: start.
    ^newCollection
!

copyReplaceAll: oldSubCollection with: newSubCollection
    | numOld newCollection sizeDifference newSubSize oldSubSize
      newStart oldStart copySize index |
    numOld _ self countSubCollectionOccurrencesOf: oldSubCollection.
    newSubSize _ newSubCollection size.
    sizeDifference _ newSubSize - oldSubCollection size + 1.
    newCollection _ self species new: (self size - (sizeDifference * numOld)).
    oldStart _ newStart _ 1.
    [ index _ self indexOfSubCollection: oldSubCollection
    	    	   startingAt: oldStart.
      index > 0 ] whileTrue:
    	[ copySize _ index - oldStart + 1.
	  newCollection replaceFrom: newStart
	    	    	to: newStart + copySize - 1
			with: self
			startingAt: oldStart.
	  newStart _ newStart + copySize - 1.
	  newCollection replaceFrom: newStart
	    	    	to: newStart + newSubSize - 1
			with newSubCollection.
    	  oldStart _ oldStart + copySize.
    	  newStart _ newStart + newSubSize ].
    "Copy the remaining part of self onto the tail of the new collection."
    newCollection replaceFrom: newStart
    	    	 to: newCollection size
		 with: self
		 startingAt: oldStart.
    ^newCollection
!

copyReplaceFrom: start to: stop with: replacementCollection
    | newCollection newSize repSize |
    "### check for bounds "
    repSize _ replacementCollection size.
    newSize _ self size + repSize - (stop - start + 1).
    newCollection _ self species new: newSize.
    newCollection replaceFrom: 1 to: start - 1 with: self startingAt: 1.
    newCollection replaceFrom: start
    	    	  to: start + repSize - 1
		  with: replacementCollection.
    newCollection replaceFrom: start + repSize
    	    	  to: newCollection size
		  with: self
		  startingAt: stop + 1.
    ^newCollection
!

copyWith: newElement
    | newCollection len |
    len _ self size + 1.
    newCollection _ self species new: len.
    newCollection replaceFrom: 1 to: self size with: self.
    newCollection at: len put: newElement.
    ^newCollection
!

copyWithout: oldElement
    | newCollection numOccurrences i |
    numOccurrences _ 0.
    self do:
    	[ :element |
	  element = oldElement
	    ifTrue: [ numOccurrences _ numOccurrences + 1 ] ].
    newCollection _ self species new: (self size - numOccurrences).
    i _ 1.
    self do:
    	[ :element |
	  element ~= oldElement
	    ifTrue: [ newCollection at: i put: element.
	    	      i _ i + 1 ]
	].
    ^newCollection
!!



!SequenceableCollection methodsFor: 'enumerating'!

do: aBlock
    "Evaluate aBlock for all elements in the sequenceable collection"
    1 to: self size do:
    	[ :i | aBlock value: (self at: i) ]
!

findFirst: aBlock
    "Returns the index of the first element of the sequenceable collection
    for which aBlock returns true"
    1 to: self size do:
    	[ :i | (aBlock value: (self at: i))
	    	 ifTrue: [ ^i ] ].
    ^0
!

findLast: aBlock
    self size to: 1 by: -1 do:
    	[ :i | (aBlock value: (self at: i))
	    	 ifTrue: [ ^i ] ].
    ^0
!

reverseDo: aBlock
    self size to: 1 by: -1 do:
    	[ :i | aBlock value: (self at: i) ]
!

with: aSequenceableCollection do: aBlock
    self size = aSequenceableCollection size
    	ifFalse:
	    [ ^self error: 'sequenceable collections must have same size' ].
    1 to: self size do:
    	[ :i | aBlock value: (self at: i)
	    	      value: (aSequenceableCollection at: i) ]
!!



!SequenceableCollection methodsFor: 'private methods'!

countSubCollectionOccurrencesOf: aSubCollection
    | colIndex subColIndex count |
    colIndex _ 1.
    count _ 0.
    [ subColIndex _ self indexOfSubCollection: aSubCollection
    	    	    	 startingAt: colIndex.
      subColIndex > 0 ] whileTrue:
      	[ count _ count + 1.
	  colIndex _ colIndex + aSubCollection size ].
    ^count
!

grow
    | newCollection |
    newCollection _ self species new: self basicSize + self growSize.
    newCollection replaceFrom: 1 to: self size with: self.
    ^self become: newCollection
!
    
growSize
    ^10				"a randomly chosed number"

!!

