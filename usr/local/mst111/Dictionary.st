"======================================================================
|
|   Dictionary Method Definitions
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
| sbb	     12 Sep 91	  Added Dictionary class>>new so subclasses can win.
|
| sbyrne      6 May 90	  Fixed grow method to preserve associations in use in
|			  the dictionary instead of making new ones.  This
|			  should be faster, and doesn't break compiled methods
|			  that reference global variables when Smalltalk grows.
|
| sbyrne     24 Apr 90	  Fix at:ifAbsent: to deal with failure better (and be
|			  a tad more efficient).  Kudos (or BarNone's,
|			  depending on preference) to Andy Valencia.
|
| sbyrne      7 Apr 90	  Modified at:put: to resuse the existing Association
|			  if there is one, rather than create a new one all the
|			  time.  This was causing lossage when setting global
|			  variables in Smalltalk that previous usages weren't
|			  being changed.
|
| sbyrne     25 Apr 89	  created.
|
"

Set variableSubclass: #Dictionary
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: nil.

Dictionary comment: 
'I implement a dictionary, which is an object that is indexed by
unique objects (typcially instances of Symbol), and associates another
object with that index.  I use the equality operator = to determine
equality of indices.' !

"### The initblocks variable should not be globally visible, I think"
"This is a HACK HACK HACK.  We want to reference the InitBlocks global variable
from within some methods in System Dictionary.  However, after this file
redefines at:put: from the built-in one, and until UndefinedObject.st is 
loaded, defining isNil for nil, at:put: for dictionaries does not work
properly.  So we do it here.  The basic problem is that InitBlocks should
maybe be kept elsewhere, and not be globally visible."
Smalltalk at: #InitBlocks put: nil!

!Dictionary class methodsFor: 'instance creation'!

new
    "Builtins defines a #new method, so that during bootstrap there is a way
     to create dictionaries.  Unfortunately, this #new method only creates
     dictionaries, so subclasses when trying to use this method, lose big.
     This fixes the problem."
    ^self new: 32
! !


!Dictionary methodsFor: 'accessing'!
add: anAssociation
    | index |
    index _ self findKeyIndex: anAssociation key.
    (self basicAt: index) isNil
	ifTrue: [ tally _ tally + 1].
    self basicAt: index put: anAssociation.
    ^anAssociation
!

at: key put: value
    | index assoc |
    index _ self findKeyIndex: key.
    (assoc _ self basicAt: index) isNil
	ifTrue: [ self basicAt: index
		       put: (Association key: key value: value).
		  tally _ tally + 1 ]
	ifFalse: [ assoc value: value ].
    ^value
!

at: key
    ^self at: key ifAbsent: [ ^self error: 'key not found' ]
!

at: key ifAbsent: aBlock
    | assoc |
    assoc _ self basicAt: (self findKeyIndex: key).
    assoc isNil
            ifTrue: [ ^aBlock value ]
            ifFalse: [ ^assoc value ]
!
    
associationAt: key
    ^self associationAt: key ifAbsent: [ ^self error: 'key not found' ]
!

associationAt: key ifAbsent: aBlock
    | index assoc|
    index _ self findKeyIndex: key.
    assoc _ self basicAt: index.
    assoc isNil ifTrue: [ ^aBlock value ]
    	    	ifFalse: [ ^assoc ]
!

keyAtValue: value ifAbsent: exceptionBlock
    self associationsDo:
    	[ :assoc | value = assoc value
	    	     ifTrue: [ ^assoc key ] ].
    ^exceptionBlock value
!

keyAtValue: value
    ^self keyAtValue: value ifAbsent: []
!

keys
    | aSet |
    aSet _ Set new: tally.
    self keysDo: [ :aKey | aSet add: aKey ].
    ^aSet
!

values
    | aBag |
    aBag _ Bag new.
    self do: [ :aValue | aBag add: aValue ].
    ^aBag
!!



!Dictionary methodsFor: 'dictionary testing'!

includesAssociation: anAssociation
    | assoc |
    assoc _ self associationAt: anAssociation key ifAbsent: [ ^false ].
    ^assoc value = anAssociation value
!

includesKey: key
    self associationAt: key ifAbsent: [ ^false ].
    ^true
!

includes: anObject
    self do: [ :element | element = anObject ifTrue: [ ^true ] ].
    ^false
!

occurrencesOf: aValue
    | count |
    count _ 0.
    self do: [ :element | element = aValue
    			    ifTrue: [ count _ count + 1] ].
    ^count
!!



!Dictionary methodsFor: 'dictionary removing'!

removeAssociation: anAssociation
    "### does this check the value as well as the key?"
    self removeKey: anAssociation key ifAbsent: [].
    ^anAssociation
!

removeKey: key
    ^self removeKey: key ifAbsent: [ ^self error: 'key not found' ]
!

removeKey: key ifAbsent: aBlock
    | index assoc |
    index _ self findKeyIndexNoGrow: key ifAbsent: [ ^aBlock value ].
    assoc _ self basicAt: index.
    self basicAt: index put: nil.
    tally _ tally - 1.
    self rehashObjectsAfter: index.
    ^assoc value
!

remove: anObject
    self error: 'remove: not allowed in Dictionary'
!

remove: anObject ifAbsent: aBlock
    self error: 'remove:ifAbsent: not allowed in Dictionary'
!!



!Dictionary methodsFor: 'dictionary enumerating'!
associationsDo: aBlock
    super do: [ :assoc | aBlock value: assoc ]
!

"These could be implemented more efficiently by doing the super do
 directly, or doing the explicit scanning of the dictionary by hand"
keysDo: aBlock
    self associationsDo: [ :assoc | aBlock value: assoc key ]
!

do: aBlock
    self associationsDo: [ :assoc | aBlock value: assoc value ]
!

collect: aBlock
    | aBag |
    aBag _ Bag new.
    self do: [ :element | aBag add: (aBlock value: element) ].
    ^aBag
!

select: aBlock
    | newDict |
    newDict _ self species new.
    self associationsDo:
    	[ :assoc | (aBlock value: assoc value)
		     ifTrue: [ newDict add: assoc ] ].
    ^newDict
!

reject: aBlock
    self shouldNotImplement
!

inject: value into: aBlock
    self shouldNotImplement
!!



!Dictionary methodsFor: 'misc math methods'!

= aDictionary
    tally ~= aDictionary size ifTrue: [ ^false ].
    self associationsDo:
    	[ :assoc | assoc value ~= (aDictionary at: assoc key
	    	    	    	    	       ifAbsent: [ ^false ])
    	    	     ifTrue: [ ^false ] ].
    ^true
!

hash
    | hashValue |
    hashValue _ tally.
    self associationsDo:
    	[ :assoc | hashValue _ hashValue + assoc hash ].
    ^hashValue
!!



!Dictionary methodsFor: 'printing'!

printOn: aStream
    aStream nextPutAll: self class name , ' (' .
    self associationsDo:
    	[ :assoc | assoc key storeOn: aStream.
	    	   aStream nextPut: $,.
		   assoc value storeOn: aStream.
		   aStream nextPut: Character space ].
    aStream nextPut: $)
!!



!Dictionary methodsFor: 'storing'!

storeOn: aStream
    | hasElements |
    aStream nextPutAll: '(', self class name , ' new'.
    hasElements _ false.
    self associationsDo:
    	[ :assoc | aStream nextPutAll: ' at: '.
    	    	   assoc key storeOn: aStream.
	    	   aStream nextPutAll: ' put: '.
		   assoc value storeOn: aStream.
		   aStream nextPut: $;.
		   hasElements _ true ].
    hasElements ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!!



!Dictionary methodsFor: 'private methods'!

rehashObjectsAfter: index
    "Rehashes all the objects in the collection after index to see if any of
    them hash to index.  If so, that object is copied to index, and the
    process repeats with that object's index, until a nil is encountered."
    | i size count assoc |
    i _ index.
    size _ self basicSize.
    count _ size.
    [ count > 0 ]
    	whileTrue:
	    [ i _ i \\ size + 1.
    	      assoc _ self basicAt: i.
	      assoc isNil ifTrue: [ ^self ].
    	      ((assoc key hash \\ size) + 1) = index
	      	ifTrue: [ self basicAt: index put: assoc.
		    	  self basicAt: i put: nil.  "Be tidy"
			  index _ i ].
    	      count _ count - 1 ]
!

findKeyIndex: aKey ifFull: aBlock
    "Tries to see if aKey exists as the key of an indexed variable (which is an
    association).  If it's searched the entire dictionary and the key is 
    not to be found, aBlock is evaluated and it's value is returned."
    | index count size assoc |
    size _ self basicSize.
    index _ aKey hash \\ size + 1.
    count _ size.
    [ count > 0 ]
    	whileTrue:
	    [ assoc _ self basicAt: index.
    	      (assoc isNil or: [ assoc key = aKey ])
	    	ifTrue: [ ^index ].
	      index _ index \\ size + 1.
	      count _ count - 1. ].
    ^aBlock value
!
	    
findKeyIndex: aKey
    "Finds an association with the given key in the dictionary and returns its
    index.  If the dictionary doesn't contain the object and there is no nil
    element, the dictionary is grown and then the index of where the object
    would go is returned."
    ^self findKeyIndex: aKey
     	  ifFull: [ self grow.
	    	    self findKeyIndexNoGrow: aKey
		     	 ifAbsent: [ ^self error: 'failed to grow a new empty element!!!' ] ]
!

findKeyIndexNoGrow: aKey ifAbsent: aBlock
    | index |
    index _ self findKeyIndex: aKey ifFull: [ 0 ].
    (index = 0 )
    	ifTrue: [ ^aBlock value ]
	ifFalse: [ ^index ]
!

grow
    | newDict |
    newDict _ self species new: self basicSize + self growSize.
    self associationsDo: [ :assoc | newDict add: assoc ].
    ^self become: newDict
!

growSize
    ^32

!!

