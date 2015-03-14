"======================================================================
|
|   ArrayedCollection Method Definitions
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

SequenceableCollection variableSubclass: #ArrayedCollection
		       instanceVariableNames: ''
		       classVariableNames: ''
		       poolDictionaries: ''
		       category: nil.

ArrayedCollection comment: 
'My instances are objects that are generally fixed size, and are accessed
by an integer index.  The ordering of my instance''s elements is determined 
externally; I will not rearrange the order of the elements.' !

!ArrayedCollection class methodsFor: 'instance creation'!

with: element1
    | anArrayedCollection |
    anArrayedCollection _ self new: 1.
    anArrayedCollection at: 1 put: element1.
    ^anArrayedCollection
!

with: element1 with: element2
    | anArrayedCollection |
    anArrayedCollection _ self new: 2.
    anArrayedCollection at: 1 put: element1.
    anArrayedCollection at: 2 put: element2.
    ^anArrayedCollection
!

with: element1 with: element2 with: element3
    | anArrayedCollection |
    anArrayedCollection _ self new: 3.
    anArrayedCollection at: 1 put: element1.
    anArrayedCollection at: 2 put: element2.
    anArrayedCollection at: 3 put: element3.
    ^anArrayedCollection
!

with: element1 with: element2 with: element3 with: element4
    | anArrayedCollection |
    anArrayedCollection _ self new: 4.
    anArrayedCollection at: 1 put: element1.
    anArrayedCollection at: 2 put: element2.
    anArrayedCollection at: 3 put: element3.
    anArrayedCollection at: 4 put: element4.
    ^anArrayedCollection
!!



!ArrayedCollection methodsFor: 'basic'!

add: value
    self shouldNotImplement
!!



!ArrayedCollection methodsFor: 'comparing'!

= anArrayedCollection
    "Return true if the receiver and anArrayedCollection represent arrays
    with equal contents."
    self size = anArrayedCollection size ifFalse: [ ^false ].
    1 to: self size do:
    	[ :i | (self at: i) = (anArrayedCollection at: i)
	    	  ifFalse: [ ^false ] ].
    ^true
!

hash
    "Return the hash value of the array"
    "### I don't like this hashing algorithm"
    | hashValue |
    hashValue _ 0.
    self do: [ :element | hashValue _ ((hashValue bitShift: 1)
    	    	    	    	    	bitXor:  element hash)
					bitAnd: 16r1FFFFFFF ].
    ^hashValue
!!


!ArrayedCollection methodsFor: 'printing'!

printOn: aStream
    | firstTime |
    aStream nextPutAll: self classNameString.
    aStream nextPutAll: ' ('.
    firstTime _ true.
    self do:
    	[ :element | firstTime ifTrue: [ firstTime _ false ]
    	    	    	       ifFalse: [ aStream nextPut: Character space ].
    	    	     element storeOn: aStream ].
    aStream nextPut: $)
!!



!ArrayedCollection methodsFor: 'storing'!

storeOn: aStream
    | index |
    aStream nextPutAll: '(('; nextPutAll: self classNameString; 
        nextPutAll: ' basicNew: '.
    self basicSize printOn: aStream.
    aStream nextPut: $).
    index _ 1.
    self do:
    	[ :element | aStream nextPutAll: ' at: '.
	    	     index printOn: aStream.
		     aStream nextPutAll: ' put: '.
    	    	     element storeOn: aStream.
		     aStream nextPut: $;.
    	    	     index _ index + 1 ].
    index > 1 ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!!

