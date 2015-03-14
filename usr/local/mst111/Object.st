"======================================================================
|
|   Object Method Definitions
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
| sbb	     12 Sep 91	  Fixed #addDependent: syntax problem.
|
| sbyrne     26 Apr 90	  Fixed shallowCopy to send new messages to the
|   	    	    	  object's class instead of the object itself.
|
| sbyrne     19 Sep 89	  Converted to use real method categories.
|
| sbyrne      4 Jul 89	  Added support for dependence relationships.
|                         (-: how appropriate: July 4 is when the US declared
|                             its INdependence from England :-)
|
| sbyrne     25 Apr 89	  created.
|
"

Object comment: 
'I am the root of the Smalltalk class system. 
All classes in the system are subclasses of me.' !


!Object methodsFor: 'Relational operators'!

~= anObject
    ^(self = anObject) == false
!

~~ anObject
    ^(self == anObject) == false 
!

isNil
    ^false
!

notNil
    ^true
!!



!Object methodsFor: 'testing functionality'!

isKindOf: aClass
    ^(self isMemberOf: aClass) or:
    	[ self class inheritsFrom: aClass ]
!

isMemberOf: aClass
    "Returns true if the receiver is an instance of the class 'aClass'"
    ^self class == aClass
!!



!Object methodsFor: 'copying'!

copy
    ^self shallowCopy
!

shallowCopy
    | class aCopy |
    class _ self class.
    class isVariable
    	ifTrue: [ aCopy _ class basicNew: self basicSize ]
	ifFalse: [ aCopy _ class basicNew ].

    " copy the instance variables (if any) "
    1 to: class instSize do:
    	[ :i | aCopy instVarAt: i
	    	     put: (self instVarAt: i) ].

    " copy the indexed variables (if any) "
    class isVariable
    	ifTrue: [ 1 to: self basicSize do:
	    	    [ :i | aCopy basicAt: i
		    	    	 put: (self basicAt: i) ] ].
    ^aCopy
!

deepCopy
    | class aCopy |
    class _ self class.
    class isVariable
    	ifTrue: [ aCopy _ class basicNew: self basicSize ]
	ifFalse: [ aCopy _ class basicNew ].

    " copy the instance variables (if any) "
    1 to: class instSize do:
    	[ :i | aCopy instVarAt: i
	    	     put: (self instVarAt: i) deepCopy ].

    " copy the indexed variables (if any) "
    class isVariable
    	ifTrue: [ 1 to: self basicSize do:
	    	    [ :i | aCopy basicAt: i
		    	    	 put: (self basicAt: i) deepCopy ] ].
    ^aCopy
!!



!Object methodsFor: 'class type methods'!

species
    ^self class
!

yourself
    ^self
!!



!Object methodsFor: 'dependents access'!

addDependent: anObject
    | dependencies |
    dependencies _ Smalltalk dependenciesAt: self.
    dependencies isNil ifTrue:
    	[ dependencies _ Set new.
	  (Smalltalk at: #Dependencies) at: self put: dependencies ].
    dependencies add: anObject
!

removeDependent: anObject
    | dependencies |
    dependencies _ Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies remove: anObject ifAbsent: [] ]
!

dependents
    | dependencies |
    dependencies _ Smalltalk dependenciesAt: self.
    dependencies isNil ifTrue: [ dependencies _ Set new ].
    ^dependencies asOrderedCollection
!

release
    " +++ I'm not sure that this is the right thing to do here; the book is
      so vague... "
    (Smalltalk at: #Dependencies) removeKey: self
!!



!Object methodsFor: 'change and update'!

changed
    self changed: self
!

changed: aParameter
    | dependencies |
    dependencies _ Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
	    [ :dependent | dependent update: aParameter ] ]
!

update: aParameter
    "Default behavior is to do nothing"
!

broadcast: aSymbol
    | dependencies |
    dependencies _ Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
	    [ :dependent | dependent perform: aSymbol ] ]
!

broadcast: aSymbol with: anObject
    | dependencies |
    dependencies _ Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
	    [ :dependent | dependent perform: aSymbol with: anObject ] ]
!!



!Object methodsFor: 'printing'!

printString
    | stream |
    stream _ WriteStream on: (String new: 0).
    self printOn: stream.
    ^stream contents
!

printOn: aStream
    | article nameString |
    nameString _ self classNameString.
    article _ nameString first isVowel ifTrue: [ 'an ' ] ifFalse: [ 'a ' ].
    article printOn: aStream.
    nameString printOn: aStream
!

print
    self printOn: stdout
!

printNl
    self print.
    Character nl print
!!



!Object methodsFor: 'storing'!

storeString
    | stream |
    stream _ WriteStream on: (String new: 0).
    self storeOn: stream.
    ^stream contents
!

storeOn: aStream
    | class hasSemi |
    class _ self class.
    aStream nextPut: $(.
    aStream nextPutAll: self classNameString.
    hasSemi _ false.
    class isVariable
    	ifTrue: [ aStream nextPutAll: ' basicNew: '.
	    	  self basicSize printOn: aStream ]
    	ifFalse: [ aStream nextPutAll: ' basicNew' ].
    1 to: class instSize do:
    	[ :i | aStream nextPutAll: ' instVarAt: '.
	       i printOn: aStream.
	       aStream nextPutAll: ' put: '.
	       (self instVarAt: i) storeOn: aStream.
	       aStream nextPut: $;.
	       hasSemi _ true ].
    class isVariable ifTrue: 
    	[ 1 to: self basicSize do:
	    [ :i | aStream nextPutAll: ' basicAt: '.
	    	   i printOn: aStream.
		   aStream nextPutAll: ' put: '.
		   (self basicAt: i) storeOn: aStream.
		   aStream nextPut: $;.
		   hasSemi _ true ] ].
    hasSemi ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!

store
    self storeOn: stdout
!

storeNl
    self store.
    Character nl print
!!



!Object methodsFor: 'debugging'!

inspect
    | class instVars instVal |
    class _ self class.
    instVars _ class instVarNames.
    'An instance of ' print.
    class printNl.
    1 to: instVars size do:
    	[ :i | '  ' print.
	       (instVars at: i) print.
	       ': ' print.
	       (self instVarAt: i)  printNl ]
!!



!Object methodsFor: 'private'!

classNameString
    | name |
    name _ self class name.
    name isNil
    	ifTrue: [ name _ self name , ' class' ].
    ^name

!!

