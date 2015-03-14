"======================================================================
|
|   Class Method Definitions
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
| sbyrne     16 May 90	  Improved error checking: you now cannot create a
|			  subclass of a class whose type is not compatible
|			  (i.e. non-variable subclass of a variable byte
|			  class).
|
| sbyrne     16 May 90	  Minor changes to support preserving class definitions
|			  as long as possible (i.e. if you re-invoke the class
|			  definition method, it tries to re-use the existing
|			  class if possible).
|
| sbyrne     13 Jan 90	  Began experimental addition of actual class
|			  definitions. 
|
| sbyrne     25 Apr 89	  created.
|
"

ClassDescription subclass: #Class
		 instanceVariableNames: 'classVariables sharedPools'
		 classVariableNames: ''
		 poolDictionaries: ''
		 category: nil.

Class comment: 
'I am THE class object.  My instances are the classes of the system' !



!Class methodsFor: 'accessing instances and variables'!

addClassVarName: aString
    | sym |
    sym _ aString asSymbol.	"### maybe this should remain a string? "
    (classVariables includesKey: sym)
    	ifTrue: [ ^self error: 'class variable already present' ]
	ifFalse: [ classVariables at: sym put: nil ]
!

removeClassVarName: aString
    "Removes the class variable from the class, error if not present, or
     still in use."
    | sym |
    sym _ aString asSymbol.	"### maybe this should remain a string? "
    " ### test for use in sub method "
    (classVariables includesKey: sym)
    	ifTrue: [ classVariables removeKey: sym ]
	ifFalse: [ self error: 'class variable not present' ]
!

classPool
    ^classVariables
!

addSharedPool: aDictionary
    (sharedPools includes: aDictionary)
    	ifTrue: [ ^self error: 'Attempt to add an already-present shared pool' ].
    sharedPools _ sharedPools copyWith: aDictionary
!

removeSharedPool: aDictionary
    (sharedPools includes: aDictionary)
    	ifFalse: [ ^self error: 'Attempt to remove non-existent shared pool' ].
    sharedPools _ sharedPools copyWithout: aDictionary
!

initialize			"redefined in children (?)"
    ^self
!!



!Class methodsFor: 'testing'!

= aClass
    "Returns true if the two class objects are to be considered equal."
    ^name = aClass name
!!



!Class methodsFor: 'instance creation'!


subclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    | meta |
    KernelInitialized ifFalse: [ ^nil ].
    self isVariable
	ifTrue: [ ^self error: 'cannot create a non-variable subclass 
of a variable class' ].
    meta _ self metaclassFor: classNameString.
    ^meta name: classNameString
    	    environment: Smalltalk
	    subclassOf: self
	    instanceVariableNames: stringInstVarNames
	    variable: false
	    words: true
	    pointers: true
    	    classVariableNames: stringOfClassVarNames
	    poolDictionaries: stringOfPoolNames
	    category: categoryNameString
	    comment: nil
	    changed: nil
!

variableSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    | meta |
    KernelInitialized ifFalse: [ ^nil ].
    self isVariable & (self isWords | self isBytes) 
	ifTrue: [ ^self error: 'cannot create a variable subclass from a 
non-pointer variable parent class' ].
    meta _ self metaclassFor: classNameString.
    ^meta name: classNameString
    	    environment: Smalltalk
	    subclassOf: self
	    instanceVariableNames: stringInstVarNames
	    variable: true
	    words: true
	    pointers: true
    	    classVariableNames: stringOfClassVarNames
	    poolDictionaries: stringOfPoolNames
	    category: categoryNameString
	    comment: nil
	    changed: nil
!

variableWordSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    | meta |
    KernelInitialized ifFalse: [ ^nil ].
    self isVariable & (self isPointers | self isBytes) 
	ifTrue: [ ^self error: 'cannot create a word subclass from a non-word 
variable parent class' ].
    meta _ self metaclassFor: classNameString.
    ^meta name: classNameString
    	    environment: Smalltalk
	    subclassOf: self
	    instanceVariableNames: stringInstVarNames
	    variable: true
	    words: true
	    pointers: false
    	    classVariableNames: stringOfClassVarNames
	    poolDictionaries: stringOfPoolNames
	    category: categoryNameString
	    comment: nil
	    changed: nil
!

variableByteSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    | meta |
    KernelInitialized ifFalse: [ ^nil ].
    self isVariable & (self isPointers | self isWords)
	ifTrue: [ ^self error: 'Cannot create variable byte subclass from 
non-byte parent class' ].
    meta _ self metaclassFor: classNameString.
    ^meta name: classNameString
    	    environment: Smalltalk
	    subclassOf: self
	    instanceVariableNames: stringInstVarNames
	    variable: true
	    words: false
	    pointers: false
    	    classVariableNames: stringOfClassVarNames
	    poolDictionaries: stringOfPoolNames
	    category: categoryNameString
	    comment: nil
	    changed: nil
!!



!Class methodsFor: 'printing'!

printOn: aStream
    self name printOn: aStream
!

storeOn: aStream
    self printOn: aStream
!!



!Class methodsFor: 'private'!

setClassVariables: aDictionary
    classVariables _ aDictionary
!

setSharedPools: aDictionary
    sharedPools _ aDictionary
!

metaclassFor: classNameString
    | className class |
    className _ classNameString asSymbol.
    class _ Smalltalk at: className 
		      ifAbsent: [ ^Metaclass subclassOf: self class ].
    ^class class
!

setSuperclass: aClass
    self superclass == aClass
	ifTrue: [ ^self ].	"don't need to change anything"
    self superclass notNil	"remove any old knowledge of this class"
	ifTrue: [ self superclass removeSubclass: self ].
    self superclass: aClass.
    aClass addSubclass: self
!!
