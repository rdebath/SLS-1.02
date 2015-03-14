"======================================================================
|
|   MetaClass Method Definitions
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
| sbyrne     16 May 90	  Changed the implementation of name: ... to try to
|			  preserve an existing class (if possible).  The
|			  original code exists in newMeta: ...
|
| sbyrne     25 Apr 89	  created.
|
"

ClassDescription subclass: #Metaclass
		 instanceVariableNames: 'instanceClass'
		 classVariableNames: ''
		 poolDictionaries: ''
		 category: nil !

Metaclass comment: 
'I am the root of the class hierarchy.  My instances are metaclasses, one for
each real class.  My instances have a single instance, which they hold
onto, which is the class that they are the metaclass of.  I provide methods
for creation of actual class objects from metaclass object, and the creation
of metaclass objects, which are my instances.  If this is confusing to you,
it should be...the Smalltalk metaclass system is strange and complex.' !

!Metaclass class methodsFor: 'instance creation'!

subclassOf: superMeta
    | newMeta |
    newMeta _ self new.
    newMeta superclass: superMeta.
    superMeta addSubclass: newMeta.
    newMeta initMetaclass.
    ^newMeta

!!



!Metaclass methodsFor: 'basic'!

name: newName
    environment: aSystemDictionary
    subclassOf: superclass
    instanceVariableNames: stringOfInstVarNames
    variable: variableBoolean
    words: wordBoolean
    pointers: pointerBoolean
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryName
    comment: commentString
    changed: changed
    | aClass variableString variableArray sharedPoolNames poolName pool 
      className classVarDict oldClassPool |

    "Please don't look at this case for an example of how to create good 
     Smalltalk code.  It is inelegantly written and probably highly 
    inefficient."

    className _ newName asSymbol.
    aClass _ aSystemDictionary 
	at: className
	ifAbsent: [ ^self newMeta: newName
			  environment: aSystemDictionary
			  subclassOf: superclass
			  instanceVariableNames: stringOfInstVarNames
			  variable: variableBoolean
			  words: wordBoolean
			  pointers: pointerBoolean
			  classVariableNames: stringOfClassVarNames
			  poolDictionaries: stringOfPoolNames
			  category: categoryName
			  comment: commentString
			  changed: changed ].

    (aClass isVariable == variableBoolean)
	& (aClass isWords == wordBoolean )
	& (aClass isPointers == pointerBoolean)
	ifFalse: [ ^self newMeta: newName
			  environment: aSystemDictionary
			  subclassOf: superclass
			  instanceVariableNames: stringOfInstVarNames
			  variable: variableBoolean
			  words: wordBoolean
			  pointers: pointerBoolean
			  classVariableNames: stringOfClassVarNames
			  poolDictionaries: stringOfPoolNames
			  category: categoryName
			  comment: commentString
			  changed: changed ].

    "Here we have an existing class, so try hard to leave it alone"
    instanceClass _ aClass.
    aClass setSuperclass: superclass.

    superclass notNil
    	ifTrue: [ "Inherit instance variables from parent"
	    	  variableString _ superclass instanceVariableString
		  ]
        ifFalse: [ variableString _ '' ].
    variableString _ variableString , stringOfInstVarNames.
    variableArray _ self parseVariableString: variableString.
    1 to: variableArray size do:
    	[ :i | variableArray at: i put: (variableArray at: i) asSymbol ].
    variableArray = aClass instVarNames
	ifFalse: [ 'Recompilation required!' printNl.
		   "### This should be fixed soon" ].
    aClass setInstanceVariables: variableArray.

    aClass setInstanceSpec: variableBoolean words: wordBoolean
    	pointers: pointerBoolean instVars: variableArray size.

    classVarDict _ (self parseToDict: stringOfClassVarNames).
    oldClassPool _ aClass classPool.
    oldClassPool isNil 
	ifTrue: [ aClass setClassVariables: classVarDict ]
	ifFalse: [ classVarDict associationsDo:
		       [ :assoc | (oldClassPool includesKey: assoc key)
				      ifFalse: 
					  [ aClass addClassVarName: 
						assoc key ] ] ].
    classVarDict keys  ~= aClass classPool keys
	ifTrue: [ 'Recompilation required: different class variables!' 
                      printNl ].

    sharedPoolNames _ self parseVariableString: stringOfPoolNames.
    1 to: sharedPoolNames size do:
    	[ :i | poolName _ (sharedPoolNames at: i) asSymbol.
    	       "### Check that the pool name starts with an uppercase letter
		here."
	       "??? Should this create the pool if not there?"
	       pool _ aSystemDictionary
	       	    	at: poolName
	       	    	ifAbsent: [ ^self error: 'Pool name ', poolName ,
			    	    	    	 ' does not exist' ].
    	      sharedPoolNames at: i put: pool ].
    "### probably should check for recompilation required here in case
     the intersection of the sets of pool dictionaries shrinks"
    aClass setSharedPools: sharedPoolNames.

    "### not done"
    aClass category: categoryName. "### need to remove the old category maybe"
    "### don't know what to do with changed"
    ^aClass
!


newMeta: newName
    environment: aSystemDictionary
    subclassOf: superclass
    instanceVariableNames: stringOfInstVarNames
    variable: variableBoolean
    words: wordBoolean
    pointers: pointerBoolean
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryName
    comment: commentString
    changed: changed
    | aClass variableString variableArray sharedPoolNames poolName pool |

    sharedPoolNames _ self parseVariableString: stringOfPoolNames.
    1 to: sharedPoolNames size do:
    	[ :i | poolName _ (sharedPoolNames at: i) asSymbol.
    	       "### Check that the pool name starts with an uppercase letter
		here."
	       pool _ aSystemDictionary
	       	    	at: poolName
	       	    	ifAbsent: [ ^self error: 'Pool name ', poolName ,
			    	    	    	 ' does not exist' ].
    	      sharedPoolNames at: i put: pool ].
    aClass _ self new.
    instanceClass _ aClass.
    aSystemDictionary at: (newName asSymbol) put: aClass.
    aClass superclass: superclass.
    aClass setName: newName asSymbol.
    superclass notNil
    	ifTrue: [ superclass addSubclass: aClass.
    	    	  "Inherit instance variables from parent"
	    	  variableString _ superclass instanceVariableString
		  ]
        ifFalse: [ variableString _ '' ].
    variableString _ variableString , stringOfInstVarNames.
    variableArray _ self parseVariableString: variableString.
    1 to: variableArray size do:
    	[ :i | variableArray at: i put: (variableArray at: i) asSymbol ].
    aClass setInstanceVariables: variableArray.
    aClass setInstanceSpec: variableBoolean words: wordBoolean
    	pointers: pointerBoolean instVars: variableArray size.
    aClass setClassVariables: (self parseToDict: stringOfClassVarNames).
    aClass setSharedPools: sharedPoolNames.
    "### not done"
    aClass category: categoryName.
    aClass comment: commentString.
    "### don't know what to do with changed"
    ^aClass
!!



!Metaclass methodsFor: 'accessing'!

instanceClass
    ^instanceClass
!!




!Metaclass methodsFor: 'printing'!

printOn: aStream
    instanceClass printOn: aStream.
    ' class' printOn: aStream
!

storeOn: aStream
    self printOn: aStream
!!



!Metaclass methodsFor: 'private'!

initMetaclass
    instanceVariables _ Class instVarNames.
    instanceSpec _ Class instanceSpec
!

parseVariableString: aString
    | stream |
    stream _ TokenStream on: aString.
    ^stream contents
!

parseToDict: aString
    | tokenArray dict |
    tokenArray _ self parseVariableString: aString.
    dict _ Dictionary new.
    tokenArray do:
    	[ :element | dict at: element asSymbol put: nil ].
    ^dict

!!
