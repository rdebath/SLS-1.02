"======================================================================
|
|   ClassDescription Method Definitions
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
| sbyrne     23 Sep 89	  fileOutCategory: is dangerous, so I make it write to
|			  a subdirectory called './categories'.
|
| sbyrne     25 Apr 89	  created.
|
"

Behavior subclass: #ClassDescription
	 instanceVariableNames: 'name comment instanceVariables category'
	 classVariableNames: ''
	 poolDictionaries: ''
	 category: nil.

ClassDescription comment: 
'My instances record information generally attributed to classes and
metaclasses; namely, the class name, class comment (you wouldn''t be
reading this if it weren''t for me), a list of the instance variables
of the class, and the class category.  I provide methods that
access classes by category, and allow whole categories of classes to be
filed out to external disk files.' !



!ClassDescription methodsFor: 'accessing class description'!

name
    ^name
!

comment
    ^comment
!

comment: aString
    comment _ aString
!

addInstVarName: aString
    instanceVariables _ instanceVariables copyWith: aString
!

removeInstVarName: aString
    instanceVariables _ instanceVariables copyWithout: aString
!!



!ClassDescription methodsFor: 'organization of messages and classes'!

category
    ^category
!

category: aString
    aString isNil
	ifTrue: [ category _ nil ]
	ifFalse: [ category _ aString asSymbol ]
!

removeCategory: aString
    | selector method category |
    methodDictionary isNil
    	ifTrue: [ ^self ].
    category _ aString asSymbol.
    methodDictionary associationsDo:
    	[ :assoc | method _ assoc key.
	    	   method methodCategory = category
		   	ifTrue: [ methodDictionary remove: assoc ] ].
!

whichCategoryIncludesSelector: selector
    | method |
    methodDictionary isNil
    	ifTrue: [ ^nil ].
    method _ methodDictionary at: selector.
    ^method methodCategory
!!



!ClassDescription methodsFor: 'copying'!

copy: selector from: aClass
    | method |
    method _ aClass compiledMethodAt: selector.
    methodDictionary at: selector put: method.
!

copy: selector from: aClass classified: categoryName
    | method |
    method _ (aClass compiledMethodAt: selector) deepCopy.
    method methodCategory: categoryName.
    methodDictionary at: selector put: method
!

copyAll: arrayOfSelectors from: class
    arrayOfSelectors do:
	[ :selector | self copy: selector
			   from: class ]
!

copyAll: arrayOfSelectors from: class classified: categoryName
    arrayOfSelectors do:
	[ :selector | self copy: selector
			   from: class
			   classified: categoryName ]
!

copyAllCategoriesFrom: aClass
    | method |
    aClass selectors do:
	[ :selector | self copy: selector from: aClass ]
!

copyCategory: categoryName from: aClass
    | method |
    aClass selectors do:
	[ :selector | method _ aClass compiledMethodAt: selector.
		      method methodCategory = categoryName
			  ifTrue: [ self copy: selector from: aClass ] ]
!

copyCategory: categoryName from: aClass classified: newCategoryName
    | method |
    aClass selectors do:
	[ :selector | method _ aClass compiledMethodAt: selector.
		      method methodCategory = categoryName
			  ifTrue: [ self copy: selector
					 from: aClass
					 classified: newCategoryName ] ]
!!



!ClassDescription methodsFor: 'compiling'!

compile: code classified: categoryName
    | method |
    self notYetImplemented
!

compile: code classified: categoryName notifying: requestor
    self notYetImplemented
!!



!ClassDescription methodsFor: 'accessing instances and variables'!

instVarNames
    ^instanceVariables
!!



!ClassDescription methodsFor: 'printing'!

classVariableString
    self subclassResponsibility
!

instanceVariableString
    | aString |
    instanceVariables isNil ifTrue: [ ^'' ].
    aString _ String new: 0.
    instanceVariables do: [ :instVarName | aString _ aString ,
    	    	    	    	    	      instVarName , ' ' ].
    ^aString
!

sharedVariableString
    self subclassResponsibility
!!



!ClassDescription methodsFor: 'filing'!

fileOutOn: aFileStream
    | categories now |
    categories _ Set new.
    methodDictionary isNil ifTrue: [ ^self ].
    methodDictionary do:
	[ :method | categories add: (method methodCategory) ].
    '''Filed out from ' printOn: aFileStream.
    Version printOn: aFileStream.
    ' on ' printOn: aFileStream.
    now _ Date dateAndTimeNow.
    (now at: 1) printOn: aFileStream.
    '  ' printOn: aFileStream.
    (now at: 2) printOn: aFileStream.
    ' GMT''!' printOn: aFileStream.    
    Character nl printOn: aFileStream.
    Character nl printOn: aFileStream.
    categories asSortedCollection do:
        [ :category | self emitCategory: category toStream: aFileStream ]
!

fileOutCategory: categoryName
    | aFileStream fileName |
    name notNil
    	ifTrue: [ fileName _ name ]
	ifFalse: [ fileName _ (self instanceClass name) , '-class' ].
    fileName _ './categories/', fileName , '.st' .
    aFileStream _ FileStream open: fileName mode: 'w'.
    self emitCategory: categoryName toStream: aFileStream.
    aFileStream close
!!



!ClassDescription methodsFor: 'private'!

emitCategory: category toStream: aFileStream
    "I write legal Smalltalk load syntax definitions of all of my methods
     are in the 'category' category to the aFileStream"
    '!' printOn: aFileStream.
    self printOn: aFileStream.
    ' methodsFor: ''' printOn: aFileStream.
    category printOn: aFileStream.
    '''!' printOn: aFileStream.
    methodDictionary notNil
  	ifTrue: [ methodDictionary do:
		      [ :method | (method methodCategory) = category
			          ifTrue: [ '

'			                    printOn: aFileStream.
				            method methodSourceString
				                 printOn: aFileStream.
			                    '!' printOn: aFileStream ] ] ].
    '!

'   printOn: aFileStream

!

setName: aSymbol
    name _ aSymbol
!

setInstanceVariables: instVariableArray
    instanceVariables _ instVariableArray

!!
