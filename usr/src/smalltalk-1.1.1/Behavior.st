"======================================================================
|
|   Behavior Method Definitions
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
| sbb	     12 Sep 91	  Fixed isBytes and isWords so that subclassing for
|			  things like Dictionary works properly.
|
| sbyrne     25 Apr 89	  created.
|
"

Object subclass: #Behavior
       instanceVariableNames: 'superClass subClasses 
    	    	    	       methodDictionary instanceSpec'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Behavior comment: 
'I am the parent class of all "class" type methods.  My instances know
about the subclass/superclass relationships between classes, contain
the description that instances are created from, and hold the method
dictionary that''s associated with each class.  I provide methods for
compiling methods, modifying the class inheritance hierarchy, examining the
method dictionary, and iterating over the class hierarchy.' !

CFunctionDescs at: #CFunctionGensym put: 1!

!Behavior class methodsFor: 'C interface'!

defineCFunc: cFuncNameString
  withSelectorArgs: selectorAndArgs
  forClass: aClass 
  returning: returnTypeSymbol 
  args: argsArray
    | stream gensym descriptor |
    "This is pretty complex.  What I want to provide is a very efficient way
     of calling a C function.  I create a descriptor object that holds the
     relevant information regarding the C function.  I then compile the
     method that's to be invoked to call the C function.  This method uses the
     primitive #255 to perform the actual call-out.  To let the primitive
     know which descriptor to use, I arrange for the first and only method
     literal of the compiled method to be an association that contains as
     its value the C function descriptor object.  I add new associations to
     the global shared pool 'CFunctionDescs', and reference the newly
     generated key in the text of the compiled method."
    gensym _ Symbol intern: ('CFunction' , CFunctionGensym printString).
    CFunctionGensym _ CFunctionGensym + 1.
    descriptor _ self makeDescriptorFor: cFuncNameString
    	              returning: returnTypeSymbol
	    	      withArgs: argsArray.
    CFunctionDescs at: gensym put: descriptor.
    stream _ WriteStream on: (String new: 5).
    stream nextPutAll: selectorAndArgs.    
    stream nextPutAll:
'
    <primitive: 255>
    ^'.
    gensym printOn: stream.
    aClass compile: stream contents
!!



!Behavior methodsFor: 'creating method dictionary'!

methodDictionary: aDictionary
    methodDictionary _ aDictionary
!

addSelector: selector withMethod: compiledMethod
    methodDictionary at: selector put: compiledMethod
!

removeSelector: selector
    methodDictionary removeKey: selector
!

compile: code
    (code isKindOf: PositionableStream)
    	ifTrue: [ code _ code contents ].
    (code isMemberOf: String)
    	ifFalse: [ code _ code asString ].
    self compileString: code
!

compile: code notifying: requestor
    self notYetImplemented
!

recompile: selector
    self compile: (self sourceCodeAt: selector)
!

decompile: selector
    | method source |
    method _ self compiledMethodAt: selector.
    source _ method methodSourceString.
    source isNil
    	ifTrue: [ ^self error: 'decompiler can''t decompile methods without source (yet)' ]
	ifFalse: [ ^source ]
!

edit: selector
    | method sourceFile sourcePos |
    method _ self compiledMethodAt: selector.
    sourceFile _ method methodSourceFile.
    sourceFile isNil
    	ifTrue: [ ^self error: 'decompiler can''t decompile methods without source (yet)' ].
    sourcePos _ method methodSourcePos.
    Smalltalk system: 'emacs -l st -smalltalk ', sourceFile, ' ', sourcePos printString
!

compileAll
    methodDictionary keysDo: [ :selector | self recompile: selector ]
!

compileAllSubclasses
    self allSubclassesDo: [ :subclass | subclass compileAll ]
!!



!Behavior methodsFor: 'creating a class hierarchy'!

superclass: aClass
    superClass _ aClass
!

addSubclass: aClass
    subClasses isNil ifTrue: [ subClasses _ Array new: 0 ].
    subClasses _ subClasses copyWithout: aClass. "remove old class if any"
    subClasses _ subClasses copyWith: aClass
!

removeSubclass: aClass
    subClasses _ subClasses copyWithout: aClass
!!



!Behavior methodsFor: 'accessing the methodDictionary'!

selectors
    methodDictionary isNil
    	ifTrue: [ ^Set new ]
	ifFalse: [ ^methodDictionary keys ]
!

allSelectors
    | aSet |
    aSet _ self selectors.
    self allSuperclassesDo:
    	[ :superclass | aSet addAll: superclass selectors ].
    ^aSet
!

compiledMethodAt: selector
    "Return the compiled method associated with selector, from the local
    method dictionary.  Error if not found."
    ^methodDictionary at: selector
!

sourceCodeAt: selector
    | method |
    method _ self compiledMethodAt: selector.
    ^method methodSourceString
!

sourceMethodAt: selector
    "This is too dependent on the original implementation"
    self shouldNotImplement
!!



!Behavior methodsFor: 'accessing instances and variables'!

allInstances
    "Returns a set of all instances of the receiver"
    | aSet |
    aSet _ Set new.
    self allInstancesDo: [ :anInstance | aSet add: anInstance ].
    ^aSet
!

instanceCount
    | count anInstance |
    count _ 0.
    anInstance _ self someInstance.
    [ anInstance notNil ]
    	whileTrue: [ count _ count + 1.
	    	     anInstance _ anInstance nextInstance ].    
    ^count
!
    
instVarNames
    self subclassResponsibility "### is this right?  Why is it here instead of
    	    	    	    	 in ClassDescription?"
!

subclassInstVarNames
    self subclassResponsibility
!

allInstVarNames
    self subclassResponsibility
!

classVarNames
    self subclassResponsibility 
!

allClassVarNames
    self subclassResponsibility
!

sharedPools
    self subclassResponsibility
!

allSharedPools
    self subclassResponsibility
!!



!Behavior methodsFor: 'accessing class hierarchy'!

subclasses
    subClasses isNil
	ifTrue: [ ^Set new ]
	ifFalse: [ ^subClasses asSet ]
!

allSubclasses
    | aSet |
    aSet _ Set new.
    self allSubclassesDo: [ :subclass | aSet addAll: subclass subclasses ].
    ^aSet
!

withAllSubclasses
    | aSet |
    aSet _ Set with: self.
    self allSubclassesDo: 
    	[ :subclass | aSet addAll: (subclass withAllSubclasses)	].
    ^aSet
!

superclass
    ^superClass
!

allSuperclasses
    | supers |
    supers _ OrderedCollection new.
    self allSuperclassesDo:
    	[ :superclass | supers addLast: superclass ].
    ^supers
!!



!Behavior methodsFor: 'testing the method dictionary'!

hasMethods
    ^methodDictionary notNil and: [ methodDictionary size ~= 0 ]
!

includesSelector: selector
    "Returns true if the local method dictionary"
    ^methodDictionary notNil and: [ methodDictionary includesKey: selector ]
!

canUnderstand: selector
    (self includesSelector: selector)
    	ifTrue: [ ^true ].
    self allSuperclassesDo:
    	[ :superclass | (superclass includesSelector: selector)
	    	    	    ifTrue: [ ^true ] ].
    ^false
!

whichClassIncludesSelector: selector
    self allSuperclassesDo:
    	[ :superclass | (superclass includesSelector: selector)
	    	    	    ifTrue: [ ^superclass ] ].
    ^nil
!

whichSelectorsAccess: instVarName
    self notYetImplemented
!

whichSelectorsReferTo: anObject
    self notYetImplemented
!

scopeHas: name ifTrue: aBlock
    self notYetImplemented
!!



!Behavior methodsFor: 'testing the form of the instances'!

isPointers
    "Due to our representation bit 30 is inverted, so we invert the sense
    of this test, and things work out fine."
    ^(self instanceSpec bitAt: 30) = 0
!

isBits
    ^self isPointers not
!

isBytes
    ^self isPointers not & self isWords not
!

isWords
    ^self isPointers not & ((self instanceSpec bitAt: 29) ~= 0)
!

isFixed
    ^self isVariable not
!

isVariable
    ^(self instanceSpec bitAt: 28) ~= 0
!

instSize
    ^self instanceSpec bitAnd: 16r0FFFFFFF
!!



!Behavior methodsFor: 'testing the class hierarchy'!

inheritsFrom: aClass
    "Returns true if aClass is a superclass of the receiver"
    | sc |
    sc _ self.
    [ sc _ sc superclass.
      sc isNil ]
    	whileFalse:
	    [ sc == aClass ifTrue: [ ^true ] ].
    ^false
!

kindOfSubclass
    self isVariable
    	ifTrue: [ self isBytes ifTrue: [ ^'variableByteSubclass: ' ].
	    	  self isPointers
		    ifTrue: [ ^'variableSubclass: ' ]
		    ifFalse: [ ^'variableWordSubclass: ' ] ]
	ifFalse: [ ^'subclass: ' ]
!!



!Behavior methodsFor: 'enumerating'!

allSubclassesDo: aBlock
    "### I hope this means all direct subclasses"
    subClasses notNil
	ifTrue: [ subClasses do: [ :subclass | aBlock value: subclass ] ]
!

allSuperclassesDo: aBlock
    | class superclass |
    class _ self.
    [ superclass _ class superclass.
      class _ superclass.
      superclass notNil ] whileTrue:
      	[ aBlock value: superclass ]
!

allInstancesDo: aBlock
    | anInstance |
    anInstance _ self someInstance.
    [ anInstance notNil ]
    	whileTrue: [ aBlock value: anInstance.
	    	     anInstance _ anInstance nextInstance ]
!

allSubinstancesDo: aBlock
    self allSubclassesDo:
    	[ :subclass | subclass allInstancesDo: aBlock ]
!

selectSubclasses: aBlock
    | aSet |
    aSet _ Set new.
    self allSubclassesDo: [ :subclass | (aBlock value: subclass)
    	    	    	    	    	    ifTrue: [ aSet add: subclass ] ].
    ^aSet
!

selectSuperclasses: aBlock
    | aSet |
    aSet _ Set new.
    self allSuperclassesDo: [ :superclass | (aBlock value: superclass)
    	    	    	    	    	    ifTrue: [ aSet add: superclass ] ].
    ^aSet
!!




!Behavior methodsFor: 'private'!

instanceSpec
    ^instanceSpec
!

setInstanceSpec: variableBoolean
  words: wordsBoolean
  pointers: pointersBoolean
  instVars: anIntegerSize
    instanceSpec _ 0.
    "Due to our representation bit 30 is inverted, so we invert the sense
    of this test, and things work out fine."
    pointersBoolean
    	ifFalse: [ instanceSpec _ instanceSpec bitOr: ( 1 bitShift: 30 ) ].
    wordsBoolean
        ifTrue: [ instanceSpec _ instanceSpec bitOr: ( 1 bitShift: 29 ) ].
    variableBoolean
        ifTrue: [ instanceSpec _ instanceSpec bitOr: ( 1 bitShift: 28 ) ].
    instanceSpec _ instanceSpec bitOr: (anIntegerSize bitAnd: 16r0FFFFFFF).
!!
