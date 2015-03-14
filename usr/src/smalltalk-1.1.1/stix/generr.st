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
| sbyrne     24 May 90	  created.
|
"

Smalltalk at: #DataTypeMap put: Dictionary new.
Smalltalk at: #DataTypeSize put: Dictionary new!


DataTypeMap at: #card8 put: #ubyte.
DataTypeMap at: #bool put: #byte.
DataTypeMap at: #keycode put: #byte.
DataTypeMap at: #card16 put: #uword.
DataTypeMap at: #int16 put: #word.
DataTypeMap at: #setOfKeyButMask put: #word.
DataTypeMap at: #card32 put: #ulong.
DataTypeMap at: #window put: #long.
DataTypeMap at: #drawable put: #long.
DataTypeMap at: #atom put: #long.
DataTypeMap at: #colormap put: #long.
DataTypeMap at: #timestamp put: #long.



DataTypeSize at: #card8 put: 1.
DataTypeSize at: #bool put: 1.
DataTypeSize at: #keycode put: 1.
DataTypeSize at: #card16 put: 2.
DataTypeSize at: #int16 put: 2.
DataTypeSize at: #setOfKeyButMask put: 2.
DataTypeSize at: #card32 put: 4.
DataTypeSize at: #window put: 4.
DataTypeSize at: #drawable put: 4.
DataTypeSize at: #atom put: 4.
DataTypeSize at: #colormap put: 4.
DataTypeSize at: #timestamp put: 4
!

!Object class methodsFor: 'hacking'!

genErrorInit
    | stream |
    stream _ FileStream open: 'Xerr.st' mode: 'w'.
    stream close.
!

genErrorClass: className args: anArray
    | stream |
    stream _ FileStream open: 'Xerr.st' mode: 'a'.
    self genErrorClassDef: className on: stream args: anArray.
    self genErrorClassMethods: className on: stream args: anArray.
    stream nextPut: Character newPage.
    stream nl.
    stream close
!

" private definitions "

genErrorClassDef: className on: aStream args: anArray
    self genClassDef: 'Error' forClass: className on: aStream args: anArray
!

genClassDef: type forClass: className on: aStream args: anArray
    'X', type, ' subclass: #' printOn: aStream.
    className, type printOn: aStream.
    aStream nl.
    self genClassInstVars: anArray on: aStream.
'	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''X hacking''
!' printOn: aStream.
    aStream nl.
    aStream nl
!

genErrorClassMethods: className on: aStream args: anArray
    self genClassMethods: 'Error' forClass: className on: aStream args: anArray
!

genClassMethods: type forClass: className on: aStream args: anArray
    | totalBytes dataType | 
    '!', className, type, ' class methodsFor: ''instance creation''!

newFrom: aStream
    ^self new initFrom: aStream
!!

' printOn: aStream.
    '!', className, type ' methodsFor: ''private''!

' printOn: aStream.
'initFrom: aStream' printOn: aStream.  aStream nl.
    totalBytes _ 0.
    anArray do:
	[ :var | '    ' printOn: aStream.
		 var size > 1
		     ifTrue: [ (var at: 1) printOn: aStream.
			       ' _ ' printOn: aStream ].
		 'aStream ' printOn: aStream.
		 dataType _ (var at: var size).
    	    	 (self mapType: dataType) printOn: aStream.
		 totalBytes _ totalBytes + (self typeSize: dataType).
		 '. ' printOn: aStream. aStream nl ].
    '    aStream skipBytes: ' printOn: aStream.
    (30 - totalBytes) printOn: aStream.
    aStream nl.
    '!!' printOn: aStream.
    aStream nl.
    aStream nl.
!

mapType: aType
    ^DataTypeMap at: aType
		 ifAbsent: [ self error: 'no such type: ', (aType printString) ]
!

typeSize: aType
    ^DataTypeSize at: aType
    		 ifAbsent: [ self error: 'no such type: ', (aType printString) ]
!	

genClassInstVars: anArray on: aStream
    | varName |
    aStream tab.
    'instanceVariableNames: ''' printOn: aStream.
    anArray do: 
	[ :var | var size = 2
		     ifTrue: [ (#(sequenceNumber minorOp majorOp)
				   indexOf: (varName _ var at: 1)) = 0
				   ifTrue: [ varName, ' ' printOn: aStream ] ]
		     ].
    '''' printOn: aStream.
    aStream nl
!!

Object genErrorInit.
Object genErrorClass: 'Request' 
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Value'
    args: #((sequenceNumber card16) (badValue card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Window' 
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Pixmap'
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Atom'
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Cursor'
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Font' 
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Match'
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Drawable'
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Access' 
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Alloc'
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Colormap' 
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'GContext' 
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'IDChoice'
    args: #((sequenceNumber card16) (badId card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Name'
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Length'
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8)).
Object genErrorClass: 'Implementation'
    args: #((sequenceNumber card16) (card32) (minorOp card16) (majorOp card8))
!

Smalltalk quitPrimitive!
