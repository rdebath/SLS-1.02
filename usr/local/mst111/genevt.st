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
Smalltalk at: #DataTypeSize put: Dictionary new.
Smalltalk at: #MapInfo put: Dictionary new!


DataTypeMap at: #card8 put: #ubyte.
DataTypeMap at: #bool put: #byte.
DataTypeMap at: #keycode put: #byte.
DataTypeMap at: #button put: #byte.
DataTypeMap at: #card16 put: #uword.
DataTypeMap at: #bitmask put: #uword.
DataTypeMap at: #int16 put: #word.
DataTypeMap at: #setOfKeyButMask put: #word.
DataTypeMap at: #card32 put: #ulong.
DataTypeMap at: #window put: #long.
DataTypeMap at: #drawable put: #long.
DataTypeMap at: #atom put: #long.
DataTypeMap at: #colormap put: #long.
DataTypeMap at: #timestamp put: #long.
DataTypeMap at: #keymap put: 'getUnpaddedString: 31'.


DataTypeSize at: #card8 put: 1.
DataTypeSize at: #bool put: 1.
DataTypeSize at: #keycode put: 1.
DataTypeSize at: #button put: 1.
DataTypeSize at: #card16 put: 2.
DataTypeSize at: #bitmask put: 2.
DataTypeSize at: #int16 put: 2.
DataTypeSize at: #setOfKeyButMask put: 2.
DataTypeSize at: #card32 put: 4.
DataTypeSize at: #window put: 4.
DataTypeSize at: #drawable put: 4.
DataTypeSize at: #atom put: 4.
DataTypeSize at: #colormap put: 4.
DataTypeSize at: #timestamp put: 4.
DataTypeSize at: #keymap put: 31.


MapInfo at: #window put: 'self mappedId: '.
MapInfo at: #drawable put: 'self mappedId: '.
MapInfo at: #atom put: 'self mappedId: '.
MapInfo at: #colormap put: 'self mappedId: '
!


Object subclass: #XEvent
       instanceVariableNames: 'sequenceNumber'
       classVariableNames: 'EventMap'
       poolDictionaries: ''
       category: 'X hacking'
!

!Object class methodsFor: 'hacking'!

genErrorInit
    | stream |
    stream _ FileStream open: 'Xerr.st' mode: 'w'.
    stream close.
!

genEventInit
    | stream |
    stream _ FileStream open: 'Xevt.st' mode: 'w'.
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

genEventClass: className args: anArray
    | stream |
    stream _ FileStream open: 'Xevt.st' mode: 'a'.
    self genEventClassDef: className on: stream args: anArray.
    self genEventClassMethods: className on: stream args: anArray.
    stream nextPut: Character newPage.
    stream nl.
    stream close
!

" private definitions "

genErrorClassDef: className on: aStream args: anArray
    self genClassDef: 'Error' forClass: className on: aStream args: anArray
!

genEventClassDef: className on: aStream args: anArray
    self genClassDef: 'Event' forClass: className on: aStream args: anArray
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

genEventClassMethods: className on: aStream args: anArray
    self genClassMethods: 'Event' forClass: className on: aStream args: anArray
!

genClassMethods: type forClass: className on: aStream args: anArray
    | totalBytes dataType | 
    '!', className, type, ' class methodsFor: ''instance creation''!

newFrom: aStream
    ^self new initFrom: aStream
!!

' printOn: aStream.
    self genAccessorMethods: type forClass: className on: aStream args: anArray.
    '!', className, type, ' methodsFor: ''private''!

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
    (31 - totalBytes) printOn: aStream.
    aStream nl.
    '!!' printOn: aStream.
    aStream nl.
    aStream nl.
!

genAccessorMethods: type forClass: className on: aStream args: anArray
    | name varType |
    '!', className, type, ' methodsFor: ''accessing''!

' printOn: aStream.
    anArray do: 
	[ :var | var size > 1
		     ifTrue: [ name _ (var at: 1).
			       varType _ (var at: var size).
			       name printOn: aStream.
			       aStream nl.
			       '    ^' printOn: aStream.
    	    	    	       (MapInfo includesKey: varType)
				   ifTrue: [ (MapInfo at: varType), ' ',
						 name printOn: aStream]
				   ifFalse: [ name printOn: aStream ].
			       aStream nl.
			       '!' printOn: aStream.
			       aStream nl.
			       aStream nl ].
    	].
    '!' printOn: aStream.
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
		     ifTrue: [ (#(sequenceNumber)
				   indexOf: (varName _ var at: 1)) = 0
				   ifTrue: [ varName, ' ' printOn: aStream ] ]
		     ].
    '''' printOn: aStream.
    aStream nl
!!

Object genEventInit.
Object genEventClass: 'KeyPress' 
    args: #((detail keycode) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (sameScreen bool)).
Object genEventClass: 'KeyRelease' 
    args: #((detail keycode) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (sameScreen bool)).
Object genEventClass: 'ButtonPress' 
    args: #((detail button) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (sameScreen bool)).
Object genEventClass: 'ButtonRelease' 
    args: #((detail button) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (sameScreen bool)).
Object genEventClass: 'MotionNotify' 
    args: #((detail card8) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (sameScreen bool)).
Object genEventClass: 'EnterNotify' 
    args: #((detail card8) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (mode card8) (focus card8)).
Object genEventClass: 'LeaveNotify' 
    args: #((detail card8) (sequenceNumber card16) (time timestamp) 
		(root window) (event window) (child window) 
		(rootX int16) (rootY int16) (eventX int16) (eventY int16)
		(state setOfKeyButMask) (mode card8) (focus card8)).
Object genEventClass: 'FocusIn' 
    args: #((detail card8) (sequenceNumber card16) (event window) 
		(mode card8)).
Object genEventClass: 'FocusOut' 
    args: #((detail card8) (sequenceNumber card16) (event window) 
		(mode card8)).

Object genEventClass: 'KeymapNotify' 
    args: #((keys keymap)).

Object genEventClass: 'Expose' 
    args: #((card8) (sequenceNumber card16) (window window) 
		(x card16) (y card16) (width card16) (height card16)
		(count card16)).

Object genEventClass: 'GraphicsExposure' 
    args: #((card8) (sequenceNumber card16) (drawable drawable) 
		(x card16) (y card16) (width card16) (height card16)
		(minorOp card16) (count card16) (majorOp card8)).
Object genEventClass: 'NoExposure' 
    args: #((card8) (sequenceNumber card16) (drawable drawable) 
		(minorOp card16) (majorOp card8)).
Object genEventClass: 'VisibilityNotify' 
    args: #((card8) (sequenceNumber card16) (window window) 
		(state card8)).
Object genEventClass: 'CreateNotify' 
    args: #((card8) (sequenceNumber card16) (parent window) (window window)
		(x int16) (y int16) (width card16) (height card16)
		(borderWidth card16) (overrideRedirect bool)).
Object genEventClass: 'DestroyNotify' 
    args: #((card8) (sequenceNumber card16) (event window) (window window)).
Object genEventClass: 'UnmapNotify' 
    args: #((card8) (sequenceNumber card16) (event window) (window window)
		(fromConfigure bool)).
Object genEventClass: 'MapNotify' 
    args: #((card8) (sequenceNumber card16) (event window) (window window)
		(overrideRedirect bool)).
Object genEventClass: 'MapRequest' 
    args: #((card8) (sequenceNumber card16) (parent window) (window window)).
Object genEventClass: 'ReparentNotify' 
    args: #((card8) (sequenceNumber card16) 
		(event window) (window window) (parent window)
		(x int16) (y int16) (overrideRedirect bool)).
Object genEventClass: 'ConfigureNotify' 
    args: #((card8) (sequenceNumber card16) 
		(event window) (window window) (aboveSibling window)
		(x int16) (y int16) (width card16) (height card16)
		(borderWidth card16) (overrideRedirect bool)).
Object genEventClass: 'ConfigureRequest' 
    args: #((stackMode card8) (sequenceNumber card16) 
		(parent window) (window window) (sibling window)
		(x int16) (y int16) (width card16) (height card16)
		(borderWidth card16) (valueMask bitmask)).
Object genEventClass: 'GravityNotify' 
    args: #((card8) (sequenceNumber card16) 
		(event window) (window window)
		(x int16) (y int16)).
Object genEventClass: 'ResizeRequest' 
    args: #((card8) (sequenceNumber card16) 
		(window window)
		(width card16) (height card16)).
Object genEventClass: 'CirculateNotify' 
    args: #((card8) (sequenceNumber card16) 
		(event window) (window window) (window)
		(place card8)).
Object genEventClass: 'CirculateRequest' 
    args: #((card8) (sequenceNumber card16) 
		(parent window) (window window) (card32)
		(place card8)).
Object genEventClass: 'PropertyNotify' 
    args: #((card8) (sequenceNumber card16) 
		(window window) (atom atom)
		(time timestamp) (state card8))
!

Object genEventClass: 'SelectionClear' 
    args: #((card8) (sequenceNumber card16) 
		(time timestamp) (owner window) (selection atom)).
Object genEventClass: 'SelectionRequest' 
    args: #((card8) (sequenceNumber card16) 
		(time timestamp) (owner window) (requestor window)
		(selection atom) (target atom) (property atom)).
Object genEventClass: 'SelectionNotify' 
    args: #((card8) (sequenceNumber card16) 
		(time timestamp) (requestor window)
		(selection atom) (target atom) (property atom)).
Object genEventClass: 'ColormapNotify' 
    args: #((card8) (sequenceNumber card16) 
		(window window) (colormap colormap)
		(new bool) (state card8)).
Object genEventClass: 'ClientMessage' 
    args: #((format card8) (sequenceNumber card16) 
		(window window) (type atom)
		"says: 20 data").
Object genEventClass: 'MappingNotify' 
    args: #((card8) (sequenceNumber card16) 
		(request card8) (keycode keycode)
		(count card8)).
!

Smalltalk quitPrimitive!
