"Provide some example interface to SunView.  See the comment in win.c
for details."

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
| GNU Smalltalk; see the file LICENSE.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"



"| pairs |
    pairs _ #((Frame	    	'FRAME')
	      (Panel	    	'PANEL')
	      (PanelMessage	'PANEL_MESSAGE')
	      (PanelButton	'PANEL_BUTTON')
	      (PanelChoice	'PANEL_CHOICE')
	      (PanelSlider	'PANEL_SLIDER')
	      (PanelText	'PANEL_TEXT')
	      (PanelToggle	'PANEL_TOGGLE')
	      (PanelLine	'PANEL_LINE')
	     ).
    pairs do:
        [ :aPair | SuntoolObjectTypes at: (aPair at: 1)
			   	      put: (Smalltalk lookupCAddr: (aPair at: 2)) ]!"

Object subclass: #Suntools
    instanceVariableNames: 'stObject'
    classVariableNames: ''
    poolDictionaries: 'SuntoolObjectTypes WindowAttrs'
    category: 'Cool hacking'!

Suntools subclass: #Window
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Cool hacking'.

Suntools subclass: #Frame
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'FrameAttrs' "Maybe WindowAttrs too?"
    category: 'Cool hacking'.

Suntools subclass: #Panel
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'PanelAttrs'
    category: 'Cool hacking'.

Suntools subclass: #PanelItem
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'PanelAttrs'
    category: 'Cool hacking'.

Suntools subclass: #Textsw
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'TextswAttrs'
    category: 'Cool hacking'.

Suntools subclass: #Canvas
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'CanvasAttrs'
    category: 'Cool hacking'.

Suntools subclass: #Develop
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'CanvasAttrs TextswAttrs PanelAttrs FrameAttrs SuntoolObjectTypes WindowAttrs'
    category: 'Cool hacking'!


'about to create a window' printNl!

Behavior defineCFunc: 'windowCreate'
         withSelectorArgs: 'windowCreate: parent type: aType attrs: anArray'
	 forClass: Suntools class
	 returning: #cObject
	 args: #(cObject cObject variadic)
!

!Suntools class methodsFor: 'instance creation'!

createWindowObject: parent type: objectType attrs: anArray
    | parentObject |
    parentObject _ parent isNil ifTrue:  [ nil ]
	    	    	    	ifFalse: [ parent stObject ].
    ^self newObject: (self windowCreate: parentObject type: objectType attrs: anArray)
!

newObject: suntoolsObject
    ^(self new) stObject: suntoolsObject
!!


!Suntools methodsFor: 'private'!

stObject
    ^stObject
!

stObject: suntoolsObject
    stObject _ suntoolsObject
!!


Behavior defineCFunc: 'windowLoop'
    	 withSelectorArgs: 'windowLoop: frame'
    	 forClass: Frame
	 returning: #void
	 args: #(cObject)
!

!Frame class methodsFor: 'instance creation'!

create: parent attributes: anArray
    ^self createWindowObject: parent type: FrameType attrs: anArray
!!

!Frame methodsFor: 'processing'!

windowMainLoop
    self windowLoop: stObject
!!

!Panel class methodsFor: 'instance creation'!

create: parent attributes: anArray
    ^self createWindowObject: parent type: PanelType attrs: anArray
!!

Behavior defineCFunc: 'panelCreateItem'
         withSelectorArgs: 'panelCreateItem: panel type: aType attrs: anArray'
	 forClass: Panel
	 returning: #cObject
	 args: #(cObject cObject variadic)!

!Panel methodsFor: 'processing'!

createItem: type attributes: anArray
    "not right"
    ^self panelCreateItem: stObject type: type attrs: anArray
!!    

!Develop class methodsFor: 'test'!

test
| frame aStream panel |
    aStream _ WriteStream on: (Array new: 0).
    aStream nextPut: frameLabel.  aStream nextPut: 'Test window'.
    aStream nextPut: 0.
    frame _ Frame create: nil attributes: aStream contents.
    aStream _ WriteStream on: (Array new: 3).
    aStream nextPut: panelLayout.
    aStream nextPut: (Smalltalk lookupCAddr: 'PANEL_VERTICAL').
    aStream nextPut: 0.
    panel _ Panel create: frame attributes: aStream contents.
    

    aStream _ WriteStream on: (Array new: 0).
    aStream nextPut: panelLabelString.
    aStream nextPut: 'Rambo'.
    aStream nextPut: panelChoiceStrings.
        aStream nextPut: 'foo'.
        aStream nextPut: 'bar'.
        aStream nextPut: 'baz'.
	aStream nextPut: 0.
    aStream nextPut: 0.
    panel createItem: PanelChoiceType attributes: aStream contents.    

    aStream _ WriteStream on: (Array new: 0).
    aStream nextPut: panelLabelString.    aStream nextPut: 'Brightness: '.
    aStream nextPut: panelValue.    	  aStream nextPut: 75.
    aStream nextPut: panelMinValue. 	  aStream nextPut: 0. 
    aStream nextPut: panelMaxValue. 	  aStream nextPut: 100.
    aStream nextPut: panelSliderWidth.	  aStream nextPut: 300.
    panel createItem: PanelSliderType attributes: aStream contents.
    frame windowMainLoop
!!

Develop test!


