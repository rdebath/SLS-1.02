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

Object subclass: #XPacket
       instanceVariableNames: 'stream pos values'
       classVariableNames: 'EventNames PointerEventNames DeviceEventNames
                            KeyButMaskNames KeyMaskNames WinGravityNames
    	    	    	    BitGravityNames'
       poolDictionaries: ''
       category: 'X hacking'
!

XPacket subclass: #XWindowAttrPacket
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X window attributes'
!

XPacket subclass: #XConfigPacket
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X window configuration'
!

XPacket subclass: #XGCAttrPacket
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X graphic context attributes'
!


Smalltalk at: #ShowPacket put: false!


!XPacket class methodsFor: 'instance creation'!

command: aByte
    ^self command: aByte aux: 0
!

command: aByte aux: aByte2
    ^(self new) init: aByte aux: aByte2
!

initialize
    EventNames _ #(KeyPress KeyRelease ButtonPress ButtonRelease EnterWindow
		       LeaveWindow PointerMotion PointerMotionHint
		       Button1Motion Button2Motion Button3Motion Button4Motion
		       Button5Motion ButtonMotion KeymapState Exposure
		       VisibilityChange StructureNotify ResizeRedirect
		       SubstructureNotify SubstructureRedirect FocusChange
		       PropertyChange ColormapChange OwnerGrabButton).
    PointerEventNames _ EventNames copy.
    self remove: #(KeyPress KeyRelease Exposure
		       VisibilityChange StructureNotify ResizeRedirect
		       SubstructureNotify SubstructureRedirect FocusChange
		       PropertyChange ColormapChange OwnerGrabButton)
	 from: PointerEventNames.
    DeviceEventNames _ EventNames copy.
    self remove: #(KeyPress KeyRelease Exposure
		       VisibilityChange StructureNotify ResizeRedirect
		       SubstructureNotify SubstructureRedirect FocusChange
		       PropertyChange ColormapChange OwnerGrabButton)
	 from: PointerEventNames.
    DeviceEventNames _ EventNames copy.
    self remove: #(EnterWindow LeaveWindow PointerMotionHint
		       KeymapState Exposure
		       VisibilityChange StructureNotify ResizeRedirect
		       SubstructureNotify SubstructureRedirect FocusChange
		       PropertyChange ColormapChange OwnerGrabButton)
	 from: DeviceEventNames.

    KeyMaskNames _ #(Shift Lock Control Mod1 Mod2 Mod3 Mod4 Mod5).
    KeyButMaskNames _ KeyMaskNames, #(Button1 Button2 Button3 Button4 Button5).

    BitGravityNames _ #(Forget NorthWest North NorthEast
			    West Center East SouthWest
			    South SouthEast Static).
    WinGravityNames _ BitGravityNames copy.
    WinGravityNames at: 1 put: #Unmap.
!

remove: anArray from: eventNames
    1 to: eventNames size do:
	[ :i | (anArray includes: (eventNames at: i))
		   ifTrue: [ eventNames at: i put: nil ] ]
!!

XPacket initialize!		"Temp: make sure to do this"


!XPacket methodsFor: 'accessing'!

done
    ^self done: false
!

doneWord
    ^self done: true
!

done: isWord
    | length |
    self pad.
    values notNil
	ifTrue: [ self emitBitValues: isWord ].
    length _ stream position // 4.
    stream position: 3.
    self word: length.
    ShowPacket ifTrue:
	[ stream contents printNl ].
    ^stream contents    
!

emitBitValues: isWord
    | bitMask |
    bitMask _ 0.
    values inject: 1 into: 
	[ :bit :value | value notNil
			    ifTrue: [ bitMask _ bitMask bitOr: bit ].
			bit * 2 ].
    isWord
	ifTrue: [ self uword: bitMask ]
	ifFalse: [ self ulong: bitMask ].
    values do: [ :value | value notNil
			      ifTrue: [ self long: value ] ]
!

byte: aByte
    stream nextPut: aByte
!

bool: aBool
    aBool			"### May extend this to support #True & #False"
	ifTrue: [ stream nextPut: 1 ]
	ifFasle:[ stream nextPut: 0 ]
!

word: aWord
    Bigendian 
	ifTrue: [ stream nextPut: aWord // 256. 
		  stream nextPut: (aWord bitAnd: 255) ]
	ifFalse: [ stream nextPut: (aWord bitAnd: 255).
		   stream nextPut: aWord // 256 ]
!

uword: aWord
    self word: aWord		"hack for now"
!

long: aLong
    Bigendian 
	ifTrue: [ self word: aLong // 65536. 
		  self word: (aLong bitAnd: 65535) ]
	ifFalse: [ self word: (aLong bitAnd: 65535).
		   self word: aLong // 65536 ]
!

ulong: aLong
    self long: aLong		"hack for now"
!

bytes: byteArray
    stream nextPutAll: byteArray
!


point: aPoint
    self word: aPoint x.
    self word: aPoint y
!

rectangle: aRectangle
    self point: aRectangle origin.
    self point: aRectangle corner
!

arc: anArc
    anArc emitTo: self
!

pad
    | curPos numBytes |
    curPos _ stream position.
    numBytes _ 4 - (curPos - 1) bitAnd: 3.

    stream next: numBytes put: 0 
!!

!XWindowAttrPacket methodsFor: 'variable arity requests'!


backgroundPixmap: aValue
    self at: 1 ulong: (X maybeMap: aValue into: #(None ParentRelative))
!

backgroundPixel: aValue
    self at: 2 ulong: aValue
!

borderPixmap: aValue
    self at: 3 ulong: (X maybeMap: aValue into: #(CopyFromParent))
!

borderPixel: aValue
    self at: 4 ulong: aValue
!

bitGravity: aValue
    self at: 5
    self byte: (X map: aValue into: BitGravityNames)
!

winGravity: aValue
    self at: 6 byte: (X map: aValue into: WinGravityNames) 
!

backingStore: aValue
    self at: 7 byte: (X map: aValue into: #(NotUseful WhenMapped Always))
!

backingPlanes: aValue
    self at: 8 ulong: aValue
!

backingPixel: aValue
    self at: 9 ulong: aValue
!

overrideRedirect: aValue
    self at: 10 bool: aValue
!

saveUnder: aValue
    self at: 11 bool: aValue
!

eventMask: aValue
    self at: 12 ulong: (self makeBitmask: aValue using: EventNames)
!    


doNotPropagateMask: aValue
    self at: 13 ulong: (self makeBitmask: aValue using: DeviceEventNames)
!

colormap: aValue
    self at: 14 ulong: (X maybeMap: aValue into: #(CopyFromParent))
!

cursor: aValue
    self at: 15 ulong: (X maybeMap: aValue into: #(None))
!!


!XConfigPacket methodsFor: 'variable arity request'!

x: aValue
    self at: 1 word: aValue
!

y: aValue
    self at: 2 word: aValue
!

width: aValue
    self at: 3 uword: aValue
!

height: aValue
    self at: 4 uword: aValue
!

borderWidth: aValue
    self at: 5 uword: aValue
!

sibling: aValue
    self at: 6 ulong: aValue id
!

stackMode: aValue
    self at: 7 byte: (X map: aValue into: #(Above Below TopIf BottomIf Opposite))
!!


!XGCAttrPacket methodsFor: 'variable arity requests'!

"GC related requests"

function: aValue
    self at: 1 
	 byte: (X map: aValue
		  into: #(Clear And AndReverse Copy AndInverted
			      NoOp Xor Or Nor Equiv Invert
			      OrReverse CopyInverted OrInverted
			      Nand Set))
!
    
planeMask: aValue
    self at: 2 ulong: aValue
!

foreground: aValue
    self at: 3 ulong: aValue
!

background: aValue
    self at: 4 ulong: aValue
!

lineWidth: aValue
    self at: 5 uword: aValue
!

lineStyle: aValue
    self at: 6 byte: (X map: aValue into: #(Solid OnOffDash DoubleDash))
!

capStyle: aValue
    self at: 7 byte: (X map: aValue into: #(NotLast Butt Round Projecting))
!

joinStyle: aValue
    self at: 8 byte: (X map: aValue into: #(Miter Round Bevel))
!

fillStyle: aValue
    self at: 9 byte: (X map: aValue into: #(Solid Tiled Stippled OpaqueStippled))
!

fillRule: aValue
    self at: 10 byte: (X map: aValue into: #(NotLast Butt Round Projecting))
!

tile: aValue
    self at: 11 ulong: aValue id
!

stipple: aValue
    self at: 12 ulong: aValue id
!

tileStippleXOrigin: aValue
    self at: 13 word: aValue
!

tileStippleYOrigin: aValue
    self at: 14 word: aValue
!

font: aValue
    self at: 15 ulong: aValue id
!

subwindowMode: aValue
    self at: 16 byte: (X map: aValue into: #(ClipByChildren IncludeInferiors))
!

graphicsExposures: aValue
    self at: 17 bool: aValue
!

clipXOrigin: aValue
    self at: 18 word: aValue
!

clipYOrigin: aValue
    self at: 19 word: aValue
!


clipMask: aValue
    self at: 20 ulong: (X maybeMap: aValue into: #(None))
!

dashOffset: aValue
    self at: 21 uword: aValue
!

dashes: aValue
    self at: 22 ubyte: aValue
!

arcMode: aValue
    self at: 23 byte: (X map: aValue into: #(Chord PieSlice))
!!


!XPacket methodsFor: 'variable arity support'!


at: index byte: aValue
    ^self at: index long: aValue
!

at: index bool: aValue
    ^self at: index 
	  long: [ aValue ifTrue: [ 1 ] ifFalse: [ 0 ] ]
!

at: index ubyte: aValue
    ^self at: index long: aValue
!

at: index word: aValue
    ^self at: index long: aValue
!

at: index uword: aValue
    ^self at: index long: aValue
!

at: index ulong: aValue
    ^self at: index long: aValue
!

at: index long: aValue
    values isNil ifTrue: [ values _ Array new: 32 ]. "large enough"
    values at: index put: aValue
!


noBits
    values _ Array new: 32
!

makeBitmask: symArray using: nameArray
    | bits | 
    bits _ 0.
    symArray do: [ :sym | bits _ bits bitOr: 
			      (1 bitShift: (X map: sym into: nameArray)) ].
    ^bits
! !

    

!XPacket methodsFor: 'writing'!

textItem: aTextItem
    aTextItem emitTo: self
!!



!XPacket methodsFor: 'private'!

init: aByte aux: aByte2
    stream _ ReadWriteStream on: (ByteArray new: 0).
    self byte: aByte.
    self byte: aByte2.
    self word: 0		"placeholder for length"
!!

