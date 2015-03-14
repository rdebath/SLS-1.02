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

Drawable subclass: #Window
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!
    

!Window class methodsFor: 'instance creation'!
    
new: aDisplay
    ^self new init: aDisplay id: aDisplay nextId
!

new: aDisplay id: anId
    ^self new init: aDisplay id: anId
	
!!


!Window methodsFor: 'X protocol'!

createWindow: depth x: x y: y width: width height: height
    borderWidth: borderInteger class: aWindowClass visual: aVisual
    attrs: aBlock
    | packet win |
    win _ Window new: display.

    packet _ XWindowAttrPacket command: 1 aux: depth.
    packet long: win id; long: self id; word: x; word: y.
    packet uword: width; uword: height; uword: borderInteger.
    packet word: (X map: aWindowClass into: #(CopyFromParent InputOutput InputOnly)).
    packet long: (X maybeMap: aVisual into: #(CopyFromParent)).

    aBlock notNil
	ifTrue: [ aBlock value: packet ]
	ifFalse: [ packet noBits ].

    display socket bytes: packet done.
    ^win	
!

changeWindowAttributes: aBlock
    | packet |

    packet _ XWindowAttrPacket command: 2.
    packet long: self id.
    aBlock notNil
	ifTrue: [ aBlock value: packet ]
	ifFalse: [ packet noBits ].
    display socket bytes: packet done
!

getWindowAttributes
    | packet |

    packet _ XPacket command: 3.
    packet long: self id.
    display socket bytes: packet done.
    
    ^packet getResultAuxByte: #(backingStore (NotUseful WhenMapped Always))
	using: #((visual visualId)
		     (class (nil InputOutput InputOnly) word)
		     (bitGravity bitGravity)
		     (winGravity winGravity)
		     (backingPlanes card32)
		     (backingPixel card32)
		     (saveUnder bool)
		     (mapIsInstalled bool)
		     (mapState (Unmapped Unviewable Viewable) byte)
		     (overrideRedirect bool)
		     (colormap colormap (None))
		     (allEventMasks bitMask EventNames)
		     (yourEventMask bitMask EventNames)
		     (doNotPropagateMask bitMask DeviceEventNames))
!

destroyWindow
    | packet |
    packet _ XPacket command: 4.
    display socket bytes: (packet long: self id; done)
!

destroySubwindows
    | packet |
    packet _ XPacket command: 5.
    display socket bytes: (packet long: self id; done)
!

changeSaveSet: mode
    | packet |
    packet _ XPacket command: 6 aux: (X map: mode into: #(Insert Delete)).
    display socket bytes: (packet long: self id; done)
!    

reparentWindow: parentWindow x: x y: y
    | packet |
    packet _ XPacket command: 7.
    display socket bytes: (packet long: self id; long: parentWindow id; 
				  word: x; word: y; done)
!

mapWindow
    | packet |
    packet _ XPacket command: 8.
    display socket bytes: (packet long: self id; done)
!

mapSubwindows
    | packet |
    packet _ XPacket command: 9.
    display socket bytes: (packet long: self id; done)
!

unmapWindow
    | packet |
    packet _ XPacket command: 10.
    display socket bytes: (packet long: self id; done)
!

unmapSubwindows
    | packet |
    packet _ XPacket command: 11.
    display socket bytes: (packet long: self id; done)

!

configureWindow: aBlock
    | packet |
    packet _ XConfigPacket command: 12.
    
    aBlock notNil
	ifTrue: [ aBlock value: packet ]
	ifFalse: [ packet noBits ].

    display socket bytes: packet doneWord
    
!

circulateWindow: direction
    | packet |
    packet _ XPacket command: 13 
		     aux: (X map: direction
			     into: #(RaiseLowest LowerHighest)).
    display socket bytes: (packet long: self id; done)
!

queryTree
    | packet result numWins wins s |
    packet _ XPacket command: 15. 
    display socket bytes: (packet long: self id; done).

    s _ display socket.
    s getReply
	ifFalse: [ ^self ].
    
    result _ Dictionary new.
    result at: #root put: s mappedId.
    result at: #parent put: (s maybeMappedId: #(None)).
    numWins _ s word.
    s skipBytes: 14.	"there should be a better way"
    wins _ Array new: numWins.
    1 to: numWins do:
	[ :i | wins at: i put: (s mappedId) ].
    result at: #children put: wins.
    ^result
!

"not really here ""
internAtom: aString ifExists: aFlag
    | packet |
    packet _ XPacket command: 16 
	aux: (aFlag ifTrue: [ 1 ] ifFalse: [ 0 ]).
    display socket bytes: (packet string: aString; done).
    ^self notYetImplemented
""needs to return some information"

changeProperty: propertyAtom type: anAtom mode: aMode format: formatByte data: data
    | packet |
    "### Not completely done yet: doesn't handle non-8 bit data"
    packet _ XPacket command: 18
		     aux: (X map: aMode
			     into: #(Replace Prepend Append)).
    packet long: self id; long: propertyAtom mapToId; long: anAtom mapToId.
    packet byte: formatByte; pad; long: (data size); bytes: data.
    display socket bytes: (packet done)
!

deleteProperty: propertyAtom
    | packet |
    packet _ XPacket command: 19.
    display socket bytes: (packet long: self id; long: propertyAtom mapToId; done)
!!

!Window methodsFor: 'private'!

init: aDisplay id: anId
    display _ aDisplay.
    id _ anId.
    Registry at: id put: self
!!


