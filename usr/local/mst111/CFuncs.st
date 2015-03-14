"======================================================================
|
|   SystemDictionary Method Definitions
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
| sbyrne      8 Jul 89	  Created.
|
"

Object variableSubclass: #CFunctionDescriptor
       instanceVariableNames: 'cFunction cFunctionName returnType numFixedArgs'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil !

CFunctionDescriptor comment: 
'I am not part of the Smalltalk definition.  My instances contain information
about C functions that can be called from within Smalltalk, such as number
and type of parameters.  This information is used by the C callout mechanism
to perform the actual call-out to C routines.' !


"A couple of simple, but useful callout functions, as examples."

Behavior defineCFunc: 'system'
         withSelectorArgs: 'system: aString'
	 forClass: SystemDictionary
	 returning: #int
	 args: #(string)!

Behavior defineCFunc: 'getenv'
         withSelectorArgs: 'getenv: aString'
	 forClass: SystemDictionary
	 returning: #string
	 args: #(string)!


Smalltalk at: #XGlobals put: (Dictionary new)!
XGlobals at: #Registry put: (Dictionary new)!

FileStream fileIn: 'X.st'.
FileStream fileIn: 'XPacket.st'!



Object subclass: #Display
       instanceVariableNames: 'socket majorVersion minorVersion releaseNum resourceIdBase
    	    	    	       resourceIdMask motionBufferSize maxRequestLen imageByteOrder
    	    	    	       bitmapBitOrder bitmapFormatScanUnit bitmapFormatScanPad 
                               minKeycode maxKeycode vendorName formats screens idCounter'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!

Object subclass: #Format
       instanceVariableNames: 'depth bitsPerPixel scanlinePad'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!

Object subclass: #Screen
       instanceVariableNames: 'window defaultColormap whitePixel blackPixel
    	    	    	       currentInputMasks widthPixels heightPixels
                               widthMM heightMM minInstalledMaps maxInstalledMaps
                               rootVisual backingStores saveUnders rootDepth
    	    	    	       depths'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!

Object subclass: #Depth
       instanceVariableNames: 'depth visuals'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!

Object subclass: #VisualType
       instanceVariableNames: 'visualId class bitsPerRGB colormapEntries 
                               redMask greenMask blueMask'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!



!Display class methodsFor: 'instance creation'!

host: hostName display: anInteger
    | xStream result |
    xStream _ X connectTo: hostName display: anInteger.

    xStream char: (Bigendian ifTrue: [ $B ] ifFalse: [ $l ]).
    xStream byte: 0.
    
    xStream word: 11.
    xStream word: 0.
    xStream word: 0.		"length of auth string"
    xStream word: 0.		"length of auth data"
    xStream word: 0.		"unused"
    
    xStream byte == 1		"succeeded?"
	ifTrue: [ xStream byte.	"unused here "
		  ^self new: xStream ]
	ifFalse: [ ^nil ]	"maybe issue an error?"
!

new: fromStream
    ^self new init: fromStream
!!


!Display methodsFor: 'accessing'!

socket
    ^socket
!

nextId
    | id |
    id _ resourceIdBase bitOr: (idCounter bitAnd:  resourceIdMask).
    idCounter _ idCounter + 1.
    ^id
!!


!Display methodsFor: 'private'!

init: xStream
    | vendorLen numScreens numFormats |

    idCounter _ 0.

    socket _ xStream.

    majorVersion _ socket uword.
    minorVersion _ socket uword.
    socket uword.		"skip length"

    releaseNum _ socket ulong.
    resourceIdBase _ socket ulong.
    BaseId _ resourceIdBase.
    resourceIdMask _ socket ulong.
    BaseMask _ resourceIdMask.

    motionBufferSize _ socket ulong.
    vendorLen _ socket uword.
    maxRequestLen _ socket uword.
    numScreens _ socket ubyte.
    numFormats _ socket ubyte.

    imageByteOrder _ socket byte.
    bitmapBitOrder _ socket byte.
    bitmapFormatScanUnit _ socket ubyte.
    bitmapFormatScanPad _ socket ubyte.
    minKeycode _ socket ubyte.
    maxKeycode _ socket ubyte.
    socket long.			"ignored "
    
    vendorName _ socket getString: vendorLen.    

    formats _ Array new: numFormats.
    screens _ Array new: numScreens.

    1 to: numFormats do: 
	[ :i | formats at: i put: (Format new: socket) ].
    1 to: numScreens do: 
	[ :i | screens at: i put: (Screen new: socket) ]
!!


!Format class methodsFor: 'instance creation'!

new: xStream
    ^self new init: xStream
!!

!Format methodsFor: 'private'!

init: socket
    depth _ socket ubyte.
    bitsPerPixel _ socket ubyte.
    scanlinePad _ socket ubyte.
    5 timesRepeat: [ socket ubyte ] "skip trailing junk"
!!


!Screen class methodsFor: 'instance creation'!

new: xStream
    ^self new init: xStream
!!

!Screen methodsFor: 'private'!

init: socket
    | depthsAllowed |

    window _ socket ulong.
    RootWindowID isNil
	ifTrue: [ RootWindowID _ window ]. "only want first one"
    defaultColormap _ socket ulong.
    whitePixel _ socket ulong.
    blackPixel _ socket ulong.

WhitePixel _ whitePixel.
BlackPixel _ blackPixel.

    currentInputMasks _ socket ulong.
    widthPixels _ socket uword.
    heightPixels _ socket uword.
    widthMM _ socket uword.
    heightMM _ socket uword.

    minInstalledMaps _ socket uword.
    maxInstalledMaps _ socket uword.
    rootVisual _ socket ulong.
    backingStores _ socket byte.

    saveUnders _ socket byte.
    rootDepth _ socket byte.
    depthsAllowed _ socket byte.

    depths _ Array new: depthsAllowed.
    
    1 to: depthsAllowed do:
	[ :i | depths at: i put: (Depth new: socket) ]
!!


!Depth class methodsFor: 'instance creation'!

new: xStream
    ^self new init: xStream
!!


!Depth methodsFor: 'private'!

init: socket
    | numVisuals |
    depth _ socket byte.

    socket byte.		" ignore "
    numVisuals _ socket word.
    socket long.		" pad "
    
    visuals _ Array new: numVisuals.

    1 to: numVisuals do:
	[ :i | visuals at: i put: (VisualType new: socket) ]
!!




!VisualType class methodsFor: 'instance creation'!

new: xStream
    ^self new init: xStream
!!


!VisualType methodsFor: 'accessing'!

id
    ^visualId
!!


!VisualType methodsFor: 'private'!

init: socket
    | visualId class bitsPerRGB colormapEntries redMask greenMask blueMask |

    visualId _ socket long.

VisualId isNil
    ifTrue: [ VisualId _ visualId ]. "pick up first (bw)"

    class _ socket byte.
  
"(#(StaticGray GrayScale StaticColor PseudoColor TrueColor DirectColor)
   at: class + 1) bugOut: 'Display type: '."

    bitsPerRGB _ socket byte.

    colormapEntries _ socket word.

    redMask _ socket long.
    greenMask _ socket long.
    blueMask _ socket long.

    socket long			" ignored "
!!



FileStream fileIn: 'Point.st'!
FileStream fileIn: 'Rectangle.st'!

FileStream fileIn: 'error.st'!
FileStream fileIn: 'event.st'!

FileStream fileIn: 'Atom.st'!

FileStream fileIn: 'TextItem.st'!

!X methodsFor: 'reading'!

getPacket
    | type err |
    type _ self byte.
    type == 0 ifTrue: [ err _ XError newFrom: self.
			err inspect.
			^nil ].
    type > 1 ifTrue: [ ^XEvent newFrom: self type: type ]
!

getReply
    | err |
    (self byte) == 0
	ifTrue: [ err _ XError newFrom: self.
		  err inspect.
		  ^false ].
    self byte.			"skip the unused reply value"
    self word.			"skip the sequence number"
    self long.			"skip the length"
    ^true
!!

FileStream fileIn: 'Arc.st'!
FileStream fileIn: 'Drawable.st'!
FileStream fileIn: 'GC.st'!
FileStream fileIn: 'Window.st'!
FileStream fileIn: 'Pixmap.st'!
FileStream fileIn: 'Pen.st'!
