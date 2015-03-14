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



!GC class methodsFor: 'instance creation'!

new: aDisplay drawable: aDrawable
    ^super new init: aDisplay drawable: aDrawable
!!

!GC methodsFor: 'X hacking'!

changeGC: aBlock
    | packet gc |
    
    packet _ XGCAttrPacket command: 56.

    packet long: self id.
    aBlock notNil
	ifTrue: [ aBlock value: packet ]
	ifFalse: [ packet noBits ].
    display socket bytes: packet done.
!

polyPoint: pointsVec coordMode: coordMode
    | packet |
    
    packet _ XPacket command: 64 
		     aux: (X map: coordMode into: #(Origin Previous)).
    packet long: drawable id; long: self id.
    pointsVec do: [ :point | packet point: point ].
    display socket bytes: packet done
!    

polyLine: pointsVec coordMode: coordMode 
    | packet |
    
    packet _ XPacket command: 65 
		     aux: (X map: coordMode into: #(Origin Previous)).
    packet long: drawable id; long: self id.
    pointsVec do: [ :point | packet point: point ].
    display socket bytes: packet done
!	

polyLine: aPoint to: otherPoint coordMode:  coordMode
    | packet |
    
    self polyLine: (Array with: aPoint with: otherPoint) 
	  coordMode: coordMode
!


polySegment: segmentVec
    | packet |

    packet _ XPacket command: 66. "segments are currently implemented using rectangles"
    packet long: drawable id; long: self id.
    segmentVec do: [ :seg | packet rectangle: seg ].
    display socket bytes: packet done
!


polyRectangle: rectangleVec
    | packet |    

    (rectangleVec isMemberOf: Rectangle) "ugh"
	ifTrue: [ rectangleVec _ Array with: rectangleVec ].

    packet _ XPacket command: 67.
    packet long: drawable id; long: self id.
    rectangleVec do: [ :rect | packet point: rect origin.
			       packet point: rect extent ].
    display socket bytes: packet done
!
    
polyArc: arcs
    | packet |    

    packet _ XPacket command: 68.
    packet long: drawable id; long: self id.
    arcs do: [ :arc | packet arc: arc ].
    display socket bytes: packet done
!

"fillPoly"

polyFillRectangle: rectangles
    | packet |

    packet _ XPacket command: 70.
    packet long: drawable id; long: self id.
    rectangles do: [ :rect | packet rectangle: rect ].
    display socket bytes: packet done
!    


" ... "

polyText8: textItems x: x y: y
    | packet |
    
    packet _ XPacket command: 74.
    packet long: drawable id; long: self id; word: x; word: y.
    textItems do: [ :textItem | packet textItem: textItem ].
    display socket bytes: packet done
!!


!GC methodsFor: 'accessing'!

id
    ^id
!!

!GC methodsFor: 'example class'!


drawExampleImage: button
    self polyText8: (Array with: (TextItem onString: 'Welcome to ')
			 with: (TextItem onString: Version ))
       x: 50 y: 30.

    self polyLine: 20@20 to: 40@40 coordMode: #Origin.
    self polyLine: 40@20 to: 20@40 coordMode: #Origin.
	
    self polyRectangle: (Array with: (10 @ 10 corner: (400 - 20) @ (200 - 20))). 
    self drawExampleButton: button.
"    self polyFillRectangle: (Array with:  (10 @ 10 corner: 50 @ 50))."
    self polyArc: (Array with: (Arc new: 10 @ 10 size: 40 @ 40 angles: (90*64) @ (180*64))).
    self polyArc: (Array with: (Arc new: 10 @ 50 size: 40 @ 40 angles: (270*64) @ (180*64)))
!


drawExampleButton: button
    | textStart |
    self polyRectangle: (Array with: button). 
    textStart _ button leftCenter.
    self polyText8: (Array with: (TextItem onString: 'Click here to exit'))
	 x: textStart x + 10 y: textStart y
!!


!GC methodsFor: 'private'!

init: aDisplay drawable: aDrawable
    display _ aDisplay.
    drawable _ aDrawable.
    id _ display nextId.
    Registry at: id put: self
	
!!


