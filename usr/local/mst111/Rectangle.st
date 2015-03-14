"========================================================================
|   Rectangle Class
 ========================================================================"

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
| sbb	     13 Sep 91	  Added #= and #hash methods.
|
| dougm      19 Apr 90    Initial definitions for Rectangle class (needs Point)
|
"

Object subclass: #Rectangle
       instanceVariableNames: 'origin corner'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil !

Rectangle comment:
'Beginning of the Rectangle class for simple display manipulation.
 Rectangles require the Point class to be available.  An extension
 to the Point class is made here that since it requires Rectangles
 to be defined (see converting)' !

!Rectangle class methodsFor: 'instance creation'!

origin: originPoint corner: cornerPoint
    ^self new origin: originPoint corner: cornerPoint
!

origin: originPoint extent: extentPoint
    ^self new origin: originPoint corner: (originPoint + extentPoint)
!

left: leftNumber right: rightNumber top: topNumber bottom: bottomNumber
    ^self new origin: (Point x: leftNumber y: topNumber) corner: (Point x: rightNumber y: bottomNumber)
!!

!Rectangle methodsFor: 'accessing'!

origin: originPoint corner: cornerPoint
    origin _ originPoint.
    corner _ cornerPoint
!

origin
    ^origin
!

corner
       ^corner
!

topLeft
    ^origin
!

topCenter
    ^Point x: (origin x + (self width / 2)) y: (origin y)
!

topRight
    ^Point x: (corner x) y: (origin y)
!

rightCenter
    ^Point x: (corner x) y: (origin y + (self height / 2))
!

bottomCenter
    ^Point x: (origin x + (self width / 2)) y: (corner y)
!

bottomLeft
    ^Point x: (origin x) y: (corner y)
!

bottomRight
    ^corner
!

leftCenter
    ^Point x: (origin x) y: (origin y + (self height / 2))
!

center
    ^Point x: (origin x + (self width / 2))
	   y: (origin y + (self height / 2))
!

area
    ^ (self width) * (self height)
!

width
    ^(corner x) - (origin x)
!

height
    ^(corner y) - (origin y)
!

extent
    ^Point x: (self width) y: (self height)
!

top
    ^(origin y)
!

right
    ^(corner x)
!

bottom
    ^(corner y)
!

left
    ^(origin x)
!!


"Not yet implemented: messages to set the locations as mentioned on p 346"



!Rectangle methodsFor: 'testing'!

containsPoint: aPoint
    ^(origin < aPoint) & (corner > aPoint)
!

contains: aRectangle
    ^(self containsPoint: (aRectangle origin)) &
       (self containsPoint: (aRectangle corner))
!

intersects: aRectangle
    ^(self containsPoint: aRectangle topLeft) or:
       [(self containsPoint: aRectangle topRight) or:
           [(self containsPoint: aRectangle bottomLeft) or:
               [(self containsPoint: aRectangle bottomRight)]]]
!

= aRectangle
    ^(origin = aRectangle origin)
	and: [ corner = aRectangle corner ]
!

hash
    ^(origin hash) * (corner hash)
!!

!Rectangle methodsFor: 'rectangle functions'!

amountToTranslateWithin: aRectangle
    ^(aRectangle origin)-origin
!

"----------------------------------------------------------------
|areasOutside: aRectangle
| most complicated of the Rectangle primitives
| The basic methodology is to first determine that there is an
| intersection by finding the overlapping rectangle.  From the
| overlapping rectangle, first determine if it runs along an edge.
| If it doesn't, extend the rectangle up to the top edge and add
| the new rectangle to the collection and start the rest of the
| process.  If the left edge does not touch the left edge of self,
| extend it to the edge saving the new rectangle.  Then do the
| same to the right edge.  Then check top and bottom edges.  Most
| of the time only 2 or 3 rectangles get formed, occasionally 4.
| It should be possible to never get more than 3 but requires more
| work.
 ----------------------------------------------------------------"

areasOutside: aRectangle
    | collect iRect tmp |
    iRect _ self intersect: aRectangle.
    (iRect = nil) ifTrue: [^nil]. "case of no intersection"
                               "the collect collection gathers Rectangles"
    collect _ OrderedCollection new: 4.
                               "is it floating or on the edge?"
    (((iRect top) ~= self top) &
             ((iRect bottom) ~= self bottom) &
                   ((iRect left) ~= self left) & ((iRect right) ~= self right))
       ifTrue: "entirely in the center."
           [tmp _ Rectangle origin: (Point x: iRect left y: self top)
                            corner: iRect bottomRight.
            collect add: tmp.
            iRect _ iRect merge: tmp].
    ((iRect left) ~= self left)
       ifTrue:                 "doesn't touch left edge so make it touch"
           [tmp _ Rectangle origin: (Point x: self left y: iRect top)
                                corner: iRect bottomLeft.
                collect add: tmp.
                               "merge new (tmp) with overlap to keep track"
                iRect _ iRect merge: tmp].
    ((iRect right) ~= self right)
       ifTrue:                 "doesn't touch right edge so extendi it"
           [tmp _ Rectangle origin: iRect topRight
                                corner: (Point x: self right y: iRect bottom).
                collect add: tmp.
                iRect _ iRect merge: tmp].
    (((iRect left) ~= self left) | ((iRect top) ~= self top))
       ifTrue:                 "whole top part can be taken now"
           [tmp _ Rectangle origin: origin corner: iRect topRight.
                collect add: tmp].
    (((iRect right) ~= self right) | ((iRect bottom) ~= self bottom))
       ifTrue:                 "whole bottom open and can be taken"
           [tmp _ Rectangle origin: iRect bottomLeft corner: corner.
                collect add: tmp].
    ^collect
!

expandBy: delta
    delta _ delta asRectangle.	"not completed"
    (delta isMemberOf: Point) ifTrue:
       [^Rectangle origin: (origin-delta)
                   corner: (corner+delta)].
    (delta isMemberOf: Rectangle) ifTrue:
       [^Rectangle origin: (origin-(delta origin))
                   corner: (corner+(delta corner))].
    (delta isKindOf: Number) ifTrue:
       [^Rectangle left: (origin x)-delta
                   right:(corner x)+delta
                   top:  (origin y)-delta
                   bottom:(corner y)+delta].
    self error: 'Illegal delta value'
!

insetBy: delta
    (delta isMemberOf: Point) ifTrue:
       [^Rectangle origin: (origin+delta)
                   corner: (corner-delta)].
    (delta isMemberOf: Rectangle) ifTrue:
       [^Rectangle origin: (origin+(delta origin))
                   corner: (corner-(delta corner))].
    (delta isKindOf: Number) ifTrue:
       [^Rectangle left: (origin x)+delta
                   right:(corner x)-delta
                   top:  (origin y)+delta
                   bottom:(corner y)-delta].
    self error: 'Illegal delta value'
!

insetOriginBy: originDeltaPoint corner: cornerDeltaPoint
    ^Rectangle origin: origin + originDeltaPoint
              corner: corner + cornerDeltaPoint
!

merge: aRectangle
    | orig corn |
    orig _ Point x: ((origin x) min: (aRectangle origin x))
                y: ((origin y) min: (aRectangle origin y)).
    corn _ Point x: ((corner x) max: (aRectangle corner x))
                y: ((corner y) max: (aRectangle corner y)).
    ^Rectangle origin: orig corner: corn
!

"--------------------------------------------------------------
| A intersect: B
|   returns the rectangle (if any) created by the overlap of
|   rectangles A and B.  There are 10 possible overlap situations:
|      A inside B
|      B inside A
|      A overlaps B at one of the four corners (1 point inside)
|      A overlaps B on one of the four sides (2 points inside)
 ---------------------------------------------------------------"

intersect: aRectangle
    (self contains: aRectangle) ifTrue: [^Rectangle origin: aRectangle origin
                                                   corner: aRectangle corner].
    (aRectangle contains: self)  ifTrue: [^Rectangle origin: origin
                                                    corner: corner].
    (self containsPoint: aRectangle topLeft)
       ifTrue: [ (self containsPoint: aRectangle topRight)
                     ifTrue: [^Rectangle origin: aRectangle origin
                                         corner: (Point x: aRectangle corner x
                                                       y: corner y)]
                     ifFalse: [^Rectangle origin: aRectangle origin
                                          corner: corner]].
    (self containsPoint: aRectangle topRight)
       ifTrue: [^Rectangle origin: (Point x: origin x y: aRectangle origin y)
                           corner: (Point x: aRectangle corner x y: corner y)].
    (self containsPoint: aRectangle bottomLeft)
       ifTrue: [ (self containsPoint: aRectangle bottomRight)
                     ifTrue: [^Rectangle origin: (Point x: aRectangle origin x
                                                        y: origin y)
                                         corner: aRectangle corner]
                     ifFalse: [^Rectangle origin: (Point x: aRectangle origin x
                                                         y: origin y)
                                          corner: aRectangle corner]].
    (self containsPoint: aRectangle bottomRight)
       ifTrue: [^Rectangle origin: origin corner: aRectangle corner]
       ifFalse: [^nil]
!!

!Rectangle methodsFor: 'printing'!

printOn: aStream
    origin printOn: aStream.
    ' corner: ' printOn: aStream.
    corner printOn: aStream
!

storeOn: aStream
    '(Rectangle origin: ' storeOn: aStream.
    origin storeOn: aStream.
    ' corner: ' storeOn: aStream.
    corner storeOn: aStream.
    ')' storeOn: aStream
!!

!Rectangle methodsFor: 'truncation and round off'!

rounded
    ^Rectangle origin: origin rounded corner: corner rounded
!!

!Rectangle methodsFor: 'transforming'!
moveBy: aPoint
    origin _ origin + aPoint.
    corner _ corner + aPoint
!

moveTo: aPoint
    | diff |
    diff _ aPoint - origin.
    origin _ aPoint.
    corner _ corner + diff
!

scaleBy: scale
    ^Rectangle origin: origin * scale corner: corner * scale
!

translateBy: factor
    ^Rectangle origin: origin + factor corner: corner + factor
!!

!Point methodsFor: 'converting'!

corner: aPoint
    ^Rectangle origin: self corner: aPoint
!

extent: aPoint
    ^Rectangle origin: self extent: aPoint
!!
