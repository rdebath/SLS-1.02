"=====================================================================
|    Point Class Definitions
 ====================================================================="


"======================================================================
|
| Copyright (C) 1990, 1991 Free Software Foundation, Inc.
| By Doug McCallum <uunet!ico.isc.com!dougm>
| Additions by sbb@eng.sun.com (Steve Byrne)
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
| sbb	     13 Sep 91	  Added = for Points (= from Object isn't right)
|
| dougm      16 Apr 90    Created basic Point class.
|
"

Object subclass: #Point
       instanceVariableNames: 'x y'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil !

Point comment:
'Beginning of a Point class for simple display manipulation.  Has not been
 exhaustively tested but appears to work for the basic primitives and for
 the needs of the Rectangle class.' !

!Number methodsFor: 'point creation'!

@ y
    ^ Point x: self y: y
!

asPoint
    ^Point x: self y: self
!!


!Point class methodsFor: 'instance creation'!

x: xInteger y: yInteger
    ^self new x: xInteger y: yInteger
!!

!Point methodsFor: 'printing'!

printOn: aStream
    x printOn: aStream.
    '@' printOn: aStream.
    y printOn: aStream
!!

!Point methodsFor: 'storing'!

storeOn: aStream
    x storeOn: aStream.
    '@' storeOn: aStream.
    y storeOn: aStream
!!

!Point methodsFor: 'accessing'!

x
    ^x
!

y
    ^y
!

x: aNumber
    x _ aNumber
!

y: aNumber
    y _ aNumber
!

x: anXNumber y: aYNumber
    x _ anXNumber.
    y _ aYNumber
!!

!Point methodsFor: 'converting'!

asPoint
    ^self			"But I already AM a point!"
!

hash
    ^(x abs + y abs) truncated
!!

!Point methodsFor: 'arithmetic'!

+ delta
    delta _ delta asPoint.
    ^Point x: (x + delta x) y: (y + delta y)
!

- delta
    delta _ delta asPoint.
    ^Point x: (x - delta x) y: (y - delta y)
!

* scale
    scale _ scale asPoint.
    ^Point x: (x * scale x) y: (y * scale y)
!

/ scale
    scale _ scale asPoint.
    ^Point x: (x / scale x) y: (y / scale y)
!

// scale
    scale _ scale asPoint.
    ^Point x: (x // scale x) y: (y // scale y)
!

abs
    ^Point x: (x abs) y: (y abs)
!!

!Point methodsFor: 'truncation and round off'!

rounded
    ^Point x: (x rounded) y: (y rounded)
!

truncateTo: grid
    ^Point x: (x truncateTo: grid) y: (y truncateTo: grid)

!!


!Point methodsFor: 'comparing'!

= aPoint
    ^(x = aPoint x) and: [ y = aPoint y ]
!

< aPoint
    ^(x < aPoint x) and: [ (y < aPoint y) ]
!

> aPoint
    ^(x > aPoint x) and: [ (y > aPoint y) ]
!

<= aPoint
    ^(self > aPoint) not	"unverified"
!

>= aPoint
    ^(self < aPoint) not	"unverified"
!

max: aPoint
    (self >aPoint )
       ifTrue: [ ^self ]
       ifFalse:[ ^aPoint ]
!

min: aPoint
    (self < aPoint)
       ifTrue: [^ self ]
       ifFalse:[^ aPoint ]
!!

!Point methodsFor: 'point functions'!

dist: aPoint
    | a b |
    a _ x - (aPoint x).
    b _ y - (aPoint y).
    ^((a squared) + (b squared)) sqrt
!

dotProduct: aPoint
    ^(x * aPoint x) + (y * aPoint y)
!

grid: aPoint
    ^Point x: (x roundTo: (aPoint x)) y: (y roundTo: (aPoint y))
!

normal
"rotate the Point 90degrees clockwise and get the unit vector"
    |len|
    len _ ((x squared) + (y squared)) sqrt.
    ^Point x: ((y asFloat negated)/len) y: (x/len)
!

transpose
    ^Point x: y y: x
!

truncatedGrid: aPoint
    ^Point x: (x truncateTo: (aPoint x)) y: (y truncateTo: (aPoint y))
!!



