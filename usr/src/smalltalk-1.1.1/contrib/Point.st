"=====================================================================
|    Point Class Definitions
 =====================================================================

By Doug McCallum <uunet!ico.isc.com!dougm>
Additions by uunet!nfsun!nfstar!sbyrne (Steve Byrne)
"

"
|     Change Log
| ============================================================================
| Author       Date       Change 
| dougm	     25 Apr 90	  Some cleanup plus merged changes by sbyrne
|
| dougm	     16 Apr 90	  Created basic Point class.
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

"move to Number ??? "
"VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"
!Number methodsFor: 'point creation'!

@ y
    ^Point x: self y: y
!

asPoint
    ^Point x: self y: self
!!
"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"

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
!

asPoint
    ^self			"But I already am a point!"
!!

!Point methodsFor: 'arithmetic'!

+ delta
    | deltapt |
    deltapt _ delta asPoint.
    ^Point x: x + deltapt x y: y + deltapt y
!

- delta
    | deltapt |
    deltapt _ delta asPoint.
    ^Point x: x - deltapt x y: y - deltapt y
!

* scale
    |deltapt|
    deltapt _ scale asPoint.
    ^Point x: (x * deltapt x) y: (y * deltapt y)
!

/ scale
    | deltapt |
    deltapt _ scale asPoint.
    ^Point x: (x / deltapt x) y: (y / deltapt y)
!

// scale
    | deltapt |
    deltapt _ scale asPoint.
    ^Point x: (x // deltapt x) y: (y // deltapt y)
!

abs
    ^Point x: (x abs) y: (y abs)
!!

!Point methodsFor: 'truncation and round off'!
rounded
    ^Point x: (x rounded) y: (y rounded)
!

truncateTo: grid
    ^Point x: ((x // grid) * grid) y: (y // grid) * grid
!!

!Point methodsFor: 'comparing'!

< aPoint
    ^(x < (aPoint x)) and: [ (y < (aPoint y)) ]
!

> aPoint
    ^(x > (aPoint x)) and: [ (y > (aPoint y)) ]
!

<= aPoint
    ^(self > aPoint) not	"unverified"
!

>= aPoint
    ^(self < aPoint) not	"unverified"
!

max: aPoint
    (self>aPoint)
	ifTrue: [^self]
	ifFalse:[^aPoint]
!

min: aPoint
    (self<aPoint)
	ifTrue: [^self]
	ifFalse:[^aPoint]
!!    

!Point methodsFor: 'point functions'!

dist: aPoint
    | a b |
    a _ x - (aPoint x).
    b _ y - (aPoint y).
    ^((a squared)+(b squared)) sqrt
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
