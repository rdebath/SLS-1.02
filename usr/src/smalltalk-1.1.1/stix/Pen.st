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

Object subclass: #Pen
       instanceVariableNames: 'gc direction marking location'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil
!



!Pen class methodsFor: 'instance creation'!

new: gc
    ^super new init: gc
!!


!Pen methodsFor: 'accessing'!

direction
    ^direction
!

location
    ^location
!

gc
    ^gc
!!



!Pen methodsFor: 'moving'!

down
    marking _ true
!

up
    marking _ false
!

turn: degrees
    direction _ direction + degrees
!

north
    direction _ 0
!

go: distance
    | dest dirRad |
    dirRad _ direction degreesToRadians.
    dest _ location + ((distance * dirRad sin) negated @ (distance * dirRad cos)).
    self goto: dest rounded
!

goto: aPoint
    marking
	ifTrue: [ gc polyLine: location to: aPoint coordMode: #Origin ].
    location _ aPoint
!


place: aPoint
    location _ aPoint
!

home
    location _ 0@0
!!


!Pen methodsFor: 'examples'!

spiral: n angle: a
    1 to: n do:
	[ :i | self go: i. 
	       self turn: a ]
!

dragon: n
    n = 0
	ifTrue: [ self go: 10 ]
	ifFalse:
	    [ n > 0
		  ifTrue:
		      [ self dragon: n - 1.
			self turn: 90.
			self dragon: 1 - n ]
		  ifFalse:
		      [ self dragon: -1 - n.
			self turn: -90.
			self dragon: 1 + n ] ]
!!
    


!Pen methodsFor: 'private'!

init: aGC
    marking _ false.
    location _ 0@0.
    direction _ 0.
    gc _ aGC
!!

