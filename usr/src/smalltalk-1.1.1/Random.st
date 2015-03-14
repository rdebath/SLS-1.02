"======================================================================
|
|   Random number Method Definitions
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
| sbyrne     19 Sep 89	  Converted to use real method categories.
|
| sbyrne      3 Jul 89	  created.
|
"

Stream subclass: #Random
       instanceVariableNames: 'seed'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Random comment: "Here's a random comment :-)"
'My instances are generator streams that produce random numbers, which are 
floating point values between 0 and 1.'!

!Random class methodsFor: 'instance creation'!

new
    ^self basicNew setSeed
!!



!Random methodsFor: 'testing'!

chiSquare
    "returns on Sun3 93.40000000000009"
    ^self chiSquare: 1000 range: 100
!

chiSquare: n range: r
    | f t seed |
    seed _ 1234567.
    f _ Array new: r + 1.
    1 to: r + 1 do: [ :i | f at: i put: 0 ].
    n timesRepeat:
	[ seed _ (seed * 31415821) + 1 bitAnd: 16r3FFFFFFF.
    	  t _ seed \\ r.
	  f at: t + 1 put: (f at: t + 1) + 1 ].
    t _ 0.
    1 to: r do: [ :i | t _ t + (f at: i) squared ].
    ^r asFloat * t / n - n

!!


!Random methodsFor: 'basic'!

atEnd
    ^false
!

next
    | value |
    "From Sedgewick's 'Algorithms', based on Lehmer's method"
    seed _ (seed * 31415821) + 1 bitAnd: 16r3FFFFFFF.
    ^seed / 16r3FFFFFFF.0
!

nextPut: value
    self shouldNotImplement
!

next: anInteger
    | collection |
    collection _ OrderedCollection new.
    anInteger timesRepeat: [ collection add: self next ]. 
    ^collection
!

nextMatchFor: aNumber
    ^self next = aNumber
!!



!Random methodsFor: 'private'!

setSeed
    seed _ Time secondClock
!!
