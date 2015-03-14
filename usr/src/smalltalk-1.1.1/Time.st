"======================================================================
|
|   Time Method Definitions
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
| sbyrne     24 Apr 90	  Fixed printOn: to print the instance time, instead of
|			  now, and added printOn: for Time class to print now.
|
| sbyrne     19 Sep 89	  Changed to use real method categories.
|
| sbyrne     12 Aug 89	  Implemented many methods.  The book is exceptionally
|			  vague here, so please feel free to change the
|			  behavior to something which is more correct.
|
| sbyrne     25 Apr 89	  created.
|
"

Magnitude subclass: #Time
	  instanceVariableNames: 'seconds'
	  classVariableNames: ''
	  poolDictionaries: ''
	  category: nil.

Time comment: 
'My instances represent times of the day.  I provide methods for instance 
creation, methods that access components (hours, minutes, and seconds) of a 
time value, and a block execution timing facility.' !

!Time class methodsFor: 'basic'!

now
    ^self new setSeconds: Time secondClock
!

fromSeconds: secondCount
    ^self new setSeconds: (Time secondClock \\ (24*60*60)) + secondCount
!

millisecondClockValue
    ^self millisecondClock
!

millisecondsToRun: timedBlock
    | startTime|
    startTime _ self millisecondClock.
    timedBlock value.
    ^self millisecondClock - startTime
!!



!Time methodsFor: 'accessing'!

hours
    ^(seconds // (60*60)) \\ 24
!

minutes
    ^(seconds // 60) \\ 60
!

seconds
    ^seconds \\ 60
!!



!Time methodsFor: 'arithmetic'!

addTime: timeAmount
    ^Time new setSeconds: seconds + timeAmount
!

subtractTime: timeAmount
    ^Time new setSeconds: seconds - timeAmount
!

printOn: aStream
    self hours printOn: aStream.
    aStream nextPut: $:.
    self minutes < 10 ifTrue: [ aStream nextPut: $0 ].
    self minutes printOn: aStream.
    aStream nextPut: $:.
    self seconds < 10 ifTrue: [ aStream nextPut: $0 ].
    self seconds printOn: aStream
!!



!Time methodsFor: 'private'!

setSeconds: secs
    seconds _ secs
!!
