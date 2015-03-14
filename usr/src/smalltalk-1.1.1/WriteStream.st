"======================================================================
|
|   WriteStream Method Definitions
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
| sbyrne     20 May 90	  Fixed semantics of write streams so that they return
|                         only the characters that have been written to them.
|
| sbyrne     19 Sep 89	  Changed to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

PositionableStream subclass: #WriteStream
		   instanceVariableNames: 'maxSize'
		   classVariableNames: ''
		   poolDictionaries: ''
		   category: nil.

WriteStream comment: 
'I am the class of writeable streams.  I only allow write operations to
my instances; reading is strictly forbidden.' !

!WriteStream class methodsFor: 'instance creation'!

on: aCollection
    ^(self new initCollection: aCollection) access: 2
!

with: aCollection
    | stream |
    stream _ self on: aCollection.
    stream moveToEnd.
    ^stream
!

with: aCollection from: firstIndex to: lastIndex
    | stream |
    stream _ self on: aCollection fromFirstIndex to: lastIndex.
    stream moveToEnd.
    ^stream
!!



!WriteStream methodsFor: 'accessing-writing'!

nextPut: anObject
    (access bitAnd: 2) = 0
       ifTrue: [ ^self shouldNotImplement ].
    ptr > maxSize ifTrue: [ self growCollection ].
    collection at: ptr put: anObject.
    ptr > endPtr ifTrue: [ endPtr _ ptr ].
    ptr _ ptr + 1.
    ^anObject
!!



!WriteStream methodsFor: 'positioning'!

setToEnd
    ptr _ endPtr + 1
!!



!WriteStream methodsFor: 'character writing'!

cr
    self nextPut: Character cr
!

nl
    self nextPut: Character nl
!

crTab
    self cr.
    self tab
!

nlTab
    self nl.
    self tab
!

space
    self nextPut: Character space
!

tab
    self nextPut: Character tab
!!



!WriteStream methodsFor: 'private methods'!

initCollection: aCollection
    collection _ aCollection.
    ptr _ 1.
    endPtr _ 0.
    maxSize _ aCollection size
!

moveToEnd
    endPtr _ collection size.
    self setToEnd
!

workingSize
    "Return the actual number of valid elements in the stream"
    ^ptr max: endPtr
!

growCollection
    collection grow.
    maxSize _ collection size

!!

