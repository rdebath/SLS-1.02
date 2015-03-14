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



XError subclass: #RequestError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!RequestError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!RequestError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #ValueError
	instanceVariableNames: 'badValue '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ValueError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ValueError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badValue _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #WindowError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!WindowError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!WindowError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #PixmapError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!PixmapError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!PixmapError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #AtomError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!AtomError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!AtomError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #CursorError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!CursorError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!CursorError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #FontError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!FontError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!FontError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #MatchError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!MatchError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!MatchError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #DrawableError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!DrawableError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!DrawableError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #AccessError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!AccessError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!AccessError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #AllocError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!AllocError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!AllocError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #ColormapError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ColormapError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ColormapError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #GContextError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!GContextError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!GContextError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #IDChoiceError
	instanceVariableNames: 'badId '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!IDChoiceError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!IDChoiceError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    badId _ aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #NameError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!NameError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!NameError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #LengthError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!LengthError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!LengthError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


XError subclass: #ImplementationError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ImplementationError class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ImplementationError methodsFor: 'private'!

initFrom: aStream
    sequenceNumber _ aStream word. 
    aStream long. 
    minorOp _ aStream word. 
    majorOp _ aStream byte. 
    aStream skipBytes: 21
!!


